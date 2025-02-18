implementation module TextPicker.EnglishAPI

import Data.Func
import System.Process
import Text
import Text.Encodings.UrlEncoding
import Text.HTML

import iTasks

:: EnglishAPISettings =
	{ api_key  :: !String
	, bible_id :: !String
	}

derive class iTask EnglishAPISettings

settings :: SimpleSDSLens EnglishAPISettings
settings =: sdsFocus "english_api_settings.json" $ jsonFileStore "TextPicker" False False $ ?Just
	{ api_key = ""
	, bible_id = "bf8f1c7f3f9045a5-01" // JPS 1917
	}

manageEnglishAPI :: Task ()
manageEnglishAPI =
	Hint "To get an API key, follow the instructions on https://docs.api.bible/getting-started/setup-an-account." @>>
	updateSharedInformation [] settings @! ()

derive JSONEncode ProcessResult

getEnglishText :: !String -> Task HtmlTag
getEnglishText query =
	get settings >>- \settings ->
	accWorldOSError (callProcessWithOutput "curl" (options settings) ?None) >>- \result
		| result.exitCode <> 0 ->
			throw ("curl failed with exit code " +++ toString result.exitCode)
		| otherwise ->
			case jsonQuery "data/passages/0/content" (fromString result.stdout) of
				?None -> throw ("failed to parse API response: " +++ result.stdout)
				?Just content -> return (Html (replaceSubString "</span>" " </span>" content))
where
	options settings =
		[ "-s"
		, "-X", "GET"
		, concat
			[ "https://api.scripture.api.bible/v1/bibles/",settings.bible_id,"/search?query="
			, urlEncodePairs
				[ ("query", query)
				, ("limit", "1")
				, ("sort", "relevance")
				]
			]
		, "-H", "accept: application/json"
		, "-H", "api-key: " +++ settings.api_key
		]
