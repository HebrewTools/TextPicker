implementation module TextPicker.FindTexts.Advanced

import StdEnv

import Control.Applicative
import Control.Monad => qualified return, join, forever, sequence
import Data.Func
import Data.Functor
import Data.List => qualified group
import qualified Data.Set
import Data.Set.GenJSON
import Text
import Text.HTML

import iTasks

import Regex

import TextFabric

import TextPicker.Data
import TextPicker.FindTexts
import TextPicker.Vocabulary

:: EditableWeighSettings :== WeighSettings Feature String
:: PreparedWeighSettings :== WeighSettings FeatureId CompiledRegex

:: WeighSettings feature regex :== [WeighSettingsItem feature regex]

:: WeighSettingsItem feature regex =
	{ filters :: ![Filter feature regex]
	, weight  :: !Int
	}

:: Filter feature regex
	= FeatureEqualsOneOf !feature ![String]
	| LexemeRegex !regex
	| FromVocabulary
	| NOT !(Filter feature regex)

:: Feature
	= Lexeme
	| VerbalStem
	| VerbalTense
	| NominalState
	| Person | Gender | Number
	| PersonSuffix | GenderSuffix | NumberSuffix
	| PhraseDependentPartOfSpeech

instance toString Feature
where
	toString Lexeme = "lex"
	toString VerbalStem = "vs"
	toString VerbalTense = "vt"
	toString NominalState = "st"
	toString Person = "ps"
	toString Gender = "gn"
	toString Number = "nu"
	toString PersonSuffix = "prs_ps"
	toString GenderSuffix = "prs_gn"
	toString NumberSuffix = "prs_nu"
	toString PhraseDependentPartOfSpeech = "pdp"

derive class iTask WeighSettingsItem, Filter, Feature
derive gDefault WeighSettingsItem, Filter, Feature

selectedVocabularyLists :: SimpleSDSLens [String]
selectedVocabularyLists =: sdsFocus "advanced-selectedVocabularyLists.json" $ jsonFileStore "TextPicker" False False (?Just [])

weighSettings :: SimpleSDSLens EditableWeighSettings
weighSettings =: sdsFocus "advanced-weighSettings.json" $ jsonFileStore "TextPicker" False False (?Just defaultValue)

prepareWeighSettings :: !DataSet !EditableWeighSettings -> MaybeError String PreparedWeighSettings
prepareWeighSettings data settings = mapM prepareItem settings
where
	prepareItem {filters,weight} =
		mapM prepare filters >>= \filters ->
		pure
			{ filters = filters
			, weight  = weight
			}

	prepare (FeatureEqualsOneOf f vals) = case get_node_feature_id (toString f) data of
		?None -> Error "data did not contain the right features"
		?Just f -> Ok (FeatureEqualsOneOf f vals)
	prepare (LexemeRegex rgx) = case compileRegex rgx of
		Error e -> Error ("failed to compile regex: " +++ e)
		Ok rgx -> Ok (LexemeRegex rgx)
	prepare FromVocabulary = Ok FromVocabulary
	prepare (NOT f) = NOT <$> prepare f

findTexts :: Task ()
findTexts = findTextsTask
	[ Hint "Selected vocabulary (for 'From vocabulary' filters):" @>>
		editSharedMultipleChoiceWithSharedAs [ChooseFromCheckGroup fst] vocabularyLists fst selectedVocabularyLists @! ()
	, Hint "Filters and their weight (only the first matching filter applies):" @>>
		updateSharedInformation [] weighSettings @! ()
	, Hint "Feature values (for reference):" @>>
		viewInformation []
			( TableTag []
				[ TrTag [] [TdTag [] [Text feature], TdTag [] [Text (join "; " values)]]
				\\ (feature, values) <-
					[ ("Lexeme", ["any, capitalized in ETCBC transliteration"])
					, ("Verbal stem", ["hif","hit","htpo","hof","nif","piel","poal","poel","pual","qal"]) /* and Aramaic values... */
					, ("Verbal tense", ["perf","impf","wayq","impv","infa","infc","ptca","ptcp"])
					, ("Nominal state", ["a","c"]) /* and Aramaic values... */
					, ("Person (suffix)", ["p1","p2","p3","NA","unknown"])
					, ("Gender (suffix)", ["m","f","NA","unknown"])
					, ("Number (suffix)", ["sg","du","pl","NA","unknown"])
					, ("Part of speech", ["art","verb","subs","nmpr","advb","prep","conj","prps","prde","prin","intj","nega","inrg","adjv"])
					]
				]
			) @! ()
	, Hint "Text selection settings:" @>>
		updateSharedInformation [] textSelectionSettings @! ()
	]
	getScoringFunction
where
	getScoringFunction =
		loadDataSet >>- \data ->
		get weighSettings >>- \weigh_settings -> case prepareWeighSettings data weigh_settings of
			Error e ->
				throw e
			Ok weigh_settings ->
				get selectedVocabularyLists >>- \selection ->
				get vocabularyLists
					@ filter (flip isMember selection o fst)
					@ concatMap snd @ 'Data.Set'.fromList >>- \chosenVocabulary ->
				return (score weigh_settings chosenVocabulary data)

	score settings vocabulary data words = toReal (sum (map (scoreWord settings) words)) / toReal (length words)
	where
		scoreWord [] _ = 0
		scoreWord [{filters,weight}:rest] word
			| all matches filters
				= weight
				= scoreWord rest word
		where
			matches (FeatureEqualsOneOf f vals) = isMember (get_node_feature f word) vals
			matches (LexemeRegex rgx)
				# (?Just lex) = get_node_feature_id "lex" data
				= not (isEmpty (match rgx (get_node_feature lex word)))
			matches FromVocabulary
				# (?Just lex) = get_node_feature_id "lex" data
				= 'Data.Set'.member (get_node_feature lex word) vocabulary
			matches (NOT f) = not (matches f)
