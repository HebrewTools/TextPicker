implementation module TextPicker.FindTexts.Advanced

import StdEnv

import Control.Applicative
import Data.Func
import Data.List => qualified group
import qualified Data.Set
import Data.Set.GenJSON
import Text
import Text.HTML

import iTasks

import TextFabric

import TextPicker.Data
import TextPicker.FindTexts
import TextPicker.Vocabulary

:: WeighSettings :== [WeighSettingsItem]

:: WeighSettingsItem =
	{ filters :: ![Filter]
	, weight  :: !Int
	}

:: Filter
	= FeatureEquals !Feature !String
	| FromVocabulary

:: Feature
	= Lexeme
	| VerbalStem
	| VerbalTense
	| NominalState
	| Person | Gender | Number
	| PersonSuffix | GenderSuffix | NumberSuffix

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

derive class iTask WeighSettingsItem, Filter, Feature
derive gDefault WeighSettingsItem, Filter, Feature

selectedVocabularyLists :: SimpleSDSLens [String]
selectedVocabularyLists =: sdsFocus "advanced-selectedVocabularyLists.json" $ jsonFileStore "TextPicker" False False (?Just [])

weighSettings :: SimpleSDSLens WeighSettings
weighSettings =: sdsFocus "basic-weighSettings.json" $ jsonFileStore "TextPicker" False False (?Just defaultValue)

findTexts :: Task ()
findTexts = findTextsTask
	[ Hint "Selected vocabulary (for 'From vocabulary' filters):" @>>
		editSharedMultipleChoiceWithSharedAs [ChooseFromCheckGroup fst] vocabularyLists fst selectedVocabularyLists @! ()
	, Hint "Filters and their weight:" @>>
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
					]
				]
			) @! ()
	, Hint "Text selection settings:" @>>
		updateSharedInformation [] textSelectionSettings @! ()
	]
	getScoringFunction
where
	getScoringFunction =
		get selectedVocabularyLists >>- \selection ->
		get weighSettings >>- \weigh_settings ->
		get vocabularyLists
			@ filter (flip isMember selection o fst)
			@ concatMap snd @ 'Data.Set'.fromList >>- \chosenVocabulary ->
		loadDataSet >>- \data ->
		return (score weigh_settings chosenVocabulary data)

	score settings vocabulary data words = toReal (sum (map (scoreWord settings) words)) / toReal (length words)
	where
		scoreWord [] _ = 0
		scoreWord [{filters,weight}:rest] word
			| all matches filters
				= weight
				= scoreWord rest word
		where
			matches (FeatureEquals f val)
				# (?Just f) = get_node_feature_id (toString f) data
				= get_node_feature f word == val
			matches FromVocabulary
				# (?Just lex) = get_node_feature_id "lex" data
				= 'Data.Set'.member (get_node_feature lex word) vocabulary
