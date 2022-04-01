implementation module TextPicker.FindTexts.Basic

import StdEnv

import Control.Applicative
import Data.Func
import Data.List => qualified group
import qualified Data.Set
import Data.Set.GenJSON

import iTasks

import TextFabric

import TextPicker.Data
import TextPicker.FindTexts
import TextPicker.Vocabulary

:: VerbSettings =
	{ stems  :: !Stems
	, tenses :: !Tenses
	}

:: Stems =
	{ qal     :: !Bool
	, nifal   :: !Bool
	, piel    :: !Bool
	, pual    :: !Bool
	, hitpael :: !Bool
	, hifil   :: !Bool
	, hofal   :: !Bool
	, poal    :: !Bool
	, poel    :: !Bool
	, hitpoel :: !Bool
	}

:: Tenses =
	{ perfect              :: !Bool
	, imperfect            :: !Bool
	, wayyiqtol            :: !Bool
	, imperative           :: !Bool
	, infinitive_absolute  :: !Bool
	, infinitive_construct :: !Bool
	, participle           :: !Bool
	, participle_passive   :: !Bool
	}

:: RankingSettings =
	{ objective            :: !Objective
	, absolute_or_relative :: !AbsoluteOrRelative
	, extra_verb_weight    :: !Int
	}

:: Objective
	= MinimizeUnknownness
	| MaximizeKnownness

:: AbsoluteOrRelative
	= CountAbsoluteItems
	| CountRatio

derive class iTask \ gEditor VerbSettings
derive class iTask Stems, Tenses
derive gDefault VerbSettings, Stems, Tenses

derive class iTask RankingSettings, Objective, AbsoluteOrRelative
derive gDefault RankingSettings, Objective, AbsoluteOrRelative

gEditor{|VerbSettings|} purpose =
	mapEditorWrite (uncurry (liftA2 \stems tenses -> {stems=stems, tenses=tenses})) $
	mapEditorRead (\vs -> (vs.stems, vs.tenses)) $
	container1 $
	classAttr ["itasks-horizontal"] @>>
	container2 (gEditor{|*|} purpose) (gEditor{|*|} purpose)

stems :: !VerbSettings -> 'Data.Set'.Set String
stems {stems} = 'Data.Set'.fromList $ catMaybes
	[ ?Just "NA"
	, if stems.qal     (?Just "qal")  ?None
	, if stems.nifal   (?Just "nif")  ?None
	, if stems.piel    (?Just "piel") ?None
	, if stems.pual    (?Just "pual") ?None
	, if stems.hitpael (?Just "hit")  ?None
	, if stems.hifil   (?Just "hif")  ?None
	, if stems.hofal   (?Just "hof")  ?None
	, if stems.poal    (?Just "poal") ?None
	, if stems.poel    (?Just "poel") ?None
	, if stems.hitpoel (?Just "htpo") ?None
	]

tenses :: !VerbSettings -> 'Data.Set'.Set String
tenses {tenses} = 'Data.Set'.fromList $ catMaybes
	[ ?Just "NA"
	, if tenses.perfect              (?Just "perf") ?None
	, if tenses.imperfect            (?Just "impf") ?None
	, if tenses.wayyiqtol            (?Just "wayq") ?None
	, if tenses.imperative           (?Just "impv") ?None
	, if tenses.infinitive_absolute  (?Just "infa") ?None
	, if tenses.infinitive_construct (?Just "infc") ?None
	, if tenses.participle           (?Just "ptca") ?None
	, if tenses.participle_passive   (?Just "ptcp") ?None
	]

selectedVocabularyLists :: SimpleSDSLens [String]
selectedVocabularyLists =: sdsFocus "basic-selectedVocabularyLists.json" $ jsonFileStore "TextPicker" False False (?Just [])

verbSettings :: SimpleSDSLens VerbSettings
verbSettings =: sdsFocus "basic-verbSettings.json" $ jsonFileStore "TextPicker" False False (?Just defaultValue)

rankingSettings :: SimpleSDSLens RankingSettings
rankingSettings =: sdsFocus "basic-rankingSettings.json" $ jsonFileStore "TextPicker" False False (?Just defaultValue)

findTexts :: Task ()
findTexts = findTextsTask
	[ Hint "Find texts with the following vocabulary:" @>>
		editSharedMultipleChoiceWithSharedAs [ChooseFromCheckGroup fst] vocabularyLists fst selectedVocabularyLists @! ()
	, Hint "Only recognize these verbal forms:" @>>
		updateSharedInformation [] verbSettings @! ()
	, Hint "Text selection settings:" @>>
		updateSharedInformation [] textSelectionSettings @! ()
	, Hint "Ranking settings:" @>>
		updateSharedInformation [] rankingSettings @! ()
	]
	getScoringFunction
where
	getScoringFunction =
		get selectedVocabularyLists >>- \selection ->
		get verbSettings >>- \verb_settings ->
		get rankingSettings >>- \ranking_settings ->
		get vocabularyLists
			@ filter (flip isMember selection o fst)
			@ concatMap snd @ 'Data.Set'.fromList >>- \chosenVocabulary ->
		loadDataSet >>- \data ->
		let (?Just [lex,vs,vt:_]) = get_node_feature_ids ["lex","vs","vt"] data in
		return (score ranking_settings chosenVocabulary (stems verb_settings) (tenses verb_settings) lex vs vt)

	score settings vocabulary stems tenses lex vs vt words =
		case settings.absolute_or_relative of
			CountAbsoluteItems ->
				case settings.objective of
					MinimizeUnknownness ->
						toReal (0 - length unknown_items)
					MaximizeKnownness ->
						toReal (length known_items)
			CountRatio ->
				toReal (length known_items) / toReal (length items)
	where
		items = repeatVerbs [(get_node_feature lex w, get_node_feature vs w, get_node_feature vt w) \\ w <- words]
		(known_items,unknown_items) = partition isKnown items

		repeatVerbs [] = []
		repeatVerbs [w=:(_,stem,_):ws]
			| stem == "NA"
				= [w:repeatVerbs ws]
				= repeatn (1+settings.extra_verb_weight) w ++ repeatVerbs ws

		isKnown (lexeme, stem, tense) =
			'Data.Set'.member stem stems &&
			'Data.Set'.member tense tenses &&
			'Data.Set'.member lexeme vocabulary
