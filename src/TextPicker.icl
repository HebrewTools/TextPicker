module TextPicker

import StdEnv
import StdOverloadedList

import Control.Applicative
import Data.Func
import Data.List => qualified group
import qualified Data.Set
import Data.Set.GenJSON
import Data.Tuple
import Text
import Text.HTML

import iTasks

import TextPicker.Data
import TextPicker.Vocabulary

import TextFabric
import TextFabric.Filters

import Bible

Start w = doTasks main w

main =
	tryToLoadData >-|
	allTasks
		[ findTexts <<@ Title "Find texts"
		, manageVocabulary <<@ Title "Manage vocabulary"
		]
	<<@ ArrangeWithTabs False

tryToLoadData =
	catchAll loadDataSet
	\_ ->
		Hint "Failed to load Text-Fabric data. Enter the correct path:" @>>
		updateSharedInformation [] dataPath >!|
		tryToLoadData

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

derive class iTask \ gEditor VerbSettings
derive class iTask Stems, Tenses
derive gDefault VerbSettings, Stems, Tenses

gEditor{|VerbSettings|} purpose =
	mapEditorWrite (uncurry (liftA2 \stems tenses -> {stems=stems, tenses=tenses})) $
	mapEditorRead (\vs -> (vs.stems, vs.tenses)) $
	container1 $
	classAttr ["itasks-horizontal"] @>>
	container2 (gEditor{|*|} purpose) (gEditor{|*|} purpose)

stems :: !VerbSettings -> 'Data.Set'.Set String
stems {stems} = 'Data.Set'.unions
	[ 'Data.Set'.singleton "NA"
	, if stems.qal     ('Data.Set'.singleton "qal")  'Data.Set'.newSet
	, if stems.nifal   ('Data.Set'.singleton "nif")  'Data.Set'.newSet
	, if stems.piel    ('Data.Set'.singleton "piel") 'Data.Set'.newSet
	, if stems.pual    ('Data.Set'.singleton "pual") 'Data.Set'.newSet
	, if stems.hitpael ('Data.Set'.singleton "hit")  'Data.Set'.newSet
	, if stems.hifil   ('Data.Set'.singleton "hif")  'Data.Set'.newSet
	, if stems.hofal   ('Data.Set'.singleton "hof")  'Data.Set'.newSet
	, if stems.poal    ('Data.Set'.singleton "poal") 'Data.Set'.newSet
	, if stems.poel    ('Data.Set'.singleton "poel") 'Data.Set'.newSet
	, if stems.hitpoel ('Data.Set'.singleton "htpo") 'Data.Set'.newSet
	]

tenses :: !VerbSettings -> 'Data.Set'.Set String
tenses {tenses} = 'Data.Set'.unions
	[ 'Data.Set'.singleton "NA"
	, if tenses.perfect              ('Data.Set'.singleton "perf") 'Data.Set'.newSet
	, if tenses.imperfect            ('Data.Set'.singleton "impf") 'Data.Set'.newSet
	, if tenses.wayyiqtol            ('Data.Set'.singleton "wayq") 'Data.Set'.newSet
	, if tenses.imperative           ('Data.Set'.singleton "impv") 'Data.Set'.newSet
	, if tenses.infinitive_absolute  ('Data.Set'.singleton "infa") 'Data.Set'.newSet
	, if tenses.infinitive_construct ('Data.Set'.singleton "infc") 'Data.Set'.newSet
	, if tenses.participle           ('Data.Set'.singleton "ptca") 'Data.Set'.newSet
	, if tenses.participle_passive   ('Data.Set'.singleton "ptcp") 'Data.Set'.newSet
	]

:: FindTextSettings =
	{ text_boundaries      :: !TextBoundaries
	, goal                 :: !Goal
	, number_of_results    :: !Int
	, verb_weight          :: !Int
	, must_include_lexemes :: ![String]
	}

:: TextBoundaries
	= NumberOfVerses !Int
	| Paragraphs !ParagraphSettings

:: ParagraphSettings =
	{ minimum_number_of_words :: !Int
	}

:: Goal =
	{ objective            :: !Objective
	, absolute_or_relative :: !AbsoluteOrRelative
	, words_or_forms       :: !WordsOrForms
	}

:: Objective
	= MinimizeUnknownness
	| MaximizeKnownness

:: AbsoluteOrRelative
	= CountAbsoluteItems
	| CountRatio

:: WordsOrForms
	= Words
	| Forms

derive class iTask FindTextSettings
derive class iTask TextBoundaries, ParagraphSettings
derive class iTask Goal, Objective, AbsoluteOrRelative, WordsOrForms

:: TextResult =
	{ start   :: !Reference
	, end     :: !Reference
	}

derive class iTask \ gEditor TextResult
derive class iTask Reference, Book

gEditor{|TextResult|} EditValue = gEditor EditValue
where
	derive gEditor TextResult
gEditor{|TextResult|} ViewValue =
	mapEditorRead pretty ((mapEditorWrite \_ -> EmptyEditor) htmlView)
where
	pretty {start,end} = ATag
		[ TargetAttr "_blank"
		, HrefAttr link
		]
		[ Html (replaceSubString "-" "&ndash;" reference)
		]
	where
		link = concat
			[ "https://parabible.com/"
			, replaceSubString " " "-" (englishName start.book)
			, "/", toString start.chapter
			, "#", toString start.verse
			]

		reference = concat
			[ englishName start.book, " "
			, toString start.chapter, ":"
			, toString start.verse
			: if (start == end)
				[]
				[ "-"
				: if (start.book <> end.book)
					end_ref
					(if (start.chapter <> end.chapter)
						end_chapter_and_verse
						end_verse)
				]
			]

		end_ref = [englishName end.book," ":end_chapter_and_verse]
		end_chapter_and_verse = [toString end.chapter,":":end_verse]
		end_verse = [toString end.verse]

defaultFindTextSettings :: FindTextSettings
defaultFindTextSettings =
	{ text_boundaries = Paragraphs
		{ minimum_number_of_words = 50
		}
	, goal =
		{ objective            = MaximizeKnownness
		, absolute_or_relative = CountRatio
		, words_or_forms       = Words
		}
	, number_of_results = 25
	, verb_weight = 1
	, must_include_lexemes = []
	}

selectedVocabularyLists :: SimpleSDSLens [String]
selectedVocabularyLists =: sdsFocus "selectedVocabularyLists.json" $ jsonFileStore "TextPicker" False False (?Just [])

verbSettings :: SimpleSDSLens VerbSettings
verbSettings =: sdsFocus "verbSettings.json" $ jsonFileStore "TextPicker" False False (?Just defaultValue)

findTextSettings :: SimpleSDSLens FindTextSettings
findTextSettings =: sdsFocus "findTextSettings.json" $ jsonFileStore "TextPicker" False False (?Just defaultFindTextSettings)

findTexts =
	// prevent exceptions when the type of FindTextSettings has changed:
	catchAll (get findTextSettings) (\_ -> set defaultFindTextSettings findTextSettings) >-|
	// actual tasks:
	(ArrangeWithSideBar 1 BottomSide True @>> (
		(Title "Settings" @>> ApplyLayout settings_layout @>> allTasks
			[ Hint "Find texts with the following vocabulary:" @>>
				editSharedMultipleChoiceWithSharedAs [ChooseFromCheckGroup fst] vocabularyLists fst selectedVocabularyLists @! ()
			, Hint "Only recognize these verbal forms:" @>>
				updateSharedInformation [] verbSettings @! ()
			, Hint "Miscellaneous settings:" @>>
				updateSharedInformation [] findTextSettings @! ()
			]
		)
	-&&-
		forever (ScrollContent @>> (
			get selectedVocabularyLists >>- \selection ->
			get verbSettings >>- \verb_settings ->
			get findTextSettings >>- \settings ->
			get vocabularyLists
				@ filter (flip isMember selection o fst)
				@ concatMap snd @ 'Data.Set'.fromList >>- \chosenVocabulary ->
			loadDataSet >>-
			findSuitableTexts chosenVocabulary verb_settings settings >>-
			viewInformation [] >>*
			[ OnAction (Action "Search again") $ always $ return ()
			]
		))
	)) @! ()
where
	settings_layout = sequenceLayouts
		[ arrangeSplit Horizontal False
		, layoutSubUIs SelectChildren (setUIAttributes (widthAttr WrapSize))
		, scrollContent
		]

findSuitableTexts :: !('Data.Set'.Set String) !VerbSettings !FindTextSettings !DataSet -> Task [TextResult]
findSuitableTexts vocabulary verb_settings settings data =
	case get_node_feature_ids ["book", "chapter", "verse", "pargr", "lex", "vs", "vt"] data of
		?Just [book,chapter,verse,pargr,lex,vs,vt:_] ->
			let
				all_texts = textCandidates book chapter verse pargr data
				scored_texts = sortBy ((<) `on` snd) [(t,s) \\ (t,?Just s) <- map (appSnd (score lex vs vt)) all_texts]
				best_texts = take settings.number_of_results $ avoidOverlap $ map fst scored_texts
			in
			Hint "Finding suitable texts..." @>> compute (hyperstrict best_texts)
		_ ->
			throw "Text-Fabric data did not contain the required features"
where
	known_stems = stems verb_settings
	known_tenses = tenses verb_settings

	textCandidates :: !FeatureId !FeatureId !FeatureId !FeatureId !DataSet -> [(TextResult,[Node])]
	textCandidates book chapter verse pargr data = case settings.text_boundaries of
		NumberOfVerses n ->
			collectVerses n [v \\ v <|- filter_nodes (isOfType "verse") data]
		Paragraphs settings ->
			collectParagraphs settings
				[ (ca, split "." (get_node_feature pargr ca))
				\\ ca <|- filter_nodes (isOfType "clause_atom") data
				]
	where
		collectVerses _ [] = []
		collectVerses number_of_verses [this:rest]
			| length theseVerses == number_of_verses
				= [thisText:collectVerses number_of_verses rest]
				= collectVerses number_of_verses rest
		where
			thisBook = get_node_feature book this
			thisChapter = get_node_feature chapter this

			startRef = {book=fromString thisBook, chapter=toInt thisChapter, verse=toInt (get_node_feature verse this)}
			thisText =
				(
					{ start = startRef
					, end   = {startRef & verse=toInt (get_node_feature verse (last theseVerses))}
					}
				, theseVerses
				)

			theseVerses = take number_of_verses [this:takeWhile partOfThisText rest]
			where
				partOfThisText n =
					get_node_feature chapter n == thisChapter &&
					get_node_feature book n == thisBook

		collectParagraphs _ [] = []
		collectParagraphs settings [(node,p):rest]
			| length theseWords >= settings.minimum_number_of_words
				= [thisText:collectParagraphs settings rest`]
				= collectParagraphs settings rest`
		where
			(restOfParagraph,rest`) = span (sameParagraph p o snd) rest
			where
				sameParagraph [x:_] [y:_] = x == y

			theseNodes = [node:map fst restOfParagraph]
			theseWords = [w \\ ca <- theseNodes, w <|- get_child_node_refs_with (isOfType "word") ca data]

			firstVerse = getVerse (Hd (get_child_node_refs_with (isOfType "word") node data))
			lastVerse = getVerse (Last (get_child_node_refs_with (isOfType "word") (last theseNodes) data))
			startRef =
				{ book    = fromString (get_node_feature book firstVerse)
				, chapter = toInt (get_node_feature chapter firstVerse)
				, verse   = toInt (get_node_feature verse firstVerse)
				}
			thisText =
				(
					{ start = startRef
					, end   = {startRef & verse=toInt (get_node_feature verse lastVerse)}
					}
				, theseNodes
				)

			getVerse node = Hd (get_ancestor_nodes_with (isOfType "verse") node data)

	score lex vs vt verses
		| any (not o flip isMember (map fst3 word_features)) settings.must_include_lexemes = ?None
		| otherwise = ?Just case settings.goal.absolute_or_relative of
			CountAbsoluteItems ->
				case settings.goal.objective of
					MinimizeUnknownness ->
						toReal (length unknown_items)
					MaximizeKnownness ->
						toReal (0 - length known_items)
			CountRatio ->
				toReal (length unknown_items) / toReal (length items)
	where
		words = concatMap (\v -> [c \\ c <|- get_child_nodes_with (isOfType "word") v data]) verses
		word_features = [(get_node_feature lex w, get_node_feature vs w, get_node_feature vt w) \\ w <- words]
		items = repeatVerbs case settings.goal.words_or_forms of
			Words -> word_features
			Forms -> removeDup word_features
		(known_items,unknown_items) = partition isKnown items

		repeatVerbs [] = []
		repeatVerbs [w=:(_,stem,_):ws]
			| stem == "NA"
				= [w:repeatVerbs ws]
				= repeatn settings.verb_weight w ++ repeatVerbs ws

		isKnown (lexeme, stem, tense) =
			'Data.Set'.member stem known_stems &&
			'Data.Set'.member tense known_tenses &&
			'Data.Set'.member lexeme vocabulary

	avoidOverlap [] = []
	avoidOverlap [this=:{start,end}:rest] = [this:avoidOverlap (filter noOverlap rest)]
	where
		noOverlap {start=start2,end=end2} =
			(start.book <> end2.book ||
				(end2.chapter < start.chapter ||
					(end2.chapter == start.chapter && end2.verse < start.verse))) &&
			(start2.book <> end.book ||
				(start2.chapter > end.chapter ||
					(start2.chapter == end.chapter && start2.verse > end.verse)))
