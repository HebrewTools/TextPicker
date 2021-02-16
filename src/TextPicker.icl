module TextPicker

import StdEnv
import StdOverloadedList

import Data.Func
import Data.List => qualified group
import qualified Data.Set
import Data.Set.GenJSON
import Data.Tuple
import Text

import iTasks

import TextPicker.Data
import TextPicker.Vocabulary

import TextFabric
import TextFabric.Filters

Start w = doTasks main w

main =
	tryToLoadData >-|
	allTasks
		[ forever findTexts <<@ Title "Find texts"
		, manageVocabulary <<@ Title "Manage vocabulary"
		]
	<<@ ArrangeWithTabs False

tryToLoadData =
	catchAll loadDataSet
	\_ ->
		Hint "Failed to load Text-Fabric data. Enter the correct path:" @>>
		updateSharedInformation [] dataPath >!|
		tryToLoadData

:: FindTextSettings =
	{ number_of_verses  :: !Int
	, goal              :: !Goal
	, number_of_results :: !Int
	}

:: Goal =
	{ objective            :: !Objective
	, absolute_or_relative :: !AbsoluteOrRelative
	, words_or_lexemes     :: !WordsOrLexemes
	}

:: Objective
	= MinimizeUnknownness
	| MaximizeKnownness

:: AbsoluteOrRelative
	= CountAbsoluteItems
	| CountRatio

:: WordsOrLexemes
	= Words
	| Lexemes

derive class iTask FindTextSettings, Goal, Objective, AbsoluteOrRelative, WordsOrLexemes

defaultFindTextSettings :: FindTextSettings
defaultFindTextSettings =
	{ number_of_verses  = 1
	, goal              =
		{ objective            = MaximizeKnownness
		, absolute_or_relative = CountRatio
		, words_or_lexemes     = Words
		}
	, number_of_results = 25
	}

selectedVocabularyLists :: SimpleSDSLens [String]
selectedVocabularyLists =: sdsFocus "selectedVocabularyLists.json" $ jsonFileStore "TextPicker" False False (?Just [])

findTextSettings :: SimpleSDSLens FindTextSettings
findTextSettings =: sdsFocus "findTextSettings.json" $ jsonFileStore "TextPicker" False False (?Just defaultFindTextSettings)

findTexts =
	// prevent exceptions when the type of FindTextSettings has changed:
	catchAll (get findTextSettings) (\_ -> set defaultFindTextSettings findTextSettings) >-|
	// actual tasks:
	(ArrangeSplit Horizontal False @>>
		(
			(Hint "Find texts with the following vocabulary:" @>>
				editSharedMultipleChoiceWithSharedAs [ChooseFromCheckGroup fst] vocabularyLists fst selectedVocabularyLists)
		-&&-
			(Hint "Miscellaneous settings:" @>>
				updateSharedInformation [] findTextSettings)
		)
	) >>! \(chosenVocabularyLists, settings) ->
	get vocabularyLists
		@ filter (flip isMember chosenVocabularyLists o fst)
		@ concatMap snd @ 'Data.Set'.fromList >>- \chosenVocabulary ->
	loadDataSet >>-
	findSuitableTexts chosenVocabulary settings >>-
	viewInformation [] >>*
	[ OnAction (Action "Back") $ always $ return ()
	]

findSuitableTexts :: !('Data.Set'.Set String) !FindTextSettings !DataSet -> Task [String]
findSuitableTexts vocabulary settings data =
	case get_node_feature_ids ["book", "chapter", "verse", "lex"] data of
		?Just [book,chapter,verse,lex:_] ->
			let
				all_verses = [v \\ v <|- filter_nodes (isOfType "verse") data]
				all_texts = makeTexts book chapter verse all_verses
				scored_texts = sortBy ((<) `on` snd) (map (appSnd (score lex)) all_texts)
				best_verses = map toReference $ take settings.number_of_results $ avoidOverlap $ map fst scored_texts
			in
			return best_verses
		_ ->
			throw "Text-Fabric data did not contain the required features"
where
	makeTexts :: !FeatureId !FeatureId !FeatureId ![Node] -> [((String,Int,Int,Int),[Node])]
	makeTexts _ _ _ [] = []
	makeTexts book chapter verse [this:rest]
		| length theseVerses == settings.number_of_verses
			= [thisText:makeTexts book chapter verse rest]
			= makeTexts book chapter verse rest
	where
		thisBook = get_node_feature book this
		thisChapter = get_node_feature chapter this

		thisText =
			(
				( thisBook
				, toInt thisChapter
				, toInt (get_node_feature verse this)
				, toInt (get_node_feature verse (last theseVerses))
				)
			, theseVerses
			)

		theseVerses = take settings.number_of_verses [this:takeWhile partOfThisText rest]
		where
			partOfThisText n =
				get_node_feature chapter n == thisChapter &&
				get_node_feature book n == thisBook

	score lex verses = case settings.goal.absolute_or_relative of
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
		lexemes = [get_node_feature lex w \\ w <- words]
		items = case settings.goal.words_or_lexemes of
			Words   -> lexemes
			Lexemes -> removeDup lexemes
		(known_items,unknown_items) = partition (flip 'Data.Set'.member vocabulary) items

	avoidOverlap [] = []
	avoidOverlap [this=:(book,chapter,start,end):rest] = [this:avoidOverlap (filter noOverlap rest)]
	where
		noOverlap (book2,chapter2,start2,end2) =
			book2 <> book ||
			chapter2 <> chapter ||
			end2 < start ||
			end < start2

	toReference (book,chapter,start,end) = concat
		[ replaceSubString "_" " " book
		, " "
		, toString chapter
		, ":"
		, toString start
		: if (start == end) [] ["–", toString end]
		]
