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

:: FindTextSettings =
	{ text_boundaries   :: !TextBoundaries
	, goal              :: !Goal
	, number_of_results :: !Int
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

derive class iTask FindTextSettings
derive class iTask TextBoundaries, ParagraphSettings
derive class iTask Goal, Objective, AbsoluteOrRelative, WordsOrLexemes

defaultFindTextSettings :: FindTextSettings
defaultFindTextSettings =
	{ text_boundaries   = Paragraphs
		{ minimum_number_of_words = 50
		}
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
	(ArrangeWithSideBar 1 BottomSide True @>> (
		(Title "Settings" @>> ArrangeSplit Horizontal False @>>
			(
				(Hint "Find texts with the following vocabulary:" @>>
					editSharedMultipleChoiceWithSharedAs [ChooseFromCheckGroup fst] vocabularyLists fst selectedVocabularyLists)
			-&&-
				(Hint "Miscellaneous settings:" @>>
					updateSharedInformation [] findTextSettings)
			)
		)
	-&&-
		forever (ScrollContent @>> (
			get selectedVocabularyLists >>- \selection ->
			get findTextSettings >>- \settings ->
			get vocabularyLists
				@ filter (flip isMember selection o fst)
				@ concatMap snd @ 'Data.Set'.fromList >>- \chosenVocabulary ->
			loadDataSet >>-
			findSuitableTexts chosenVocabulary settings >>-
			viewInformation [] >>*
			[ OnAction (Action "Search again") $ always $ return ()
			]
		))
	)) @! ()

findSuitableTexts :: !('Data.Set'.Set String) !FindTextSettings !DataSet -> Task [String]
findSuitableTexts vocabulary settings data =
	case get_node_feature_ids ["book", "chapter", "verse", "pargr", "lex"] data of
		?Just [book,chapter,verse,pargr,lex:_] ->
			let
				all_texts = textCandidates book chapter verse pargr data
				scored_texts = sortBy ((<) `on` snd) (map (appSnd (score lex)) all_texts)
				best_verses = map toReference $ take settings.number_of_results $ avoidOverlap $ map fst scored_texts
			in
			return best_verses
		_ ->
			throw "Text-Fabric data did not contain the required features"
where
	textCandidates :: !FeatureId !FeatureId !FeatureId !FeatureId !DataSet -> [((String,Int,Int,Int),[Node])]
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

			thisText =
				(
					( thisBook
					, toInt thisChapter
					, toInt (get_node_feature verse this)
					, toInt (get_node_feature verse (last theseVerses))
					)
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
			thisText =
				(
					( get_node_feature book firstVerse
					, toInt (get_node_feature chapter firstVerse)
					, toInt (get_node_feature verse firstVerse)
					, toInt (get_node_feature verse lastVerse)
					)
				, theseNodes
				)

			getVerse node = Hd (get_ancestor_nodes_with (isOfType "verse") node data)

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
