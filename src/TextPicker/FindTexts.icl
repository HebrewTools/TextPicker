implementation module TextPicker.FindTexts

import StdEnv
import StdOverloadedList

import Data.Func
import Data.List => qualified group
import Data.Tuple
import Text

import iTasks
import iTasks.Extensions.DateTime

import Bible

import TextFabric
import TextFabric.Filters

import TextPicker.Data
import TextPicker.Result

derive class iTask TextSelectionSettings
derive class iTask TextBoundaries, ParagraphSettings

textSelectionSettings :: SimpleSDSLens TextSelectionSettings
textSelectionSettings =: sdsFocus "textSelectionSettings.json" $ jsonFileStore "TextPicker" False False (?Just defaultTextSelectionSettings)

defaultTextSelectionSettings :: TextSelectionSettings
defaultTextSelectionSettings =
	{ text_boundaries = Paragraphs
		{ minimum_number_of_words = 50
		}
	, count_unique_forms = False
	, number_of_results = 25
	, must_include_lexemes = []
	}

findSuitableTexts :: !([Node] -> Real) !TextSelectionSettings !DataSet -> Task [TextResult]
findSuitableTexts score settings data =
	case get_node_feature_ids (["book","chapter","verse","pargr","lex"] ++ morphology_features) data of
		?Just [book,chapter,verse,pargr,lex:morphology_features] ->
			let
				all_texts = textCandidates book chapter verse pargr [lex:morphology_features] data
				included_texts = if (isEmpty settings.must_include_lexemes)
					all_texts
					(filter (\(_,nodes) -> all (\lex` -> any (\n -> get_node_feature lex n == lex`) nodes) settings.must_include_lexemes) all_texts)
				scored_texts = sortBy ((>) `on` snd) $ map (appSnd score) included_texts
				best_texts = take settings.number_of_results $ avoidOverlap $ map fst scored_texts
			in
			Hint "Finding suitable texts..." @>>
			enterInformation [EnterUsing id (mapEditorWrite ValidEditor loader)] ||-
			// NB: ugly hack: waitForTimer is needed so that the UILoader is shown
			(waitForTimer False 1 >-| return (hyperstrict best_texts))
		_ ->
			throw "Text-Fabric data did not contain the required features"
where
	morphology_features = ["gn","nu","prs_gn","prs_nu","prs_ps","ps","st","vs","vt"]

	textCandidates :: !FeatureId !FeatureId !FeatureId !FeatureId ![FeatureId] !DataSet -> [(TextResult,[Node])]
	textCandidates book chapter verse pargr morphology_features data
		# candidates = case settings.text_boundaries of
			NumberOfVerses n ->
				collectVerses n [v \\ v <|- filter_nodes (isOfType "verse") data]
			Paragraphs settings ->
				collectParagraphs settings
					[ (ca, split "." (get_node_feature pargr ca))
					\\ ca <|- filter_nodes (isOfType "clause_atom") data
					]
		# candidates = map (appSnd (concatMap (\n -> [c \\ c <|- get_child_nodes_with (isOfType "word") n data]))) candidates
		| settings.count_unique_forms
			= map (appSnd (nubBy (eqMorphology morphology_features))) candidates
			= candidates
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

		eqMorphology [] _ _ = True
		eqMorphology [f:fs] x y = get_node_feature f x == get_node_feature f y && eqMorphology fs x y

	avoidOverlap [] = []
	avoidOverlap [this=:{start,end}:rest] = [this:avoidOverlap (filter noOverlap rest)]
	where
		noOverlap {start=start2,end=end2} =
			(start.book <> end2.book ||
				(end2.chapter < start.chapter ||
					(end2.chapter == start.chapter && end2.verse < start.verse))) ||
			(start2.book <> end.book ||
				(start2.chapter > end.chapter ||
					(start2.chapter == end.chapter && start2.verse > end.verse)))

// Only needed for encoding of `[Node] -> Real` functions below; never used:
JSONEncode{|Node|} _ _ = []

findTextsTask :: ![Task ()] !(Task ([Node] -> Real)) -> Task ()
findTextsTask editSettings getScoringFunction =
	// prevent exceptions when the type of TextSelectionSettings has changed:
	catchAll (get textSelectionSettings) (\_ -> set defaultTextSelectionSettings textSelectionSettings) >-|
	// actual tasks:
	(ArrangeWithSideBar 1 BottomSide True @>> (
		(Title "Settings" @>> ApplyLayout settings_layout @>> allTasks editSettings)
	-&&-
		forever (ScrollContent @>> (
			catchAll (
				getScoringFunction >>- \score ->
				get textSelectionSettings >>- \selection_settings ->
				loadDataSet >>-
				findSuitableTexts score selection_settings >>-
				viewInformation [] @! ()
			) (\e -> Hint "An error occurred:" @>> viewInformation [] e @! ()) >>*
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
