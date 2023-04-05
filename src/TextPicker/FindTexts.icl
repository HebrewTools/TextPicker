implementation module TextPicker.FindTexts

import StdEnv
import StdOverloadedList

import Data.Func
import Data.List => qualified group
import Data.Tuple
import Text
import Text.Unicode
from Text.Unicode.UChar import instance toInt UChar
import Text.Unicode.Encodings.UTF8

import iTasks
import iTasks.Extensions.DateTime

import Bible

import TextFabric
import TextFabric.BHSA
import TextFabric.Filters

import TextPicker.Data
import TextPicker.Result

derive class iTask TextSelectionSettings
derive class iTask TextBoundaries, OneNodeSettings

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
	, only_consonantally_distinct_results = True
	}

findSuitableTexts :: !([Node] -> Real) !TextSelectionSettings !DataSet -> Task [TextResult]
findSuitableTexts score settings data =
	case get_node_feature_ids (["otype","book","chapter","verse","pargr","lex"] ++ morphology_features) data of
		?Just [otype,book,chapter,verse,pargr,lex:morphology_features] ->
			let
				all_texts = textCandidates otype book chapter verse pargr [lex:morphology_features] data
				included_texts = if (isEmpty settings.must_include_lexemes)
					all_texts
					(filter (\(_,nodes) -> all (\lex` -> any (\n -> get_node_feature lex n == lex`) nodes) settings.must_include_lexemes) all_texts)
				scored_texts = sortBy ((>) `on` \r -> r.score) [{res & score=score nodes} \\ (res,nodes) <- included_texts]
				best_texts = take settings.number_of_results $ onlyDistinct $ avoidOverlap scored_texts
			in
			Hint "Finding suitable texts..." @>>
			enterInformation [EnterUsing id (mapEditorWrite ValidEditor loader)] ||-
			// NB: ugly hack: waitForTimer is needed so that the UILoader is shown
			(waitForTimer False 1 >-| return (hyperstrict best_texts))
		_ ->
			throw "Text-Fabric data did not contain the required features"
where
	morphology_features = ["gn","nu","prs_gn","prs_nu","prs_ps","ps","st","vs","vt"]

	textCandidates :: !FeatureId !FeatureId !FeatureId !FeatureId !FeatureId ![FeatureId] !DataSet -> [(TextResult,[Node])]
	textCandidates otype book chapter verse pargr morphology_features data
		# candidates = case settings.text_boundaries of
			NumberOfVerses n ->
				collectVerses n [v \\ v <|- filter_nodes (isOfType "verse") data]
			Paragraphs settings ->
				collectParagraphs settings
					[ (ca, split "." (get_node_feature pargr ca))
					\\ ca <|- filter_nodes (isOfType "clause_atom") data
					]
			Phrases settings -> collectNodes settings "phrase"
			Clauses settings -> collectNodes settings "clause"
		# candidates = map (appSnd (concatMap nodeWords)) candidates
		| settings.count_unique_forms
			= map (appSnd (nubBy (eqMorphology morphology_features))) candidates
			= candidates
	where
		nodeWords node
			| get_node_feature otype node == "word"
				= [node]
				= [w \\ w <|- get_child_nodes_with (isOfType "word") node data]

		eqMorphology [] _ _ = True
		eqMorphology [f:fs] x y = get_node_feature f x == get_node_feature f y && eqMorphology fs x y

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
					, text  = ?None
					, score = 0.0
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
					, text  = ?None
					, score = 0.0
					}
				, theseNodes
				)

			getVerse node = Hd (get_ancestor_nodes_with (isOfType "verse") node data)

		collectNodes settings type =
			[ ({start=ref, end=ref, text= ?Just text, score=0.0}, words)
			\\ node <|- filter_nodes (isOfType type) data
			, let
				node_words = get_child_node_refs_with (isOfType "word") node data
				word_refs = [Hd node_words..Last node_words] // phrases and clauses may contain gaps
				words = [data.nodes.[i] \\ i <- word_refs]
				text = concat (map snd $ flatten [get_text r data \\ r <- word_refs])
			, verse_node <|- get_ancestor_nodes_with (isOfType "verse") (Hd node_words) data
			, let
				this_book = get_node_feature book verse_node
				this_chapter = get_node_feature chapter verse_node
				this_verse = get_node_feature verse verse_node
				ref = {book=fromString this_book, chapter=toInt this_chapter, verse=toInt this_verse}
			| length words >= settings.minimum_number_of_words
			]

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

	onlyDistinct results
		| not settings.only_consonantally_distinct_results
			= results
			= map hd $ partitionGroupBy ((==) `on` \r -> onlyConsonants (fromMaybe "" r.TextResult.text)) results
	where
		onlyConsonants text = toString only_consonants
		where
			only_consonants :: UTF8
			only_consonants = fromUnicode (filter isConsonant unicode)

			unicode = toUnicode (fromMaybe empty (utf8StringCorrespondingTo text))

			isConsonant char = int == toInt ' ' || (0x05d0 <= int && int <= 0x05ea)
			where
				int = toInt char

		partitionGroupBy _ [] = []
		partitionGroupBy eq [x:xs] = [[x:ys] : partitionGroupBy eq zs]
		where
			(ys,zs) = partitionBy (eq x) xs

			partitionBy p xs = ([x \\ x <- xs & True <|- matches], [x \\ x <- xs & False <|- matches])
			where
				matches = [#p x \\ x <|- xs]

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
		forever (
			catchAll (
				getScoringFunction >>- \score ->
				get textSelectionSettings >>- \selection_settings ->
				loadDataSet >>-
				findSuitableTexts score selection_settings >>- \texts ->
				ArrangeWithSideBar 1 RightSide True @>> (
					ScrollContent @>> editChoice [ChooseFromGrid id] texts (listToMaybe texts) >&^ \mbSelection ->
					styleAttr "box-sizing:border-box;width:100%;" @>> viewSharedInformation [] mbSelection
				) @! ()
			) (\e -> Hint "An error occurred:" @>> viewInformation [] e @! ()) >>*
			[ OnAction (Action "Search again") $ always $ return ()
			]
		)
	)) @! ()
where
	settings_layout = sequenceLayouts
		[ arrangeSplit Horizontal False
		, layoutSubUIs SelectChildren (setUIAttributes (widthAttr WrapSize))
		, scrollContent
		]
