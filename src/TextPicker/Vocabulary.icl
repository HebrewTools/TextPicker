implementation module TextPicker.Vocabulary

import Data.Either
import Data.Func
import qualified Data.Map
from Text import concat4
import Text.HTML

import iTasks

import TextPicker.Data

vocabulary :: SimpleSDSLens Vocabulary
vocabulary =: sdsFocus "vocabulary.json" $ jsonFileStore "TextPicker" False False (?Just 'Data.Map'.newMap)

scopedVocabulary :: SDSLens String [String] [String]
scopedVocabulary =: mapLens "scopedVocabulary" vocabulary ?None

vocabularyLists :: SimpleSDSLens [(String, [String])]
vocabularyLists =: mapReadWrite ('Data.Map'.toList, \xs _ -> ?Just ('Data.Map'.fromList xs)) ?None vocabulary

manageVocabulary :: Task ()
manageVocabulary =
	Hint "Edit or create a vocabulary list:" @>>
	enterChoiceWithShared [ChooseFromCheckGroup id] (mapRead (map fst) vocabularyLists) >>*
	[ OnAction ActionEdit $ hasValue \name -> editVocabulary name >-| manageVocabulary
	, OnAction (Action "Rename") $ hasValue \name -> renameVocabulary name >-| manageVocabulary
	, OnAction ActionNew $ always $ addVocabulary >-| manageVocabulary
	]

editVocabulary :: !String -> Task ()
editVocabulary list =
	upd ('Data.Map'.alter (?Just o fromMaybe []) list) vocabulary >-|
	let
		thisVocabulary = sdsFocus list scopedVocabulary
		thisVocabularyInformation = sdsSequence
			"thisVocabularyInformation"
			id
			(\_ r -> Right (r, id))
			(SDSWriteConst \_ () -> Error $ exception "share write from thisVocabularyInformation")
			(SDSSequenceWriteConst \_ _ () -> Error $ exception "share write from thisVocabularyInformation")
			thisVocabulary
			lexemesInformation
	in
	(ArrangeSplit Horizontal False @>> allTasks
		[ Title "Lexemes in SHEBANQ transcription" @>>
			updateSharedInformation [] thisVocabulary @! ()
		, Title "Lexemes in Hebrew" @>> AddCSSClass "hebrew" @>>
			viewSharedInformation [ViewAs (map viewMbLexemeInformation)] thisVocabularyInformation @! ()
		, Title "Transcription details" @>>
			viewInformation [] transcriptionDetails @! ()
		]
	) >>*
	[ OnAction (Action "Cancel unsaved changes") $ withoutValue $ ?Just (return ())
	, OnAction (Action "Save") $ hasValue \_ -> return ()
	]
where
	viewMbLexemeInformation ?None = "could not find lexeme"
	viewMbLexemeInformation (?Just info) = concat4 info.vocalized " (" info.gloss ")"

	transcriptionDetails = TableTag []
		[ TheadTag [] [TrTag [] [ThTag [] [Text "Glyph"], ThTag [] [Text "Transcription"]]]
		, TbodyTag []
			[ TrTag []
				[ TdTag [ClassAttr "hebrew"] [Text hebrew]
				, TdTag [] [TtTag [] [Text transcription]]
				]
			\\ (hebrew,transcription) <- consonants
			]
		]
	where
		consonants =
			[ ("א", ">")
			, ("ב", "B")
			, ("ג", "G")
			, ("ד", "D")
			, ("ה", "H")
			, ("ו", "W")
			, ("ז", "Z")
			, ("ח", "X")
			, ("ט", "V")
			, ("י", "J")
			, ("כ", "K")
			, ("ל", "L")
			, ("מ", "M")
			, ("נ", "N")
			, ("ס", "S")
			, ("ע", "<")
			, ("פ", "P")
			, ("צ", "Y")
			, ("ק", "Q")
			, ("ר", "R")
			, ("שׁ", "C")
			, ("שׂ", "F")
			, ("ת", "T")
			]

renameVocabulary :: !String -> Task ()
renameVocabulary name =
	Hint "Enter the new name for this list:" @>>
	updateInformation [] name >>! \new_name
		| name == new_name ->
			return ()
		| otherwise ->
			get vocabulary >>- \lists
				| 'Data.Map'.member new_name lists ->
					throw "A list with this name already exists"
				| otherwise ->
					get (sdsFocus name scopedVocabulary) >>- \words ->
					upd ('Data.Map'.del name) vocabulary >-|
					set words (sdsFocus new_name scopedVocabulary) @!
					()

addVocabulary :: Task ()
addVocabulary =
	Hint "Enter a name for this list:" @>>
	enterInformation [] >>!
	editVocabulary
