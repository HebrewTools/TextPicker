definition module TextPicker.FindTexts

from Data.GenEq import generic gEq
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode

from iTasks.Internal.Generic.Visualization import :: TextFormat, generic gText
from iTasks.SDS.Definition import :: SDSLens, :: SimpleSDSLens
from iTasks.UI.Editor import :: Editor, :: EditorReport
from iTasks.UI.Editor.Generic import :: EditorPurpose, generic gEditor
from iTasks.WF.Definition import class iTask, :: Task

from TextFabric import :: EdgeSet, :: Node, :: Node`

from TextPicker.Result import :: TextResult

:: TextSelectionSettings =
	{ text_boundaries                     :: !TextBoundaries
	, count_unique_forms                  :: !Bool
	, number_of_results                   :: !Int
	, must_include_lexemes                :: ![String]
	, only_consonantally_distinct_results :: !Bool
	}

:: TextBoundaries
	= NumberOfVerses !Int
	| Paragraphs !OneNodeSettings
	| SingleNodes !NodeTypes !OneNodeSettings

:: NodeTypes =
	{ phrases :: !Bool
	, clauses :: !Bool
	, verses  :: !Bool
	}

:: OneNodeSettings =
	{ minimum_number_of_words :: !Int
	, count_definite_article  :: !Bool
	, count_conjunction       :: !Bool
	}

derive class iTask TextSelectionSettings

textSelectionSettings :: SimpleSDSLens TextSelectionSettings

/**
 * @param Tasks to edit settings.
 * @param A task to retrieve the scoring function.
 */
findTextsTask :: ![Task ()] !(Task ([Node] -> Real)) -> Task ()
