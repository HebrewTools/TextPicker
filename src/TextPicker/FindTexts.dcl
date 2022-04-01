definition module TextPicker.FindTexts

from Data.GenEq import generic gEq
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode

from iTasks.Internal.Generic.Visualization import :: TextFormat, generic gText
from iTasks.SDS.Definition import :: SDSLens, :: SimpleSDSLens
from iTasks.UI.Editor import :: Editor, :: EditorReport
from iTasks.UI.Editor.Generic import :: EditorPurpose, generic gEditor
from iTasks.WF.Definition import class iTask, :: Task

from TextFabric import :: DataSet, :: DataSet`, :: EdgeSet, :: Node, :: Node`

from TextPicker.Result import :: TextResult

:: TextSelectionSettings =
	{ text_boundaries      :: !TextBoundaries
	, count_unique_forms   :: !Bool
	, number_of_results    :: !Int
	, must_include_lexemes :: ![String]
	}

:: TextBoundaries
	= NumberOfVerses !Int
	| Paragraphs !ParagraphSettings

:: ParagraphSettings =
	{ minimum_number_of_words :: !Int
	}

derive class iTask TextSelectionSettings

textSelectionSettings :: SimpleSDSLens TextSelectionSettings

/**
 * @param A scoring function. Receives a list of nodes in the text. If
 *   `count_unique_forms` is set, this does not contain duplicate forms.
 * @param Settings with which to find texts.
 * @param The data set.
 */
findSuitableTexts :: !([Node] -> Real) !TextSelectionSettings !DataSet -> Task [TextResult]

/**
 * @param Tasks to edit settings.
 * @param A task to retrieve the scoring function.
 */
findTextsTask :: ![Task ()] !(Task ([Node] -> Real)) -> Task ()
