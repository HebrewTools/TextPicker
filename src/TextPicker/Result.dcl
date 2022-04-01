definition module TextPicker.Result

from Data.GenEq import generic gEq
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode

from iTasks.Internal.Generic.Visualization import :: TextFormat, generic gText
from iTasks.UI.Editor import :: Editor, :: EditorReport
from iTasks.UI.Editor.Generic import :: EditorPurpose, generic gEditor
from iTasks.WF.Definition import class iTask

from Bible import :: Reference

:: TextResult =
	{ start   :: !Reference
	, end     :: !Reference
	}

derive class iTask \ gEditor TextResult
derive gEditor TextResult
