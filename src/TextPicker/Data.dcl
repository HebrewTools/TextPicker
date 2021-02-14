definition module TextPicker.Data

from Data.GenEq import generic gEq
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode

from iTasks.Internal.Generic.Visualization import :: TextFormat, generic gText
from iTasks.UI.Editor import :: Editor
from iTasks.UI.Editor.Generic import :: EditorPurpose, generic gEditor
from iTasks.SDS.Definition import :: SDSLens, :: SimpleSDSLens
from iTasks.WF.Definition import :: Task, class iTask

from TextFabric import :: DataSet, :: DataSet`, :: EdgeSet

:: LexemeInformation =
	{ vocalized :: !String
	, gloss     :: !String
	}

derive class iTask LexemeInformation

derive gEq DataSet`, EdgeSet
derive gText DataSet`, EdgeSet
derive gEditor DataSet`, EdgeSet
derive JSONEncode DataSet`, EdgeSet
derive JSONDecode DataSet`, EdgeSet

dataPath :: SimpleSDSLens String

dataSet :: SimpleSDSLens (?DataSet)

loadDataSet :: Task DataSet

lexemeInformation :: SDSLens String (?LexemeInformation) ()

//* This version of lexemeInformation is unfortunately needed because sdsSequence can only read one second share.
lexemesInformation :: SDSLens [String] [?LexemeInformation] ()
