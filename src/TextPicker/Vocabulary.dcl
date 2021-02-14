definition module TextPicker.Vocabulary

from Data.Map import :: Map

from iTasks.SDS.Definition import :: SDSLens, :: SimpleSDSLens
from iTasks.WF.Definition import :: Task

:: Vocabulary :== Map String [String]

vocabulary :: SimpleSDSLens Vocabulary
vocabularyLists :: SimpleSDSLens [(String, [String])]

manageVocabulary :: Task ()
