module TextPicker

import iTasks

import TextPicker.Data
import qualified TextPicker.FindTexts.Basic as Basic
import TextPicker.Vocabulary

Start w = doTasks main w

main =
	tryToLoadData >-|
	allTasks
		[ 'Basic'.findTexts <<@ Title "Find texts"
		, manageVocabulary <<@ Title "Manage vocabulary"
		]
	<<@ ArrangeWithTabs False

tryToLoadData =
	catchAll loadDataSet
	\_ ->
		Hint "Failed to load Text-Fabric data. Enter the correct path:" @>>
		updateSharedInformation [] dataPath >!|
		tryToLoadData
