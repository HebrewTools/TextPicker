module TextPicker

import iTasks

import TextPicker.Data
import qualified TextPicker.FindTexts.Basic as Basic
import qualified TextPicker.FindTexts.Advanced as Advanced
import TextPicker.Vocabulary

Start w = doTasks main w

main =
	tryToLoadData >-|
	allTasks
		[ 'Basic'.findTexts <<@ Title "Find texts"
		, 'Advanced'.findTexts <<@ Title "Find texts (advanced)"
		, manageVocabulary <<@ Title "Manage vocabulary"
		]
	<<@ ArrangeWithTabs False

tryToLoadData =
	catchAll loadDataSet
	\_ ->
		Hint "Failed to load Text-Fabric data. Enter the correct path:" @>>
		updateSharedInformation [] dataPath >!|
		tryToLoadData
