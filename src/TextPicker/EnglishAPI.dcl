definition module TextPicker.EnglishAPI

from Text.HTML import :: HtmlTag

from iTasks.WF.Definition import :: Task

manageEnglishAPI :: Task ()

/**
 * @param The reference to search for, e.g. `Genesis 1:1` or `Genesis 1:1-2`.
 */
getEnglishText :: !String -> Task HtmlTag
