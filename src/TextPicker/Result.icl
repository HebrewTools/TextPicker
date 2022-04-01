implementation module TextPicker.Result

import Text
import Text.HTML

import iTasks

import Bible

derive class iTask Reference, Book
derive class iTask \ gEditor TextResult

gEditor{|TextResult|} EditValue = gEditor EditValue
where
	derive gEditor TextResult
gEditor{|TextResult|} ViewValue =
	mapEditorRead pretty ((mapEditorWrite \_ -> EmptyEditor) htmlView)
where
	pretty {start,end} = ATag
		[ TargetAttr "_blank"
		, HrefAttr link
		]
		[ Html (replaceSubString "-" "&ndash;" reference)
		]
	where
		link = concat
			[ "https://parabible.com/"
			, replaceSubString " " "-" (englishName start.book)
			, "/", toString start.chapter
			, "#", toString start.verse
			]

		reference = concat
			[ englishName start.book, " "
			, toString start.chapter, ":"
			, toString start.verse
			: if (start == end)
				[]
				[ "-"
				: if (start.book <> end.book)
					end_ref
					(if (start.chapter <> end.chapter)
						end_chapter_and_verse
						end_verse)
				]
			]

		end_ref = [englishName end.book," ":end_chapter_and_verse]
		end_chapter_and_verse = [toString end.chapter,":":end_verse]
		end_verse = [toString end.verse]
