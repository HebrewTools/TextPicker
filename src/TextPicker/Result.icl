implementation module TextPicker.Result

import Data.Functor
import Text
import Text.HTML

import iTasks

import Bible

derive class iTask Reference, Book
derive class iTask \ gEditor, gText TextResult

gEditor{|TextResult|} EditValue = gEditor EditValue
where
	derive gEditor TextResult
gEditor{|TextResult|} ViewValue =
	mapEditorRead pretty ((mapEditorWrite \_ -> EmptyEditor) htmlView)
where
	pretty result=:{start,text} = DivTag []
		[ H2Tag [] [Html (replaceSubString "-" "&ndash;" (prettyReference result))]
		, PTag []
			[ ATag [TargetAttr "_blank", HrefAttr parabible_link] [Text "Parabible"]
			, Text " | "
			, ATag [TargetAttr "_blank", HrefAttr sefaria_link] [Text "Sefaria"]
			, BrTag []
			]
		, case text of
			?None -> PTag [] [Text "(click links to see Hebrew text)"]
			?Just text -> DivTag [StyleAttr "font-family:'SBL Hebrew';font-size:250%;text-align:right;"] [Text text]
		]
	where
		parabible_link = concat
			[ "https://parabible.com/"
			, replaceSubString " " "-" (englishName start.book)
			, "/", toString start.chapter
			, "#", toString start.verse
			]
		sefaria_link = concat
			[ "https://sefaria.org/"
			, replaceSubString " " "." (englishName start.book)
			, ".", toString start.chapter
			, ".", toString start.verse
			, "?lang=bi"
			]

gText{|TextResult|} fmt mbResult = gText{|*|} fmt (prettyReference <$> mbResult)

prettyReference :: !TextResult -> String
prettyReference {start,end} = concat
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
where
	end_ref = [englishName end.book," ":end_chapter_and_verse]
	end_chapter_and_verse = [toString end.chapter,":":end_verse]
	end_verse = [toString end.verse]
