BeginPackage["ImportMarkdown`"]

ImportMarkdown
MarkdownElement
(* 
  Headers
  Links
  Bold
  Italics
  Code Blocks
*)

$MarkdownParseRules = <|
  "Link" -> TemplateObject[Hyperlink[TemplateSlot["Name"], TemplateSlot["URL"]]],
  "Section" -> TemplateObject @ Style[TemplateSlot["Text"], "Section"],
  "Subsection" -> TemplateObject @ Style[TemplateSlot["Text"], "Subsection"],
  "Bold" -> TemplateObject @ Text[TemplateSlot["Text"], BaseStyle -> {Bold}],
  "Italic" -> TemplateObject @ Text[TemplateSlot["Text"], BaseStyle -> {Italic}]
|>

Begin["`Private`"]

Attributes[parseData] = {Listable};

ImportMarkdown[expr_, opt: OptionsPattern[]] :=
  (* NOTE: Expeirmental, I don't know the side effects of this *)
  (* Ideally it should display rich format as in Markdown *)
  Column @ parseData[StringSplit[expr,"\n"], opt]

parseData[expr_String, opt: OptionsPattern[]] :=
  Row@Flatten @ Block[
    {
      linksPos
    },
    StringCases[
      expr,
      {
        (* Bold *)
        h___~~"**"~~text__~~"**"~~ b___ :>
          Sequence[
            parseData @ h,
            MarkdownElement[<|
              "Text" -> text
            |>, "Bold"],
            parseData @ b
          ],
        (* Italic *)
        h___~~"__"~~text__~~"__"~~ b___ :>
          Sequence[
            parseData @ h,
            MarkdownElement[<|
              "Text" -> parseData @ text
            |>, "Italic"],
            parseData @ b
          ],
        (* Links to URLs *)
        h___~~"["~~name__~~"]("~~url__~~")"~~ b___ :>
          Sequence[
            parseData @ h,
            MarkdownElement[<|
              "Name" -> name,
              "URL"  -> url
            |>, "Link"], 
            parseData @ b
          ],
        (* Titles and sections *)
        (hashes: "#"..) ~~ " "~~name__ :>
          MarkdownElement[<|
            "Text" -> parseData @ name
          |>, "Section"],
        "" :> Nothing,
        x__ :> x
      }
    ] //.{
      x_String :> x,
      MarkdownElement[prop_, style_String] :>
        $MarkdownParseRules[style][ prop ],
      "" -> Nothing
    }
    
    ]



End[] (* End `Private` *)

EndPackage[]