(* ::Package:: *)

CompileTeXCell:=Block[{texcell,tex,imgcell,im},
FrontEndExecute[FrontEndToken["FindEvaluatingCell"]];
texcell=NotebookRead[EvaluationNotebook[]];
tex=texcell[[1]];
im=MyTeX[tex];
If[StringQ[tex],
imgcell=Cell[BoxData[FormBox[ButtonBox[ToBoxes[im],With[{ttex=tex},ButtonFunction:>(RecoverTeXSource[ttex])],Evaluator->Automatic],"TeXOutput"]]];
FrontEndExecute[FrontEndToken["FindEvaluatingCell"]];
NotebookWrite[EvaluationNotebook[],imgcell]
]];


RecoverTeXSource[text_String]:=Block[{nb},
FrontEndExecute[FrontEndToken["FindEvaluatingCell"]];
CellPrint[Cell[text,"TeX"]];
NotebookDelete[nb=EvaluationNotebook[]];
SelectionMove[nb,After,CellContents];
SelectionMove[nb,All,Character]
]


DoTeX::usage = "DoTeX[] compliles a complite tex code";

DoTeX[txt_, OptionsPattern[{Scale -> 1/2}]] := 
Block[{Pth,is}, 
Pth = $UserBaseDirectory<>"/MaTeXmatica/";
SetDirectory[Pth]; 
If[FileExistsQ[Pth<>"tmp.tex"],DeleteFile[StringJoin[Pth, "tmp.tex"]]]; 
Export[StringJoin[Pth, "tmp.tex"], txt, "text"]; Run["tex2bmp.bat"]; 
is = Import["tmp.png", "ImageSize"];Import["tmp.png"] /. Automatic -> (is[[1]]*OptionValue[Scale])/2];


MyTeX::usage = "MyTeX[] compiles a tex code. Definitionas are added from a template.";

MyTeX[a_]:=DoTeX["\\documentclass[12pt,a4paper]{article}\\begin{document}\\pagestyle{empty}"<>a<>"\\end{document}",Scale->1/3];


MaTeXmaticaLoaded=True;
