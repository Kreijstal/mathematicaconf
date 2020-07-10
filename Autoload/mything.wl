(* ::Package:: *)

BeginPackage["MyThing`"]
Begin["`Private`"]
Block[{Notation`AutoLoadNotationPalette = False},
  Needs["Notation`"]
]
End[ ]
(*AddInputAlias["vec" -> ParsedBoxWrapper[
OverscriptBox["\[SelectionPlaceholder]", "\<\[Rule]\>"]]]*)
(*CurrentValue[EvaluationNotebook[],{InputAliases,"vec"}]=OverscriptBox["\[SelectionPlaceholder]", "\<\[Rule]\>"]*)
CurrentValue[EvaluationNotebook[],{InputAliases,"dcint"}]=RowBox[{"\[DoubleContourIntegral]", 
  RowBox[{"\[SelectionPlaceholder]", 
    RowBox[{"\[DifferentialD]", "\[Placeholder]"}]}]}]
CurrentValue[EvaluationNotebook[],{InputAliases,"dli"}]=\[DotlessI]
CurrentValue[EvaluationNotebook[],{InputAliases,"dlj"}]=\[DotlessJ]
CurrentValue[EvaluationNotebook[],{InputAliases,"TF"}]=InterpretationBox[StyleBox["TF",FontSlant->Italic,FontColor->GrayLevel[0.5],Selectable->False],TableForm]
Notation[ParsedBoxWrapper[
FractionBox[
RowBox[{"\[PartialD]", 
RowBox[{"f_", "[", "x_", "]"}]}], 
RowBox[{"\[PartialD]", "x_"}]]] \[DoubleLongLeftArrow] 
  ParsedBoxWrapper[
RowBox[{
RowBox[{
RowBox[{"Derivative", "[", "1", "]"}], "[", "f_", "]"}], "[", "x_", 
     "]"}]]]
Notation[ParsedBoxWrapper[
FractionBox[
RowBox[{
SuperscriptBox["\[PartialD]", "n_"], 
RowBox[{"f_", "[", "x_", "]"}]}], 
RowBox[{"\[PartialD]", 
SuperscriptBox["x_", "n_"]}]]] \[DoubleLongLeftArrow] ParsedBoxWrapper[
RowBox[{
RowBox[{
RowBox[{"Derivative", "[", "n_", "]"}], "[", "f_", "]"}], "[", "x_", 
     "]"}]]]
Notation[ParsedBoxWrapper[
RowBox[{
FractionBox[
RowBox[{"\[PartialD]", "f_"}], 
RowBox[{"\[PartialD]", "x_"}]], " "}]] \[DoubleLongRightArrow] 
  ParsedBoxWrapper[
RowBox[{"D", "[", 
RowBox[{"f_", ",", "x_"}], "]"}]]]
Notation[ParsedBoxWrapper[
RowBox[{
FractionBox[
RowBox[{
SuperscriptBox["\[PartialD]", "n_"], "f_"}], 
RowBox[{"\[PartialD]", 
SuperscriptBox["x_", "n_"]}]], " "}]] \[DoubleLongRightArrow] 
  ParsedBoxWrapper[
RowBox[{"D", "[", 
RowBox[{"f_", ",", 
RowBox[{"{", 
RowBox[{"x_", ",", "n_"}], "}"}]}], "]"}]]]

FormattedRules[s_] := Module[{w, a, cache}, (
   cache = {};
   a = MapIndexed[(w = #1[[2]] //. cache; 
       cache = Join[cache, {(#1[[1]] -> w)}]; RawBoxes[
        ToString[#1[[1]],
          TraditionalForm] <>
         "\[Equal]" <>
         ToString[#1[[2]], TraditionalForm] <>
         If[(w == #1[[2]]) === True,
          "",
          (*else*)"\[Equal]" <> ToString[w,
            TraditionalForm]]
        ]) &, s];
   Clear[w];
   (*Print[cache];*)
   Clear[cache];
   ((HoldForm @@ #) &) /@ MakeExpression /@ ToBoxes /@ a
   )]
   
EndPackage[ ]
