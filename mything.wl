(* ::Package:: *)

BeginPackage["MyThing`"]
Begin["`Private`"]

End[ ]
CurrentValue[EvaluationNotebook[],{InputAliases,"vec"}]=Overscript[\[SelectionPlaceholder], "->"]
CurrentValue[EvaluationNotebook[],{InputAliases,"dcint"}]=DoubleContourIntegral[\[SelectionPlaceholder],\[SelectionPlaceholder]]
CurrentValue[EvaluationNotebook[],{InputAliases,"dli"}]=\[DotlessI]
CurrentValue[EvaluationNotebook[],{InputAliases,"dlj"}]=\[DotlessJ]
CurrentValue[EvaluationNotebook[],{InputAliases,"TF"}]=InterpretationBox[StyleBox["TF",FontSlant->Italic,FontColor->GrayLevel[0.5],Selectable->False],TableForm]
EndPackage[ ]