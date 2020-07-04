BeginPackage["NaturalLanguage`Customizations`"]

FormattingBlock 

Begin["`Private`"]

SetAttributes[FormattingBlock, HoldAll]

FormattingBlock[expr_] := Module[{boxes},
	boxes = If[FreeQ[expr, _ResourceFunction],
		With[{syms = blockedSymbols},
			Internal`InheritedBlock[syms,
				Unprotect[syms];
				blockedRules;

				Block[{TraditionalFormDump`HeldMatrixHeuristicQ = False&},
					MakeBoxes[expr, TraditionalForm]
				]
			]
		],
		"["
	];
	If[FreeQ[boxes, "["],
		boxes,
		Replace[
			expr,
			{
				Defer[e_] :> ToString[Unevaluated[e], InputForm],
				e_ :> ToString[e, InputForm]
			}
		]
	]
]

blockedRules := Map[
	Function[sym,
		FormatValues[sym] = {
			RuleDelayed[
				HoldPattern[MakeBoxes[sym[a__], TraditionalForm]], 
				MakeBoxes[custom[sym][a], TraditionalForm]
			]
		}
	],
	blockedSymbols
]

$Invalid = "Invalid`Output";

MakeBoxes[custom[Plot][a_, {x_, min_, max_}, OptionsPattern[]], TraditionalForm] ^:= RowBox[{
	"plot ",
	MakeBoxes[a, TraditionalForm],
	RowBox[{"from ", MakeBoxes[x, TraditionalForm], " = ", MakeBoxes[min, TraditionalForm], " to ", MakeBoxes[max,TraditionalForm]}]
}]

MakeBoxes[custom[LogPlot][a_, {x_, min_, max_}, OptionsPattern[]], TraditionalForm] ^:= RowBox[{
	"log plot ",
	MakeBoxes[a, TraditionalForm],
	RowBox[{"from ", MakeBoxes[x, TraditionalForm], " = ", MakeBoxes[min, TraditionalForm], " to ", MakeBoxes[max,TraditionalForm]}]
}]

MakeBoxes[custom[LogLinearPlot][a_, {x_, min_, max_}, OptionsPattern[]], TraditionalForm] ^:= RowBox[{
	"log-linear plot ",
	MakeBoxes[a, TraditionalForm],
	RowBox[{"from ", MakeBoxes[x, TraditionalForm], " = ", MakeBoxes[min, TraditionalForm], " to ", MakeBoxes[max,TraditionalForm]}]
}]

MakeBoxes[custom[LogLogPlot][a_, {x_, min_, max_}, OptionsPattern[]], TraditionalForm] ^:= RowBox[{
	"log-log plot ",
	MakeBoxes[a, TraditionalForm],
	RowBox[{"from ", MakeBoxes[x, TraditionalForm], " = ", MakeBoxes[min, TraditionalForm], " to ", MakeBoxes[max,TraditionalForm]}]
}]

MakeBoxes[custom[ReplaceAll][a_, b_], TraditionalForm] ^:= If[FreeQ[Unevaluated[b], Rule[_, _List]], 
	RowBox[{
		MakeBoxes[a, TraditionalForm],
		" where ",
		commaFormBoxes[toEqual[Unevaluated[b]]]
	}],
	$Invalid
]

MakeBoxes[custom[Series][expr_, {x_, x0_, n_}, OptionsPattern[]], TraditionalForm] ^:= RowBox[{
	"series ",
	MakeBoxes[expr, TraditionalForm],
	" at ",
	MakeBoxes[x = x0, TraditionalForm],
	" to order ",
	MakeBoxes[n, TraditionalForm]
}]

MakeBoxes[custom[Minimize][expr_, x_, OptionsPattern[]], TraditionalForm] ^:= RowBox[{
	"minimize ",
	MakeBoxes[expr, TraditionalForm],
	" for ",
	varForm[x]
}]

MakeBoxes[custom[Minimize][{expr_, conds_}, x_, OptionsPattern[]], TraditionalForm] ^:= RowBox[{
	"minimize ",
	MakeBoxes[expr, TraditionalForm],
	" where ",
	MakeBoxes[conds, TraditionalForm],
	" for ",
	varForm[x]
}]

MakeBoxes[custom[Maximize][expr_, x_, OptionsPattern[]], TraditionalForm] ^:= RowBox[{
	"maximize ",
	MakeBoxes[expr, TraditionalForm],
	" for ",
	varForm[x]
}]

MakeBoxes[custom[Maximize][{expr_, conds_}, x_, OptionsPattern[]], TraditionalForm] ^:= RowBox[{
	"maximize ",
	MakeBoxes[expr, TraditionalForm],
	" where ",
	MakeBoxes[conds, TraditionalForm],
	" for ",
	varForm[x]
}]

MakeBoxes[custom[Solve][expr_, vars_, OptionsPattern[]], TraditionalForm] ^:= RowBox[{
	"solve ",
	parenform[expr],
	" for ",
	commaFormExpression[vars]
}]

MakeBoxes[custom[Solve][expr_, _, domain_:(Reals|Complexes|Integers|Rationals|Algebraics|Primes), OptionsPattern[]], TraditionalForm] ^:= RowBox[{
	"solve ",
	MakeBoxes[expr, TraditionalForm],
	" over the ",
	ToLowerCase @ SymbolName[domain] 
}]

MakeBoxes[custom[And][a__], TraditionalForm] ^:= If[VectorQ[Unevaluated[{a}], IntegerQ],
	$Invalid,
	RowBox @ BoxForm`MakeInfixForm[And[a], " & ", TraditionalForm]
] 

MakeBoxes[custom[LaplaceTransform][expr__], TraditionalForm] ^:=  If[FreeQ[Unevaluated[expr], _List],
	RowBox[{
		"Laplace transform",
		parenform[{expr}]
	}],
	$Invalid
]

MakeBoxes[custom[InverseLaplaceTransform][expr__], TraditionalForm] ^:=  If[FreeQ[Unevaluated[expr], _List],
	RowBox[{
		"inverse Laplace transform",
		parenform[{expr}]
	}],
	$Invalid
]

MakeBoxes[custom[FourierTransform][expr__], TraditionalForm] ^:= If[FreeQ[Unevaluated[expr], _List], 
	RowBox[{
		"Fourier transform",
		parenform[{expr}]
	}],
	$Invalid
]

MakeBoxes[custom[InverseFourierTransform][expr__], TraditionalForm] ^:= If[FreeQ[Unevaluated[expr], _List], 
	RowBox[{
		"inverse Fourier transform",
		parenform[{expr}]
	}],
	$Invalid
]

MakeBoxes[custom[Det][mat_List], TraditionalForm] ^:= RowBox[{
	"determinant ",
	MakeBoxes[mat, StandardForm]
}]

MakeBoxes[custom[D][expr_, {x_, n_}], TraditionalForm] ^:= If[!MatrixQ[Unevaluated[expr]], 
	RowBox[{
		"d^",
		MakeBoxes[n, TraditionalForm],
		"/d", 
		MakeBoxes[x, TraditionalForm],
		"^",
		MakeBoxes[n, TraditionalForm],
		parenform[expr]
	}]
]

MakeBoxes[custom[D][expr_, x_], TraditionalForm] ^:= If[!MatrixQ[Unevaluated[expr]], 
	RowBox[{
		"d/d", 
		MakeBoxes[x, TraditionalForm],
		parenform[expr]
	}]
]

MakeBoxes[custom[D][expr_, x_, y_], TraditionalForm] ^:= If[FreeQ[Unevaluated[x], List] && FreeQ[Unevaluated[y], List], 
	RowBox[{
		"d^2/(d",
		MakeBoxes[x, TraditionalForm],
		" d",
		MakeBoxes[y, TraditionalForm],
		")",
		parenform[expr]
	}],
	$Invalid
]

MakeBoxes[custom[Reduce][expr_, vars_], TraditionalForm] ^:= If[MatchQ[Unevaluated[expr], _Equal | _Greater | _GreaterEqual | _Less | _LessEqual | _And | _Or], 
	RowBox[{
		"reduce ",
		"(",
		MakeBoxes[expr, TraditionalForm],
		",",
		commaFormExpression[vars],
		")"
	}],
	$Invalid
]

MakeBoxes[custom[FunctionDomain][expr_, _], TraditionalForm] ^:= 
RowBox[{
	"domain of ",
	parenform[expr]
}]

MakeBoxes[custom[FunctionRange][expr_, _, _], TraditionalForm] ^:= If[Head[Unevaluated[expr]] =!= List,
	RowBox[{
		"range of ",
		parenform[expr]
	}],
	$Invalid
]

MakeBoxes[custom[TrigExpand][expr_], TraditionalForm] ^:= 
RowBox[{
	"trig expand ",
	parenform[expr]
}]

MakeBoxes[custom[TrigFactor][expr_], TraditionalForm] ^:= 
RowBox[{
	"trig factor ",
	parenform[expr]
}]

MakeBoxes[custom[TrigReduce][expr_], TraditionalForm] ^:= 
RowBox[{
	"trig reduce ",
	parenform[expr]
}]

MakeBoxes[custom[TrigToExp][expr_], TraditionalForm] ^:= 
RowBox[{
	"trig to exp of ",
	parenform[expr]
}]

MakeBoxes[custom[Manipulate][Plot[expr_, plotvars_, OptionsPattern[]], manvars_, OptionsPattern[]], TraditionalForm] ^:= 
	RowBox[{
		"manipulate (plot ((",
		commaFormExpression[expr],
		"),",
		parenform[plotvars],
		"),",
		parenform[manvars],
		")"
	}]

MakeBoxes[custom[Set][a_, b_], TraditionalForm] ^:= RowBox[{"set ", MakeBoxes[a, TraditionalForm], " to ", MakeBoxes[b, TraditionalForm]}]

blockedSymbols = DeleteDuplicates @ FormatValues[NaturalLanguage`Customizations`Private`custom][[All, 1, 1, 1, 0, 1]]

SetAttributes[{varForm, ivarForm, tfboxes, formattedCommaList, commaFormExpression, parenform, iparenform}, HoldAllComplete]
varForm[a_] := If[FreeQ[Unevaluated[a], _Element], ivarForm[a], $Invalid]
ivarForm[{a_}] := MakeBoxes[a, TraditionalForm]
ivarForm[{a__}] := MakeBoxes[And[a], TraditionalForm]
ivarForm[a_] := MakeBoxes[a, TraditionalForm]

tfboxes[expr_] := MakeBoxes[expr, TraditionalForm]

formattedCommaList[a__] := formattedCommaList[{a}]
formattedCommaList[a_List] := Riffle[Map[tfboxes, Hold[a], {2}] /. Hold[b_] :> b, ","]

commaFormExpression[{a_, b_}] := RowBox[{MakeBoxes[a, TraditionalForm], " and ", MakeBoxes[b, TraditionalForm]}]
commaFormExpression[{a__, b_}] :=  RowBox[Join[formattedCommaList[a], {",", "and ", MakeBoxes[b, TraditionalForm]}]]
commaFormExpression[{a_}] := MakeBoxes[a, TraditionalForm]
commaFormExpression[a_] := MakeBoxes[a, TraditionalForm]

commaFormBoxes[{a_, b_}] := RowBox[{a, " and ", b}]
commaFormBoxes[{a__, b_}] :=  RowBox[Join[Riffle[{a}, ","], {",", "and ", b}]]
commaFormBoxes[{a_}] := a
commaFormBoxes[a_] := a

parenform[a_] := If[FreeQ[Unevaluated[a], _Element], iparenform[a], $Invalid]
iparenform[a_] := iparenform[{a}]
iparenform[{a_}] := Sequence["(", MakeBoxes[a, TraditionalForm], ")"]
iparenform[a : {__}] :=  RowBox[{"(", RowBox[formattedCommaList[a]], ")"}]

toEqual[(Rule|RuleDelayed)[a__]] := MakeBoxes[Equal[a], TraditionalForm]
toEqual[a_List] := toEqual /@ Unevaluated[a]

End[]

EndPackage[]