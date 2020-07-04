BeginPackage["CloudObject`"]

System`CloudEvaluate;
System`CloudFunction;

Begin["`Private`"]

(* CloudFunction *)
Unprotect[CloudFunction];

Options[CloudFunction] = sortOptions @ {IncludeDefinitions -> True};

CloudFunction[obj_CloudObject, rest___][args___] := CloudFunction[Get[obj], rest][args]
CloudFunction[fn:Except[_Failure], head_:Function[##], opts:OptionsPattern[]][args___] := CloudEvaluate[fn[args], head, opts]

CloudFunction[failureObj_Failure, rest___][args___] := failureObj

e : CloudFunction[args___] := Null /; (System`Private`Arguments[e, {1, 2}]; False)

SetAttributes[CloudFunction, {ReadProtected}];
Protect[CloudFunction];

(* CloudEvaluate *)

Unprotect[CloudEvaluate];

Options[CloudEvaluate] = sortOptions @ {CloudBase -> Automatic, IncludeDefinitions -> True};

CloudEvaluate[expr_, head_:Function[##], opts:OptionsPattern[]] /; TrueQ[$CloudEvaluation] := head @@ {expr}

CloudEvaluate[expr_, opts:OptionsPattern[]] := CloudEvaluate[expr, Function[##], opts]

CloudEvaluate[expr_, head_, OptionsPattern[]] :=
    Block[{
        $CloudBase = handleCBase[OptionValue[CloudBase]],
        includeDefinitions = Quiet[OptionValue[IncludeDefinitions]],
        connected := connected = ensureCloudConnected[],
        useWXF := useWXF = TrueQ @ $CloudEvaluateUsingWXF
        },
        If[
            !BooleanQ[includeDefinitions],
            Message[General::invincludedf, includeDefinitions];
            Return[$Failed]
        ];

        If[
            Not @ StringQ[connected],
            Return[connected]
        ];

        Replace[
            execute[
                $CloudBase,
                "POST",
                {"evaluations"},
                {Body -> If[TrueQ[includeDefinitions],
                            With[{defs = getDefinitionsList[Unevaluated[expr]]},
                               exprToStringBytesIncludingDefinitions[Unevaluated[expr], defs]
                            ],
                        (* Else *) 
                            exprToStringBytesNotIncludingDefinitions[Unevaluated[expr]]
                        ],
                    Parameters -> {"_responseform" -> If[useWXF, "WXF", "WL"]}
                }
            ], {
                result:{_String, _List} :>
                    handleSuccessfulEvaluation[
                        If[useWXF, evaluateWXF, evaluateWL],
                        result,
                        head
                    ],
                (* TODO msghd should be CloudFunction if this is being called by CloudFunction *)
                result_ :> (
                    checkError[result, CloudEvaluate];
                    Message[CloudEvaluate::srvfmt];
                    $Failed
                ) 
            }
        ]
    ]

$supportWXF := $supportWXF = MemberQ[$ImportFormats, "WXF"]

$CloudEvaluateUsingWXF :=
    TrueQ @ And[
        $supportWXF,
        getCloudVersionNumber[] > 1.49
    ]

evaluateWXF[bytes_] :=
    (*
        WXF Associations are already constructed and won't evaluate content anymore,
        we need to deconstruct them in order to preserve ToExpression evaluation semantics.

        The code is doing a double ReplaceAll pass because of https://bugs.wolfram.com/show?number=373802.
        The following code ReplaceAll[expr, Association -> Association] cannot be used as well, 
        it is not deconstructing associations for some reason.

    *)
    BinaryDeserialize[
        ByteArray[bytes],
        Function[
            expr,
            Module[
                {iAssociation},
                ReleaseHold @ ReplaceAll[
                    ReplaceAll[
                        HoldComplete[expr],
                        Association :> iAssociation
                    ],
                    iAssociation :> Association
                ]
            ],
            HoldAllComplete
        ]
    ]

evaluateWL[bytes_] := 
    (* 
        this is done to remove all eventual newlines that are returning Null and return just the expression 
        https://jira.wolfram.com/jira/browse/CLOUD-14916
    *)
    ToExpression[
        FromCharacterCode[bytes, "UTF-8"],
        InputForm,
        Function[
            {code},
            CompoundExpression @@ DeleteCases[HoldComplete[code], Null],
            HoldAllComplete
        ]
    ]


handleSuccessfulEvaluation[toExpression_, {_, bytes_List}, head_:Function[##]] := 
    Replace[
        toExpression[bytes], {
            KeyValuePattern[{_["Result", result_], _["MessagesExpressions", messages_List]}] :> (
                Scan[ReleaseHold, messages];
                head[result]
            ),
            _ :> (
                Message[CloudEvaluate::srvfmt];
                $Failed
            )
        }
    ]

internalCloudEvaluate[args___] := 
    Block[{$IncludedContexts = {"CloudObject"}},
        CloudEvaluate[args]
    ]

SetAttributes[{CloudEvaluate, internalCloudEvaluate}, {HoldFirst, ReadProtected}];
Protect[CloudEvaluate];

End[]

EndPackage[]
