(* Mathematica package *)
BeginPackage["CloudObject`"];

CloudSubmit;
System`MessageObject;


Begin["`Private`"];


(*
   User may provide handlers for these
*)
$CloudSubmitHandlerNames = {
    "TaskStarted",
    "TaskFinished",
    "TaskRemoved",
    "TaskStatusChanged",
    "PrintOutputGenerated",
    "MessageGenerated",
    "FailureOccurred",
    "ResultReceived",
    "UnhandledEvent"
}

(*
    Which keys to use in an association, which is the argument of event handlers
*)
$CloudSubmitKeyNames = {
    (* meta properties *)
    "TaskUUID",
    "Task",
    "TaskType",
    "EvaluationExpression",
    "EventName",
    (* set by events *)
    "TaskStatus",
    "PrintOutput",
    "MessageOutput",
    "Failure",
    "EvaluationResult",
    (* cloud objects *)
    "RemoteObject" (* task *),
    "RemoteDataObject" (* data *)
}


(* Polling method *)
$DefaultUpdateInterval = 1;

(*
    <|...|> - event data field and function to use to update it
    {...} - which user-level events to activate. The user may have supplied handlers for these.
*)
$CloudTaskEventActions = <|
    "CloudTaskStarted" -> {
        <|
            "TaskStatus" -> makeTaskStatusRunning
        |>,
        {"TaskStarted", "TaskStatusChanged"}
    },
    "CloudTaskFinished" -> {
        <|
            "TaskStatus" -> makeTaskStatusFinished
        |>,
        {"TaskFinished", "TaskStatusChanged", "ResultReceived"}
    },
    "CloudTaskRemoved" -> {
        <|
            "TaskStatus" -> makeTaskStatusRemoved
        |>,
        {"TaskRemoved", "TaskStatusChanged"}
    },
    "CloudMessageGenerated" -> {
        <|
            "MessageOutput" -> makeIdentity
        |>,
        {"MessageGenerated"}
    },
    "CloudPrintOutputGenerated" -> {
        <|
            "PrintOutput" -> makeIdentity
        |>,
        {"PrintOutputGenerated"}
    },
    "CloudFailure" -> {
        <|
            "Failure" -> makeFailure
        |>,
        {"FailureOccurred"}
    }
|>

(************************************************************)

(*
    Generic function to verify symbol's options
*)
CheckAndGetOptions[name_Symbol, opts_Association] :=
    Block[{allopts, tmp = opts, rest},
        allopts = Association[Options[name]];
        If[Length[tmp] != 0,
            rest = KeyDrop[tmp, Keys[allopts]];
        ];
        If[Length[rest] == 0,
            AppendTo[allopts, tmp];
            allopts
            ,
            (* Unknown option(s) *)
            Message[MessageName[name, "optx"], First[Keys[rest]], name];
            $Failed
        ]
    ]

CheckAndGetOptions[___] := $Failed

(*
    Given handlers as an association, which is specified by a user, pick only those relevant
    for the caller and return them. Generate a message and return $Failed, if unknown entries are found.
*)
filterHandlerFunctions[caller_, hfuns_, taskEventNames_] :=
    Block[{defaults, handlerEventNames, extra},
        If[AssociationQ[hfuns],
            extra = KeyDrop[hfuns, taskEventNames];
            If[Length[extra] != 0,
                Message[MessageName[caller, "invhf"], First[Keys[extra]]]
            ];
            KeyTake[hfuns, taskEventNames],
            Message[MessageName[caller, "invak"], hfuns];
            $Failed
        ]
    ]

iFilterKeyNanes[caller_, hkeys_, taskKeyNames_] :=
    Block[{handlerKeyNames = hkeys, extra},
        If[!ContainsAll[taskKeyNames, handlerKeyNames],
            extra = Complement[handlerKeyNames, taskKeyNames];
            Message[MessageName[caller, "invhk"], First[extra]];
            handlerKeyNames = Intersection[handlerKeyNames, taskKeyNames];
        ];
        If[Length[handlerKeyNames] > 0,
            handlerKeyNames,
            {}
        ]
    ]

filterKeyNames[caller_, hkeys_, taskKeyNames_] :=
    Which[
        hkeys === Automatic,
        Automatic (* will use default values *),
        hkeys === All,
        taskKeyNames,
        StringQ[hkeys],
        iFilterKeyNanes[caller, {hkeys}, taskKeyNames],
        ListQ[hkeys],
        iFilterKeyNanes[caller, hkeys, taskKeyNames],
        True,
        Message[MessageName[caller, "nslist"], hkeys];
        $Failed
    ]

(************************************************************)

(* Evaluated in the Cloud *)

appendToObject[arg_, name_String, obj_] :=
    Module[{tmp, val},
        tmp = CloudGet[obj];
        If[AssociationQ[tmp],
            val = Lookup[tmp, name, {}];
            If[MissingQ[val], val = {}];
            If[!ListQ[val], val = {val}];
            AppendTo[val, arg];
            AssociateTo[tmp, {name -> val}];
            CloudPut[tmp, obj]
        ]
    ]

printHandler[obj_][arg_] := (appendToObject[arg, "Print", obj]; (* do not issue the message *) False)

messageHandler[obj_][arg_] :=
    Module[{msg, visible},
        visible = Extract[arg, {2}];
        If[TrueQ[visible],
            (*
                arg is Hold[Message[MessageName[symbol, tag], HoldForm[param]...], True|False]
                msg is Hold[{symbol::tag, HoldForm[param]...}]
            *)
            msg = Apply[List, Extract[arg, {1}, Hold], {1}];
            (*
                symbol is HoldForm[sym]; held because it can self-evaluate (e.g. $RecursionLimit)
                tag is a string
                params is a list {HoldForm[param]...}
            *)
            appendToObject[
                With[{
                        sym = Extract[msg, {1, 1, 1}, HoldForm],
                        tag = Extract[msg, {1, 1, 2}],
                        prm = Rest[msg[[1]]]
                    },
                    System`MessageObject[<|
                        "MessageSymbol" -> sym,
                        "MessageTag" -> tag,
                        "MessageParameters" -> prm,
                        "MessageTemplate" -> ReleaseHold[Quiet[MessageName[sym, tag]]]
                    |>]
                ],
                "Messages",
                obj
            ]
        ];
        (* do not issue the message *)
        False
    ]

updateObject[obj_, name_, val_] :=
    Module[{tmp},
        tmp = CloudGet[obj];

        If[AssociationQ[tmp],
            AssociateTo[tmp, {name -> val}];
            CloudPut[tmp, obj]
        ]
    ]

SetAttributes[doEvaluation, HoldFirst]

doEvaluation[expr_, obj_] :=
    Module[{res},
        updateObject[obj, "TaskStatus", "Running"];
        (*
            use Message.Veto and Wolfram.System.Print.Veto in kernel 11.2 and later to quiet Message and Print
        *)
        If[$VersionNumber >= 11.2,
            Internal`AddHandler["Message.Veto", messageHandler[obj]];
            Internal`AddHandler["Wolfram.System.Print.Veto", printHandler[obj]],\
            (* else *)
            Internal`AddHandler["Message", messageHandler[obj]];
            Internal`AddHandler["Wolfram.System.Print", printHandler[obj]]
        ];
        (* actual evaluation *)
        res = expr;
        updateObject[obj, "EvaluationResult", res];
        (* *)
        If[$VersionNumber >= 11.2,
            Internal`RemoveHandler["Wolfram.System.Print.Veto", printHandler[obj]];
            Internal`RemoveHandler["Message.Veto", messageHandler[obj]],
            (* else *)
            Internal`RemoveHandler["Wolfram.System.Print", printHandler[obj]];
            Internal`RemoveHandler["Message", messageHandler[obj]]
        ];
        updateObject[obj, "TaskStatus", "Finished"];
        res
    ]

(************************************************************)

(* Evaluated in the local kernel *)

makeIdentity[dataID_, name_, newData_] :=
    Tasks`SetTaskEventValue[dataID, name, newData]

makeTaskStatusRunning[dataID_, _, _] :=
    Tasks`SetTaskEventValue[dataID, "TaskStatus", "Running"]

makeTaskStatusFinished[dataID_, _, _] :=
    Tasks`SetTaskEventValue[dataID, "TaskStatus", "Finished"]

makeTaskStatusRemoved[dataID_, _, _] :=
    Tasks`SetTaskEventValue[dataID, "TaskStatus", "Removed"]

makeFailure[dataID_, _, _] :=
    Tasks`SetTaskEventValue[dataID, "Failure", None] (* TODO *)

(*
    Handle low-level events: "ReturnPacket", "TextPacket", "MessagePacket", ...
*)
handleEvent[eventName_, eventDataID_, eventData_, eventObj_] :=
    Module[{work, data},
        work = Lookup[eventObj, eventName, None];

        (* If there is an entry for this event... *)
        If[AssociationQ[work],
            (* work is <|"updates" -> updateFuns, "events" -> eventFuns|> *)
            (* update dataObj with event data *)
            Scan[(
                    #[[1]][eventDataID, #[[2]], eventData]
                ) &,
                work["updates"]
            ];
            (* evaluate user-specified handlers *)
            Scan[(
                    Tasks`SetTaskEventValue[eventDataID, "EventName", #[[1]]];
                    data = Tasks`GetTaskEventData[eventDataID];
                    ReleaseHold[#[[2]]][data]
                ) &,
                work["events"]
            ];
            Tasks`SetTaskEventValue[eventDataID, "EventName", None];
        ]
    ]

activateEvents[localDataID_, localData_, remoteData_, eventFuncs_] :=
    Module[{localMessages, remoteMessages, newMessages, localText, remoteText, newText},
        localMessages = Lookup[localData, "Messages", {}];
        remoteMessages = Lookup[remoteData, "Messages", {}];

        If[Length[localMessages] =!= Length[remoteMessages],
            newMessages = Complement[remoteMessages, localMessages];

            Map[
                handleEvent["CloudMessageGenerated", localDataID, #, eventFuncs]&,
                newMessages
            ]
        ];

        localText = Lookup[localData, "Print", {}];
        remoteText = Lookup[remoteData, "Print", {}];
        If[Length[localText] =!= Length[remoteText],
            newText = Complement[remoteText, localText];
            Map[
                handleEvent["CloudPrintOutputGenerated", localDataID, #, eventFuncs]&,
                newText
            ]
        ]
    ]

deleteObject[obj_] := DeleteFile[obj]

makeReadHandler[dataObj_, hashIn_, handlerKeys_, eventFuncs_, origExprHeld_, taskObj_] :=
    Module[{localHash = hashIn, localData = <||>, firstTime = True, localDataID = None, mainObj = taskObj},
        If[!AssociationQ[localData], Throw[$Failed]];

        Function[{},
            Module[{remoteData = <||>, taskStatus, res, tmp, remoteHash},
                res = Catch[
                    If[localDataID === None,
                        localDataID = Tasks`GetTaskEventDataID[$CurrentTask];
                        If[localDataID === $Failed, Throw[$Failed]]
                    ];
                    If[taskID === None,
                        taskID = Tasks`GetTaskID[$CurrentTask];
                        If[taskID === $Failed, Throw[$Failed]]
                    ];

                    If[MemberQ[handlerKeys, "RemoteObject"],
                        Tasks`SetTaskEventValue[localDataID, "RemoteObject", mainObj]
                    ];

                    If[MemberQ[handlerKeys, "RemoteDataObject"],
                        Tasks`SetTaskEventValue[localDataID, "RemoteDataObject", dataObj]
                    ];

                    remoteHash = Quiet[Check[CloudObjectInformation[dataObj, "FileHashMD5"], $Failed]];
                    If[FailureQ[remoteHash],
                        Throw[$Failed]
                    ];

                    If[firstTime,
                        firstTime = False;
                        remoteData = Quiet[CloudGet[dataObj]];
                        If[!AssociationQ[remoteData],
                            Throw[$Failed]
                        ];
                        Tasks`SetTaskEventValue[localDataID, "TaskType", "CloudTask"];
                        handleEvent["CloudTaskStarted", localDataID, None, eventFuncs]
                    ];

                    (*
                        If remote data has changed, update locally and run event handlers
                    *)
                    If[localHash =!= remoteHash,
                        remoteData = CloudGet[dataObj];
                        If[!AssociationQ[remoteData],
                            (* TODO: Failure[ ] *)
                            Throw[$Failed]
                        ];
                        (*
                            Add special meta-data about the task to TaskEventData
                        *)
                        If[MemberQ[handlerKeys, "Task"],
                            Tasks`SetTaskEventValue[localDataID, "Task", $CurrentTask]
                        ];

                        If[MemberQ[handlerKeys, "TaskUUID"],
                            Tasks`SetTaskEventValue[localDataID, "TaskUUID", taskID]
                        ];

                        If[MemberQ[handlerKeys, "TaskType"],
                            Tasks`SetTaskEventValue[localDataID, "TaskType", "CloudTask"]
                        ];

                        If[MemberQ[handlerKeys, "EvaluationExpression"],
                            Tasks`SetTaskEventValueHeld[localDataID, "EvaluationExpression", origExprHeld]
                        ];

                        If[MemberQ[handlerKeys, "EvaluationResult"],
                            tmp = Lookup[remoteData, "EvaluationResult", Missing["NotAvailable"]];
                            Tasks`SetTaskEventValue[localDataID, "EvaluationResult", tmp]
                        ];

                        activateEvents[localDataID, localData, remoteData, eventFuncs];
                        localHash = remoteHash;
                        localData = remoteData;
                    ];

                    taskStatus = Lookup[remoteData, "TaskStatus", "Running"];

                    If[taskStatus === "Finished",
                        handleEvent["CloudTaskFinished", localDataID, None, eventFuncs];
                        If[taskComplete[mainObj],
                            handleEvent["CloudTaskRemoved", localDataID, None, eventFuncs];
                            DeleteObject[dataObj];
                            DeleteObject[mainObj];
                            TaskRemove[$CurrentTask],
                            (* else, waiting for next run *)
                            updateObject[dataObj, "TaskStatus", "Waiting"]
                        ]
                    ];
                ];
                If[res === $Failed,
                    (* TODO: "task failed" event *)
                    handleEvent["CloudTaskRemoved", localDataID, None, eventFuncs];
                    DeleteObject[dataObj];
                    DeleteObject[mainObj];
                    TaskRemove[$CurrentTask];
                ]
            ]
        ]
    ]

taskComplete[task_] :=
    Module[{info},
        info = Quiet[Check[Normal[ScheduledTaskInformation[task, {"RepeatCount", "EndDate", "NextRunDate"}]], <||>]];
        Or[
            Quiet[Lookup[info, "NextRunDate", None]] === None,
            Quiet[Loookup[info, "RepeatCount", 0]] === 0,
            And[
                Quiet[Lookup[info, "EndDate", None]] =!= None,
                info["EndDate"] <= Now
            ]
        ]
    ]

(************************************************************
 *	new CloudSubmit
 ************************************************************)

SetAttributes[doCloudDeploy, HoldFirst];

doCloudDeploy[ScheduledTask[expr_, sched_, taskopts:OptionsPattern[]], dataObj_, taskObj_, optsIn:OptionsPattern[]] :=
    Block[{},
        With[{opts = Sequence @@ FilterRules[DeleteDuplicatesBy[Join[{taskopts}, {optsIn}, {AutoRemove -> True}], First], Options[ScheduledTask]]},
            CloudDeploy[
                ScheduledTask[
                    CloudObject`Private`doEvaluation[expr, dataObj],
                    sched,
                    opts
                ],
                taskObj,
                Sequence @@ FilterRules[{optsIn}, Options[CloudDeploy]]
            ]
        ]
    ]

doCloudDeploy[_ScheduledTask, ___] := ( (* Message[]; *) $Failed)

doCloudDeploy[expr:Except[_ScheduledTask], dataObj_, taskObj_, optsIn:OptionsPattern[]] :=
    Module[{cloud, uuid, name, cronned, params, opts = Flatten[{optsIn}], taskJson, rJson},
        {cloud, uuid, name} = Replace[getCloudAndUUIDOrPath[taskObj], None -> Null, {1}];
        name = Replace[
            name,
            {
                n : {"user-" ~~ $CloudUserUUID, __} :> FileNameJoin[Rest[n]],
                n_List :> FileNameJoin[n]
            }
        ];

        cronned = {Null, {Null, 1}, Null};

        If[MatchQ[cronned, $Failed],
            Message[CloudSubmit::sched, sched];
            Throw[$Failed, $tag]
        ];

        cronned = ReplaceAll[cronned, {None -> Null}];

        taskJson = generateTaskJson[ScheduledTask[doEvaluation[expr, dataObj], cronned, Sequence @@ FilterRules[opts, Options[ScheduledTask]]], {name, uuid}, cronned, Join[opts, {"RunImmediately" -> True}], opts];
        If[taskJson === $Failed,
            Throw[$Failed, $tag]
        ];

        params = {"task" -> taskJson};

        With[{mh = CloudSubmit},
            rJson = Replace[
                execute[
                    cloud,
                    "POST",
                    {"tasks"},
                    Body -> ToCharacterCode[ExportString[params, "JSON"], "UTF-8"]
                ],
                {
                    {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content]),
                    HTTPError[400, ___] :> (Message[mh::argu]; Throw[$Failed, $tag]),
                    HTTPError[403, content_, ___] :> (
                        ToExpression[Lookup[ImportString[content, "JSON"], "error", "ScheduledTask::restr"], InputForm, Message];
                        Throw[$Failed, $tag]
                    ),
                    other_ :> (Message[mh::srverr]; Message[mh::crea]; Throw[$Failed, $tag])
                }
            ];
        ];

        obj
    ]

doCloudDeploy[___] := $Failed

removeHandler[TaskObject[arg_Association]] :=
    Module[{taskObj, dataObj, status},

        status = Lookup[arg, "TaskStatus", "Finished"];

        If[status === "Finished",

            taskObj = Lookup[arg, "RemoteObject", $Failed];
            dataObj = Lookup[arg, "RemoteDataObject", $Failed];

            If[taskObj =!= $Failed,
                Quiet[DeleteObject[taskObj]]
            ];
            If[dataObj =!= $Failed,
                Quiet[DeleteObject[dataObj]]
            ];
        ]
    ]

SetAttributes[iCloudSubmit, HoldFirst]

iCloudSubmit[expr_, taskObj_, optsIn:OptionsPattern[]] :=
    Module[{opts, hopts, hf, hk, handlerFunctions, handlerKeys, updateInterval,
        remoteData, mainobj, eventFuncs, spec, cloudBase, tmpObj, dataObj = CloudObject[]},

        (* Check options. Options optsIn, which are added later, take precedence. *)
        opts = Association[Join[Options[CloudSubmit], Flatten[{optsIn}]]];
        If[!BooleanQ[Lookup[opts, IncludeDefinitions]],
            Message[General::invincludedf, Lookup[opts, IncludeDefinitions]];
            Return[$Failed];
        ];
        hopts = CheckAndGetOptions[CloudSubmit, opts];
        If[!AssociationQ[hopts],
            Return[$Failed]
        ];

        cloudBase = Lookup[hopts, CloudBase, $CloudBase];

        (* Extract specified handler functions *)
        hf = Lookup[hopts, HandlerFunctions, Automatic];

        If[hf === Automatic,
            handlerFunctions = Association[],
            handlerFunctions = filterHandlerFunctions[CloudSubmit, hf, $CloudSubmitHandlerNames]
        ];
        If[handlerFunctions === $Failed,
            Return[$Failed]
        ];

        (* here handlerFunctions is an association *)
        If[!AssociationQ[handlerFunctions], Return[$Failed]];

        (* Extract specified handler keys *)
        hk = Lookup[hopts, HandlerFunctionsKeys, Automatic];
        handlerKeys = filterKeyNames[CloudSubmit, hk, $CloudSubmitKeyNames];
        If[handlerKeys === Automatic,
            handlerKeys = $CloudSubmitKeyNames
        ];

        If[handlerKeys === $Failed, Return[$Failed]];

        (*
            Polling method, update interval
            TODO: use Method option
        *)
        updateInterval = Lookup[hopts, "UpdateInterval", $Failed];
        If[updateInterval === $Failed,
            updateInterval = $DefaultUpdateInterval
        ];

        (* Upload all the definitions and metadata *)
        remoteData = <|
            "TaskStatus" -> "Starting",
            "RemoteDataObject" -> dataObj
        |>;
        tmpObj = CloudPut[remoteData, dataObj];
        If[tmpObj === $Failed, Return[$Failed]];

        (* Run the cloud task *)
        mainobj = With[
            {objOpts = Normal[KeyTake[opts, Keys[Join[Options[ScheduledTask], Options[CloudSubmit]]]]]},
            Catch[doCloudDeploy[expr, dataObj, taskObj, Sequence @@ DeleteDuplicatesBy[objOpts, First]], $tag]
        ];

        (* Create local polling *)
        eventFuncs = Tasks`Package`makeEventFunctions[$CloudTaskEventActions, handlerFunctions, handlerKeys];
        If[!AssociationQ[eventFuncs], Return[$Failed]];

        With[{readHandler = makeReadHandler[dataObj, Quiet[Check[CloudObjectInformation[obj, "FileHashMD5"], $Failed]], handlerKeys, eventFuncs, Hold[expr], mainobj]},
            spec = <|
                "Caller" -> CloudSubmit,
                "TaskEnvironment" -> "Cloud",
                "TaskType" -> "Cloud",
                "RealEvaluationExpression" -> Hold[readHandler[]], (* what to evaluate *)
                "EvaluationExpression" -> HoldForm[expr], (* what to display *)
                "Schedule" -> <|
                    "Type" -> "Repeated",
                    "TotalRunCount" -> Infinity,
                    "RunInterval" -> updateInterval
                |>,
                HandlerFunctions -> <||>, (* handlerFunctions *)
                HandlerFunctionsKeys -> handlerKeys,
                "RemoveHandler" -> removeHandler,
                "UserData" -> <|
                    "RemoteDataObject" -> dataObj,
                    "RemoteObject" -> mainobj
                |>
            |>
        ];
        Tasks`ScheduledToTaskObject[spec]
    ]

SetAttributes[formatCloudSubmitExpr, {HoldFirst}];

formatCloudSubmitExpr[ScheduledTask[expr_, sched_, taskOpts:OptionsPattern[]], opts_List] :=
    With[{formattedOpts = formattedOpts = Sequence @@ FilterRules[
            DeleteDuplicatesBy[Flatten[Join[{taskOpts}, opts, Options[ScheduledTask]]], First],
            Options[ScheduledTask]
        ]},
        ScheduledTask[
            expr,
            sched,
            formattedOpts
        ]
    ]
formatCloudSubmitExpr[expr_, opts_List] :=
    With[{formattedOpts = Sequence @@ FilterRules[Flatten[Join[opts, Options[ScheduledTask]]], Options[ScheduledTask]]},
        ScheduledTask[
            expr,
            {Now},
            formattedOpts
        ]
    ]

Unprotect[CloudSubmit];
Clear[CloudSubmit];
SetAttributes[CloudSubmit, {HoldFirst, ReadProtected}];

(* Check to see if this is a post-11.3 kernel *)
If[TrueQ[$VersionNumber >= 11.2],

    (*********************** New CloudSubmit **********************)
    Unprotect[HandlerFunctionsKeys, NotificationFunction];

	(* Options need to be kept alphabetically sorted *)
    Options[CloudSubmit] = {
        CloudBase :> $CloudBase,
        HandlerFunctions -> <||>,
        HandlerFunctionsKeys -> Automatic,
        IncludeDefinitions -> True,
        Method -> Automatic,
        NotificationFunction -> Automatic
    };

    Protect[HandlerFunctionsKeys, NotificationFunction];

    CloudSubmit[expr_, opts:OptionsPattern[]] :=
        Block[{$CloudBase = handleCBase[OptionValue[CloudBase]], obj},
            obj = CloudObject[];
            Catch[iCloudSubmit[expr, obj, opts], $Failed]
        ];

    CloudSubmit[expr_, obj:_String|_CloudObject|_URL, opts:OptionsPattern[]] :=
        Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
            Catch[iCloudSubmit[expr, obj, opts], $Failed]
        ];

    CloudSubmit[expr_, failureObj_Failure, opts:OptionsPattern[]] := failureObj;

    e:CloudSubmit[_, opts:Except[OptionsPattern[]]] :=
        Module[{},
            Message[CloudSubmit::nonopt1, Last[{opts}], 1, HoldForm[e]];
            $Failed
        ];

    CloudSubmit[___] := $Failed

    , (* else, if $Version is < 11.2, we use the old implementation *)

	(* Options need to be kept alphabetically sorted *)
    Options[CloudSubmit] = {
        CloudBase :> $CloudBase,
        IncludeDefinitions -> True,
        NotificationFunction -> Automatic
    };

    CloudSubmit[expr_, o:OptionsPattern[]] :=
        Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
            CloudSubmit[expr, CloudObject[], o]
        ];

    CloudSubmit[expr_, obj:_String|_CloudObject|_URL, o:OptionsPattern[]] :=
        Module[{opts = Join[Flatten[{o}], Options[CloudSubmit]]},
            Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
                With[{taskOpts = Sequence @@ Join[FilterRules[opts, Options[ScheduledTask]], {AutoRemove -> True}],
                    objOpts = Sequence @@ FilterRules[opts, Options[CloudDeploy]]},
                    CloudDeploy[
                        ScheduledTask[expr, {Now}, taskOpts],
                        obj,
                        objOpts
                    ]
                ]
            ]
        ]
]

Protect[CloudSubmit];

(************************************************************)

End[]

EndPackage[]
