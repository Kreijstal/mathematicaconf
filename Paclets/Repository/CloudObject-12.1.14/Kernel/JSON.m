BeginPackage["CloudObject`"]

Begin["`Private`"]

If[$VersionNumber >= 11.3,
    importFromJSON = Symbol["Developer`FromJSON"];
    With[{toJSONSymbol = Symbol["Developer`ToJSON"]},
        exportToJSON[list : {__Rule}, opts___] := toJSONSymbol[Association @@ list, opts];
        exportToJSON[x_, opts___] := toJSONSymbol[x, opts]
    ]
    ,(* else *)
    Needs["JSONTools`"];
    importFromJSON = Symbol["JSONTools`FromJSON"];
    exportToJSON = Symbol["JSONTools`ToJSON"]
]

(* handler for JSON object response from GET /users API. This will be modified once server side changes are merged for CLOUD-15724 *)
handleUsersJSONObject[userInfo_List] := If[KeyExistsQ[userInfo, "users"], Flatten[Lookup[userInfo, "users"], 1], userInfo]

End[]

EndPackage[]
