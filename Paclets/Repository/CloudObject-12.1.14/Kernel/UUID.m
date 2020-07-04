(* Mathematica package *)
BeginPackage["CloudObject`"]

System`CreateUUID;

ParseUUID;
UUIDQ;

Begin["`Private`"]

Needs["UUID`"];

(* CreateUUID *)

Unprotect[System`CreateUUID];

System`CreateUUID[base_String: ""] := base <> UUID`UUID[];

SetAttributes[System`CreateUUID, {ReadProtected}];
Protect[System`CreateUUID];

(* ParseUUID *)

Unprotect[ParseUUID]

repeated[pattern_, count_Integer] := StringExpression[Sequence @@ Table[pattern, {count}]]

parseExtension[name_String] := Module[{split},
    split = StringSplit[name, ".", 2];
    If[Length[split] === 2,
        split,
    (* else *)
        {First @ split, None}
    ]
]

ParseUUID[uuid_] := Module[{name, ext, parts, number},
    {name, ext} = parseExtension[uuid];
    parts = StringSplit[name, "-"];
    If[Length[parts] >= 5 && StringStartsQ[parts[[3]], "4"] && And @@ MapThread[
	        StringMatchQ[#1, repeated[HexadecimalCharacter, #2]] &,
	        {number = Take[parts, 5], {8, 4, 4, 4, 12}}
        ],
    (* if: valid UUID *)
        {StringJoin@Riffle[number, "-"], ext},
    (* else *)
        {None, None}
    ]
]

uuidRegex =
    RegularExpression["[0-9a-fA-F]{8}\-[0-9a-fA-F]{4}"] ~~
    "-4" ~~ RegularExpression["[0-9a-fA-F]{3}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}"];

checkUUID[uuid_] :=
    If[StringMatchQ[uuid, uuidRegex],
        {uuid, None},
        cloudObjectFailureObject["invuuid", uuid]
    ]

SetAttributes[ParseUUID, {ReadProtected}];
Protect[ParseUUID];

(* UUIDQ *)

Unprotect[UUIDQ];

UUIDQ[uuid_String] := ParseUUID[uuid] =!= {None, None}
UUIDQ[_] := False

SetAttributes[UUIDQ, {ReadProtected}];
Protect[UUIDQ];

End[]

EndPackage[]
