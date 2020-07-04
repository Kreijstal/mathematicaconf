BeginPackage["CloudObject`"]

Hold[System`$CloudBase];
Hold[System`$EvaluationCloudBase];

$WolframCloudBase::usage = "$WolframCloudBase is the setting for $CloudBase to connect to the public Wolfram cloud.";

Begin["`Private`"]

If[!StringQ[$UrlScheme],
    $UrlScheme = "https";
];

If[!ValueQ[System`$EvaluationCloudBase],
	System`$EvaluationCloudBase = None;
];

$WolframCloudBase = "https://www.wolframcloud.com/";
SetAttributes[$WolframCloudBase, {ReadProtected, Protected, Locked}]

If[!StringQ[$CloudBase], $CloudBase = $WolframCloudBase];

$CloudBase /: Set[HoldPattern[$CloudBase] , base_] /; ! TrueQ[$set] := 
 Block[ {$set = True, res = base},
     setCloudBase[res, Set]
 ]
$CloudBase /: SetDelayed[HoldPattern[$CloudBase] ,base_] /; ! TrueQ[$set] := 
 Block[ {$set = True, res = base},
     setCloudBase[res, SetDelayed]
 ]

setCloudBase[base_String, set : Set | SetDelayed] := 
	If[set === Set, Identity, Function[Null]][$CloudBase = URLUtilities`URLCorrect[Replace[base, $cloudBaseAbbreviations], "https"]]
setCloudBase[URL[base_String], set_] := setCloudBase[base, set]
setCloudBase[base_, set_] := (Message[$CloudBase::cbase, base]; base)
  
If[ValueQ[CloudSystem`$ApplicationDomain],
    If[CloudSystem`$ApplicationDomain === "localhost" || CloudSystem`$ApplicationDomain === "localhost:8080",
        $UrlScheme = "http";
    ];
    $CloudBase = $UrlScheme<>"://" <> CloudSystem`$ApplicationDomain <> "/";
];

SetAttributes[{$CloudBase, $EvaluationCloudBase}, ReadProtected];

End[]

EndPackage[]
