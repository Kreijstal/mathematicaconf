BeginPackage["CloudObject`"]

Hold[System`CloudConnections];

System`CloudConnections::usage = "CloudConnections[] gives a list of cloud accounts that are authenticated.";

Begin["`Private`"]

Unprotect[CloudConnections];

CloudConnections[] := 
    With[{connections = Select[rawCloudConnections[], newFormatQ]},
        Map[Normal, SortBy[Map[normalizeKeychainCloudBase, connections], connectionOrderingData]]
    ]

rawCloudConnections[] :=
    Map[KeyTake[{CloudBase, CloudUserID}], getKeyChainData[]]

newFormatQ[connection_] := StringFreeQ[connection[CloudBase], "http://" | "https://"]

normalizeKeychainCloudBase[connection_] := 
    Prepend[connection,
        CloudBase ->
            Replace[connection[CloudBase], {
                "localhost" -> "http://localhost",
                domain_ :> "https://" <> domain
            }]
    ]

connectionOrderingData[conn_] :=
    With[{cbase = conn[CloudBase]}, 
        With[{domain = extractURLDomain[cbase]}, 
            {If[currentCloudBaseQ[cbase], 0, 1], domain, conn[CloudUserID]}
        ]
    ]

(* currentCloudBaseQ[cbase] returns True if the provided CloudBase matches the current $CloudBase *)
currentCloudBaseQ[cbase_] := 
    SameQ[extractURLDomain[cbase], extractURLDomain[$CloudBase]]


SetAttributes[
	{CloudConnections},
    {Protected, ReadProtected}
];

End[]
EndPackage[]
