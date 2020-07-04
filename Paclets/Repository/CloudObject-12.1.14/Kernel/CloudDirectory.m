BeginPackage["CloudObject`"]

Hold[System`$CloudRootDirectory];
System`CloudDirectory;
System`SetCloudDirectory;

Begin["`Private`"]

Unprotect[CloudDirectory, SetCloudDirectory];

CloudDirectory[] := $CloudDirectory;

SetCloudDirectory[dir_] := ($CloudDirectory = CloudObject[dir]);

SetCloudDirectory[] := ($CloudDirectory = $CloudRootDirectory);

$CloudRootDirectory := Quiet[CloudObject[CloudObject`JoinURL[{$CloudBase, $CloudObjectsRoot, "~"}]]]

$CloudDirectory := Quiet[CloudObject[CloudObject`JoinURL[{$CloudBase, $CloudObjectsRoot, "~"}]]];

(* Update the related info when $UserURLBase is changed from the web interface Preference menu *)
updateUserURLBase[newBaseURL_] := (
    Unprotect[$UserURLBase];
    $UserURLBase = newBaseURL;
    Protect[$UserURLBase];
    $CloudDirectory = CloudObject[URLBuild[Replace[URLParse[$CloudDirectory[[1]]], 
        <|x___, Rule["Path", {"", root_, usr_, rest___}], y___|> :> <|x, Rule["Path", {"", root, newBaseURL, rest}], y|>]]]
)

SetAttributes[$CloudRootDirectory, ReadProtected];
SetAttributes[{CloudDirectory, SetCloudDirectory}, {Protected, ReadProtected}];

End[]

EndPackage[]
