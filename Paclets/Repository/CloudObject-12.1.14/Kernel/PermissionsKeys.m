(* ::Package:: *)

BeginPackage["CloudObject`"]

System`PermissionsKey;
System`DeletePermissionsKey;
System`PermissionsKeys;

Begin["`Private`"]

Unprotect[PermissionsKey, DeletePermissionsKey, PermissionsKeys];

validatePermissionsKey[PermissionsKey[key_String]] := validatePermissionsKey[key]

validatePermissionsKey[key_String] := StringLength[key] > 0

validatePermissionsKey[PermissionsKey[]] := True
    
validatePermissionsKey[key___] := False

(* PermissionsKey *)

PermissionsKey[] := PermissionsKey[CreateUUID[]]

e : PermissionsKey[args__] := Null /; (System`Private`Arguments[e, {0, 1}]; False)

    
(* DeletePermissionsKey *)

Options[DeletePermissionsKey] = sortOptions @ {CloudBase->Automatic}

DeletePermissionsKey[keys:{Alternatives[_PermissionsKey, _String] ..}, opts:OptionsPattern[]] := Map[DeletePermissionsKey[#, opts]&, keys]

DeletePermissionsKey[key_, opts:OptionsPattern[]] :=
	If[validatePermissionsKey[key]
			,
			Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]}, deletePermissionsKey[key]
			]
			,
			Message[DeletePermissionsKey::invkey, key]; 
			$Failed
		]
	
e : DeletePermissionsKey[args___] := Null /; (System`Private`Arguments[e, 1]; False)

deletePermissionsKey[key_, msgHeader_: DeletePermissionsKey] :=
	Replace[
		execute[$CloudBase, "DELETE", {"permissionskeys", Replace[key, PermissionsKey[x_] :> x]}],
		{
			{_String, {}} :> Null,
			HTTPError[404, ___] :> (Message[msgHeader::keynf, key]; $Failed),
			other_ :> (checkError[other, msgHeader]; $Failed)
		}
	]
	
(* PermissionsKeys *)
Options[PermissionsKeys] = sortOptions @ {CloudBase->Automatic}

PermissionsKeys[opts:OptionsPattern[]] :=
	Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
		Replace[
			execute[$CloudBase, "GET", {"permissionskeys"}],
			{
				{_String, res:{_Integer ...}} :> constructPermsKeyList[res],
				other_ :> (checkError[other, PermissionsKeys]; $Failed)
			}
		]
]

e : PermissionsKeys[args___]  := Null /; (System`Private`Arguments[e, 0]; False)

constructPermsKeyList[bytes_] :=
	With[{json = importFromJSON[FromCharacterCode[bytes]]},
		Map[PermissionsKey, json]
	]

SetAttributes[{PermissionsKey, DeletePermissionsKey, PermissionsKeys}, {Protected, ReadProtected}];

End[]

EndPackage[]
