(* Wolfram Language Package *)

Unprotect[System`ResourceObject];

BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

defineResourceUpValue[fun_]:=(
	System`ResourceObject/:HoldPattern[fun][ro_System`ResourceObject,after___]:=resourceAccess[fun,ro,after];
)

defineResourceCurriedUpValue[fun_]:=(
	System`ResourceObject/:HoldPattern[f_fun][ro_System`ResourceObject,after___]:=resourceAccess[f[#]&,ro,after];
)

$ResourceUpValueFunctions={
	Get,Values,
	Dataset,Keys,Select,
	TimeSeries, EventSeries,
	Options, SetOptions,
	Normal};


$ResourceUpValueCurriedFunctions={};
	
defineResourceUpValue/@$ResourceUpValueFunctions;
	
defineResourceCurriedUpValue/@$ResourceUpValueCurriedFunctions;


resourceAccess[Normal,resource:rpat]:=usableResourceInfo[getResourceInfo[resourceObjectID[resource]]]
resourceAccess[Options,resource:rpat]:=sortBasicInfo[getResourceInfo[resourceObjectID[resource]]]
resourceAccess[SetOptions,resource:rpat, rest___]:=setResourceInfo[resourceObjectID[resource], rest]


resourceAccess[fun_,resource:rpat, rest___]:=Catch[With[{id=resourceObjectID[resource]},
	resourceAccess0[fun,id,resourceInfo[id], rest]
]]

resourceAccess0[fun_,id_,info_, rest___]:=With[{rtype=getResourceType[info]},
	If[!StringQ[rtype],Throw[$Failed]];
	loadResourceType[rtype];
	repositoryresourceaccess[rtype,fun,id, info,rest]
]

repositoryresourceaccess[___]:=$Failed

System`ResourceObject /: CloudObject`CloudDeployActiveQ[_System`ResourceObject] := True
System`ResourceObject /: GenerateHTTPResponse[ro_System`ResourceObject, rest___] := 
	GenerateHTTPResponse[HTTPResponse[createResourceShingle[ro]], rest]

System`ResourceObject /: CloudDeploy[ro_System`ResourceObject, rest___] := Catch[cloudDeployResource[ro, rest]]

System`ResourceObject /: LocalCache[ro_System`ResourceObject,args___]:=Catch[resourceLocalCache[ro, args]]/;(Length[{args}]<=1)

End[] (* End Private Context *)

EndPackage[]

SetAttributes[{ResourceObject},
   {ReadProtected, Protected}
];