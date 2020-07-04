(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {System`ResourceAcquire}

BeginPackage["ResourceSystemClient`"]

System`ResourceAcquire
System`ResourceRemove

Begin["`Private`"] (* Begin Private Context *) 

$ResourceSystemSyncedQ=True/;$CloudEvaluation
$ResourceSystemSyncedQ=False


(*** ResourceAcquire ***)

System`ResourceAcquire[args___]:=Catch[resourceAcquireOpts[args]]

Options[System`ResourceAcquire]={CloudBase:>Automatic, "AddToAccount"->True}

resourceAcquireOpts[ro_,opts:OptionsPattern[System`ResourceAcquire]]:=resourceAcquire[ro,
	OptionValue[System`ResourceAcquire, {opts}, "AddToAccount"],
	OptionValue[System`ResourceAcquire, {opts}, CloudBase]]

resourceAcquire[resource:rpat, rest___]:=resourceAcquire[resourceObjectID[resource],rest]

resourceAcquire[id_,___]:=(loadResource[id];resourceAcquire[id])/;MemberQ[$localResources,id]&&!MemberQ[$loadedResources,id]
resourceAcquire[id_,___]:=System`ResourceObject[id]/;MemberQ[$localResources, id]

resourceAcquire[id_,rest___]:=With[{info=loadResource[id]},
	If[AssociationQ[info],
        cacheresourceinfo[info];
		resourceAcquire[id, rest]
		,
		$Failed
	]]/;MemberQ[$cloudResources,id]||MemberQ[$myResourceSystemResources,id]

resourceAcquire[str_,addToAccount_:True]:=resourceacquire[str,addToAccount, Automatic]
resourceAcquire[str_,addToAccount_, cloudbase_]:=resourceacquire[str,addToAccount, cloudbase]

resourceacquire[str_,addToAccount_,cloudbase_]:=Block[{info, id},
	info=importresourceInfo[str,addToAccount,cloudbase];
	id=info["UUID"];
	System`ResourceObject[id]
]

importresourceInfo[str_,addToAccount_,cloudbase_:Automatic]:=Block[{params, info, id, resourcebase=resourcesystembase[cloudbase]},
	params={If[uuidQ[str],
        "UUID"->str,
        "Name"->str
    ],"RecordUserAccess"->addToAccount,"Elements"->"True","ContentElementFunctions"->"True"};
	info=importresourceinfo[params, resourcebase];
	If[Quiet[TrueQ[KeyExistsQ[info,"UUID"]]],
		info=fillResourceMetadata[info, Association["RepositoryLocation"->URL[resourcebase],"MyAccount"->addToAccount]];
		id=info["UUID"];
		cacheresourceinfo[info]
		,
		Message[ResourceObject::notf];
		Throw[$Failed]
	]
]


importresourceinfo[params_, resourcebase_]:=With[{res=apifun["AcquireResource",params, System`ResourceAcquire,resourcebase]},
	standardizeResourceInfo[res]
]


myAccountQ[id_]:=myaccountQ[resourceinfo[id]]
myaccountQ[as_Association]:=Lookup[as,"MyAccount",False]
myaccountQ[_]:=False




(*** ResourceRemove ***)

System`ResourceRemove[args___]:=Catch[resourceRemove[args]]

resourceRemove[resource:rpat]:=resourceRemove[resourceObjectID[resource]]

resourceRemove[id_]:=Block[{info},
	info=resourceInfo[id];
	If[AssociationQ[info],
		deleteresourcecache[info];
		If[myaccountQ[info]&&$CloudConnected,
			info=removeResourceFromAccount[If[uuidQ[id],
		        {"UUID"->id},
		        {"Name"->id}
		    ],resourcerepositoryBase[info]]
		];
	    If[KeyExistsQ[info,"UUID"],
			id
			,
			Throw[$Failed]
		]
		,
		deleteresourcecache[id, Missing[]];
		id
	]
	
]

removeResourceFromAccount[params_, resourcebase_]:=With[{res=apifun["RemoveResource",params, System`ResourceAcquire,resourcebase]},
	res
]


End[] (* End Private Context *)

EndPackage[]

SetAttributes[{ResourceAcquire},
   {ReadProtected, Protected}
];