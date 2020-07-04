(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}



BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  

ResourceSystemClient`resourceLocalDeploy

ResourceSystemClient`$DeployResourceContent=True;
ResourceSystemClient`$CreateCloudResourceCache=True;

Begin["`Private`"] (* Begin Private Context *) 

cloudDeployResource[ro_System`ResourceObject, rest___]:=clouddeployResourceObject[resourceObjectID[ro],ro,rest]

cloudDeployResource[___]:=$Failed

clouddeployResourceObject[id_, _,rest___]:=clouddeployresourceObject[id, getResourceInfo[id], rest]/;MemberQ[$localResources, id]
clouddeployResourceObject[id_, _,rest___]:=(importandclouddeployresourceObject[id, resourceInfo[id], rest])/;MemberQ[$loadedResources, id]
clouddeployResourceObject[id_, ro_,rest___]:=loadandclouddeployresourceObject[id, ro, rest]

clouddeployResourceObject[___]:=Throw[$Failed]

importandclouddeployresourceObject[id_, info_,rest___]:=Block[{acquired},
	acquired=resourceAcquire[id,False, resourcerepositoryBase[info]];
	If[Head[acquired]===System`ResourceObject,
		clouddeployresourceObject[id, resourceInfo[id], rest]
	]
]/;marketplacebasedResourceQ[info]

importandclouddeployresourceObject[id_, info_,rest___]:=clouddeployresourceObject[id, info, rest]/;userdefinedResourceQ[info]

importandclouddeployresourceObject[___]:=Throw[$Failed]

loadandclouddeployresourceObject[id_, ro_, rest___]:=Block[{loaded},
	loaded=loadResource[id];
	If[AssociationQ[loaded]&&MemberQ[$loadedResources, id],
		clouddeployResourceObject[id, ro, rest]
	]
]

loadandclouddeployresourceObject[___]:=Throw[$Failed]


clouddeployresourceObject[id_, info_, rest___]:=clouddeployresourceobject[id,info, rest]
	
clouddeployresourceobject[id_,localinfo_, rest___]:=Block[{type,newinfo=Association[],cloudinfo, res, fullinfo,cloudinfoco},
	type=getResourceType[localinfo];
	loadResourceType[type];
	fullinfo=repositoryBundleResourceObject[type,id, localinfo];
	If[TrueQ[ResourceSystemClient`$DeployResourceContent],
		newinfo=repositoryclouddeployResourceContent[type, id, fullinfo, rest];
		,
		If[containsLocalFileContentQ[localinfo],
			Message[ResourceObject::nocdep]
		]
	];
	If[TrueQ[ResourceSystemClient`$CreateCloudResourceCache],
		{cloudinfo, cloudinfoco}=repositoryclouddeployResourceInfo[type,id,  fullinfo,newinfo, rest];
		addToCloudResourceIndex[cloudinfo];
		AppendTo[$cloudResources,id];
		,
		cloudinfo=Join[fullinfo, newinfo];
		cloudinfo["RepositoryLocation"]=None;
		cloudinfo["ResourceLocations"]={};
	];
	cloudinfo["Autoload"]=True;
	If[MatchQ[{rest},{None,___}],
		cloudinfoco
		,
		Block[{System`ResourceObject},
			res=CloudDeploy[ExportForm[System`ResourceObject[cloudinfo],"WL"], rest];
			If[Head[res]===CloudObject,
				res
				,
				$Failed
			]
		]
	]
]

repositoryclouddeployResourceInfo[_,id_,  localinfo_, newinfo_, rest___]:=Block[{cloudinfo=localinfo,infoco},
	cloudinfo["ResourceLocations"]={CloudObject[cloudpath[resourceDirectory[id]]]};
	infoco=cloudResourceDirectoryObject[FileNameJoin[{StringTake[id,3], id,"metadata"}]];
	cloudinfo=Join[cloudinfo,newinfo];
	CloudPut[cloudinfo,infoco,takeFunctionOptions[CloudPut,rest]];
	{cloudinfo,infoco}
]

repositoryclouddeployResourceContent[___]:=Association[]
repositoryclouddeployResourceInfo[___]:=Throw[$Failed]



repositoryBundleResourceObject[_,_, localinfo_]:=localinfo

containsLocalFileContentQ[localinfo_]:=containslocalFileContentQ[Lookup[localinfo,{"ContentElementLocations","ContentLocation"}]]

containslocalFileContentQ[locations_]:=containslocalfileContentQ[Flatten[locations /. as_Association :> Values[as]]]

containslocalfileContentQ[locations_]:=(!FreeQ[locations,LocalObject|System`File])||AnyTrue[Select[locations,StringQ],FileExistsQ]

createResourceShingle[ro:HoldPattern[System`ResourceObject][as_Association]]:=Block[{id=Lookup[as,"UUID",Throw[$Failed]], res},
	res=If[MemberQ[$loadedResources,id],
		createresourceShingle[resourceInfo[id]]
		,
		createresourceShingle[as]
	];
	If[StringQ[res],
		res,
		ro
	]
]
	
	
createresourceShingle[info_Association]:=Block[{template, rtype},
	rtype=getResourceType[info];
	loadResourceType[rtype];
	template=shingletemplate[rtype];
	If[Head[template]===TemplateObject,
		createresourceshingle[template, info]
	]	
]

createresourceshingle[template_, info_]:=With[{shingleinfo=formatShingleInfo[info]},
	TemplateApply[template, shingleinfo]	
]

shingletemplate[rtype_]:=(shingletemplate[rtype]=fileTemplate[shingletemplatefile[rtype]])

fileTemplate[file_]:=FileTemplate[file]/;FileExistsQ[file]
fileTemplate[_]:=Throw[$Failed]

shingletemplatefile[_]:=FileNameJoin[{$rscDirectory,"templates","shingle.xml"}]

formatShingleInfo[info_]:=info

takeFunctionOptions[f_, rest___?OptionQ]:=Sequence@@FilterRules[Flatten[{rest}], Keys[Options[f]]]

takeFunctionOptions[f_, expr_,rest___?OptionQ]:=takeFunctionOptions[f,rest]

resourceLocalCache[ro_ResourceObject]:=resourcelocalDeploy[ro, LocalObject[]]
resourceLocalCache[ro_ResourceObject, lo_LocalObject]:=resourcelocalDeploy[ro, lo]

ResourceSystemClient`resourceLocalDeploy[args___]:=Catch[resourcelocalDeploy[args]]

resourcelocalDeploy[ro_ResourceObject]:=resourcelocalDeploy[ro, None]

resourcelocalDeploy[ro_ResourceObject, location_]:=Block[{res=saveResourceObject[ro]},
	If[!FailureQ[res],
		res=Switch[location,
			None, Null,
			_File,Put[ro, location[[1]]],
			_String|_LocalObject,Put[ro, location],
			_,(Message[ResourceRegister::invloc, location]);$Failed
		];
		If[!FailureQ[res],
			res
			,
			$Failed
		]
	]
]


resourcelocalDeploy[___]:=$Failed

End[] (* End Private Context *)

EndPackage[]



SetAttributes[{},
   {ReadProtected, Protected}
];