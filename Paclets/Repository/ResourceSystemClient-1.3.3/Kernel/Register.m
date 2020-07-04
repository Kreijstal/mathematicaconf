(* Wolfram Language Package *)

BeginPackage["ResourceSystemClient`"]

System`ResourceRegister

ResourceSystemClient`$CacheCloudRegistries

Begin["`Private`"] (* Begin Private Context *) 

System`ResourceRegister[args___]:=Catch[resourceRegister[args]]

resourceRegister[ro_]:=resourceRegister[ro, defaultResourceRegistries[]]

resourceRegister[ro_, expr_]:=resourceRegister[ro,{expr}]/;!ListQ[expr]

resourceRegister[ro_ResourceObject, locations_]:=With[{id=resourceObjectID[ro]},
	resourceregister[id, getResourceInfo[id], locations]
]

resourceregister[id_, info_Association, locations_]:=With[{rtype=info["ResourceType"]},
	If[StringQ[rtype],
		loadResourceType[rtype];
		repositoryResourceRegister[rtype, id, info, locations]
		,
		$Failed
	]
]

defaultResourceRegistries[]:={$DefaultLocalBase, $CloudBase}

(* default *)
repositoryResourceRegister[rtype_, id_, info_, locations_List]:=
	repositoryresourceRegister[rtype, id, info, #]&/@locations

repositoryResourceRegister[___]:=$Failed

repositoryresourceRegister[rtype_, id_, info_, location_]:=repositoryResourceRegisterLocal[rtype, id, info]/;localResourceRegistryQ[location]
repositoryresourceRegister[rtype_, id_, info_, location_]:=repositoryResourceRegisterRemote[rtype, id, info,location]/;cloudResourceRegistryQ[location]

repositoryresourceRegister[__,location_]:=(Message[ResourceRegister::invloc,location];$Failed)

resourceRegister[___]:=$Failed

localResourceRegistryQ[$DefaultLocalBase]=True;
localResourceRegistryQ[___]:=False
cloudResourceRegistryQ[$CloudBase]=True;
cloudResourceRegistryQ[___]:=False;

(* default *)
repositoryResourceRegisterLocal[rtype_, id_,info_]:=Block[{name=Lookup[info,"Name",Throw[$Failed]]},
	saveresourceObject[info];
	addToLocalRegistry[rtype,name,id];
	localObject[resourceDirectory[id]]
]

repositoryResourceRegisterRemote[rtype_, id_, info_, cloudbase_]:=Block[{name=Lookup[info,"Name",Throw[$Failed]], co},
	If[MemberQ[$loadedResources, id],
		co=clouddeployResourceObject[id, resourceInfo[id], None,CloudBase->cloudbase]
		,
		autoloadResource[info];
		If[MemberQ[$loadedResources, id],
			co=clouddeployResourceObject[id, resourceInfo[id], None,CloudBase->cloudbase]
			,
			Throw[$Failed]
		]
	];
	addToCloudRegistry[cloudbase,rtype,name,id];
	co
]

$RegistryBase="ResourceRegistry";
localRegistryDirectory[]:=FileNameJoin[{LocalObjects`PathName[LocalObject[$DefaultLocalBase]], $RegistryBase}]
localRegistryDirectory[rtype_]:=FileNameJoin[{localRegistryDirectory[], rtype}]
localRegistryFile[rtype_, name_]:=localregistryFile[rtype, stringhash[name]]
localregistryFile[rtype_, hash_]:=FileNameJoin[{localRegistryDirectory[rtype], StringTake[hash, 3],hash}]

localRegisteredResourceTypes[]:=FileNameTake/@FileNames["*",localRegistryDirectory[]]

$CloudRegistryBase="ResourceRegistry";
cloudRegistryDirectory[cloudbase_]:=FileNameJoin[{$CloudRootDirectory,$CloudRegistryBase}]
cloudRegistryDirectory[cloudbase_, rtype_]:=FileNameJoin[{cloudRegistryDirectory[cloudbase], rtype}]
cloudRegistryFile[cloudbase_, rtype_, name_]:=cloudregistryFile[cloudbase, rtype, stringhash[name]]
cloudregistryFile[cloudbase_, rtype_, hash_]:=FileNameJoin[{cloudRegistryDirectory[cloudbase, rtype], StringTake[hash, 3], hash}]

addToLocalRegistry[rtype_,name_,id_]:=With[{file=localRegistryFile[rtype, name]},
	If[FileExistsQ[file],
		If[Get[file]===(name->id),Return[Null]];
		Message[ResourceRegister::namext,name->id];Throw[$Failed]
		,
		createDirectory[FileNameDrop[file]];
		addtoRegistry[rtype, file,name,id]
	]
]

addToCloudRegistry[cloudbase_,rtype_,name_,id_]:=With[{file=cloudRegistryFile[cloudbase, rtype, name]},
	If[FileExistsQ[file],
		If[Get[file]===(name->id),Return[Null]];
		Message[ResourceRegister::namext,name->id];Throw[$Failed]
		,
		addtoRegistry[rtype, file,name,id]
	];
	If[ResourceSystemClient`$CacheCloudRegistries,
		addToCloudRegistryCache[cloudbase, rtype, name->id]
	]
]

addtoRegistry[rtype_, file_,name_,id_]:=Put[name->id, file]

getLocalRegistry[rtype_]:=getlocalRegistry[localRegistryDirectory[rtype]]

getlocalRegistry[dir_]:=With[{files=FileNames["*",dir,{2}]},
	Cases[Get/@Select[files,!DirectoryQ[#]&],_Rule,{1}]
]/;DirectoryQ[dir]

getlocalRegistry[_]:={}

getCloudRegistry[cloudbase_, rtype_]:={}

deleteFromLocalRegistry[rtype_, name_String]:=deletefromLocalRegistry[localRegistryFile[rtype, name]]
deleteFromLocalRegistry[rtype_, r_Rule]:=deleteFromLocalRegistry[rtype,First[r]]

deleteFromLocalRegistry[___]:=$Failed

deletefromLocalRegistry[file_]:=Null/;!FileExistsQ[file]
deletefromLocalRegistry[file_]:=DeleteFile[file]

localRegistryLookup[Rule[rtype_String, name_String]]:=localregistryLookup[rtype, name]
localRegistryLookup[name_String]:=localregistryLookup[All, name]

localRegistryLookup[___]:=None

localregistryLookup[rtype_String, name_String]:=registryLookup[localRegistryFile[rtype, name]]

localregistryLookup[All, name_String]:=With[{types=localRegisteredResourceTypes[]},
	If[Length[types]>0,
		Last@Catch[With[{res=localregistryLookup[#, name]},
			If[uuidQ[res],
				Throw[{res},"ResourceFound"]
				,
				None
			]
		]&/@types,"ResourceFound"]
		,
		None
	]
]

registryLookup[co_CloudObject]:=registrylookup[co]
registryLookup[File[file_,___]]:=registrylookup[First[file]]
registryLookup[str_String]:=registrylookup[str]/;FileExistsQ[str]

registryLookup[_]:=None

registrylookup[file_]:=With[{rule=Quiet[Get[file]]},
	registrylookup0[rule]
]

registrylookup[__]:=None

registrylookup0[Rule[name_String, uuid_String]]:=uuid
registrylookup0[___]:=None

ResourceSystemClient`$CacheCloudRegistries=False;

cloudRegistryLookup[Rule[rtype_String, name_String], base_]:=cloudregistryLookup[rtype, name, base]
cloudRegistryLookup[Rule[rtype_String, name_String]]:=cloudregistryLookup[rtype, name, All]
cloudRegistryLookup[name_String]:=cloudregistryLookup[All, name, All]

cloudRegistryLookup[___]:=None

cloudregistryLookup[rtype_String, name_String]:=cloudregistryLookup[rtype, name, All]

cloudregistryLookup[rtype_, name_String, All]:=With[{bases={$CloudBase}},
	Last@Catch[
		With[{res=cloudregistryLookup[rtype, name,#]},
			If[uuidQ[res],
				Throw[{res},"ResourceFound"]
				,
				None
			]
		]&/@bases
		,
		"ResourceFound"
	]	
]

cloudregistryLookup[rtype_String, name_String,base_]:=If[ResourceSystemClient`$CacheCloudRegistries,
	cachedCloudRegistryLookup[rtype, name, base],
	cloudregistrylookup[rtype, name, base]
]


cloudregistryLookup[All, name_String,base_]:=With[{types=cloudRegisteredResourceTypes[base]},
	If[Length[types]>0,
		Last@Catch[With[{res=cloudregistryLookup[#, name, base]},
			If[uuidQ[res],
				Throw[{res},"ResourceFound"]
				,
				None
			]
		]&/@types,"ResourceFound"]
		,
		None
	]
]

cloudregistrylookup[rtype_, name_, base_]:=registryLookup[cloudRegistryFile[base, rtype, name]]

cloudregistryLookup[_]:=None

cloudRegisteredResourceTypes[base_]:=If[ResourceSystemClient`$CacheCloudRegistries,
	cloudregisteredResourceTypes[base]
	,
	cloudregisteredresourceTypes[base]
]


cloudregisteredResourceTypes[base_]:=If[ResourceSystemClient`$CacheCloudRegistries,
	cloudregisteredResourceTypes[base]=cloudregisteredresourceTypes[base]
]

cloudregisteredresourceTypes[base_]:=With[{co=cloudRegistryDirectory[base]},
	CloudEvaluate[FileNameTake/@FileNames["*",FileNameDrop[CloudObjectInformation[co,"Path"],1]], CloudBase->base]
]

cachedCloudRegistryLookup[rtype_, name_, base_]:=Lookup[cloudRegistryCache[rtype, base],name,None]

cloudRegistryCache[rtype_, base_]:=If[AssociationQ[cloudregistryCache[rtype, base]],
	cloudregistryCache[rtype, base],
	getCloudRegistryCache[rtype, base]
]

getCloudRegistryCache[rtype_, base_]:=With[{res=getcloudRegistryCache[rtype, base]},
	If[AssociationQ[res],
		cloudregistryCache[rtype, base]=res
		,
		Association[]
	]
]

getcloudRegistryCache[rtype_, base_]:=Block[{co=cloudRegistryDirectory[base, rtype], res},
	res=CloudEvaluate[
		Cases[(Get/@Select[
			FileNames["*",FileNameDrop[CloudObjectInformation[co,"Path"],1],2]
			,
			!DirectoryQ[#]&
		]),_Rule,{1}], 
		CloudBase->base];
	If[MatchQ[res,{_Rule...}],Association[res],None]
]

addToCloudRegistryCache[base_, rtype_, name_, id_]:=With[{cache=cloudregistryCache[rtype, base]},
	If[AssociationQ[cache],
		cloudregistryCache[rtype, base][name]=id
	]	
]

End[] (* End Private Context *)

EndPackage[]