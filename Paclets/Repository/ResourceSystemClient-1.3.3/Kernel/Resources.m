(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}

BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  

ResourceSystemClient`MyResources
ResourceSystemClient`SyncResources

Begin["`Private`"] (* Begin Private Context *) 

$localResources:=($localResources=importlocalResourceIDs[])
$cloudResources:=($cloudResources=importCloudResourceIDs[])/;$CloudConnected
$cloudResources:={}

$myResourceSystemResources={};

$loadedResources={};

localResourceNameMap=Association[];

importLocalResourceIDs[]:=($localResources=importlocalResourceIDs[])

importlocalResourceIDs[]:=With[{files=FileNames["metadata",resourceCacheDirectory[],3]},
    FileNameTake[#,{-2}]&/@files
    ]

importCloudResourceIDs[]:=(importLocalResourceIDs[];
    $cloudResources=$localResources;
    $localResources
)/;$CloudEvaluation

importCloudResourceIDs[]:=With[{
	res=CloudEvaluate[importlocalResourceIDs[]]},
	If[ListQ[res],
		$cloudResources=res
		,
		{}
	]
]

importSystemResourceIDs[]:=With[{res=myResourceSystemResources[False]},
	If[ListQ[res],
		$myResourceSystemResources=res
	]
]

myResourceSystemResources[includeMetadataQ_:True]:=Block[{res},
    res=apifun["UserResources",{"IncludeMetadata"->includeMetadataQ}, ResourceSystemClient`MyResources];
    Lookup[res,"Resources",{}]
]/;requestBaseConnected[]

myResourceSystemResources[args___]:=(
    cloudConnect[ResourceSystemClient`MyResources];
    myResourceSystemResources[args]
    )

importLocalResourceInfo[]:=With[{ids=importLocalResourceIDs[]},
      Select[getResourceInfo/@ids,AssociationQ]
    ]


(* Syncing resource info *)
ResourceSystemClient`SyncResources[args___]:=syncResources[args]

syncResources[]:=Block[{progress=0},
    PrintTemporary[ProgressIndicator[Dynamic[progress], {0,3}]];
    progress=1;
    syncWithSystemResources[];
    progress=2;
    syncWithCloudResources[];
    progress=3;
    $ResourceSystemSyncedQ=True
]

syncWithSystemResources[]:=Block[{marketplaceresourceInfo, allinfo=Association[], ids={},cloudresouceInfo},
    marketplaceresourceInfo=myResourceSystemResources[True];
    marketplaceresourceInfo=Select[marketplaceresourceInfo,AssociationQ];
    marketplaceresourceInfo=fillResourceMetadata[#,Association["RepositoryLocation"->URL[$resourceSystemRequestBase],"MyAccount"->True]]&/@marketplaceresourceInfo;
    $myResourceSystemResources=Select[Lookup[marketplaceresourceInfo,"UUID"],StringQ];
    cacheresourceInfo[Select[marketplaceresourceInfo,AssociationQ]]
]

syncWithCloudResources[]:=Block[{cloudresources},
    cloudresources=CloudEvaluate[importLocalResourceInfo[]];
    cacheresourceInfo[cloudresources];
    $cloudResources=With[{localresources=importLocalResourceInfo[]},
    CloudEvaluate[syncwithCloudResources[localresources]]
    ];
    importLocalResourceIDs[]
]/;$CloudConnected&&(!$CloudEvaluation)

syncWithCloudResources[]:=Null

syncwithCloudResources[localresources_]:=(cacheresourceInfo[localresources];
        importlocalResourceIDs[])

(* loading resources *)
loadResource[id_String]:=resourceInfo[id]/;MemberQ[$loadedResources,id]
loadResource[id_String]:=With[{info=getResourceInfo[id]},
	If[AssociationQ[info],
        loadresource[id, info];
		info
		,
		$Failed
	]]/;MemberQ[$localResources,id]

loadResource[name_String]:=loadResource[
	Lookup[localResourceNameMap,name,$Failed]]/;KeyExistsQ[localResourceNameMap,name]
	
loadResource[id_String]:=With[{
	info=CloudEvaluate[getResourceInfo[id]]},
    If[AssociationQ[info],
        cacheresourceinfo[info]
        ,
        $Failed
    ]]/;MemberQ[$cloudResources,id]
    
loadResource[id_String]:=With[{info=importresourceInfo[id, False]},
    If[AssociationQ[info],
        info
        ,
        $Failed
    ]]/;MemberQ[$myResourceSystemResources,id]
    
loadResource[id_String]:=With[{info=importresourceInfo[id, False]},
    If[AssociationQ[info],
        info
        ,
        $Failed
    ]]/;uuidQ[id]
    
loadResource[name_String]:=With[{info=importresourceInfo[name, False]},
    If[Quiet[KeyExistsQ[info,"UUID"]],
        info
        ,
        $Failed
    ]]

loadResource[___]:=$Failed

loadresource[info_]:=loadresource[Lookup[info,"UUID"],info]
loadresource[id_String,info_Association]:=(
    resourceInfo[id]=info;
	If[marketplacebasedResourceQ[info],
    	AppendTo[localResourceNameMap,info["Name"]->id]
	];
    $loadedResources=DeleteDuplicates[Append[$loadedResources,id]];
)
loadresource[__]:=$Failed

clearresource[id_, name_]:=(
    Quiet[resourceInfo[id]=.];
    If[StringQ[name],
    	localResourceNameMap=KeyDrop[localResourceNameMap,name]
    ];
    $loadedResources=DeleteCases[$loadedResources,id];
)


(*** MyResources ***)


ResourceSystemClient`MyResources[args___]:=Catch[myResources[args]]

myResources[args___]:=If[$ResourceSystemSyncedQ,
        myresources[args]
        ,
        syncResources[args];
        myresources[args]
    ]

myresources[___]:=System`ResourceObject/@$localResources

findResourceObject[type_, name_, opts___]:=Block[{res, temp,progress=0, id,cloudbase},
	temp=PrintTemporary[ProgressIndicator[Dynamic[progress], {0,5}]];
	
	res=lookupResourceNameMap[type, name];
	progress=1;
	If[uuidQ[res],
		Return[ResourceObject[res]]
	];
	
	progress=2;
	If[StringContainsQ[name,"://"],
		NotebookDelete[temp];
		res=findResourceObjectByURL[name];
		If[res=!=$Failed,
			Return[res]
		]
	];
	res=localregistryLookup[type, name];
	progress=3;
	If[uuidQ[res],
		Return[ResourceObject[res]]
	];
	res=resourcesearchLocalName[name, 1, type];
	progress=4;
	If[MatchQ[res,{_?uuidQ,___}],
		Return[ResourceObject[First[res]]]
	];
	res=cloudregistryLookup[type, name];
	progress=5;
	If[uuidQ[res],
		Return[ResourceObject[res]]
	];
	cloudbase=OptionValue[ResourceObject,{opts},CloudBase];
	res=If[cloudbase=!=$CloudBase,
		resourceAcquire[name, False,cloudbase]
		,
		resourceAcquire[name, False]
	];
	NotebookDelete[temp];
	res
]


findResourceObjectByURL[url_URL]:=findResourceObjectByURL[First[url]]
findResourceObjectByURL[url_String]:=With[{as=URLParse[url]},
	If[AssociationQ[as],
		repositoryResourceByURL[as["Domain"],as["Path"]]
	]
]

repositoryResourceByURL[_,{___,"resources"|"Resources",name_}]:=findResourceObject[All, name]/;StringFreeQ[name,"://"]
repositoryResourceByURL[___]:=$Failed

lookupResourceNameMap[All, name_]:=Lookup[localResourceNameMap,name,$Failed]/;KeyExistsQ[localResourceNameMap,name]

lookupResourceNameMap[type_, name_]:=With[{id=Lookup[localResourceNameMap,name,$Failed]},
	If[Quiet[MemberQ[Flatten[{type}],Lookup[getResourceInfo[id],"ResourceType"]]],
		id,
		$Failed
	]
	]/;KeyExistsQ[localResourceNameMap,name]

lookupResourceNameMap[__]:=$Failed	

End[] (* End Private Context *)

EndPackage[]

SetAttributes[{},
   {ReadProtected, Protected}
];