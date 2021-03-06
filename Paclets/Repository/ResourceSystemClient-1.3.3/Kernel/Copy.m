(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}


BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  
ResourceSystemClient`ResourceDownload
ResourceSystemClient`ResourceUpdate

Begin["`Private`"] (* Begin Private Context *) 


ResourceSystemClient`ResourceDownload[args___]:=Catch[resourceDownload[args]]

Options[ResourceSystemClient`ResourceDownload]={CloudBase->Automatic};

resourceDownload[resource:rpat, rest___]:=resourceDownload[resourceObjectID[resource], rest]

resourceDownload[str_String]:=resourceDownload[str,Association[]]

resourceDownload[str_String, params_Association, opts:OptionsPattern[ResourceSystemClient`ResourceDownload]]:=Block[
	{res,resource,downloadedversion, id=str, 
		repositorybase=resourcesystembase[OptionValue[ResourceSystemClient`ResourceDownload, {opts}, CloudBase]]},
	If[!MemberQ[$localResources,str],
		resource=resourceAcquire[str, False,repositorybase];
		If[MatchQ[resource,rpat],
			id=resourceObjectID[resource];
			repositorybase=resourceRepositoryBase[resource];
			,
			Return[$Failed]
		]
		,
		repositorybase=resourcerepositoryBase[getResourceInfo[str]]
	];
	res=apifun["CopyResource",Join[params,Association["UUID"->id]],ResourceSystemClient`ResourceDownload,repositorybase];
	resourcedownload[res];
	System`ResourceObject[id]
]

resourcedownload[res_]:=With[{rtype=getResourceType[res]},
	If[!StringQ[rtype],Throw[$Failed]];
	loadResourceType[rtype];
	repositoryresourcedownload[rtype,
	Lookup[res,"UUID",Throw[$Failed]],res]/;KeyExistsQ[res,"UUID"]
]

resourcedownload[___]:=$Failed

repositoryresourcedownload[___]:=Null

storeDownloadVersion[id_,res_, locations_, formats_, as_Association]:=storeDownloadVersion[id,res, Association[as,"Locations"->locations, "Formats"->formats]]

storeDownloadVersion[id_,res_, as_:Association[]]:=Block[
	{dv=Lookup[res,"Version"],infofile, info},
	If[!MatchQ[dv,None|_String],Return[$Failed]];
	infofile=resourceInfoFile[id];
	info=Get[infofile];
	info["DownloadedVersion"]=dv;
	info=updateRepositoryResourceInfo[getResourceType[info],id, info, as];
	Put[info,infofile];
	resourceInfo[id]=info
]

updateRepositoryResourceInfo[_,_, info_, __]:=info



cloudResourceDownload[info_, as_]:=With[{rtype=getResourceType[info]},
	If[!StringQ[rtype],Throw[$Failed]];
	loadResourceType[rtype];
	repositorycloudResourceDownload[rtype, info, as]	
]

repositorycloudResourceDownload[___]:=$Failed


End[] (* End Private Context *)

EndPackage[]



SetAttributes[{},
   {ReadProtected, Protected}
];