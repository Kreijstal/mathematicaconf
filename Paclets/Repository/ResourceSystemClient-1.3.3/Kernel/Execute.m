(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}


BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  
ResourceSystemClient`ResourceInformation

Begin["`Private`"] (* Begin Private Context *) 

ResourceSystemClient`ResourceInformation[args___]:=Catch[resourceInformation[args]]

resourceInformation[resource_ResourceObject,rest___]:=resourceInformation[resourceObjectID[resource],rest]

resourceInformation[id_String,rest___]:=resourceinformation[id,resourceInfo[id],rest]/;MemberQ[$loadedResources, id]

resourceInformation[id_String,rest___]:=With[{loaded=loadResource[id]},
		If[AssociationQ[loaded],
			resourceinformation[id,loaded,rest]
			,
			Throw[$Failed]
		]
	]

resourceInformation[expr_,___]:=(Message[ResourceObject::invro,expr];Throw[$Failed])

resourceinformation[id_String,info_Association,rest___]:=resourceinformation0[getResourceType[info],id,info,rest]

$ReservedProperties={"Properties","DocumentationLink"};

resourceinformation0[type_,id_,info_,as_Association,rest___]:=With[{prop=as["Property"]},
	If[KeyExistsQ[info, prop]||MemberQ[$ReservedProperties,prop],
		resourceMetadataLookup[type, id, info, prop,as,rest]
		,
		Message[ResourceObject::unkpar,prop];Throw[$Failed]
	]
]/;KeyExistsQ[as,"Property"]

resourceMetadataLookup[type_, id_, info_, "Properties",___]:=Keys[info]

resourceMetadataLookup[type_, id_, info_, "SourceMetadata",as_,___]:=formatResourceMetadata[Lookup[info,"SourceMetadata"], "SourceMetadata", as["Parameters"]]/;KeyExistsQ[as,"Parameters"]

resourceMetadataLookup[type_, id_, info_, "DocumentationLink",as_,___]:=resourceURL[info]

resourceMetadataLookup[type_, id_, info_, str_,rest___]:=formatResourceMetadata[Lookup[info,str], str, rest]/;KeyExistsQ[info, str]

formatResourceMetadata[elems_,"ContentElements",___]:=DeleteCases[elems,Automatic]

formatResourceMetadata[smd_, "SourceMetadata", {}]:=smd

formatResourceMetadata[smd_, "SourceMetadata", {str_String}]:=Lookup[smd,str]
formatResourceMetadata[smd_, "SourceMetadata", _]:=$Failed

formatResourceMetadata[expr_,___]:=expr

readresource[id_, params_]:=apifun["ReadResource",Join[params,Association["UUID"->id]],System`ResourceObject]


End[] (* End Private Context *)

EndPackage[]

SetAttributes[{},
   {ReadProtected, Protected}
];