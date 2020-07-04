(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {System`ResourceSearch}

BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  

System`ResourceSearch
ResourceSystemClient`RandomResources
ResourceSystemClient`ResourceSearchData

Begin["`Private`"] (* Begin Private Context *) 

Options[resourcesearch]=Options[System`ResourceSearch]={"SearchLocations"->All, Method->Automatic, "UpdateCloudIndex"->Automatic, CloudBase->Automatic};

System`ResourceSearch[args___]:=Catch[resourceSearch[args]]

ResourceSystemClient`ResourceSearchData[args___]:=Catch[resourceSearchData[args]]

resourceSearch[Rule[rtypes_,str_], rest___]:=resourceSearch[{rtypes,str}, rest]
resourceSearchData[Rule[rtypes_,str_], rest___]:=resourceSearchData[{rtypes,str}, rest]

resourceSearch[str_String, rest___]:=resourceSearch[{All,str}, rest]
resourceSearchData[str_String, rest___]:=resourceSearchData[{All,str}, rest]

resourceSearch[{rtypes_,str_}, rest___]:=createResourceObjects[resourcesearch[{rtypes,str}, rest]]
resourceSearchData[{rtypes_,str_}, rest___]:=createResourceDataset[resourcesearch[{rtypes,str}, rest]]

resourceSearch[]:=(Message[ResourceSearch::argt, ResourceSearch, 0, 1, 2];$Failed)
resourceSearchData[]:=(Message[ResourceSearch::argt, ResourceSearch, 0, 1, 2];$Failed)

resourceSearch[expr_,___]:=(Message[ResourceSearch::invquery,expr];$Failed)

resourceSearch[___]:=$Failed
resourceSearchData[___]:=$Failed

resourcesearch[q_, opts:OptionsPattern[]]:=resourcesearch[q, Automatic, opts]

resourcesearch[{rtypes_, str_String}, n_, OptionsPattern[]]:=Block[{list,progress=0, count,locations, res, temp, info},
    temp=PrintTemporary[ProgressIndicator[Dynamic[progress], {0,3}]];
	count=Switch[n,
		Automatic|Infinity, Infinity,
		0,Return[{}],
		_Integer, If[n>0, n, Message[ResourceSearch::invcount,n];Throw[$Failed]],
		_,Message[ResourceSearch::invcount,n];Throw[$Failed]
	];	
    locations=OptionValue["SearchLocations"];
    If[locations=!=All,locations=Flatten[{locations}]];
    If[locations===All||MemberQ[locations, "Local"],
		list=selectResourceTypes[resourcesearchLocal[str, n,OptionValue[Method]],rtypes];
    ];
    progress=1;
	If[Length[list]>=count,	Return[minimalSearchInfo/@Take[list,UpTo[count]]]];
    If[locations===All||MemberQ[locations, "Cloud"],
		list=DeleteDuplicates[Join[list,
			selectResourceTypes[resourceSearchCloud[str, count, OptionValue["UpdateCloudIndex"]],rtypes]],
			#1["UUID"]==#2["UUID"]&];
    ];
    progress=2;
	If[Length[list]>=count,	Return[minimalSearchInfo/@Take[list,UpTo[count]]]];
    If[locations===All||MemberQ[locations, "Repository"],	    
		list=Take[DeleteDuplicates[Join[list,
			selectResourceTypes[resourcesearchResourceSystem[str, n,rtypes,OptionValue[Method],OptionValue[CloudBase]],rtypes]],
			#1["UUID"]==#2["UUID"]&],UpTo[count]];
    ];
    NotebookDelete[temp];
	res=minimalSearchInfo/@list
]

resourcesearch[___]:=$Failed

selectResourceTypes[info_,All|"All"|"all"]:=DeleteMissing[info]
selectResourceTypes[info_,rtypes_List]:=Select[DeleteMissing[info],MemberQ[getresourceType/@rtypes,Lookup[#,"ResourceType"]]&]
selectResourceTypes[info_,rtypes_]:=selectResourceTypes[info,{rtypes}]

minimalSearchInfo[info_]:=If[MemberQ[$loadedResources,info["UUID"]],usableResourceInfo[info],info]

resourceSearchCloud[str_, n_, _]:=resourcesearchLocal[str, n, Automatic]/;$CloudEvaluation

resourceSearchCloud[str_, count_, True]:=Block[{cloudindex=updateCloudResourceIndex[], info},
	If[Head[cloudindex]===Dataset,
		$cloudResourceIndex=cloudindex;
		resourceSearchDataset[str,count,cloudindex]
		,
		{}
	]
]

resourceSearchCloud[str_, count_, update_]:=resourcesearchCloud[str,count, update]

resourcesearchCloud[str_,count_, Automatic]:=resourceSearchDataset[str, count,$cloudResourceIndex]/;Head[$cloudResourceIndex]===Dataset
resourcesearchCloud[str_,count_, Automatic]:=resourceSearchCloud[str, count, True]

resourceSearchDataset[str_,count_, ds_Dataset]:=With[{info=Normal[createResourceQuery[str][ds]]},
	If[ListQ[info],
		DeleteCases[Quiet[Normal[Catch[autoloadResource[DeleteMissing[#]]]]&/@Take[info, UpTo[count]]],$Failed]
		,
		{}
	]
]


createResourceQuery[q_String]:=With[{sp = allSortingProperties[]},
	Query[
	Select[MemberQ[Lookup[#, "Keywords", {}], q] ||
		MemberQ[Flatten[Lookup[#, sp, {}]], q]||
		(MemberQ[TextWords[ToLowerCase[#Name]], ToLowerCase[q]])|| 
		(MemberQ[TextWords[ToLowerCase[Lookup[#, "Description",""]]], ToLowerCase[q]])  &]]
]

$cloudResourceIndex=None;

updateCloudResourceIndex[]:=Block[{info=cloudResourceSearchInfo[]},
	If[MatchQ[info,{_Association...}],
		Dataset[KeyUnion[info,defaultsearchvalue]]
		,
		None
	]
]

defaultsearchvalue["Keywords"]:={}
defaultsearchvalue["Name"|"Description"]:=""
defaultsearchvalue[prop_]:={}/;MemberQ[allSortingProperties[],prop]
defaultsearchvalue[_]:=Missing[]
	
addToCloudResourceIndex[info_]:=addToCloudResourceIndex[info,$cloudResourceIndex]

addToCloudResourceIndex[info_,None]:=With[{index=addToCloudResourceIndex[]},
	If[Head[index]===Dataset,
		addToCloudResourceIndex[info,index]
		,
		$Failed
	]
]

addToCloudResourceIndex[info_Association,index_Dataset]:=With[{id=info["UUID"]},
	If[StringQ[id],
		If[MemberQ[index[All,"UUID"],id],
			dropFromCloudResourceIndex[id]
		];
		addtoCloudResourceIndex[info, index]
		,
		$Failed
	]
]

addtoCloudResourceIndex[info_, index_]:=Block[{keys, first, newindex, indexinfo},
	If[Length[index]>0,
		first=Normal[First[index]];
		indexinfo=Last[KeyUnion[{first,KeyTake[info, Keys[first]]},defaultsearchvalue]];
		newindex=Check[Append[$cloudResourceIndex,indexinfo],$Failed];
		If[newindex=!=$Failed,
			$cloudResourceIndex=newindex
			,
			$Failed
		]
	]
]

addToCloudResourceIndex[___]:=$Failed

dropFromCloudResourceIndex[id_]:=dropFromCloudResourceIndex[id,$cloudResourceIndex]

dropFromCloudResourceIndex[id_String,index_Dataset]:=Block[{newindex},
	newindex=Check[DeleteCases[index, _?(StringMatchQ[#UID, id] &)],$Failed];
	If[newindex=!=$Failed,
		$cloudResourceIndex=newindex
		,
		$Failed
	]
]

dropFromCloudResourceIndex[___]:=Null

cloudResourceSearchInfo[]:=Quiet[
	    CloudEvaluate[importLocalResourceInfo[]]]

resourcesearchLocal[str_, n_, method_Association]:=resourcesearchLocal[str, n, Lookup[method,"Local",Automatic]]

resourcesearchLocal[str_, n_, "BruteForce"]:=Block[{ids, count=n/.Automatic->Infinity},
	ids=Select[$localResources,resourceKeywordMatchQ[str,#]&,UpTo[count]];
	getResourceInfo/@ids
]


resourcesearchLocal[str_, n_, "TextSearch"]:=Block[{ids},
	ids=textSearchResources[str];
	If[ListQ[ids],
		getResourceInfo/@ids
		,
		$Failed
	]
]

resourcesearchLocal[str_, n_, Automatic]:=Block[{res=None},
	If[$AllowResourceTextSearch,
		res=Quiet[resourcesearchLocal[str, n, "TextSearch"]]
	];
	If[ListQ[res],
		res
		,
		resourcesearchLocal[str, n, "BruteForce"]
	]
]
resourceKeywordMatchQ[str_,id_]:=Block[{info=Quiet[getResourceInfo[id,{"Name","Keywords"}]]},
	TrueQ[!FreeQ[info,str]]
]


resourcesearchLocalName[str_, n_, rtype_:All]:=Block[{ids, count=n/.Automatic->Infinity},
	ids=Select[$localResources,resourceNameMatchQ[rtype,str,#]&,UpTo[count]];
	ids
]

resourceNameMatchQ[rtype_,str_,id_]:=Block[{info=Quiet[
	getResourceInfo[id,{"Name","ResourceType", "RepositoryLocation"}]]},
	If[marketplacebasedResourceQ[info],
		AppendTo[localResourceNameMap,info["Name"]->id];
		str===info["Name"]&&(rtype===All||rtype===info["ResourceType"])
		,
		False
	]
]

$resourceSystemSearchMethods={"Automatic",Automatic,"TextSearch","Table"};

resourcesearchResourceSystem[str_String, n_, rtypes_,method_, cloudbase_]:=Block[
	{res, opts=Sequence[], rtypestr=searchRTypeString[rtypes], resourcebase=resourcesystembase[cloudbase]},
	If[AssociationQ[method],
		If[KeyExistsQ[method,"ResourceSystem"],
			If[MemberQ[$resourceSystemSearchMethods,method["ResourceSystem"]],
				opts="SearchMethod"->method["ResourceSystem"]
			]
		]
	];
	res=apifun["SearchResources",{"Query"->str,"Count"->n,"ResourceTypes"->rtypestr,opts},System`ResourceSearch,resourcebase];
	If[KeyExistsQ[res,"Resources"],
		res=Select[Lookup[res,"Resources",{}],KeyExistsQ["UUID"]];
		res=standardizeResourceInfo/@res;
		res=fillResourceMetadata[#, Association["RepositoryLocation"->URL[resourcebase]]]&/@res;
		cacheresourceInfo[res];
		res
		,
		$Failed
	]
]

searchRTypeString[All|"All"|"all"]:="All"
searchRTypeString[l_List]:=StringRiffle[getresourceType/@l,","]
searchRTypeString[expr_]:=searchRTypeString[{expr}]

createResourceObjects[l_List]:=System`ResourceObject/@Select[l,AssociationQ]
createResourceObjects[expr_]:=expr

createResourceDataset[l_List]:=Dataset[KeyUnion[l,$SearchDataKeys]]
createResourceDataset[expr_]:=expr

ResourceSystemClient`RandomResources[args___]:=Catch[randomResources[args]]

Options[ResourceSystemClient`RandomResources]={CloudBase:>$CloudBase};

randomResources[opts:OptionsPattern[ResourceSystemClient`RandomResources]]:=randomResources[1,opts]

randomResources[n_Integer,opts:OptionsPattern[ResourceSystemClient`RandomResources]]:=Block[{res, resourcebase},
	resourcebase=resourcesystembase[OptionValue[ResourceSystemClient`RandomResources,{opts},CloudBase]];
    res=apifun["SearchResources",{"Query"->"","Count"->n,"Method"->"RandomSample"},System`ResourceSearch,resourcebase];
    If[KeyExistsQ[res,"Resources"],
        res=Lookup[res,"Resources",{}];
		res=fillResourceMetadata[#, Association["RepositoryLocation"->URL[$resourceSystemRequestBase]]]&/@res;
		cacheresourceInfo[res];
        System`ResourceObject[usableResourceInfo[#]]&/@res
        ,
        $Failed
    ]
]

randomResources[___]:=$Failed
End[] (* End Private Context *)

EndPackage[]

SetAttributes[{ResourceSearch},
   {ReadProtected, Protected}
];