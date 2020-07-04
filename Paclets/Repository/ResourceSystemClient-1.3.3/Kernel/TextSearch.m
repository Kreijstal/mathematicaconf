(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}

BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  


Begin["`Private`"] (* Begin Private Context *) 

$AllowResourceTextSearch=TrueQ[$VersionNumber>=11.0]

$textSearchIndexKeys:=Join[{"Name","Keywords","Description"},allSortingProperties[]]

$ResourceSearchIndexName="ResourceObjectSearchIndex";

buildLocalResourceSearchIndex[]:=Block[{files},
	If[checkDeleteSearchIndex[],
		createTextSearchIndexFile/@$localResources;
		buildlocalResourceSearchIndex[]
		,
		$AllowResourceTextSearch=False;
		$Failed		
	]
]/;$AllowResourceTextSearch

buildlocalResourceSearchIndex[]:=With[{res=Quiet[CreateSearchIndex[textSearchDirectory[],$ResourceSearchIndexName]]},
	If[Head[res]===SearchIndexObject,
		res
		,
		$Failed
	]
]

checkCreateSearchIndex[args___]:=Quiet[With[{index=CreateSearchIndex[args]},
	If[Head[index]===SearchIndexObject,
		index,
		$AllowResourceTextSearch=False;
		$Failed
	]	
]]

updateLocalResourceSearchIndex[]:=updatelocalResourceSearchIndex[Quiet[SearchIndexObject[$ResourceSearchIndexName]]]
updatelocalResourceSearchIndex[sio_SearchIndexObject]:=
With[{res=Quiet[UpdateSearchIndex[sio]]},
	If[res===$Failed,
		buildLocalResourceSearchIndex[]
		,
		sio]
]

updatelocalResourceSearchIndex[_]:=buildLocalResourceSearchIndex[]

checkDeleteSearchIndex[]:=
	Quiet[
		DeleteSearchIndex[SearchIndexObject[$ResourceSearchIndexName]];
		SearchIndexObject[$ResourceSearchIndexName]===$Failed
	]


$updateResourceSearchIndex=True;

addToLocalSearchIndex[id_]:=addToLocalSearchIndex[id, getResourceInfo[id],resourceDirectory[id]]
addToLocalSearchIndex[id_, info_]:=addToLocalSearchIndex[id, info,resourceDirectory[id]]
addToLocalSearchIndex[id_, info_, dir_]:=With[{indexfile=createtextSearchIndexFile[id,info, dir]},
	If[TrueQ[Quiet[FileExistsQ[indexfile]]]&&$updateResourceSearchIndex,
		updateLocalResourceSearchIndex[]		
	]
]

createTextSearchIndexFile[id_]:=createtextSearchIndexFile[id,getResourceInfo[id],resourceDirectory[id]]
createtextSearchIndexFile[id_,info_Association, dir_]:=Block[{indexfile,string},	
	indexfile=textSearchIndexFile[id];
	createDirectory[FileNameDrop[indexfile]];
	string=Quiet[createtextsearchIndexString[info]];
	If[!TrueQ[Quiet[StringLength[string]>0]],
		Return[Null]
	];
	Export[indexfile,string,"String"];
	indexfile
]/;DirectoryQ[dir]
	
createtextSearchIndexFile[___]:=Null	

createtextsearchIndexString[info_]:=Block[{sourceinfostring,infostring},
	infostring=StringJoin[createTextSearchProperty[info,#]&/@$textSearchIndexKeys];
	sourceinfostring=createTextSearchSourceInfoString[info];
	StringJoin[{infostring,"\n",sourceinfostring}]
]

textSearchDirectory[]:=With[{dir=FileNameJoin[{resourceCacheDirectory[],"searchdata"}]},
	createDirectory[dir];
	textSearchDirectory[]=dir
]

textSearchIndexFile[id_]:=FileNameJoin[{textSearchDirectory[], StringTake[id, 3], id<>".txt"}]

createTextSearchProperty[info_,key_]:=""/;!KeyExistsQ[info,key]
createTextSearchProperty[info_,key_]:=createtextSearchProperty[key,info[key]]

createtextSearchProperty[key_String,l_List]:=StringJoin[createtextSearchProperty[key,#]&/@l]
createtextSearchProperty[key_String,str_String]:=
	StringJoin[{key," ",textSearchEscape[str]," \n"}]
createtextSearchProperty[key_String,ent_Entity]:=createtextSearchProperty[key,CommonName[ent]]

createtextSearchProperty[__]:=""

createTextSearchSourceInfoString[info_Association]:=""/;!KeyExistsQ[info,"SourceMetadata"]
createTextSearchSourceInfoString[info_Association]:=createtextSearchSourceInfoString[DeleteMissing[info["SourceMetadata"]]]
createTextSearchSourceInfoString[_]=""

createtextSearchSourceInfoString[sourceinfo_Association]:=KeyValueMap[createtextSearchProperty,sourceinfo]
createtextSearchSourceInfoString[_]:=""

tsecapeRules[]:=(tsecapeRules[]=(#->"textsearch"<>#)&/@$textSearchIndexKeys)

textSearchEscape[str_]:=ToLowerCase[StringReplace[str,tsecapeRules[]]]

clearSearchIndex[id_]:=clearsearchIndex[textSearchIndexFile[id]]
clearsearchIndex[file_]:=(DeleteFile[file];
	UpdateSearchIndex[$ResourceSearchIndexName];
)/;FileExistsQ[file]

textSearchResources[q_String]:=textsearchResources[q]/;$AllowResourceTextSearch

textSearchResources[_]:={}

textsearchResources[q_String]:=Block[{index, contobjs},
    index=Quiet[SearchIndexObject[$ResourceSearchIndexName]];
    If[Head[index]=!=SearchIndexObject,
        index=buildLocalResourceSearchIndex[]
    ];
    If[Head[index]===SearchIndexObject,
    	contobjs=normalSearchResults[TextSearch[index, q]];
    	getSearchResultIDs[contobjs]
    	,
    	$Failed
    ]    
]

getSearchResultIDs[l_List]:=Select[FileBaseName[#["FileName"]]&/@Cases[l,_ContentObject],StringQ]

normalSearchResults[sro_SearchResultObject]:=Normal[sro]
normalSearchResults[expr_]:=expr


End[] (* End Private Context *)

EndPackage[]

SetAttributes[{},
   {ReadProtected, Protected}
];