(* Wolfram Language Package *)

BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  
ResourceSystemClient`$ResourceSystemRequestBase
Begin["`Private`"] (* Begin Private Context *) 

$resourceSystemRequestBase:=ResourceSystemClient`$ResourceSystemRequestBase

ResourceSystemClient`$ResourceSystemRequestBase:=resourcesystembase[$CloudBase]

$rsbaseauto=False;

resourcesystembase[Automatic]:=Block[{$rsbaseauto=True},
	$resourceSystemRequestBase]/;!TrueQ[$rsbaseauto]
	
resourcesystembase[Automatic]:=$defaultRSbase

resourcesystembase[wolframcloud_]:=checkrsbase[URLBuild[{wolframcloud, "objects","resourcesystem","api","1.0"}]]/;!StringFreeQ[wolframcloud,"wolframcloud"]
resourcesystembase[_]:=(Message[ResourceObject::unkbase];Missing[])

checkrsbase[publiccloudbase_]:=publiccloudbase/;StringMatchQ[publiccloudbase,"https://www.wolframcloud.com/objects/resourcesystem/api/*"]

checkrsbase[base_]:=With[{check=Quiet[URLFetch[URLBuild[{base, "TestSystem"}],
	"StatusCode","VerifyPeer"->False,"ConnectTimeout"->3,"ReadTimeout"->1]]},
	If[check===200,
		checkrsbase[base]=base;
		base,
		If[rscheckattempts[base]>=3,
			checkrsbase[base]=resourcesystembase["https://www.wolframcloud.com/"]
			,
			rscheckattempts[base]=rscheckattempts[base]+1;
			resourcesystembase["https://www.wolframcloud.com/"]]
	]
]

rscheckattempts[_]:=0

defaultRSBase[]:=Which[
	StringQ[$resourceSystemRequestBase],$resourceSystemRequestBase,
	StringQ[resourcesystembase[$CloudBase]],resourcesystembase[$CloudBase],
	True,$defaultRSbase	
]

$defaultRSbase="https://www.wolframcloud.com/objects/resourcesystem/api/1.0";

$UnauthenticatedRequests={};
$ResourceSystemClientVersion="1.0";
authenticationRequired[endpoint_, requestBase_]:=!$CloudEvaluation

$ClientInformation:=($ClientInformation={
	"PacletVersion"->$ResourceSystemClientVersion,
	"WLVersion"->ToString[$VersionNumber],
	"MachineType"->ToString[$MachineType],
	"WordLength"->ToString[$SystemWordLength]
})

apifun[endpoint_,params_, head_]:=
	apifun[endpoint,params, head,$resourceSystemRequestBase]

apifun[endpoint_,params_, head_,requestBase_]:=apirequest[endpoint,params, head,targetResourceBase[requestBase]]

apirequest[endpoint_,params_, head_,requestBase_]:=Block[{code, headers,chars, res, fetchfun, base},
	fetchfun=If[requestBaseConnected[requestBase],
		URLFetch[CloudObject[#], ##2]&,
		If[!authenticationRequired[endpoint, requestBase],
			  URLFetch
			  ,
			  requestBaseConnect[head,requestBase];
			  URLFetch[CloudObject[#], ##2]&
		]
	];
	res=fetchfun[StringJoin[requestBase,"/",endpoint],{"StatusCode","Headers","ContentData"},
		"Parameters"->prepareParams[endpoint,params],"VerifyPeer"->False,"CredentialsProvider" -> None,Sequence@@additionalOptions[endpoint]];
	If[Length[res]===3,
		{code,headers, chars}=res,
		Message[head::rsunavail];Throw[$Failed]
	];
	If[code=!=200,
		handleError[head, endpoint, code, chars]
		,
		importResponse[head, endpoint, headers,chars]
	]
]/;StringQ[requestBase]

apifun[___]:=(Message[ResourceObject::unkbase];Throw[$Failed])
apirequest[___]:=(Message[ResourceObject::unkbase];Throw[$Failed])

targetResourceBase[str_String]:=str
targetResourceBase[Automatic]:=defaultRSBase[]

$CompressedParameterEndpoints=("SubmitResource"|"ReadResource"|"CopyResource")

prepareParams[$CompressedParameterEndpoints,params_]:={"ContentFormat"->"Compressed",(Sequence@@$ClientInformation),"Data"->Compress[params]}
prepareParams[_,{}]:=$ClientInformation
prepareParams[_,params_]:=Join[MapAt[ToString,params,{All,2}],$ClientInformation]

handleError[head_, endpoint_, code_, chars_]:=(Message[head::apierr,getErrorMessage[chars]];Throw[$Failed])

importResponse[head_,req_,headers_,chars_]:=Block[{mime, expr},
	mime=Cases[headers,{"Content-Type"|"content-type"|"ContentType",ct_}:>ct,{1},1];
	If[Length[mime]>0,mime=First[mime]];
	expr=If[!StringFreeQ[mime,"json",IgnoreCase->True],
		importJSONResponse[chars],
		ToExpression[FromCharacterCode[chars]]
	];
	importresponse[head, req,expr]	
]


importresponse[head_,req_, as_Association]:=importresponse[head,req,compressedCheck[head,warningCheck[head,as]]]/;KeyExistsQ[as,"Format"]
importresponse[head_,_,as_Association]:=warningCheck[head,as]
importresponse[_,_,expr_]:=expr

importJSONResponse[chars_]:=With[{l=ImportString[FromCharacterCode[chars],"JSON"]},
	If[ListQ[l],Association[l],Throw[$Failed]]
]

compressedCheck[head_,as_Association]:=With[{format=as["Format"]},
    If[format==="Compressed",
    	Uncompress[as["Data"]],
    	If[KeyExistsQ[as,"Data"],
    		as["Data"],
    		as
    	]
    ]
]/;KeyExistsQ[as,"Format"]

compressedCheck[_,expr_]:=expr


warningCheck[head_,as_Association]:=With[{warnings=Lookup[as,"Warnings",{}]},
	Message[head::apiwarn,#]&/@warnings;
	as
]/;KeyExistsQ[as,"Warnings"]

warningCheck[_,expr_]:=expr

additionalOptions["SubmitResource"]:={"Method"->"POST"}
additionalOptions[_]:={}

getErrorMessage[chars_]:=Block[{str=FromCharacterCode[chars], list},
	Quiet[list=ImportString[str,"JSON"];
		If[ListQ[list],
			If[KeyExistsQ[list,"Message"],
			    Lookup[list,"Message"]
			    ,
			    str
			],
			str
		]
	]]
	
       
fetchContent[head_,requestbase_,co_CloudObject,rest___]:=With[{url=First[co]},
	If[URLParse[url]["Domain"]===URLParse[$CloudBase]["Domain"],
		URLFetch[co,rest]
		,
		If[!authenticationRequired[url, requestbase],
			  URLFetch[url, rest]
			  ,
			  requestBaseConnect[head,requestbase];
			  URLFetch[co, rest]
		]
	]	
]
	
End[] (* End Private Context *)

EndPackage[]