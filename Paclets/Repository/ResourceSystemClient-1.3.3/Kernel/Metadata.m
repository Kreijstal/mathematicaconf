(* Wolfram Language Package *)

BeginPackage["ResourceSystemClient`"]

Begin["`Private`"] (* Begin Private Context *) 

validateParameter[rtype_,HoldPattern[Rule|RuleDelayed][key_,value_]]:=Rule[key,validateParameter[rtype,key,value]]

validateParameter[_,key_,value_]:=With[{res=Interpreter[$ResourceMetadataSchema[key]][value]},
	If[res===$Failed||!FreeQ[res,Failure],
		(Message[System`ResourceSubmit::invparam, key];Throw[$Failed])
		,
		res		
	]
]/;KeyExistsQ[$ResourceMetadataSchema,key]


validateParameter[rtype_,key_,value_]:=With[{res=Interpreter[respositoryMetadataSchema[rtype][key]][value]},
	If[res===$Failed||Head[res]===Failure,
		(Message[System`ResourceSubmit::invparam, key];Throw[$Failed])
		,
		res		
	]
]/;KeyExistsQ[respositoryMetadataSchema[rtype],key]

validateParameter[_,"SourceMetadata",expr_]:=validateSourceMetadata[expr]

validateParameter[_,"ExampleNotebook",expr_]:=validateexamplenotebook[expr]

validateParameter[_,"Asynchronous", expr_]:=expr

validateParameter[_,key_,___]:=(Message[System`ResourceSubmit::invparam, key];Throw[$Failed])
validateParameter[___]:=(Message[System`ResourceSubmit::invparam2];Throw[$Failed])

(********)

requiredparameters[expr_,___]:=Association["Name"->"String","UUID"->"String","ResourceType"->"String"]

resourceInfoOrder[_]:={
	"Name","UUID","ResourceType","Description",
	"RepositoryLocation","ResourceLocations",
	"ContentSize",
	"Version",
	"Keywords",
	"LatestUpdate"
};

basicInfoOrderFunc[rtype_]:=With[{order=resourceInfoOrder[rtype]},
	(# /. Thread[order -> Range[Length[order]]] &)];
	
sortBasicInfo[as_]:=KeySortBy[as, basicInfoOrderFunc[getResourceType[as]]]


$ResourceNameLengthMinimum=3;
$ResourceNameLengthLimit=80;
lengthregex[min_, max_] := 
  RegularExpression[StringJoin[".{", ToString[min], ",", ToString[max], "}"]]
$ShortMetadataLengthLimit=1000;
$MediumMetadataLengthLimit=10000;
$LongMetadataLengthLimit=100000;
$KeywordLengthLimit=40;

$ResourceMetadataSchema:=Association[
	"Name"->Restricted["String", lengthregex[$ResourceNameLengthMinimum,$ResourceNameLengthLimit]],
	"ShortName"->(With[{raw = #},Interpreter[Restricted["String", 
     	lengthregex[$ResourceNameLengthMinimum, $ResourceNameLengthLimit]], (URLEncode[URLDecode[#]] === raw) &]][#] &),
	"UUID"->Restricted["String", RegularExpression["\\w{8}-\\w{4}-\\w{4}-\\w{4}-\\w{12}$"]],
	"ResourceType"->$availableResourceTypes,
	"DisplayedSourceFields"->AnySubset[{"Contributor","Coverage","Creator","Date","Description","Language","Publisher","Rights","Source","Title"}],
	"Description"->Restricted["String", lengthregex[0,$ShortMetadataLengthLimit]],
	"Details"->Restricted["String", "*",$LongMetadataLengthLimit],
	"Keywords"->RepeatingElement[Restricted["String", lengthregex[0,$KeywordLengthLimit]]],
	"SeeAlso"->RepeatingElement[Restricted["Expression", {System`ResourceObject}] | Restricted["String",  RegularExpression["\\w{8}-\\w{4}-\\w{4}-\\w{4}-\\w{12}$"]]],
	"ExternalLinks"->RepeatingElement[Restricted["Expression", {Hyperlink, ButtonNote, Rule}] | "URL"],
	"PublisherID"->"String"
]

respositoryMetadataSchema[_]:=Association[]

validateSourceMetadata[as_Association]:=DeleteMissing[Association@KeyValueMap[validatesourceMetadata,as]]

validatesourceMetadata[key_,val_]:=With[{res=validatesourcemetadata[key, val]},
	If[res===$Failed||!FreeQ[res,Failure],
		(Message[System`ResourceSubmit::invparam, key];Throw[$Failed])
		,
		key->res		
	]
]

$sourcemetadataLengthLimit=1000;

validatesourcemetadata["Description",_List]:=$Failed
validatesourcemetadata[key_,l_List]:=Throw[$Failed]/;Length[l]>$sourcemetadataLengthLimit

$sourceinterpretertypes={
	Restricted["String","*",$ShortMetadataLengthLimit],"Person","Periodical","Company","University","School","Financial","Country","HistoricalCountry","City",
	"USState","USCounty","AdministrativeDivision","AstronomicalObservatory","Airport","Airline","WeatherStation",
	"Entity"
}

$sourceLinkTypes={Restricted["String","*",$ShortMetadataLengthLimit],"URL",Restricted["Expression", {Hyperlink, ButtonNote, Rule}]}


validatesourcemetadata["Contributor",expr_]:=interpretAlternatives["Contributor",$sourceinterpretertypes, expr]
validatesourcemetadata["Coverage",expr_]:=interpretAlternatives["Coverage",$sourceinterpretertypes, expr]
validatesourcemetadata["Creator",expr_]:=interpretAlternatives["Creator",$sourceinterpretertypes, expr]
validatesourcemetadata["Date",dates:{_DateObject...}]:=dates
validatesourcemetadata["Date",date_DateObject]:=date
validatesourcemetadata["Description",expr_]:=Interpreter[Restricted["String","*",$MediumMetadataLengthLimit]][expr]
validatesourcemetadata["Language",ent:HoldPattern[Entity]["Language", "English"|"French"|"German"|"Spanish"]]:=ent
validatesourcemetadata["Language",expr_]:=interpretAlternatives["Language",
	{Restricted["String","*",$ShortMetadataLengthLimit],"Language"}, expr]
validatesourcemetadata["Publisher",expr_]:=interpretAlternatives["Publisher",$sourceinterpretertypes, expr]
validatesourcemetadata["Rights",expr_]:=interpretAlternatives["Rights",$sourceLinkTypes, expr]
validatesourcemetadata["Source",expr_]:=interpretAlternatives["Source",$sourceLinkTypes, expr]
validatesourcemetadata["Title",expr_]:=Interpreter[Restricted["String","*",$ShortMetadataLengthLimit]][expr]

validatesourcemetadata[___]:=$Failed

validateSourceMetadata[___]:=$Failed

interpretAlternatives[key_,alts_, expr_List]:=Interpreter[Alternatives@@alts][expr]
interpretAlternatives[key_,alts_, expr_]:=Catch[Last[interpretalternatives[#,expr]&/@alts]]

interpretalternatives[interp_,expr_]:=With[{res=Interpreter[interp][expr]},
	If[res===$Failed||!FreeQ[res,Failure],
		res
		,
		Throw[res]		
	]
]

$resourceShingleProperty="DocumentationLink"
resourceURL[info_Association]:=Lookup[info, $resourceShingleProperty, resourceurl[info]]

resourceurl[info_Association]:=With[{rtype=getResourceType[info]},
	loadResourceType[rtype];
	repositoryResourceURL[rtype, info]
]

repositoryResourceURL[___]:=None

End[] (* End Private Context *)

EndPackage[]