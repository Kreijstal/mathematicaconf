(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {System`ResourceSubmit}

BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  

System`ResourceSubmit

Begin["`Private`"] (* Begin Private Context *) 

$SubmissionSizeLimit=10^6;

ResourceSystemClient`$PublisherID=Automatic;

Options[System`ResourceSubmit]=Options[resourceSubmit]={"PublisherID":>ResourceSystemClient`$PublisherID, CloudBase->Automatic};

System`ResourceSubmit[args___]:=Catch[resourceSubmitCloudConnect[args]]

resourceSubmitCloudConnect[ro_,opts:OptionsPattern[System`ResourceSubmit]]:=resourceSubmit[ro, opts]/;requestBaseConnected[OptionValue[System`ResourceSubmit, {opts}, CloudBase]]

resourceSubmitCloudConnect[ro_,opts:OptionsPattern[System`ResourceSubmit]]:=(
	cloudConnect[System`ResourceSubmit, OptionValue[System`ResourceSubmit, {opts}, CloudBase]];
	resourceSubmit[ro, opts]
)

resourceSubmitCloudConnect[___]:=(Message[ResourceSubmit::iopts];$Failed)

$submissionPublisherID=Automatic;

resourceSubmit[ro_System`ResourceObject, OptionsPattern[]]:=Block[{$submissionPublisherID, 
	resourcebase=resourcesystembase[OptionValue[CloudBase]]},
	$submissionPublisherID=checkPublisherID[OptionValue["PublisherID"]];
	resourceSubmitRO[ro, resourcebase]
]

resourceSubmit[___]:=(Message[ResourceSubmit::noro];$Failed)

resourcesubmit[as_Association,resourcebase_]:=With[{fullparams=completeResourceSubmission[as]},
	submitresourceToSystem[fullparams,resourcebase]
]

resourceSubmitRO[ro:HoldPattern[System`ResourceObject][as_Association], resourcebase_]:=With[{id=Lookup[as,"UUID",Throw[$Failed]]},
	If[MemberQ[$loadedResources,id],
		resourceSubmitRO[id,resourceInfo[id], resourcebase]
		,
		resourceSubmitRO[id,as, resourcebase]
	]
]

resourceSubmitRO[id_,info_, _]:=(Message[System`ResourceSubmit::exists];$Failed)/;!userdefinedResourceQ[info]

resourceSubmitRO[id_, info_, resourcebase_]:=With[{rtype=getResourceType[info]},
	If[!StringQ[rtype],
		Message[ResourceSubmit::invrt,rtype];Throw[$Failed]
	];
	loadResourceType[rtype];
	resourcesubmit[
		repositoryValidateSubmission[rtype,id, info]
		,
		resourcebase
	]
	
]

repositoryValidateSubmission[_,id_, info_]:=info

resourceSubmitRO[___]:=$Failed

completeResourceSubmission[as_Association]:=With[{rtype=getResourceType[as]},
	loadResourceType[rtype];
	repositorycompleteResourceSubmission[rtype,Lookup[as,"UUID"],KeyDrop[as,$nonsubmittedParameters]]]/;KeyExistsQ[as,"ResourceType"]

	
completeResourceSubmission[as_Association]:=With[{rtype=promptForResourceType[]},
    If[!StringQ[rtype],Throw[$Failed]];
	completeResourceSubmission[Append[as,"ResourceType"->rtype]]]

$nonsubmittedParameters={"Version","ContentSize","RepositoryLocation","ResourceLocations","DownloadedVersion"}

repositorycompleteResourceSubmission[rtype_,_,as0_]:=Block[{missingKeys, as, values},
	loadResourceType[rtype];
	as=DeleteMissing[AssociationMap[validateParameter[rtype,#]&,as0]];
	If[!FreeQ[as,_Failure],Message[ResourceSubmit::invprop];Throw[$Failed]];
	missingKeys=Complement[Keys[requiredparameters[rtype]],Keys[as]];
	values=promptForMissingKeys[rtype, KeyTake[requiredparameters[rtype],missingKeys]];
	If[!AssociationQ[values],Throw[$Failed]];
	Join[as, values]
]

promptForResourceType[]:=FormFunction[{"ResourceType"->{"DataResource"}},#ResourceType&][]

promptForMissingKeys[rtype_, {}|Association[]]:=Association[]
promptForMissingKeys[rtype_, signature_]:=FormFunction[signature,#&][]

submitresourceToSystem[as_, resourcebase_]:=Block[{res},
	res=apifun["SubmitResource",Append[as,"PublisherID"->$submissionPublisherID], System`ResourceSubmit,resourcebase];
	If[Quiet[KeyExistsQ[res,"SubmissionID"]],
		createSubmissionObject[res,as]
		,
		$Failed
	]
]

checkPublisherID[id_String]:=id
checkPublisherID[Automatic]:=If[StringQ[ResourceSystemClient`$PublisherID],ResourceSystemClient`$PublisherID,checkPublisherID[$Failed]]
checkPublisherID[_]:=(Message[ResourceSubmit::nopubid];Throw[$Failed])

createSubmissionObject[res_Association,as_]:=repositoryCreateSubmissionObject[Lookup[res,"ResourceType"],res, as]

repositoryCreateSubmissionObject[_,res_Association,_]:=ResourceSubmissionObject[res]

End[] (* End Private Context *)

EndPackage[]

SetAttributes[{ResourceSubmit},
   {ReadProtected, Protected}
];