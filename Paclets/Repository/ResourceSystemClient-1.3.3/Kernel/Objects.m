(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {System`ResourceObject,System`ResourceSubmissionObject}

BeginPackage["ResourceSystemClient`"]

System`ResourceObject
System`ResourceSubmissionObject

Begin["`Private`"] (* Begin Private Context *) 

(* ResourceObject *)
Options[System`ResourceObject]={CloudBase:>Automatic}

System`ResourceObject[str_?uuidQ, opts:OptionsPattern[]]:=System`ResourceObject[usableResourceInfo[resourceInfo[str]],opts]/;MemberQ[$loadedResources, str]

System`ResourceObject[str_?uuidQ,opts:OptionsPattern[]]:=With[{res=Catch[loadResource[str]]},
	If[AssociationQ[res],
		System`ResourceObject[usableResourceInfo[res],opts]
		,
		$Failed
	]
]

System`ResourceObject[name_String,opts:OptionsPattern[]]:=Catch[findResourceObject[All, name,opts]]
System`ResourceObject[url_URL,opts:OptionsPattern[]]:=findResourceObjectByURL[First[url]]
System`ResourceObject[Rule[type_, name_],opts:OptionsPattern[]]:=Catch[findResourceObject[type, name,opts]]

(resource_System`ResourceObject)[str_String, rest___]:=ResourceSystemClient`ResourceInformation[resource, Association["Property"->str,"Parameters"->{rest}]]
(resource_System`ResourceObject)[All]:=With[{info=sortBasicInfo[getResourceInfo[resourceObjectID[resource]]]},
	If[AssociationQ[info],info,$Failed]]

System`ResourceObject[info_Association]:=Catch[System`ResourceObject[usableResourceInfo[standardizecustomResourceInfo[info]]]]/;!filledResourceQ[info]

System`ResourceObject[info_Association]:=Catch[autoloadResource[info]]/;autoloadResourceQ[info]
System`ResourceObject[ro:HoldPattern[System`ResourceObject][args___],OptionsPattern[]]:=ro
System`ResourceObject[expr:Except[_Association],OptionsPattern[]]:=(Message[ResourceObject::noas,expr];$Failed)

System`ResourceObject/:
MakeBoxes[resource:System`ResourceObject[_Association], form:StandardForm|TraditionalForm] := (
Catch[standardResourceObjectBoxes[resource, form]])

standardResourceObjectBoxes[resource_, form_]:=With[{id=Quiet[resourceObjectID[resource]]},
	If[StringQ[id],
        	With[{info=If[AssociationQ[#],#,First[resource]]&@resourceInfo[id]},
        		With[{rtype=getResourceType[info, None]},
        			loadResourceType[rtype];
		            BoxForm`ArrangeSummaryBox[
		                        (* Head *)System`ResourceObject, 
		                        (* Interpretation *)resource, 
		                        (* Icon *)resourceIcon[rtype], 
		                        (* Column or Grid *)
		                        {
		                        BoxForm`SummaryItem[{"Name: ", summaryResourceName[info]}],
		                        BoxForm`SummaryItem[{"Type: ", rtype}],
		                        resourceSystemDescriptionSummaryItem[rtype,Lookup[info,"Description",Missing["NotAvailable"]]]
		                        }
		                        ,
		                        (* Plus Box Column or Grid *)
		                        repositoryBelowFoldItems[rtype,id, info]
		                        ,
		            form]
        		]
        	]
        	,
        	ToBoxes[$Failed]
	]
]


repositoryBelowFoldItems[_,id_, info_]:={
	BoxForm`SummaryItem[{"Keywords: ", Short[Row[Lookup[info,"Keywords",{}],","]]}],
	BoxForm`SummaryItem[{"UUID: ", id}],
	BoxForm`SummaryItem[{"Version: ", Lookup[info,"Version",None]}]
	}

resourceSystemDescriptionSummaryItem[_,str_String]:=BoxForm`SummaryItem[{"Description: ", 
		str
 	}]/;Snippet[str,1]===str

resourceSystemDescriptionSummaryItem[_,str_String]:=DynamicModule[{len=1},
	BoxForm`SummaryItem[{"Description: ", 
		Button[Dynamic[
			Replace[snipDots[str, len],Except[_String]->Snippet[str, len],{0}]
			], len = Ceiling[len*1.5], Appearance -> None,BaseStyle -> {}]
 	}]
]

snipDots[str_, len_] := With[{snip = Snippet[str, len]},
	If[StringLength[StringTrim[str]] <= StringLength[snip],
		str,
		snip <>ToString["..."]
		]
	]
	
resourceSystemDescriptionSummaryItem[_,expr_]:=Nothing

fallbackResourceIcon=Null;

summaryResourceName[info_]:=summaryResourceName[Lookup[info,"Name",Missing["NotAvailable"]],resourceURL[info]]

summaryResourceName[name_,url_URL]:=Row[{name," ",Hyperlink["\[RightGuillemet]",url]}]
summaryResourceName[name_,_]:=name

summaryResourceLink[info_Association]:=summaryResourceLink[resourceURL[info]]
summaryResourceLink[url_URL]:=BoxForm`SummaryItem[{"Documentation: ", Hyperlink[url]}]
summaryResourceLink[_]:=Nothing

resourceicon[file_]:=With[{img=Import[file]},
	Switch[Head[img],
		Graphics,
		formatresourceicon[img],
		List,
		formatresourceicon[img[[1]]],
		_,
		fallbackResourceIcon
	]		
]/;FileExistsQ[file]

resourceIcon[_]=fallbackResourceIcon;

formatresourceicon[gr_Graphics]:=Graphics[gr[[1]],
  AspectRatio -> 1, Axes -> False, Background -> None, Frame -> None, 
 FrameTicks -> None, 
  ImageSize -> {Automatic, Dynamic[3.5*(CurrentValue["FontCapHeight"]/
             AbsoluteCurrentValue[Magnification]), 
    ImageSizeCache -> {45., {0., 9.}}]}]

filledResourceQ[info_Association]:=KeyExistsQ[info,"UUID"]
filledResourceQ[_]:=False

autoloadResourceQ[info_Association]:=TrueQ[Lookup[info,"Autoload"]]
autoloadResourceQ[_]:=False

System`ResourceSubmissionObject[id:(_String|_Integer)]:=Catch[importSubmission[id]]

System`ResourceSubmissionObject[info_]:=$Failed/;!Quiet[KeyExistsQ[info, "Name"]]

sub_System`ResourceSubmissionObject[req_,args___]:=Catch[submissionRequest[First[sub],req,{args}]]

System`ResourceSubmissionObject/:
MakeBoxes[resource_System`ResourceSubmissionObject, form:StandardForm|TraditionalForm] := (
Catch[With[{info=First[resource]},
            With[{id=Lookup[info,"UUID"],
            	rtype=getResourceType[info],
            	name=Lookup[info,"Name"]},
                BoxForm`ArrangeSummaryBox[
                            (* Head *)System`ResourceSubmissionObject, 
                            (* Interpretation *)resource, 
                            (* Icon *)formatSubmissionIcon[resourceIcon[rtype]], 
                            (* Column or Grid *)
                            {
                            BoxForm`SummaryItem[{"Name: ", name}],
                            BoxForm`SummaryItem[{"Type: ", rtype}],
                            BoxForm`SummaryItem[{"SubmissionID: ", Lookup[info,"SubmissionID",id]}]
                            }
                            ,
                            (* Plus Box Column or Grid *)
                            {
                            BoxForm`SummaryItem[{"UUID: ", id}],
                            BoxForm`SummaryItem[{"SubmissionDate: ", Lookup[info,"SubmissionDate"]}]
                            }, 
                form]
            ]
        ]])

formatSubmissionIcon[icon_Graphics]:=Graphics[Replace[icon[[1]], RGBColor[___] :> RGBColor[.4, .4, .4],  Infinity], 
	AspectRatio -> 1, Axes -> False, Background -> GrayLevel[0.9], Frame -> True,FrameStyle -> GrayLevel[0.6], FrameTicks -> None, 
 	ImageSize -> {Automatic, Dynamic[3.5*(CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification]), ImageSizeCache -> {45., {0., 9.}}]}]

End[] (* End Private Context *)

EndPackage[]

SetAttributes[{ResourceObject,ResourceSubmissionObject},
   {ReadProtected, Protected}
];