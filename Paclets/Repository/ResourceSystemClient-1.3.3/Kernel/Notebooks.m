(* Wolfram Language Package *)

BeginPackage["ResourceSystemClient`"]

ResourceSystemClient`CreateResourceNotebook

Begin["`Private`"] (* Begin Private Context *) 


(Unprotect[#]; Clear[#])& /@ {ResourceSystemClient`CreateResourceNotebook}

ResourceSystemClient`CreateResourceNotebook[args___]:= Catch[createResourceObjectNotebook[args]]

$availableResourceCreateTemplates={"DataResource","NeuralNet"};

createResourceObjectNotebook[args___]:=(
		CloudConnect[];
		If[$CloudConnected,
			createResourceObjectNotebook[args]
			,
			$Failed
		]
	)/;!$CloudConnected

createResourceObjectNotebook[]:=Block[{rtype},
	CreateDialog[
		rtype = "Choose a type"; 
		Column[{
			PopupMenu[Dynamic[rtype], Prepend[$availableResourceCreateTemplates,"Choose a type"]],
			ChoiceButtons[{"Create Notebook", "Cancel"}, 
	   			{DialogReturn[createResourceObjectNotebook[rtype]], DialogReturn[$Canceled]}, 
	     		{{Enabled -> Dynamic[rtype =!= "Choose a type"],Method -> "Queued"}, {}}]
	     	}]
	     ]
]

createResourceObjectNotebook[rtype_String]:=(
	loadResourceType[rtype];
	createResourceNotebook[rtype]
)

createResourceObjectNotebook[___]:=$Failed
createResourceNotebook[___]:=$Failed

SetAttributes[{ResourceSystemClient`CreateResourceNotebook},
   {ReadProtected, Protected}
];
End[] (* End Private Context *)

EndPackage[]