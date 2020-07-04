(* Wolfram Language Package *)

BeginPackage["ResourceSystemClient`"]

ResourceSystemClient`ExampleNotebook
ResourceSystemClient`CreateExampleNotebook

Begin["`Private`"] (* Begin Private Context *) 

exampleNotebookLocation[id_]:=examplenotebookLocation[resourceDirectory[id]]

examplenotebookLocation[dir_]:=examplenotebooklocation[dir]/;DirectoryQ[dir]

examplenotebookLocation[_]:=None

examplenotebooklocation[dir_]:=FileNameJoin[{dir,"examples.nb"}]

blankExampleNotebook[_,id_]:=Notebook[
	{
	Cell[CellGroupData[{Cell["Basic Examples","Subsection"],
		Cell[CellGroupData[{Cell["First Example","Subsubsection"],
			Cell["Caption 1.","Text"],
			Cell[BoxData[RowBox[{"ResourceObject","[","...","]"}]],"Input"]},System`Open]],
		Cell[CellGroupData[{Cell["Second Example","Subsubsection"],
	Cell["Caption 2.","Text"],Cell[BoxData[RowBox[{"fun","[",RowBox[{RowBox[{"ResourceObject","[","...","]"}],",","arg"}],"]"}]],"Input"]},System`Open]]},System`Open]],
	Cell["Visualization","Subsection"],
	Cell["Analysis","Subsection"]
	}
	,StyleDefinitions->"Default.nb"]
  

saveExampleNotebook[id_, nb_]:=With[{file=exampleNotebookLocation[id]},
	NotebookSave[nb, file];
	file
]

createBlankExampleNotebook[rtype_,id_,name_]:=Block[{nb,file},
	loadResourceType[rtype];
	nb=repositoryCreateBlankExampleNotebook[rtype,id,name];
	If[MemberQ[$localResources,id],
		file=saveExampleNotebook[id, nb];
		setResourceInfo[id, Association["ExampleNotebook"->file]]
		,
		setResourceInfo[id, Association["ExampleNotebook"->nb]];
	];
	nb
]


repositoryCreateBlankExampleNotebook[rtype_,id_,name_]:=Block[{nb},
	nb=NotebookPut[blankExampleNotebook[rtype,id]];
	SetOptions[nb,"WindowTitle" -> "Examples for "<>ToString[name]];
	nb
]  

deployExampleNotebook[nbo_NotebookObject]:=deployExampleNotebook[NotebookGet[nbo]]

deployExampleNotebook[nb_Notebook]:=Block[{tempfile, bytes},
	tempfile = CreateFile[];
	Put[nb, tempfile];
	bytes = Import[tempfile, "Byte"];
	DeleteFile[tempfile];
	CloudDeploy[ExportForm[bytes, {"Byte", "application/vnd.wolfram.notebook"}],Permissions->{"marketplace-admin@wolfram.com"->"Read"}]
]

deployExampleNotebook[___]:=$Failed

ResourceSystemClient`CreateExampleNotebook[args___]:=Catch[createExampleNotebook[args]]

createExampleNotebook[args___]:=Block[{resourcesystemExampleNotebook},
	resourcesystemExampleNotebook[___]:=$Failed;
	exampleNotebook[args]
]


ResourceSystemClient`ExampleNotebook[args___]:=Catch[exampleNotebook[args]]

exampleNotebook[ro_System`ResourceObject]:=exampleNotebook[resourceObjectID[ro]]

exampleNotebook[id_String]:=examplenotebook[id]/;uuidQ[id]

exampleNotebook[___]:=$Failed

examplenotebook[id_String]:=examplenotebook[{id, getResourceInfo[id]}]/;MemberQ[$localResources,id]

examplenotebook[id_String]:=examplenotebook[{id, resourceInfo[id]}]/;MemberQ[$loadedResources,id]

examplenotebook[id_String]:=With[{info=ResourceSystemClient`Private`loadResource[id]},
	If[AssociationQ[info],
		examplenotebook[{id, info}]
		,
		$Failed
	]	
]

examplenotebook[info_Association]:=examplenotebook[{info["UUID"],info}]

examplenotebook[{id_String, info_Association}]:=(customExampleNotebook[id, info])/;userdefinedResourceQ[info]
examplenotebook[{id_String, info_Association}]:=resourcesystemExampleNotebook[id, info]/;marketplacebasedResourceQ[info]

customExampleNotebook[id_, info_]:=Block[{nb=info["ExampleNotebook"]},
	Switch[nb,
		_String,NotebookOpen[nb],
		_NotebookObject,SetSelectedNotebook[nb],
		_,createBlankExampleNotebook[getResourceType[info],id,Lookup[info,"Name"]]
	]	
]/;KeyExistsQ[info,"ExampleNotebook"]

customExampleNotebook[id_, info_]:=Block[{file=exampleNotebookLocation[id]},
	If[TrueQ[Quiet[FileExistsQ[file]]],
		NotebookOpen[file],
		createBlankExampleNotebook[getResourceType[info],id,Lookup[info,"Name"]]
	]	
]

resourcesystemExampleNotebook[id_, info_]:=With[{nb=info["ExampleNotebook"]},
	If[FileExistsQ[nb],
		NotebookOpen[nb]
		,
		resourcesystemExampleNotebook[id, KeyDrop[info,"ExampleNotebook"]]
	]	
]/;KeyExistsQ[info,"ExampleNotebook"]

resourcesystemExampleNotebook[id_, info_]:=Block[{res, nb},
	res=apifun["GetExampleNotebook",{"UUID"->id}, 
		ResourceSystemClient`ExampleNotebook, resourcerepositoryBase[info]];
	If[KeyExistsQ[res,"ExampleNotebook"],
		nb=NotebookOpen[res["ExampleNotebook"]];
		If[Head[nb]===NotebookObject,
			setResourceInfo[id, Association["ExampleNotebook"->res["ExampleNotebook"]]];
			nb
			,
			$Failed
		]
	]
]


validateexamplenotebook[co_CloudObject]:=With[{res=setReviewerPermissions[co]},
    If[!ListQ[res],
        Message[ResourceSubmit::appperms,co];
        Throw[$Failed]
    ];
    co
]

validateexamplenotebook[nb:(_NotebookObject|_Notebook)]:=With[{res=deployExampleNotebook[nb]},
    If[Head[res]=!=CloudObject,
        Message[ResourceSubmit::enbdf];
        Throw[$Failed]
    ];
    res
]

validateexamplenotebook[file_String]:=With[{nb=Get[file]},
    If[Head[nb]=!=Notebook,
        Message[ResourceSubmit::enbdf];
        Throw[$Failed]
    ];
    validateexamplenotebook[nb]
]/;FileExistsQ[file]






End[] (* End Private Context *)

EndPackage[]