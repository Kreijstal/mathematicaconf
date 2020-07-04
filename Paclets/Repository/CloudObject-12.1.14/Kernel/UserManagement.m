(* Wolfram Language Package *)

BeginPackage["CloudObject`UserManagement`"]

CloudUser::usage =
"CloudUser[<|\"CloudUserUUID\"-> uuid, \"CloudUserID\" -> id, \"UserURLBase\" -> baseurl|>] represents a cloud user with given identity."

CloudUsers::usage =
"CloudUsers[] returns a list of all cloud users.
CloudUsers[\"id\"] returns a CloudUser represented by id."

CloudUserExistsQ::usage =
"CloudUserExistsQ[\"id\"] returns whether a user with the given identity exists or not.
CloudUserExistsQ[CloudUser[...]] returns whether CloudUser exists or not."

CloudUserInformation::usage =
"CloudUserInformation[] returns all information of users.
CloudUserInformation[user] returns user information.
CloudUserInformation[user, property] returns property for the user.
CloudUserInformation[CloudUser[...]] returns information for a given CloudUser."

AddAdminStatus::usage =
"AddAdminStatus[user] grants cloud admin status to user.
AddAdminStatus[CloudUser[...]] grants cloud admin status to CloudUser."

RemoveAdminStatus::usage =
"RemoveAdminStatus[user] revokes cloud admin status for user.
RemoveAdminStatus[CloudUser[...]] revokes cloud admin status for CloudUser."

Begin["CloudObject`Private`"]

(* Needs to be moved to Messages.m once CloudUserInformation is a System symbol *)
CloudUserInformation::noprop = "`1` is not a property returned by CloudUserInformation.";
CloudUserInformation::una = "Unable to get cloud user information.";
CloudUserInformation::invusr = "Invalid user.";
CloudUsers::invusr = "Invalid user.";
CloudUsers::unkusr = "Users `1` not found.";

$jsonToUserInfoFields = {
    "lastName" -> "LastName",
    "firstName" -> "FirstName",
    "cloudUserID" -> "CloudUserID",
    "cloudUserUUID" -> "CloudUserUUID",
    "userBaseURL" -> "UserURLBase",
    "lastLogin" -> "LastLogin",
    "role" -> "Admin",
    "true" -> True,
    "false" -> False,
    "registrationDate" -> "RegistrationDate",
    "ROLE_ADMIN" -> "Admin"
};

getUserInformation[user_, fields_, msghd_] := 
	Module[ {json},
		Catch[json = Replace[execute[$CloudBase, "GET", {"users"}, Parameters -> {"id" -> user, "fields" -> fields}], {
				{_String, content_List} :> FromCharacterCode[content],
                 HTTPError[404, extraData_, ___] :> handleMissingUser[extraData],
                 other_ :> (checkError[other, msghd];
                 			Throw[$Failed])}
              ];
          DeleteCases[importFromJSON[json], Rule["usersCount", _] | List[Rule["usersCount", _]]]
        ]
    ]

handleMissingUser[extraData_] :=
	With[{missingUserId = StringReplace[Lookup[importFromJSON[extraData],"extraData"], "[" ~~ userIds__ ~~ "]" :> userIds]},
		Message[CloudUsers::unkusr, missingUserId]; Throw[{}]
	]

patternBasic = { cloudUserID_, cloudUserUUID_:None, userBaseURL_:None };

patternCloudUser = CloudUser[<|"CloudUserUUID" -> cloudUserUUID_, "CloudUserID" -> cloudUserID_, "UserURLBase" -> userURLBase_|>];

findCloudUsers[user_] :=
    With[ {userInfo = getUserInformation[user, "basic", CloudUsers]},
        If[ userInfo === {},
            userInfo,
            CloudUser[Association[#]] & /@ (ReplaceAll[#, patternBasic :> {"CloudUserUUID" -> validateUserInfo[cloudUserUUID],
                "CloudUserID" -> Values@cloudUserID,
                "UserURLBase" -> validateUserInfo[userBaseURL]}] & /@ handleJSONObject[userInfo])
        ]
    ]

validateUserInfo[userInfo_] := If[(userInfo) === None, userInfo, Values@userInfo]

emptyUserStringQ[user_] := user === ""

handleJSONObject[userInfo_List]:= If[KeyExistsQ[userInfo, "users"], Flatten[Lookup[userInfo, "users"], 1], userInfo]

CloudUsers[] := findCloudUsers[""]

CloudUsers[user_String] :=
	Catch[If[emptyUserStringQ[user],
		Message[CloudUsers::invusr]; Throw[$Failed],
		findCloudUsers[user]
		]
	]

CloudUsers[users_List] := If[MemberQ[users, ""], Message[CloudUsers::invusr]; $Failed, findCloudUsers[users]]

CloudUserExistsQ[user_String] :=
	If[emptyUserStringQ[user],
		False,
		Quiet@CloudUsers[user] =!= {}
	]

CloudUserExistsQ[user_CloudUser] := With[{userID = ReplaceAll[user, patternCloudUser :> cloudUserID]}, CloudUserExistsQ[userID]]

CloudUsers[_] := $Failed

getCloudUserProperty[user_Association, opts:OptionsPattern[]] := Lookup[user, OptionValue["Elements"]]

getCloudUserProperty[user_, opts:OptionsPattern[]] := $Failed

setAdminStatus[user_, msghd_] :=
    Replace[
        execute[$CloudBase, "PUT", {"users", user, "admin"}],
            {
                {_String, content_List} :> Null,
                other_ :> (checkError[other, msghd];
                           $Failed)
            }
        ]

removeAdminStatus[user_, msghd_] :=
    Replace[
        execute[$CloudBase, "DELETE", {"users", user, "admin"}],
            {
                {_String, _List} :> Null (* success *),
                other_ :> (checkError[other, msghd];
                           $Failed)
            }
        ]

Options[getCloudUserProperty] = {"Elements"-> Automatic}

getCloudUserProperty[user_Association, opts:OptionsPattern[]] :=
        ReplaceAll[user, info_ :> Lookup[info, OptionValue["Elements"]]]

userInfoToRemove = {Rule["subscriptions",{___}], Rule["active", _]}

CloudUserInformation[user_String : None] :=
    Catch[If[ emptyUserStringQ[user],
              Message[CloudUserInformation::invusr];
              Throw[$Failed],
              (* else *)
              With[ {allUserInfo = getUserInformation[user, "all", CloudUserInformation]},
                  Catch[If[ allUserInfo === {} || allUserInfo === $Failed,
                            Throw[allUserInfo, "CloudUserInformationFailure"],
                            Map[Association[ReplaceAll[#, $jsonToUserInfoFields]]&,
                                Map[DeleteCases[handleJSONObject[#], Rule["subscriptions", {___}] | Rule["active", _], 2]&,
                                    handleJSONObject[allUserInfo]]
                            ]
                        ], "CloudUserInformationFailure", CloudUserInformation
                  ]
              ]
          ]
    ]

CloudUserInformation[allUserInfo_ , "CloudUserInformationFailure"] :=
    If[ allUserInfo === {},
        allUserInfo,
        Message[CloudUserInformation::una];
        $Failed
    ]

CloudUserInformation[user_String, property_String] :=
Catch[ If[ emptyUserStringQ[user],
           Message[CloudUserInformation::invusr];
           Throw[$Failed],
           (*else*)
           Catch [If[ MemberQ[Values@$jsonToUserInfoFields, property],
                      With[ {allCloudUserInfo = CloudUserInformation[user]},
                          Catch[If[ allCloudUserInfo === {} || allCloudUserInfo === $Failed,
                                    Throw[allCloudUserInfo, "CloudUserInformationFailure"],
                                    getCloudUserProperty[First@allCloudUserInfo, "Elements" -> property]
                                ], "CloudUserInformationFailure", CloudUserInformation
                          ]
                      ],
                      Message[CloudUserInformation::noprop, property];
                      Throw[$Failed]
                  ]
           ]
       ]
]

CloudUserInformation[user_CloudUser] :=
    With[ {userID = ReplaceAll[user, patternCloudUser :> cloudUserID]},
        ReplaceAll[CloudUserInformation[userID], $jsonToUserInfoFields]
    ]

CloudUserInformation[user_CloudUser, property_String] :=
    With[ {userID = ReplaceAll[user, patternCloudUser :> cloudUserID]},
        CloudUserInformation[userID, property]
    ]

CloudUserInformation[_, ___] := $Failed

AddAdminStatus[user_CloudUser] := setAdminStatus[First[CloudUserInformation[user, "CloudUserUUID"]], AddAdminStatus]

AddAdminStatus[user_String] := With[{cloudUserUUID = First[CloudUserInformation[user, "CloudUserUUID"]]},
	setAdminStatus[cloudUserUUID, AddAdminStatus];
]

RemoveAdminStatus[user_CloudUser] := removeAdminStatus[First[CloudUserInformation[user, "CloudUserUUID"]], RemoveAdminStatus]

RemoveAdminStatus[user_String] := With[{cloudUserUUID = First[CloudUserInformation[user, "CloudUserUUID"]]},
	removeAdminStatus[cloudUserUUID, RemoveAdminStatus];
]

End[] (* End Private Context *)
EndPackage[]
