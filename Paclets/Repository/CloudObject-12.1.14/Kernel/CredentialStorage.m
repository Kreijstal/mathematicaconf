(* Wolfram Language package *)
BeginPackage["CloudObject`"]
Begin["`Private`"]

(* This file provides a credential persistence interface. It is called from 
   CloudConnect and CloudDisconnect and related code in Authentication.m and 
   is comprised of these functions:
   
   * storeCredentials
   * setConnectionFromStorage
   * getKeyChainData
   * purgeCreds
   * hasPersistedConnectionQ
   
   The two credential persistence methods are
   * SystemCredential, available from version 12.1 and after
   * local file used in versions 12.0 and before
   The credential file used until Cloud release 1.54 (an MX file containing an encrypted string)
   is automatically migrated and written back to the local file format by version 12.0 and earlier.
   Typically a user with version 12.0 and earlier will be running this version of CloudObject via
   paclet download, and it will take effect in all Mathematica installations from 10.1 onwards, so
   the older file format will not be used after, but it is not deleted or modified as a precaution
   against data loss should the current paclet be uninstalled.

   The SystemCredential feature stores credentials using a credential storage provider, or in some
   atypical cases where none is available, in an encrypted local file.
   
   The local file feature is also an encrypted local file.
 *)

$useSystemCredentialQ = $VersionNumber >= 12.1

(********************************************************************************************)
(* storeCredentials[] saves the credentials for the current cloud connection.
   Throws: with tag of $tag on errors.
   Called by: authenticateUsingCredentials in Authentication.m
 *)
storeCredentials = If[$useSystemCredentialQ, storeSystemCredential, storeLocalCredential]

(********************************************************************************************)
(* setConnectionFromStorage[cloudBase] set $CloudUserID etc. from credentials in persistent storage
   or fail if no connection is found for the given CloudBase.
   Called by: fetchCredentials 
*)
setConnectionFromStorage[] := setConnectionFromStorage[$CloudBase]

setConnectionFromStorage[cbase_] :=
    With[{creds = getKeyChainData[cbase]},
        If[AssociationQ[creds],
            (* TODO what about the setAuthentication overload that takes UserURLBase as well? *)
            setAuthentication[creds[CloudUserID], creds["CloudUserUUID"], creds["AccountName"],
                creds["AccessSecret"], creds["AccessToken"]],
        (* Else *)
            connectionLog["Keychain entry not found for CloudBase -> " <> cbase];
            $Failed["NoEntry"]
        ]
    ]

getKeyChainData::usage = "getKeyChainData[] gives the remembered connection information for all cloud/account pairs.
getKeyChainData[cbase] gives the remembered connection information for the given CloudBase."
getKeyChainData = If[$useSystemCredentialQ, getSystemCredentialKeyChainData, getLocalKeyChainData];

(********************************************************************************************)
(* purgeCreds[cbase] removes persisted credentials for the connection in CloudBase -> cbase.
   purgeCreds[All] removes all persisted credentials.
   Called by: iCloudDisconnect
 *)

purgeCreds[arg_] /; $PurgeCredFileOnFailure := purgeCredsImpl[arg]

purgeCredsImpl = If[$useSystemCredentialQ, purgeSystemCredential, purgeLocalCredential];

(*anything else we don't evaluate*)

(********************************************************************************************)
(* hasPersistedConnectionQ *)
hasPersistedConnectionQ = If[$useSystemCredentialQ, hasSystemCredentialQ, hasLocalCredentialQ];

(***********************************************************************************************)
(* SystemCredential implementations *)

Unprotect[{$CredentialKeyPrefix, $CredentialKeyPattern}];
$CredentialKeyPrefix = "wolframcloudconnect:";
$CredentialKeyPattern = $CredentialKeyPrefix<>"*";
Protect[{$CredentialKeyPrefix, $CredentialKeyPattern}];

credentialKey[cbase_String, userid_String] := 
    StringTemplate["`1``2`&`3`"][$CredentialKeyPrefix, extractURLDomain[cbase], userid]

storeSystemCredential[] := storeSystemCredential[credentialData[]]

storeSystemCredential[dataParam_Association] :=
    With[{cbase = dataParam[CloudBase], userid = dataParam[CloudUserID]},
        If[StringQ[cbase] && StringQ[userid],
            With[{key = credentialKey[cbase, userid], 
                data = renameKeys[dataParam, $keysForSystemCredentialStorage]},
                (* Clear any existing entries for cbase; this will go away when we support
                   multiple users per CloudBase. *)
                purgeSystemCredential[cbase];
                Set[SystemCredential[key], SystemCredentialData[data, "AccessSecret"]]
           ],
        (* Else *)
            connectionLog["Missing credential data"];
            Throw[$Failed["MissingData"], $tag]
        ]
    ]

(* This assumes only one creds per cloud/user combo *)
credentialData[] := 
    <|
        CloudBase -> extractURLDomain[$CloudBase],
        "AccessToken" -> getAccessToken[],
        "AccessSecret" -> getAccessSecret[],
        (*currently PFX file also stores things like $CloudAccountName and CloudUserUUID,
        not sure they should be stored, but rather read from GET /files/auth. *)
        "AccountName" -> $CloudAccountName,
        "CloudUserUUID" -> $CloudUserUUID,
        CloudUserID -> $CloudUserID
    |>
    
renameKeys[assoc_?AssociationQ, replacements_] := KeyMap[Replace[#, replacements]&, assoc]

$keysForSystemCredentialStorage = {CloudBase -> "CloudBase", CloudUserID -> "CloudUserID"}
$keysForSystemCredentialRetrieval = Map[Reverse, $keysForSystemCredentialStorage]

getSystemCredentialKeyChainData[] := 
(
    migrateLocalCredentialsIfNeeded[];
    Cases[Map[importSystemCredential, systemCredentialKeys[]], _?AssociationQ]
)

getSystemCredentialKeyChainData[cbase_] := 
(
    migrateLocalCredentialsIfNeeded[];
    Replace[
        (* Lookup by wildcard, e.g. "wolframcloudconnect:www.wolframcloud.com&*" *)
        SystemCredentialKeys[credentialKey[cbase, "*"]], {
            {} :> Missing["NotAvailable"],
            (* pick the first result; this will change when we support multiple users per 
               CloudBase *)
            {key_, ___} :> importSystemCredential[key]
        }
    ]
)

migrateLocalCredentialsIfNeeded[] := 
    If[shouldMigrateLocalFileQ[],
        migrateLocalCredentials[]
    ]

hasSystemCredentialQ[cbase_] :=
    (* Test that the list of keys (1) is a list AND (2) has at least one item.
       We don't test keys =!= {} because that gives a false positive if keys is not a list.
     *)
    MatchQ[SystemCredentialKeys[credentialKey[cbase, "*"]], {__}]

importSystemCredential[key_String] := importSystemCredential[SystemCredential[key]]

importSystemCredential[SystemCredentialData[props_, ___]] := 
    renameKeys[props, $keysForSystemCredentialRetrieval]

(* handle missing or otherwise unsuccessful SystemCredential lookup *)
importSystemCredential[_] := Missing["NotAvailable"]

systemCredentialKeys[] := SystemCredentialKeys[$CredentialKeyPattern]

purgeSystemCredential[All] := 
    Map[Unset[SystemCredential[#]] &, systemCredentialKeys[]]

purgeSystemCredential[cbase_String] := 
    (* currently we expect only one entry *)
    Scan[
        Unset[SystemCredential[#]]&,
        SystemCredentialKeys[credentialKey[cbase, "*"]]
    ]

(* Migrate local credentials, either from the $localCredentialsFile if it exists, or from the legacy
   PFX file if that exists, into the SystemCredential system.
 *)
migrateLocalCredentials[] :=
    With[{data = loadLocalKeyChainDataAsAvailable[]},
        Map[storeSystemCredential, data];
        shouldMigrateLocalFileQ[] = False; (* memoize so we don't check preferences file again *)
    ]

(* When migrating credentials from the local file, prefer the more recent version if it exists. *)
loadLocalKeyChainDataAsAvailable[] := 
    If[FileExistsQ[$localCredentialsFile],
        If[TrueQ[$CloudDebug], Print["Migrating credentials from ", $localCredentialsFile]];
        (* We need to migrate from the local file, so artificially shut off PFX migration because
           that will be the first thing attempted in getting local keychain data. *)
        shouldMigrateLegacyPFXFileQ[] = False;
        putPersistentProperty[$pfxMigrationCheckKey, True];

        getLocalKeyChainData[],
    (* Else *)
        If[TrueQ[$CloudDebug], Print["Migrating credentials from ", $pfxCredentialsFile]];
        getPFXKeyChainData[]
    ]

(* Return true if there's a local credentials file and we've never migrated it before.
   Once the local credentials file _has_ been migrated, the migrate process overrides this function
   to return False so that we don't attempt to re-migrate in this kernel session, and it also
   remembers this decision persistently, using the persistent properties interface below.
 *)
shouldMigrateLocalFileQ[] := 
    Or[
        shouldMigrateFromFileQ[$localCredentialsFile, "LocalCredentialMigrationChecked", 
            shouldMigrateLocalFileQ],
        shouldMigrateLegacyPFXFileQ[]
    ]

(*************************************************************************************************)
(* Persistent properties storage; unencrypted properties for the application's purposes. *)

$persistentPropertiesFile = FileNameJoin[{$credsDir, "properties.wl"}]

putPersistentProperty[key_, value_] := 
    With[{prefs = getPersistentProperties[]},
        putPersistentProperties[Join[prefs, <| key -> value |>]]
    ]

putPersistentProperties[prefs_?AssociationQ] := Put[prefs, $persistentPropertiesFile]

getPersistentProperty[key_] := Lookup[getPersistentProperties[], key]

getPersistentProperty[key_, default_] := 
    Check[
        Lookup[getPersistentProperties[], key, default],
        default
    ]

getPersistentProperties[] := 
    With[{default = <||>},
        If[FileExistsQ[$persistentPropertiesFile],
            Check[Get[$persistentPropertiesFile], default],
        (* Else *)
            default
        ]
    ]

(* This is not used in the typical production workflow, but is used for testing. *)
clearCredentialStoragePreferences[] := DeleteFile[$persistentPropertiesFile]

(***********************************************************************************************)
(* Legacy local credential implementations *)

storeLocalCredential[] /; authenticatedQ[] := 
    Catch[
        storeKeyChain[addToKeyChain[getKeyChainData[], credentialData[]]],
        $tag
    ]

storeLocalCredential[___] := $Failed["NotAuthenticated"]

(*****************)
(* keychain data *)
getLocalKeyChainData[] :=
    Module[{fileDate, keyChainString},
        If[shouldMigrateLegacyPFXFileQ[],
            migrateLegacyPFXFile[];
        ];

        fileDate = Quiet[FileDate[$localCredentialsFile]];

        (* if the cache was created after the file was modified, use the cache *)
        If[shouldUseKeyChainDataCacheQ[fileDate],
            $localKeyChainDataCachedContent,
        (* Else file was modified or deleted after our cache copy, so re-fetch *)
            keyChainString = getLocalCredentials[$localCredentialsFile];
            $localKeyChainDataCacheTime = Now;
            $localKeyChainDataCachedContent = 
                If[StringMatchQ[keyChainString, "" | Whitespace],
                   {},
                (* Else *)
                    Replace[parseKeyChainExpression[keyChainString], {
                       (* fix any unexpected results *)
                       Except[{_?AssociationQ ...}] :> {}
                    }]
                ]
        ]
    ]

(* shouldMigrateLegacyPFXFileQ[] returns True if the legacy PFX file (used in C1.54 and earlier)
   should be migrated and rewritten to a local file for use in clients 12.0 and earlier for paclets 
   C1.54.2 and beyond. This function memoizes its result to avoid file system stats after a 
   determination is made.
 *)
shouldMigrateLegacyPFXFileQ[] :=
    shouldMigrateFromFileQ[$pfxCredentialsFile, $pfxMigrationCheckKey, shouldMigrateLegacyPFXFileQ]

(* Migration of the PFX file needs to be tracked separately depending on its destination. For
   example, you may migrate from PFX to local file in 12.0, then CloudDisconnect[All] removing the
   local credentials file but keeping the PFX file, and then install 12.1. You should be able to
   migrate from the PFX file into SystemCredentials without being stopped by happening to have 
   migrated to the local file previously.
 *)
$pfxMigrationCheckKey = If[$useSystemCredentialQ, "PFXMigrationToSystemCredentialChecked",
    "PFXMigrationToLocalFileChecked"]

shouldMigrateFromFileQ[filename_String, propertyKey_String, sym_Symbol] :=
    With[{res = !$CloudEvaluation && !hasCheckedMigrationQ[propertyKey] && FileExistsQ[filename]},
        If[res === False,
            sym[] = False; (* memoize so we don't try again in this session *)
        ];
        res
    ]

hasCheckedMigrationQ[key_String] := 
    With[{res = TrueQ[getPersistentProperty[key, False]]},
        If[res === False,
            putPersistentProperty[key, True] (* the check has been done now *)
        ];
        res
    ]

hasMigratedFileQ[preferenceKey_String] := 
    TrueQ[getPersistentProperty[preferenceKey, False]]

migrateLegacyPFXFile[] :=
(
    (* need to migrate pfx file format *)
    If[TrueQ[$CloudDebug], Print["Migrating credentials from legacy PFX file."]];
    Catch[
        storeKeyChain[getPFXKeyChainData[]];
        shouldMigrateLegacyPFXFileQ[] = False,
        $tag,
        Function[{val,tag}, 
            connectionLog["Failed to save imported PFX data: " <> ToString[val]]]
    ];
)

(* The cache reset function should be called once on load and then every time we write to the
   credentials file or delete it. We wouldn't strictly need to do this if FileDate and Now had
   identical time granularities, but FileDate is rounded to seconds and Now is much finer, so that
   when we write to the file, we happen to both read from the file and write to it in short 
   succession, causing the cache time (set from Now) and the file modification time to be almost
   the same, but due to FileDate rounding the file appears to be older than the cache, which would
   leave us with a stale cache. We work around this by explicitly resetting the cache when writing.
 *)
resetLocalKeyChainDataCache[] := 
(
	$localKeyChainDataCachedContent = {};
	$localKeyChainDataCacheTime = DateObject[1]; (* very old so it appears out of date *)
)

resetLocalKeyChainDataCache[]

shouldUseKeyChainDataCacheQ[fileDate_?DateObject] := 
    DateObjectQ[$localKeyChainDataCacheTime] &&
        (AbsoluteTime[$localKeyChainDataCacheTime] - AbsoluteTime[fileDate]) > 1.0
    (* In other words, if the local cache is not a DateObject, don't use it; also, we only use the
    cache if it was taken more than a second later than the file modification time, to allow for
    the second granularity of FileDate. We might miss the cache a few extra times this way, but it
    will be correct, and once a second has passed we will cache the read value until it goes stale.
    *)

(* fileDate that is not a DateObject indicates the local file doesn't exist; when this happens,
  we shouldn't use the cache, which is now stale, but should call the function to read from the 
  file, which will return the correct value to use for this empty data case. *)
shouldUseKeyChainDataCacheQ[_] := False

parseKeyChainExpression = 
    Interpreter[
        Restricted[
            "Expression", 
            {Association, List, Rule, CloudBase, CloudUserID}
        ],
        (* handle empty string input by returning {} *)
        ListQ,
        {}&
    ]

getLocalKeyChainData[cbase_] := 
    With[{keyChainData = getLocalKeyChainData[], cbasedomain = extractURLDomain[cbase]},
        FirstCase[keyChainData, KeyValuePattern[CloudBase -> cbasedomain]]
    ]

hasLocalCredentialQ[cbase_] := 
    AssociationQ[getLocalKeyChainData[extractURLDomain[cbase]]]

(*****************)
purgeLocalCredential[All] := 
(
    Quiet[DeleteFile[$localCredentialsFile]];
    resetLocalKeyChainDataCache[];
)

purgeLocalCredential[cbase_String] :=
    storeKeyChain[removeFromKeyChain[getKeyChainData[], cbase]]

(*************************************************************************************************)
(* local keychain file operations *)

$storageKey := 
Internal`HouseKeep[$credsDir, {
    "machine_ID" -> $MachineID,
    "version" -> $Version,
    "system_ID" -> $SystemID,
    "user_name" -> $UserName
}]

initencrypt[] := Symbol["NumberTheory`AESDump`RijndaelDecryption"][]

encrypt[args___] :=
(
    initencrypt[];
    With[{encryptToBytes = Symbol["NumberTheory`AESDump`Private`rijndaelEncryption"]},
        encryptToBytes[args]
    ]
)

decrypt[args___] :=
(
    initencrypt[];
    With[{decryptFromBytes = Symbol["NumberTheory`AESDump`RijndaelDecryption"]},
        Block[{DataPaclets`SocialMediaDataDump`Private`flagQ = True, 
        	NumberTheory`AESDump`Private`flagQ = True},
            decryptFromBytes[args]
        ]
    ]
)

makeCredentialsChain[] := StringJoin[
    "cloudbase=", extractURLDomain[$CloudBase],
    "token=", getAccessToken[],
    "secret=", getAccessSecret[],
    "username=", $CloudAccountName,
    "uuid=", $CloudUserUUID,
    "wolframid=", $CloudUserID
]

addToKeyChain[credentialData_List, credentialEntry_?AssociationQ] := 
    Prepend[removeEntry[credentialData, currentCloudBaseQ], credentialEntry]

removeFromKeyChain[credentialData_List, cloudBase_String] := 
    removeEntry[credentialData, SameQ[#, extractURLDomain[cloudBase]]&]

removeEntry[credentialData_List, predicate_] := 
    DeleteCases[credentialData, entry_ /; predicate[extractURLDomain[entry[CloudBase]]]]

$localCredentialsFile = FileNameJoin[{$credsDir, "credentials.dat"}]

(* storeKeyChain[keyChainData] stores data as returned by getKeyChainData[] to a local file. *)
storeKeyChain[keyChainData_List] := storeKeyChain[ToString[keyChainData, InputForm]]

storeKeyChain[keyChainString_String] := storeKeyChain[$localCredentialsFile, keyChainString]

storeKeyChain[file_String, keychain_String] := 
    With[{
        createDirectoryMessages := {CreateDirectory::filex, CreateDirectory::privv},
        saveMessages := {General::stream, Save::sym, General::privv, General::noopen},
        directory = DirectoryName[file]
    },
        If[Not[DirectoryQ[directory]],
            Quiet[Check[
                CreateDirectory[directory],
                Throw[$Failed["NoMakeDir"], $tag],
                createDirectoryMessages],
            createDirectoryMessages]
        ];
        Quiet[Check[
            Export[file, encrypt[keychain, $storageKey], "Binary"],
            Throw[$Failed["Export"], $tag],
            saveMessages],
        saveMessages];
        resetLocalKeyChainDataCache[]; (* invalidate the cached data read from the file *)
        True
    ]

getLocalCredentials[] := getLocalCredentials[$localCredentialsFile]

getLocalCredentials[filename_String] := 
    Catch[
        If[TrueQ[$CloudDebug], Identity, Quiet][
            Module[{contents},
                If[Not[FileExistsQ[filename]], Throw["", $tag]];
                With[{messages := {General::privv, General::noopen}},
                    Quiet[Check[contents = Import[filename, "Binary"], 
                        Throw["", $tag], messages], messages];
                    decrypt[contents, $storageKey]
                ]
            ]
        ],
        $tag
    ]

(*************************************************************************************************)
(* Support for legacy PFX format *)
(* getPFXKeyChainData[] returns the keychain data structure from the PFX keychain file used in C1.54
   and earlier. *)

$credsFile ="config.pfx";
$pfxCredentialsFile = FileNameJoin[{$credsDir, $credsFile}];

getPFXKeyChainData[] := importLegacyRecordKeyChainData[getPFXKeyChain[]]

importLegacyRecordKeyChainData[keyChain_String] := 
    Map[parseKeyRecord, StringSplit[keyChain, "cloudbase="]]

parseKeyRecord[keyRecord_String] :=
    (* assumes this specific order of keys, and this exact set of keys *)
    (* Note that the first record in the file doesn't start with cloudbase= but this code still
       handles that, since the StringSplit will give the same result. *)
    With[{keys = {"cloudbase=", "token=", "secret=", "username=", "uuid=", "wolframid="}},
        Association[Rule @@@ Transpose[{Map[fixKeyName, keys], StringSplit[keyRecord, keys]}]]
    ]

fixKeyName[key_] := key /. fixKeyNameDispatch

fixKeyNameDispatch = Dispatch[{"cloudbase=" -> CloudBase, "token=" -> "AccessToken", 
    "secret=" -> "AccessSecret", "username=" -> "AccountName", "uuid=" -> "CloudUserUUID", 
    "wolframid=" -> CloudUserID}];

(* getPFXKeyChain loads the local .pfx credential file and then decrypts its contents with the given
   key. It returns a string that is then parsed by the caller.
*)
getPFXKeyChain[] := getPFXKeyChain[$credsDir, $credsFile, $storageKey]

getPFXKeyChain[directory_String, filename_String, key_String] := 
ReplaceAll[
    Catch[
        If[TrueQ[$CloudDebug], Identity, Quiet][
            Block[{$KeyChain = ""}, 
                With[{
                    Getmessages := {Get::enkey, Get::notencode, General::privv, General::noopen, DumpGet::bgnew}, 
                    file = FileNameJoin[{directory, filename}]
                    },
                    If[Not[DirectoryQ[directory]], Return[""]];
                    Quiet[Check[Get[file], Return[""], Getmessages], Getmessages];
                    If[Not[MatchQ[$KeyChain, {_Integer..}]], Throw[$Failed["Bytes"], $tag]];
                    $KeyChain = decrypt[$KeyChain, key];
                    If[Not[StringQ[$KeyChain]], $KeyChain = ExportString[$KeyChain, "Byte"]];
                    $KeyChain
                ]
           ]
       ],
    $tag],
    (* ReplaceAll replacement rules *)
    _$Failed->""
]

End[]
EndPackage[]
