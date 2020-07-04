(* This is a list of all CloudObject-related messages not defined in Messages/errmsg.m *)
(* Messages should ONLY be defined here TEMPORARILY until they are defined in the messages repo *)

BeginPackage["CloudObject`"];
Begin["`Private`"];

CloudConnect::atype = "Unrecognized AuthenticationMethod specification `1`.";
CloudConnect::aurl = "Invalid AuthenticationURL specification `1`.";
CloudConnect::apkey = "Unrecognized authentication keys. Please contact technical support for assistance.";
CloudConnect::badts = "Invalid Timestamp. Please ensure your system clock is set to the correct time.";
CloudConnect::bdmtd = "HTTP method unavailable; Please contact technical support for assistance.";
CloudConnect::bdrsp = "Unrecognized login response; Please contact technical support for assistance.";
CloudConnect::cerr = "Unrecognized client error; status code `1`.";
CloudConnect::clver = "Connecting to a cloud running an earlier version of the Wolfram Engine: `1`";
CloudConnect::config = "Unrecognized CloudConnect configuration.";
CloudConnect::clver = "Connecting to a cloud running an earlier version of the Wolfram Engine: `1`";
CloudConnect::creds = "Incorrect username or password.";
CloudConnect::fbdn = "Unable to authorize request.  Please contact technical support for assistance.";
CloudConnect::gwto = "Unable to process request at this time. Please try again later.";
CloudConnect::invcfg = "Unrecognized CloudConnect configuration.";
CloudConnect::iserr = "Unable to process request. Please try again later.";
CloudConnect::nfnd = "Unable to reach Wolfram Cloud servers. Please try again later.";
CloudConnect::nocrd = "No username or password sent.";
CloudConnect::notauth = "Unable to authenticate with Wolfram Cloud server. Please try authenticating again.";
CloudConnect::oauth = "Unrecognized authentication information. Please contact technical support for assistance.";
CloudConnect::pcond = "Invalid authorization information; Please contact technical support for assistance.";
CloudConnect::serr = "Unrecognized server error; status code `1`.";
CloudConnect::surl = "Invalid SignatureURL specification `1`.";
CloudConnect::tout = "Request timed out. Please try again later.";
CloudConnect::uerr = "An unknown error occurred; status code `1`.";
CloudConnect::unav = "Wolfram Cloud temporarily unavailable. Please try again later.";
CloudEvaluate::srvfmt = "Wolfram Cloud returned an unexpected response. Please try again later.";
CloudLoggingData::inelem = "The argument `1` is not a valid logging data element.";
CloudLoggingData::invobj = "The argument `1` is not a valid logging data object.";
CloudLoggingData::invper = "The argument `1` is not a valid logging data period.";
CloudLoggingData::invopt = "Value of \"TimeSeriesBinData->`1` is not a Quantity.";
CloudObjectInformation::dirlvl = "DirectoryLevel is not one of 0, 1, or Infinity.";
CloudObject::invarg = "\"UUID\" can not be used with other arguments.";
CloudObject::invuri = "The URI `1` is not valid.";
CloudObject::noicon = "No icon named `1` found for `2`.";
CloudObject::invactive = "Active value `1` must be either True or False.";
CloudObject::invuuid = "`1` is not a valid UUID string.";
CloudObject::unauth = "URI `1` only valid when authenticated.";
CloudObject::uristring = "URI `1` expected to be a string.";
CloudObject::memorylimit = "Evaluation aborted because memory limit `1` was exceeded.";
CloudObject::timelimit = "Evaluation aborted because time limit `1` was exceeded.";
CloudObjectInformation::noprop = "`1` is not a property returned by CloudObjectInformation.";
CloudObjects::invtype = "Invalid cloud object type specification `1`.";
CloudObject::invpath = "Cloud object name `1` contains either an illegal character or the reserved path .Objects.";
CloudObjectNameFormat::una = "Unable to apply the specified name format `1`.";
CloudObjectNameFormat::inv = "CloudObjectNameFormat `1` is not one of \"UUID\", \"UserURLBase\", \"CloudUserID\", \"CloudUserUUID\" or Automatic.";
$CloudObjectNameFormat::inv = "$CloudObjectNameFormat `1` is not one of \"UUID\", \"UserURLBase\", \"CloudUserID\", \"CloudUserUUID\" or Automatic.";
ContinuousTask::restr = "Unrestricted cloud required for deployment.";
DocumentGenerator::argu = "Unrecognized document generator specification.";
DocumentGenerator::badarg = "Bad value `` for argument ``."; 
DocumentGenerator::badform = "Unrecognized output format ``."; 
DocumentGenerator::crea = "Unable to create or update generator.";
DocumentGenerator::filex = "Cannot overwrite existing cloud object `1`."; 
DocumentGenerator::inact = "Document generator `1` is inactive.";
DocumentGenerator::listing = "Unable to obtain DocumentGenerator listing.";
DocumentGenerator::nffil = "`` not found."; 
DocumentGenerator::nochan = "Unsupported notification or delivery channel `1`.";
DocumentGenerator::nonext = "Unable to obtain next scheduled run time for DocumentGenerator `1`.";
DocumentGenerator::norm = "Unable to remove DocumentGenerator.";
DocumentGenerator::nostart = "Unable to start task for document generator ``.";
DocumentGenerator::nostop = "Unable to stop DocumentGenerator `1`.";
DocumentGenerator::notrep = "Object `` not recognized as a document generator.";
DocumentGenerator::notask = "No task found for object ``.";
DocumentGenerator::optx = "Unknown option `1` in `2`.";
DocumentGenerator::tcrea = "Unable to create generator task.";
Export::argtu = "A format must be specified when exporting to a CloudObject.";
General::appearancenotsup = "AppearanceElements setting `1` is not supported with your current subscription.";
General::cbase = "Invalid CloudBase specification `1`.";
General::cloudnf = "No CloudObject found at the given address"; (* we would like an argument: "No CloudObject at `1`."; but it requires some refactoring *)
General::cloudprecondition = "A precondition check in the cloud service failed.";
General::cloudunknown = "An unknown error occurred.";
General::invappelem = "AppearanceElements value `1` must be All, None, {}, or {\"Branding\"}.";
General::invallow = "\"Allow\" or \"Disallow\" is expected when specifying constraints.";
General::invautocp = "AutoCopy value `1` must be either True or False.";
General::invbase = "Invalid CloudBase `1`; a fully qualified domain expected.";
General::invcloudobj = "`1` is not a valid cloud object.";
General::invconstpatt = "Invalid permissions constraint pattern `1`.";
General::invconstval = "Invalid value `2` for constraint pattern `1`.";
General::invincludedf = "IncludeDefinitions value `1` must be either True or False.";
General::invmeta = "Invalid meta information `1`; a list of rules or Association with string keys expected.";
General::invperm = "Invalid permissions specification `1`.";
General::invpermform = "Permissions `1` is not of the form class -> per";
General::invsharing = "SharingList `1` can only contain strings or PermissionsGroups.";
General::invsrc = "Invalid source link `1`.";
General::invusr = "Invalid user or permissions group `1`.";  
General::invkey = "The permissions key was not a non-empty string: `1`";
General::invcsk = "Invalid ConsumerKey of the SecuredAuthenticationKey : `1`";
General::invurltp = "CloudObjectURLType `1` must be either \"Object\" or \"Environment\".";
General::keynf = "The PermissionsKey `1` was not found.";
General::maxviewers = "Maximum number of `1` viewer seats exceeded.";
General::noaccess = "Access to information for user `1` is denied.";
General::nonempdir = "Unable to overwrite non-empty directory with `1`.";
General::notauth = "Unable to authenticate with Wolfram Cloud server. Please try authenticating again.";
General::notmethod = "The specified method is not allowed.";
General::notparam = "Invalid parameters were specified.";
General::notperm = "Unable to perform the requested operation. Permission denied.";
General::rejreq = "The specified request was rejected by the server.";(*rate limit exceeded, etc*)
General::selfperm = "The currently authenticated user `1` cannot be assigned specific permissions. Owners always have full permissions on their objects.";
General::srverr = "Cloud server is not able to complete a request.";
General::userunknown = "User `1` unknown to the Wolfram Cloud.";
General::unavailable = "The cloud service is not available. Please try again shortly.";
General::unkcountry = "Unknown country `1`";
General::unkdate = "Unable to interprete date `1`";
MailReceiverFunction::cloudc = "You must be cloud connected to deploy the MailReceiverFunction.";
MailReceiverFunction::invfun = "A function is expected instead of `1`";
MailReceiverFunction::invmail = "The given message `1` is not valid mail.";
MailResponseFunction::invopt = "The value `1` should be True, False, Automatic, or a function defining the desired response content.";
MailReceiverFunction::nfile = "File `1` was not found.";
MailReceiverFunction::nfrom = "The message should include a From parameter.";
MailReceiverFunction::noco = "The MailReceiverFunction could not be deployed.";
MailReceiverFunction::nomail = "The mbox file `1` does not include any messages.";
MailReceiverFunction::perms = "MailReceiverFunction does not support the Permissions option.";
PermissionsGroup::invperm = "Invalid permissions group `1`.";
PermissionsGroup::invgrp = "Illegal character in permissions group name `1`.";
ReturnReceiptFunction::invopt = "The value `1` should be True, False, or a function determining whether to return a receipt.";
RenameFile::cldnm = "Cloud object `1` does not contain a name.";
ScheduledTask::ambig = "Timespec `` is ambiguous; try explicitly specifying start and end times.";
ScheduledTask::argu = "Unrecognized scheduling specification.";
ScheduledTask::badarg = "Bad value `1` for argument `2`."; 
ScheduledTask::copied = "Local file `1` copied to `2` for cloud execution."; 
ScheduledTask::crea = "Unable to create scheduled task.";
ScheduledTask::inact = "Scheduled task `1` is inactive.";
ScheduledTask::listing = "Unable to obtain ScheduledTask listing.";
ScheduledTask::noavil = "Scheduling tasks remotely is not yet available.";
ScheduledTask::nonext = "Unable to obtain next scheduled run time for ScheduledTask `1`.";
ScheduledTask::norm = "Unable to remove ScheduledTask `1`.";
ScheduledTask::norun = "Scheduled task `1` is not presently running.";
ScheduledTask::nostart = "Unable to start ScheduledTask.";
ScheduledTask::nostop = "Unable to stop ScheduledTask `1`.";
ScheduledTask::notask = "Argument 1 in CloudDeploy is not a recognized ScheduledTask specification.";
ScheduledTask::nouri = "Unrecognized uri specificiation `1`.";
ScheduledTask::optx = "Unknown option `1` in `2`.";
ScheduledTask::restr = "Use restricted under current subscription.";
ScheduledTask::rstsch = "Scheduling frequency is restricted on current cloud. Using \"Hourly\" instead.";
ScheduledTask::sched = "`1` is not a recognized scheduling time specification.";
ScheduledTask::tasknf = "No task found at `1`.";
ScheduledTask::upda = "Unable to update scheduled task.";
ScheduledTask::unsuppsched = "The scheduling time specification `1` is not supported by the target environment.";

HTTPResponse::encfailed = "Failed to encode HTTPResponse body";
HTTPResponse::nvldstatus = "Status code `` is not an integer from 200 to 511. 500 will be used.";
HTTPResponse::nvldheaders = "Invalid headers specification ``. Using {}.";
HTTPResponse::nvldbody = "CharacterEncoding can only be specified if the body is a string. None will be used.";
HTTPResponse::nvldenc = "`` is not a supported character encoding. `` will be used.";
HTTPResponse::chrmismatch = "The ContentType charset `` is not the same as the CharacterEncoding option ``.";
HTTPResponse::nvldcttp = "Invalid ContentType ``. `` will be used.";
HTTPResponse::nvldcache = "Invalid CachePersistence specification ``. `` will be used.";

HTTPRequest::nvldmethod = "Invalid method `` specified for HTTPRequest. GET will be used.";
HTTPRequest::chrmismatch = HTTPResponse::chrmismatch;
HTTPRequest::nvldbody = HTTPResponse::nvldbody;
HTTPRequest::nvldenc = HTTPResponse::nvldenc;
HTTPRequest::nvldheaders = HTTPResponse::nvldheaders;
HTTPRequest::encfailed = "Failed to encode HTTPRequest body";
HTTPRequest::invauth = "Failed to authenticate with `1`. Using `2` instead.";

GenerateHTTPResponse::onlycloud = "`` is only supported in the Wolfram Cloud.";
GenerateHTTPResponse::nvldrequest = "The second argument `` is not a valid Association, a list of rules, a URL or an HTTPRequest.";

CreateCloudUser::notperm = "Admin privilege required to create a user.";
CreateCloudUser::notparam = "User already exists.";

AddAdminStatus::notperm = "Admin permission is required to create a admin user.";
AddAdminStatus::exists = "User already has role `1` assigned.";

CloudUserInformation::inv = "User is not a CloudUser.";
CloudUserInformation::usernf = "No cloud user found.";

CloudUser::inv = "User is not a valid CloudUser.";

GetCloudUser::usernf = "No cloud user found.";

End[];
EndPackage[];