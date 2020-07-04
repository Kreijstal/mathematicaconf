(* ::Package:: *)

(* ::Section::Closed:: *)
(*Notes*)


(* ::Text:: *)
(*For v11, the loginDialog has been redesigned and the FrontEndResources are being moved into the CloudObject paclet. And because the v10.x.x FrontEnd does not support the new v11 TextResource file TextResources/CloudLoginStrings.tr, Dialogs.m must include the Legacy (i.e., pre-v11) dialog as well as v11's. *)


(* ::Text:: *)
(*For FrontEnd versions that are v11 or newer, Dialogs.m will launch the v11 dialog. Otherwise, the Legacy dialog appears. So both v10 and v11 versions of the dialog are supported in this package.  --larrya, 2016/06/28*)


(* ::Section::Closed:: *)
(*Header*)


BeginPackage["CloudObject`"];

Begin["`Private`"];


(* ::Section::Closed:: *)
(*Utilities*)


(* ::Subsection::Closed:: *)
(*Initial Settings*)


(* ::Subsubsection::Closed:: *)
(*Colors*)


(* ::Subsubsubsection::Closed:: *)
(*Dialog*)


(*Background color for the dialog*)
$DialogBackgroundColor = RGBColor[1., 1., 1.];



(* ::Subsubsubsection::Closed:: *)
(*Banners*)


(*Title Banner*)
$TitleBannerBackgrounColor = RGBColor[0.2, 0.2, 0.2];

(*Error Banner*)
$ErrorPanelBackgrounColor = RGBColor[1., 0.9254901960784314, 0.5176470588235293];


(* ::Subsubsubsection::Closed:: *)
(*Label Text*)


(*Text*)
$SubheaderColor = RGBColor[0.8666666666666667, 0.06666666666666667, 0.];

$DefaultColor = RGBColor[0.39215686274509803, 0.39215686274509803, 0.39215686274509803];
$HoverColor = RGBColor[0.996078431372549, 0., 0.];

(*Error message colors*)
$ErrorMessageColorInControlArea = $SubheaderColor;
$ErrorMessageColorInErrorPanel = RGBColor[0.2, 0.2, 0.2];

$RememberMeTextColor = $DefaultColor;

$ForgotPasswordDefaultTextColor = $DefaultColor;
$ForgotPasswordHoverTextColor = $HoverColor;

$NoWolframIDTextColor = $DefaultColor;
$CreateWolframIDDefaultTextColor = RGBColor[0.8666666666666667, 0.06666666666666667, 0.];
$CreateWolframIDHoverTextColor = RGBColor[0.996078431372549, 0., 0.];


(* ::Subsubsubsection::Closed:: *)
(*InputField*)


(*Color of Text in InputFields*)
$InputTextColor = RGBColor[0.2, 0.2, 0.2];


(* ::Subsubsubsection::Closed:: *)
(*Buttons*)


(*SignIn button text*)
$SignInButtonTextColor = RGBColor[1., 1., 1.];
$SignInButtonDisabledTextColor = RGBColor[0.8, 0.8, 0.8];

(*Cancel button text*)
$CancelButtonTextColor = $DefaultColor;
$CancelButtonPressedTextColor = RGBColor[1., 1., 1.];
(*$CancelButtonDisabledTextColor = RGBColor[1., 1., 1., 0.5];*)


(* ::Subsubsubsection::Closed:: *)
(*Cloud Server Selector (unfinished)*)


(*Color of text in CloudServer pull-down-menu when hovered over.*)
$MenuHoverTextColor = RGBColor[0.6509803921568628, 0.6509803921568628, 0.6509803921568628];


(* ::Subsubsubsection::Closed:: *)
(*Divider line*)


$DividerColor = RGBColor[0.8980392156862745, 0.8980392156862745, 0.8980392156862745];


(* ::Subsubsection::Closed:: *)
(*Sizes and Spacings*)


(* ::Subsubsubsection::Closed:: *)
(*Dialog (overall)*)


$DialogWidth = 392;
$LeftDialogMargin = 41;
$RightDialogMargin = $LeftDialogMargin;

$DividerWidth = 330;
$LeftDividerMargin = 31;
$RightDividerMargin = $LeftDialogMargin;


(* ::Subsubsubsection::Closed:: *)
(*Font Sizes*)


$SubheaderFontSize = 18;

$InputFieldHeaderFontSize = 14;
$InputFieldFontSize = $InputFieldHeaderFontSize;

$ErrorFontSizeInControlArea = 10;
$ErrorFontSizeInPanelArea = 12;

$RememberMeFontSize = 12;

$ButtonFontSize = 12;

$ForgotPasswordFontSize = $RememberMeFontSize;

$CloudUserIDFontSize = 14;


(* ::Subsubsubsection::Closed:: *)
(*Font Family*)


$BaseFontFamily = "Source Sans Pro";


(* ::Subsubsubsection::Closed:: *)
(*Title Banner*)


$TitleBannerHeight = 44;
$IconToTextGutterSpace = 14;


(* ::Subsubsection::Closed:: *)
(*Spacing utilities*)


spacr[vertspc_] := Spacer[{30, vertspc}]


(* ::Subsection::Closed:: *)
(*FE Resources*)


(* ::Subsubsection::Closed:: *)
(*Load the Paclet's TextResources (FlushTextResourceCaches)*)


(* ::Text:: *)
(*MathLink`CallFrontEnd[FrontEnd`FlushTextResourceCaches[]] should only evaluate in a Desktop FrontEnd whose version is >= 11. It should not evaluate in standalone kernel or a cloud FE.*)


If[TrueQ[$Notebooks && Not[$CloudEvaluation] && BoxForm`sufficientVersionQ[11]],
	MathLink`CallFrontEnd[FrontEnd`FlushTextResourceCaches[]]
]; (*Flush the TextResource caches in order to load the paclet's TR. *)


(* ::Subsubsection::Closed:: *)
(*Locations*)


(* ::Subsubsubsection::Closed:: *)
(*Bitmap Directory Path*)


(*Relative directory path to bitmaps*)
$CloudLoginBitmapsDirectory = If[BoxForm`sufficientVersionQ[11], {"Dialogs", "CloudLogin", "v2.0"}, {"Dialogs", "CloudLogin"}];


(* ::Subsubsubsection::Closed:: *)
(*Strings (@@resource)*)


(*Relative directory path to bitmaps*)
$CloudLoginTextResource = If[BoxForm`sufficientVersionQ[11], "CloudLoginDialogStrings", "CloudLoginDialog"];


(* ::Subsubsection::Closed:: *)
(*Text Resources*)


tr[resource_String, id_] := Dynamic@RawBoxes@FEPrivate`FrontEndResource[resource, id];

tr[id_] := tr[$CloudLoginTextResource, id]


(* ::Subsubsection::Closed:: *)
(*Image Resources*)


imgr[relPth_List, flNm_String] := FrontEnd`FileName[relPth, flNm];

imgr[flNm_String] := imgr[$CloudLoginBitmapsDirectory, flNm];


imgimportr[relPth_List, flNm_String] := 
  Dynamic[RawBoxes@
    FEPrivate`ImportImage[FrontEnd`ToFileName[relPth, flNm]]];
    
imgimportr[flNm_String] := imgimportr[$CloudLoginBitmapsDirectory, flNm]


(*HiDPI: Vector Graphics*)
grbox["CheckboxOn"] := RawBoxes @ GraphicsBox[{Thickness[0.0625], 
  StyleBox[{FilledCurveBox[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 
        3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, {{{12.5, 
        1.}, {3.5, 1.}, {2.122, 1.}, {1., 2.121}, {1., 3.5}, {1., 
        12.5}, {1., 13.879}, {2.122, 15.}, {3.5, 15.}, {12.5, 
        15.}, {13.878, 15.}, {15., 13.879}, {15., 12.5}, {15., 
        3.5}, {15., 2.121}, {13.878, 1.}, {12.5, 1.}}}]}, FaceForm[
RGBColor[0.537, 0.537, 0.537, 1.]], StripOnInput -> False], 
  StyleBox[{FilledCurveBox[{{{1, 4, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 
        0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, {{{3.5, 14.}, {2.673, 
        14.}, {2., 13.327}, {2., 12.5}, {2., 3.5}, {2., 
        2.673}, {2.673, 2.}, {3.5, 2.}, {12.5, 2.}, {13.327, 
        2.}, {14., 2.673}, {14., 3.5}, {14., 12.5}, {14., 
        13.327}, {13.327, 14.}, {12.5, 14.}}}]}, FaceForm[
RGBColor[1., 1., 1., 1.]], StripOnInput -> False], 
  StyleBox[{FilledCurveBox[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 
        0}, {0, 1, 0}}}, {{{11.8682, 11.5039}, {10.1322, 
        12.4959}, {6.6632, 6.4269}, {4.5552, 7.8319}, {3.4452, 
        6.1679}, {7.3372, 3.5729}}}]}, FaceForm[
RGBColor[0.537, 0.537, 0.537, 1.]], StripOnInput -> False]}, 
 ImageSize -> {16., 16.}, PlotRange -> {{0., 16.}, {0., 16.}}, 
 AspectRatio -> Automatic]
 
grbox["CheckboxOff"] := RawBoxes @ GraphicsBox[{Thickness[0.0625], 
  StyleBox[{FilledCurveBox[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 
        3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, {{{12.5, 
        1.}, {3.5, 1.}, {2.122, 1.}, {1., 2.121}, {1., 3.5}, {1., 
        12.5}, {1., 13.879}, {2.122, 15.}, {3.5, 15.}, {12.5, 
        15.}, {13.878, 15.}, {15., 13.879}, {15., 12.5}, {15., 
        3.5}, {15., 2.121}, {13.878, 1.}, {12.5, 1.}}}]}, FaceForm[
RGBColor[0.537, 0.537, 0.537, 1.]], StripOnInput -> False], 
  StyleBox[{FilledCurveBox[{{{1, 4, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 
        0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, {{{3.5, 14.}, {2.673, 
        14.}, {2., 13.327}, {2., 12.5}, {2., 3.5}, {2., 
        2.673}, {2.673, 2.}, {3.5, 2.}, {12.5, 2.}, {13.327, 
        2.}, {14., 2.673}, {14., 3.5}, {14., 12.5}, {14., 
        13.327}, {13.327, 14.}, {12.5, 14.}}}]}, FaceForm[
RGBColor[1., 1., 1., 1.]], StripOnInput -> False]}, 
 ImageSize -> {16., 16.}, PlotRange -> {{0., 16.}, {0., 16.}}, 
 AspectRatio -> Automatic]



(* Replacing the titleBanner bitmap-image with a resolution-independent image for HiDPI *)
grbox["Banner"] := Framed[RawBoxes @ bannerImgr, ImageSize -> {Full, Automatic}, Alignment -> {Left, Center}, FrameStyle -> None, FrameMargins -> 0, Background -> $TitleBannerBackgrounColor];
bannerImgr = GraphicsBox[
{Thickness[0.005050505050505051], 
{FaceForm[{RGBColor[0.8670000000000001, 0.067, 0.], Opacity[1.]}], 
     FilledCurveBox[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{0., 0.}, {
      44., 0.}, {44., 44.}, {0., 44.}}}], 
     FilledCurveBox[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{0., 0.}, {
      44., 0.}, {44., 44.}, {0., 44.}}}]}, 
{FaceForm[{RGBColor[1., 1., 1.], Opacity[1.]}], 
     FilledCurveBox[{{{1, 4, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}}, {{
       0, 2, 0}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 
       0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {0, 
       1, 0}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 3}, {1, 3, 
       3}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 
       3}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 
       0}}, {{1, 4, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {
       1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}}, {{0, 2, 
       0}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {0, 
       1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {
       0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 
       0}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 0}, {
       1, 3, 3}, {0, 1, 0}, {1, 3, 3}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 
       0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}, {{1, 4, 3}, {
       1, 3, 3}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {1, 3, 3}, {1, 3, 
       3}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 
       1, 0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 
       0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {
       0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}, {{1, 4, 3}, {1, 3, 
       3}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {
       1, 3, 3}}, {{0, 2, 0}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 
       3}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 
       3, 3}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {
       1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 
       0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 
       0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 
       0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 
       0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 
       0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 
       0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 
       0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 
       0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 
       0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 
       0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 
       0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 
       0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 
       0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 
       1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {
       0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 
       0}, {0, 1, 0}}}, {{{188.9541, 17.877900000000004`}, {
       191.28910000000002`, 17.877900000000004`}, {
       192.61710000000002`, 19.204900000000002`}, {
       192.61710000000002`, 22.052900000000005`}, {
       192.61710000000002`, 24.883900000000004`}, {
       191.28910000000002`, 26.121900000000004`}, {188.9541, 
       26.121900000000004`}, {188.05210000000002`, 
       26.121900000000004`}, {188.05210000000002`, 
       17.877900000000004`}}, {{186.0001, 27.784900000000004`}, {
       189.0961, 27.784900000000004`}, {192.63410000000002`, 
       27.784900000000004`}, {194.7221, 25.873900000000006`}, {
       194.7221, 22.052900000000005`}, {194.7221, 
       18.213900000000002`}, {192.63410000000002`, 
       16.214900000000004`}, {189.20110000000003`, 
       16.214900000000004`}, {186.0001, 
       16.214900000000004`}}, CompressedData["
1:eJxTTMoPSmViYGAQBmIQHSm+/SLDg1SHOaHzV6/NMHWA8ZMXXjN5f8La4evt
643FCmkY/D6Nt7z7HEzhfC0g1+CkkcPq81fD3timOXxKPhPr/cPQYdef9i+3
MxH80IMrlvhNQ/AFuLcuq9yK0A/jw8yH8WH293i9YjH5jMmHuR/Gdzh2Z2vL
GkOHc6uADtqX5iD4cvt6ZnMDuHtgfKNn61SfiCL4MP/D9KOHDwCalJOy
"], {{
       170.9821, 22.052900000000005`}, {170.9821, 
       24.636900000000004`}, {169.81310000000002`, 
       26.212900000000005`}, {167.99110000000002`, 
       26.212900000000005`}, {166.1681, 26.212900000000005`}, {
       165.0001, 24.636900000000004`}, {165.0001, 
       22.052900000000005`}, {165.0001, 19.450900000000004`}, {
       166.1681, 17.786900000000003`}, {167.99110000000002`, 
       17.786900000000003`}, {169.81310000000002`, 
       17.786900000000003`}, {170.9821, 19.450900000000004`}, {
       170.9821, 22.052900000000005`}}, {{162.8941, 
       22.052900000000005`}, {162.8941, 25.786900000000003`}, {
       164.9821, 27.999900000000004`}, {167.99110000000002`, 
       27.999900000000004`}, {171.0171, 27.999900000000004`}, {
       173.10510000000002`, 25.786900000000003`}, {
       173.10510000000002`, 22.052900000000005`}, {
       173.10510000000002`, 18.300900000000006`}, {171.0171, 
       15.999900000000004`}, {167.99110000000002`, 
       15.999900000000004`}, {164.9821, 15.999900000000004`}, {
       162.8941, 18.300900000000006`}, {162.8941, 
       22.052900000000005`}}, {{155.00010000000003`, 
       27.784900000000004`}, {157.0531, 27.784900000000004`}, {
       157.0531, 17.947900000000004`}, {161.8641, 
       17.947900000000004`}, {161.8641, 16.214900000000004`}, {
       155.00010000000003`, 16.214900000000004`}}, CompressedData["
1:eJxTTMoPSmViYGCQBmIQrWU16XQ9U5KD4Mvt65m/mzrA+MfvbG2pOWDp4D7l
G1u8X5KDlat70c//1g4t886uOr8awU95F+VkdwvBX6/6pHne/ySHU+y2s0P9
rR2uaKdKPhJLdmhSO9S2vN3KQepRhPj2z0kOB6MVHD8aWzrkHf23qfpBksOL
Ho23vOcsHQ7Uylqkn0pyaAn0nNtgZuXQ8eX29ca1CD5nl3zyuwIEf9ef9i+3
XZMcJtX9tiqYZwHnG83UkpjKYgbnP+KevLIp0RhiXm6SwwcLoINPGsL9A+M7
2724ueYMgt/gsb9W9lmSw1LGPaxCLkYOSn2lhdK/kxzcgN7l/2nk8JrFRNBM
LtmhVGX6/wnrDB2WVx7XtGJIdkgs85cTm2XgcI8J6OKbSQ4lXX1PPv3Xd2AE
CaxE8I9cBgaQD4IPC//in/wvt8sbOaDHDwAwAcT7
"], CompressedData["
1:eJxTTMoPSmViYGCQA2IQvVkvbzHjgXiHOaHzV689Ye1QJM37QFcrAc7/2/7l
9vXsBIeSn/wvt8ubOfBsXVZ5vDjB4VZ8mb9cmqmD1KMI8e3VCQ6Rp4yObJxn
4rBB9UnzvOYEhwsdl+49uG3ssOsP0IBWBN9BOPHw5V6E+vA3+rvVpyLMS5ME
GjgXYR+YfxfhnpZ5Z1edV0/E4N8weW/ham7g0OP1isXkdQIGP7REZfr/BlM4
f9E1oIKpZg5d8snvoj4kOKxdeyO+TN7CYfLpeo/9nxMcmPewConoWzoIcAM9
/BHBD56m2Fd6OcEhAuT+OjOHK9qpko+mJTioTf8/oc7bEOKefAT//6bqTxuM
EOoPrVji5yCKMG/HRYZ7TMII/glNq0mnRRHuAdsnjXAvjA/zD4wP8y8sPmF8
AEKo1dk=
"], {{119.4681, 20.955900000000003`}, {122.4231, 
       20.955900000000003`}, {122.01610000000001`, 
       22.370900000000006`}, {121.6431, 23.591900000000003`}, {
       121.30810000000001`, 24.918900000000004`}, {120.9711, 
       26.191900000000004`}, {120.90010000000001`, 
       26.191900000000004`}, {120.58310000000002`, 
       24.899900000000002`}, {120.24510000000001`, 
       23.591900000000003`}, {119.87410000000001`, 
       22.370900000000006`}}, {{119.76710000000001`, 
       27.784900000000004`}, {122.17510000000001`, 
       27.784900000000004`}, {125.94310000000002`, 
       16.214900000000004`}, {123.78410000000002`, 
       16.214900000000004`}, {122.88210000000002`, 
       19.346900000000005`}, {118.9901, 19.346900000000005`}, {
       118.08810000000001`, 16.214900000000004`}, {116.0001, 
       16.214900000000004`}}, {{109.6971, 22.370900000000006`}, {
       111.27310000000001`, 22.370900000000006`}, {
       112.13910000000001`, 23.043900000000004`}, {
       112.13910000000001`, 24.352900000000005`}, {
       112.13910000000001`, 25.679900000000004`}, {
       111.27310000000001`, 26.139900000000004`}, {109.6971, 
       26.139900000000004`}, {108.0531, 26.139900000000004`}, {
       108.0531, 22.370900000000006`}}, {{106.0001, 
       27.784900000000004`}, {109.9111, 27.784900000000004`}, {
       112.31510000000002`, 27.784900000000004`}, {
       114.15610000000001`, 26.935900000000004`}, {
       114.15610000000001`, 24.352900000000005`}, {
       114.15610000000001`, 22.582900000000002`}, {
       113.23610000000001`, 21.522900000000003`}, {
       111.85610000000001`, 21.044900000000005`}, {
       114.61710000000001`, 16.214900000000004`}, {
       112.31510000000002`, 16.214900000000004`}, {
       109.82110000000002`, 20.743900000000004`}, {108.0531, 
       20.743900000000004`}, {108.0531, 16.214900000000004`}, {
       106.0001, 16.214900000000004`}}, {{97.0001, 
       27.784900000000004`}, {103.9881, 27.784900000000004`}, {
       103.9881, 26.051900000000003`}, {99.0531, 
       26.051900000000003`}, {99.0531, 22.777900000000002`}, {
       103.26310000000001`, 22.777900000000002`}, {
       103.26310000000001`, 21.044900000000005`}, {99.0531, 
       21.044900000000005`}, {99.0531, 16.214900000000004`}, {97.0001,
        16.214900000000004`}}, {{88.0001, 27.784900000000004`}, {
       90.0531, 27.784900000000004`}, {90.0531, 
       17.947900000000004`}, {94.86410000000001, 
       17.947900000000004`}, {94.86410000000001, 
       16.214900000000004`}, {88.0001, 16.214900000000004`}}, {{
       83.98110000000001, 22.052900000000005`}, {83.98110000000001, 
       24.636900000000004`}, {82.81410000000001, 
       26.212900000000005`}, {80.9901, 26.212900000000005`}, {
       79.16810000000001, 26.212900000000005`}, {77.99910000000001, 
       24.636900000000004`}, {77.99910000000001, 
       22.052900000000005`}, {77.99910000000001, 
       19.450900000000004`}, {79.16810000000001, 
       17.786900000000003`}, {80.9901, 17.786900000000003`}, {
       82.81410000000001, 17.786900000000003`}, {83.98110000000001, 
       19.450900000000004`}, {83.98110000000001, 
       22.052900000000005`}}, {{75.89410000000001, 
       22.052900000000005`}, {75.89410000000001, 
       25.786900000000003`}, {77.98210000000002, 
       27.999900000000004`}, {80.9901, 27.999900000000004`}, {84.0171,
        27.999900000000004`}, {86.10510000000001, 
       25.786900000000003`}, {86.10510000000001, 
       22.052900000000005`}, {86.10510000000001, 
       18.300900000000006`}, {84.0171, 15.999900000000004`}, {80.9901,
        15.999900000000004`}, {77.98210000000002, 
       15.999900000000004`}, {75.89410000000001, 
       18.300900000000006`}, {75.89410000000001, 
       22.052900000000005`}}, CompressedData["
1:eJxTTMoPSmViYGBQAmIQrXApP549zM8heeE1k/cnrB3qzFbbhSf7w/lxG/e/
mffE30Fy6hXOjE+mDovSN78q/uvvsAgobbHVxOFZIlAhd4DDp+Qzsd4Vxg7c
GUo5FWIBDsU/+V9utzdyYNrDKiQijeDrOsu8fqSFUO81t0HtkFWAQ2KZv5zY
LhOIOu8AuH0ah9qWhy8LgLtH1P7Yna0SgXD+DaAzXIsD4er59xnM1GoJdJhU
99uqYJ+Jg7YEUGJKoINKTkXV0hpjBwupA9EKSwPh7on+uvNW10oEH2zuFoT6
ySubAj0PBsLdtwrInXsGYZ/mW959BrxBcPfIi2X5fu5C8P+lfn+S+DfQQfDl
9vXM5gYO627El/lHIfh/rArOdfwLcDgX632C3dfM4RZQWu5TgMMvoHPWG5s7
HAZ6/9SzAIcWoLUNYhYOs7SAHrob4NAA9N65HxaQeLiJ4HdeuveA+zxCPdhf
hxHmxQGtsd2OsA9sXmwA3D1LJlkx+tb4w/kAlrnzkQ==
"], {{29.83910000000003, 
       17.107900000000004`}, {27.681100000000043`, 
       17.791900000000005`}, {30.34710000000001, 
       18.577900000000003`}, {32.772099999999995`, 
       17.747900000000005`}}, {{25.746100000000013`, 
       13.311900000000005`}, {25.832099999999997`, 
       16.795900000000003`}, {29.10209999999998, 
       15.758900000000004`}, {29.511100000000027`, 
       11.658900000000003`}}, {{14.89609999999999, 
       15.760900000000003`}, {18.16810000000001, 
       16.795900000000003`}, {18.252100000000013`, 
       13.313900000000004`}, {14.486099999999993`, 
       11.660900000000005`}}, {{11.227100000000007`, 
       17.749900000000004`}, {13.653099999999995`, 
       18.577900000000003`}, {16.31710000000001, 
       17.791900000000005`}, {14.161100000000033`, 
       17.109900000000003`}}, {{12.792100000000005`, 
       21.357900000000004`}, {14.783100000000019`, 
       22.096900000000005`}, {13.178100000000029`, 
       20.009900000000002`}, {10.89609999999999, 
       19.230900000000005`}}, {{9.846100000000007, 
       25.949900000000007`}, {13.868099999999998`, 
       26.825900000000004`}, {15.80110000000002, 
       24.083900000000003`}, {12.57510000000002, 
       22.886900000000004`}}, {{24.072099999999978`, 
       31.234900000000003`}, {22.000100000000003`, 
       28.587900000000005`}, {19.929100000000005`, 
       31.234900000000003`}, {22.000100000000003`, 34.7799}}, {{
       34.15510000000003, 25.947900000000004`}, {31.42609999999999, 
       22.884900000000002`}, {28.199100000000016`, 
       24.082900000000002`}, {30.133099999999985`, 
       26.824900000000003`}}, {{23.221100000000035`, 
       21.642900000000004`}, {26.749100000000027`, 
       22.829900000000002`}, {29.09609999999998, 
       19.781900000000004`}, {25.39609999999999, 
       18.689900000000005`}}, {{19.71410000000003, 
       15.293900000000004`}, {21.246100000000013`, 
       13.058900000000005`}, {21.246100000000013`, 
       10.508900000000004`}, {19.7671, 13.037900000000004`}}, {{
       17.251100000000037`, 22.829900000000002`}, {
       20.779100000000028`, 21.642900000000004`}, {18.60409999999999, 
       18.689900000000005`}, {14.9041, 19.781900000000004`}}, {{
       14.9281, 27.941900000000004`}, {14.633099999999985`, 
       30.902900000000002`}, {16.0831, 28.943900000000006`}, {
       16.15810000000002, 26.193900000000006`}}, {{
       22.754100000000022`, 26.830900000000007`}, {
       26.384100000000018`, 28.1229}, {26.278100000000023`, 
       24.263900000000003`}, {22.754100000000022`, 
       23.077900000000003`}}, {{17.72210000000001, 
       24.263900000000003`}, {17.61609999999999, 28.1229}, {
       21.246100000000013`, 26.830900000000007`}, {
       21.246100000000013`, 23.077900000000003`}}, {{
       22.000100000000003`, 14.626900000000004`}, {19.82310000000001, 
       17.801900000000003`}, {22.000100000000003`, 
       20.754900000000003`}, {24.177100000000024`, 
       17.801900000000003`}}, {{29.074099999999987`, 
       27.940900000000006`}, {27.842100000000016`, 
       26.192900000000005`}, {27.917100000000005`, 
       28.943900000000006`}, {29.370100000000008`, 
       30.902900000000002`}}, {{25.46010000000001, 
       30.559900000000006`}, {28.095100000000002`, 
       31.714900000000004`}, {26.70010000000002, 
       29.833900000000007`}, {24.195100000000025`, 
       28.944900000000004`}}, {{19.80710000000002, 
       28.943900000000006`}, {17.300100000000015`, 
       29.835900000000002`}, {15.905100000000033`, 
       31.718900000000005`}, {18.54310000000001, 
       30.559900000000006`}}, {{24.23010000000002, 
       13.035900000000005`}, {22.754100000000022`, 
       10.508900000000004`}, {22.754100000000022`, 
       13.058900000000005`}, {24.286100000000005`, 
       15.291900000000004`}}, {{33.1011, 19.229900000000004`}, {
       30.822100000000006`, 20.009900000000002`}, {29.21810000000002, 
       22.095900000000004`}, {31.207100000000025`, 
       21.356900000000003`}}, CompressedData["
1:eJxTTMoPSmViYGAQAWIQfUEy8JZ0iIPDs8SF10z+mzqkxN5xY25wclAQy/L9
fM/KYUP37QyG+XYOgXJAAWcbhxu2lRErXO0dZpfPWaSc6Ojw4YClspe3JYQv
6OBwYteOXjYGMweuLvnkd4+cHCx8lnG5bTVyqJO1SHcByk+Yd3bV+VJNh6TD
l7VTgfoTsmeVzzmk5XBva0vNBaD5DlIHohUYZRysXN2Lft63cvjQDNSwXs2h
+Cf/y+1A9ym8fmQmxSDjEFqiMv2/gqHDBaAxkkD9OiuFL7hU6Ds8uN5Y7Fai
6VBrttouvFrJYcUW8x+Hthg5vJ9no3NllzrcffM4pOfFvZRwSNA8LXDcy9Lh
Sn48+7md6g4dUZf3PHaxd0hiPycZWKXk8MJ+yf198+wchKqABpbrOzRsd2h6
VO8EcZ+8oQMAPLWHOg==
"]}]}},
AspectRatio->Automatic,
ImageSize->{198., 44.},
PlotRange->{{0., 198.}, {0., 44.}}];

CloudDialogImage["CheckboxOn"] := imgr["CheckboxOn.png"];
CloudDialogImage["CheckboxOn"] := grbox["CheckboxOn"] /; BoxForm`sufficientVersionQ[11];

CloudDialogImage["CheckboxOff"] := imgr["CheckboxOff.png"];
CloudDialogImage["CheckboxOff"] := grbox["CheckboxOff"] /; BoxForm`sufficientVersionQ[11];

CloudDialogImage["TopBanner"] := imgr["Banner.9.png"];
CloudDialogImage["TopBanner"] := grbox["Banner"] /; BoxForm`sufficientVersionQ[11];

CloudDialogImage["SigninButton","Default"] := imgr["SigninButton-Default.9.png"];
                    
CloudDialogImage["SigninButton","Hover"] := imgr["SigninButton-Hover.9.png"];
                    
CloudDialogImage["SigninButton","Pressed"] := imgr["SigninButton-Pressed.9.png"];

CloudDialogImage["SigninButton","Disabled"] := imgr["SigninButton-Disabled.9.png"];

CloudDialogImage["CancelButton","Default"] := imgr["CancelButton-Default.9.png"];
                    
CloudDialogImage["CancelButton","Hover"] := imgr["CancelButton-Hover.9.png"];
                    
CloudDialogImage["CancelButton","Pressed"] := imgr["CancelButton-Pressed.9.png"];

CloudDialogImage["CancelButton","Disabled"] := imgr["CancelButton-Disabled.9.png"];


(*Used in Legacy FrontEnd Only*)

CloudDialogImage["CloudLogoIcon"] := imgimportr["CloudLogoIcon.png"];

CloudDialogImage["BackgroundImage"] := imgr["Background.9.png"];

CloudDialogImage["JoinNowButton","Default"] := imgr["JoinNowButton-Default.9.png"];
                
CloudDialogImage["JoinNowButton","Hover"] := imgr["JoinNowButton-Hover.9.png"];
                
CloudDialogImage["JoinNowButton","Pressed"] := imgr["JoinNowButton-Pressed.9.png"];





(* ::Subsection::Closed:: *)
(*Labels*)


(* ::Subsubsection::Closed:: *)
(*Inline Styles*)


(* ::Subsubsubsection::Closed:: *)
(*Specific Styles that Are Derived from the following "Base Text Styles"*)


styledtxt[txt_, "subheader"] := styledtxt[txt, "basefont", FontSize -> $SubheaderFontSize, FontColor -> $SubheaderColor];

styledtxt[txt_, "errorInControlArea"] := styledtxt[txt, "errormssg", FontSize -> $ErrorFontSizeInControlArea, FontColor -> $ErrorMessageColorInControlArea];
styledtxt[txt_, "errorInErrorPanel"] := styledtxt[txt, "errormssg", FontSize -> $ErrorFontSizeInPanelArea, FontColor -> $ErrorMessageColorInErrorPanel];

styledtxt[txt_, "rememberme"] := styledtxt[txt, "basefont", FontSize -> $RememberMeFontSize, FontColor -> $RememberMeTextColor, LineBreakWithin -> False];

styledtxt[txt_, "forgotpassword", "default"] := styledtxt[txt, "forgotpassword", $ForgotPasswordDefaultTextColor];
styledtxt[txt_, "forgotpassword", "hover"] := styledtxt[txt, "forgotpassword", $ForgotPasswordHoverTextColor];

styledtxt[txt_, "wolframid", "leadin"] := styledtxt[txt, "wolframid", $NoWolframIDTextColor];
styledtxt[txt_, "wolframidlink", "default"] := styledtxt[txt, "wolframid", $CreateWolframIDDefaultTextColor];
styledtxt[txt_, "wolframidlink", "hover"] := styledtxt[txt, "wolframid", $CreateWolframIDHoverTextColor];


(* ::Subsubsubsection::Closed:: *)
(*Base Text Styles*)


styledtxt[txt_, "errormssg", opts___] := styledtxt[txt, "basefont", LineBreakWithin -> Automatic, LineIndent -> 0, TextAlignment -> Center, Hyphenation -> False, opts];

styledtxt[txt_, "forgotpassword", colr_] := styledtxt[txt, "basefont", FontSize -> $ForgotPasswordFontSize, LineBreakWithin -> False, FontColor -> colr];

styledtxt[txt_, "buttons", colr_] := styledtxt[txt, "basefont", FontSize -> $ButtonFontSize, LineBreakWithin -> False, FontColor -> colr];

styledtxt[txt_, "wolframid", colr_] := styledtxt[txt, "basefont", FontSize -> $CloudUserIDFontSize, LineBreakWithin -> False, FontColor -> colr];

styledtxt[txt_, opts___] := styledtxt[txt, "basefont", FontSize -> $InputFieldHeaderFontSize, FontColor -> $DefaultColor, LineBreakWithin -> False, opts];

styledtxt[txt_, "basefont", opts___] := Style[txt, FontFamily -> $BaseFontFamily, FontWeight -> "Regular", LineSpacing -> {1, 0}, AutoSpacing -> False, opts];


(* ::Subsubsection::Closed:: *)
(*Mouseover Styles*)


mouseovertxt[txt_, styleID_:"forgotpassword"] := PaneSelector[
	{
		False -> styledtxt[txt, styleID, "default"],
		True -> styledtxt[txt, styleID, "hover"]
	},
	FrontEnd`CurrentValue["MouseOver"]
];


(* ::Subsection::Closed:: *)
(*Divider*)


dividerLine = Item[Graphics[{$DividerColor, Rectangle[]}, ImageSize -> {$DividerWidth, 1}, AspectRatio -> Full], Alignment -> Center];


(* ::Subsection::Closed:: *)
(*Controls*)


(* ::Subsubsection::Closed:: *)
(*InputFields*)


inptFld[dyn:Dynamic[expr_], boxid_, fldtype_, opts:OptionsPattern[]] := InputField[Dynamic[expr], fldtype, 
	ContinuousAction -> True,  
	System`BoxID -> boxid, 
	ImageSize -> {Full, Automatic},
	BaseStyle -> {
		FontFamily -> $BaseFontFamily,
		FontWeight -> "Regular",
		FontColor -> $InputTextColor,
		FontSize -> $InputFieldFontSize 
	},
	opts];

loginFld[dyn:Dynamic[expr_], boxid_, opts:OptionsPattern[]] := inptFld[dyn, boxid, String, opts];

loginFld[dyn:Dynamic[uname_], boxid_, "username"] := loginFld[dyn, boxid];

loginFld[dyn:Dynamic[pwd_], boxid_, "password"] := loginFld[dyn, boxid, FieldMasked -> True];


(* ::Subsubsection::Closed:: *)
(*Buttons*)


button[label_, "hyperlink", events_] := MouseAppearance[button[label, events], "LinkHand"];

button[label_, events_] := EventHandler[
	label,
	events,
	PassEventsDown -> True
]


(* ::Subsection::Closed:: *)
(*Login/Quit Buttons*)


progressSpinner[] := progressSpinner[20, White];

progressSpinner[size_,color_,opts:OptionsPattern[]] := StyleBox[DynamicModuleBox[{$CellContext`v$$=1,$CellContext`rotationMatrix$$={{{1.,0},{0,1.}},{{0.8660254037844384,0.5000000000000004},{-0.5000000000000004,0.8660254037844384}},{{0.5,0.8660254037844386},{-0.8660254037844386,0.5}},{{0,1.},{-1.,0}},{{-0.5000000000000004,0.8660254037844384},{-0.8660254037844384,-0.5000000000000004}},{{-0.8660254037844386,0.5000000000000001},{-0.5000000000000001,-0.8660254037844386}},{{-1.,0},{0,-1.}},{{-0.8660254037844387,-0.49999999999999994`},{0.49999999999999994`,-0.8660254037844387}},{{-0.4999999999999998,-0.8660254037844388},{0.8660254037844388,-0.4999999999999998}},{{0,-1.},{1.,0}},{{0.5000000000000001,-0.8660254037844386},{0.8660254037844386,0.5000000000000001}},{{0.8660254037844387,-0.49999999999999994`},{0.49999999999999994`,0.8660254037844387}}}},OverlayBox[{AnimatorBox[Dynamic[$CellContext`v$$],{1,12,1},AppearanceElements->{},DefaultDuration->1,ImageSize->{1,1}],GraphicsBox[GeometricTransformationBox[{{color,Opacity[0.6],GeometricTransformationBox[PolygonBox[CompressedData["
1:eJxTTMoPSmViYGCQAmIQDQEf9s+aCQKyDqL7Pdzvvni3n/nUptKFi+QcVvtE
vKja9hoqL+9wxro+bd6CZ/u7Dphsvmam4ADR/2D/jMCgapMOBYfJbJMP1k64
sH9fhOPCqN0weQaGs2dAQMGhHyJvD5P/+x8E7tvD9EPNt4eZvxJivz3MfhGI
++xh7oO63x7mfhhfdJ37wyoREZh/7NfU6rEWcws7rIGaB5EXcjgLtW/OsXKR
dAdBB2aIf+wblti0rJ4r4DAV6l6jaXEW8eYCDuwxIsZqyxbaQPQLOHRC/QuT
/wPxz36Yflh4zYaavwIaniJQ+2HhDXMfLD5g7gcA8lu3pQ==
           "]],{{{1,0},{0,1}},{0,0}}]},{color,Opacity[0.5499999999999999],GeometricTransformationBox[PolygonBox[CompressedData["
1:eJxTTMoPSmViYGCQAmIQDQEf9s+aCQKyDqL7Pdzvvni3n/nUptKFi+QcVvtE
vKja9hoqL+9wxro+bd6CZ/u7Dphsvmam4ADR/2D/jMCgapMOBYfJbJMP1k64
sH9fhOPCqN0weQaGs2dAQMGhHyJvD5P/+x8E7tvD9EPNt4eZvxJivz3MfhGI
++xh7oO63x7mfhhfdJ37wyoREZh/7NfU6rEWcws7rIGaB5EXcjgLtW/OsXKR
dAdBB2aIf+wblti0rJ4r4DAV6l6jaXEW8eYCDuwxIsZqyxbaQPQLOHRC/QuT
/wPxz36Yflh4zYaavwIaniJQ+2HhDXMfLD5g7gcA8lu3pQ==
           "]],{{{0.8660254037844386,-0.5},{0.5,0.8660254037844386}},{0,0}}]},{color,Opacity[0.5],GeometricTransformationBox[PolygonBox[CompressedData["
1:eJxTTMoPSmViYGCQAmIQDQEf9s+aCQKyDqL7Pdzvvni3n/nUptKFi+QcVvtE
vKja9hoqL+9wxro+bd6CZ/u7Dphsvmam4ADR/2D/jMCgapMOBYfJbJMP1k64
sH9fhOPCqN0weQaGs2dAQMGhHyJvD5P/+x8E7tvD9EPNt4eZvxJivz3MfhGI
++xh7oO63x7mfhhfdJ37wyoREZh/7NfU6rEWcws7rIGaB5EXcjgLtW/OsXKR
dAdBB2aIf+wblti0rJ4r4DAV6l6jaXEW8eYCDuwxIsZqyxbaQPQLOHRC/QuT
/wPxz36Yflh4zYaavwIaniJQ+2HhDXMfLD5g7gcA8lu3pQ==
           "]],{{{0.5,-0.8660254037844386},{0.8660254037844386,0.5}},{0,0}}]},{color,Opacity[0.44999999999999996`],GeometricTransformationBox[PolygonBox[CompressedData["
1:eJxTTMoPSmViYGCQAmIQDQEf9s+aCQKyDqL7Pdzvvni3n/nUptKFi+QcVvtE
vKja9hoqL+9wxro+bd6CZ/u7Dphsvmam4ADR/2D/jMCgapMOBYfJbJMP1k64
sH9fhOPCqN0weQaGs2dAQMGhHyJvD5P/+x8E7tvD9EPNt4eZvxJivz3MfhGI
++xh7oO63x7mfhhfdJ37wyoREZh/7NfU6rEWcws7rIGaB5EXcjgLtW/OsXKR
dAdBB2aIf+wblti0rJ4r4DAV6l6jaXEW8eYCDuwxIsZqyxbaQPQLOHRC/QuT
/wPxz36Yflh4zYaavwIaniJQ+2HhDXMfLD5g7gcA8lu3pQ==
           "]],{{{0,-1},{1,0}},{0,0}}]},{color,Opacity[0.39999999999999997`],GeometricTransformationBox[PolygonBox[CompressedData["
1:eJxTTMoPSmViYGCQAmIQDQEf9s+aCQKyDqL7Pdzvvni3n/nUptKFi+QcVvtE
vKja9hoqL+9wxro+bd6CZ/u7Dphsvmam4ADR/2D/jMCgapMOBYfJbJMP1k64
sH9fhOPCqN0weQaGs2dAQMGhHyJvD5P/+x8E7tvD9EPNt4eZvxJivz3MfhGI
++xh7oO63x7mfhhfdJ37wyoREZh/7NfU6rEWcws7rIGaB5EXcjgLtW/OsXKR
dAdBB2aIf+wblti0rJ4r4DAV6l6jaXEW8eYCDuwxIsZqyxbaQPQLOHRC/QuT
/wPxz36Yflh4zYaavwIaniJQ+2HhDXMfLD5g7gcA8lu3pQ==
           
           "]],{{{-0.5,-0.8660254037844386},{0.8660254037844386,-0.5}},{0,0}}]},{color,Opacity[0.35000000000000003`],GeometricTransformationBox[PolygonBox[CompressedData["
1:eJxTTMoPSmViYGCQAmIQDQEf9s+aCQKyDqL7Pdzvvni3n/nUptKFi+QcVvtE
vKja9hoqL+9wxro+bd6CZ/u7Dphsvmam4ADR/2D/jMCgapMOBYfJbJMP1k64
sH9fhOPCqN0weQaGs2dAQMGhHyJvD5P/+x8E7tvD9EPNt4eZvxJivz3MfhGI
++xh7oO63x7mfhhfdJ37wyoREZh/7NfU6rEWcws7rIGaB5EXcjgLtW/OsXKR
dAdBB2aIf+wblti0rJ4r4DAV6l6jaXEW8eYCDuwxIsZqyxbaQPQLOHRC/QuT
/wPxz36Yflh4zYaavwIaniJQ+2HhDXMfLD5g7gcA8lu3pQ==
           
           "]],{{{-0.8660254037844386,-0.5},{0.5,-0.8660254037844386}},{0,0}}]},{color,Opacity[0.3],GeometricTransformationBox[PolygonBox[CompressedData["
1:eJxTTMoPSmViYGCQAmIQDQEf9s+aCQKyDqL7Pdzvvni3n/nUptKFi+QcVvtE
vKja9hoqL+9wxro+bd6CZ/u7Dphsvmam4ADR/2D/jMCgapMOBYfJbJMP1k64
sH9fhOPCqN0weQaGs2dAQMGhHyJvD5P/+x8E7tvD9EPNt4eZvxJivz3MfhGI
++xh7oO63x7mfhhfdJ37wyoREZh/7NfU6rEWcws7rIGaB5EXcjgLtW/OsXKR
dAdBB2aIf+wblti0rJ4r4DAV6l6jaXEW8eYCDuwxIsZqyxbaQPQLOHRC/QuT
/wPxz36Yflh4zYaavwIaniJQ+2HhDXMfLD5g7gcA8lu3pQ==
           "]],{{{-1,0},{0,-1}},{0,0}}]},{color,Opacity[0.25],GeometricTransformationBox[PolygonBox[CompressedData["
1:eJxTTMoPSmViYGCQAmIQDQEf9s+aCQKyDqL7Pdzvvni3n/nUptKFi+QcVvtE
vKja9hoqL+9wxro+bd6CZ/u7Dphsvmam4ADR/2D/jMCgapMOBYfJbJMP1k64
sH9fhOPCqN0weQaGs2dAQMGhHyJvD5P/+x8E7tvD9EPNt4eZvxJivz3MfhGI
++xh7oO63x7mfhhfdJ37wyoREZh/7NfU6rEWcws7rIGaB5EXcjgLtW/OsXKR
dAdBB2aIf+wblti0rJ4r4DAV6l6jaXEW8eYCDuwxIsZqyxbaQPQLOHRC/QuT
/wPxz36Yflh4zYaavwIaniJQ+2HhDXMfLD5g7gcA8lu3pQ==
           
           "]],{{{-0.8660254037844386,0.5},{-0.5,-0.8660254037844386}},{0,0}}]},{color,Opacity[0.19999999999999998`],GeometricTransformationBox[PolygonBox[CompressedData["
1:eJxTTMoPSmViYGCQAmIQDQEf9s+aCQKyDqL7Pdzvvni3n/nUptKFi+QcVvtE
vKja9hoqL+9wxro+bd6CZ/u7Dphsvmam4ADR/2D/jMCgapMOBYfJbJMP1k64
sH9fhOPCqN0weQaGs2dAQMGhHyJvD5P/+x8E7tvD9EPNt4eZvxJivz3MfhGI
++xh7oO63x7mfhhfdJ37wyoREZh/7NfU6rEWcws7rIGaB5EXcjgLtW/OsXKR
dAdBB2aIf+wblti0rJ4r4DAV6l6jaXEW8eYCDuwxIsZqyxbaQPQLOHRC/QuT
/wPxz36Yflh4zYaavwIaniJQ+2HhDXMfLD5g7gcA8lu3pQ==
           "]],{{{-0.5,0.8660254037844386},{-0.8660254037844386,-0.5}},{0,0}}]},{color,Opacity[0.15],GeometricTransformationBox[PolygonBox[CompressedData["
1:eJxTTMoPSmViYGCQAmIQDQEf9s+aCQKyDqL7Pdzvvni3n/nUptKFi+QcVvtE
vKja9hoqL+9wxro+bd6CZ/u7Dphsvmam4ADR/2D/jMCgapMOBYfJbJMP1k64
sH9fhOPCqN0weQaGs2dAQMGhHyJvD5P/+x8E7tvD9EPNt4eZvxJivz3MfhGI
++xh7oO63x7mfhhfdJ37wyoREZh/7NfU6rEWcws7rIGaB5EXcjgLtW/OsXKR
dAdBB2aIf+wblti0rJ4r4DAV6l6jaXEW8eYCDuwxIsZqyxbaQPQLOHRC/QuT
/wPxz36Yflh4zYaavwIaniJQ+2HhDXMfLD5g7gcA8lu3pQ==
           "]],{{{0,1},{-1,0}},{0,0}}]},{color,Opacity[0.09999999999999999],GeometricTransformationBox[PolygonBox[CompressedData["
1:eJxTTMoPSmViYGCQAmIQDQEf9s+aCQKyDqL7Pdzvvni3n/nUptKFi+QcVvtE
vKja9hoqL+9wxro+bd6CZ/u7Dphsvmam4ADR/2D/jMCgapMOBYfJbJMP1k64
sH9fhOPCqN0weQaGs2dAQMGhHyJvD5P/+x8E7tvD9EPNt4eZvxJivz3MfhGI
++xh7oO63x7mfhhfdJ37wyoREZh/7NfU6rEWcws7rIGaB5EXcjgLtW/OsXKR
dAdBB2aIf+wblti0rJ4r4DAV6l6jaXEW8eYCDuwxIsZqyxbaQPQLOHRC/QuT
/wPxz36Yflh4zYaavwIaniJQ+2HhDXMfLD5g7gcA8lu3pQ==
           "]],{{{0.5,0.8660254037844386},{-0.8660254037844386,0.5}},{0,0}}]},{color,Opacity[0.049999999999999996`],GeometricTransformationBox[PolygonBox[CompressedData["
1:eJxTTMoPSmViYGCQAmIQDQEf9s+aCQKyDqL7Pdzvvni3n/nUptKFi+QcVvtE
vKja9hoqL+9wxro+bd6CZ/u7Dphsvmam4ADR/2D/jMCgapMOBYfJbJMP1k64
sH9fhOPCqN0weQaGs2dAQMGhHyJvD5P/+x8E7tvD9EPNt4eZvxJivz3MfhGI
++xh7oO63x7mfhhfdJ37wyoREZh/7NfU6rEWcws7rIGaB5EXcjgLtW/OsXKR
dAdBB2aIf+wblti0rJ4r4DAV6l6jaXEW8eYCDuwxIsZqyxbaQPQLOHRC/QuT
/wPxz36Yflh4zYaavwIaniJQ+2HhDXMfLD5g7gcA8lu3pQ==
           "]],{{{0.8660254037844386,0.5},{-0.5,0.8660254037844386}},{0,0}}]},{GeometricTransformationBox[PolygonBox[CompressedData["
1:eJxTTMoPSmViYGCQAmIQDQEf9s+aCQKyDqL7Pdzvvni3n/nUptKFi+QcVvtE
vKja9hoqL+9wxro+bd6CZ/u7Dphsvmam4ADR/2D/jMCgapMOBYfJbJMP1k64
sH9fhOPCqN0weQaGs2dAQMGhHyJvD5P/+x8E7tvD9EPNt4eZvxJivz3MfhGI
++xh7oO63x7mfhhfdJ37wyoREZh/7NfU6rEWcws7rIGaB5EXcjgLtW/OsXKR
dAdBB2aIf+wblti0rJ4r4DAV6l6jaXEW8eYCDuwxIsZqyxbaQPQLOHRC/QuT
/wPxz36Yflh4zYaavwIaniJQ+2HhDXMfLD5g7gcA8lu3pQ==
           "]],{{{1,0},{0,1}},{0,0}}]}},Dynamic[Part[$CellContext`rotationMatrix$$,$CellContext`v$$]]],Axes->False,ImageSize->Dynamic[FEPrivate`Switch[#,Tiny,12,Small,20,Medium,25,Large,40,
_,size]],PlotRange->8.5,DefaultBaseStyle->{}]}],DynamicModuleValues:>{},
BaseStyle->{CacheGraphics->False}],"FrontFaceColor"->color, opts];


createAppearance[textLabel_, style_, opts : OptionsPattern[]] := Framed[textLabel, BaseStyle -> style, opts];
createAppearance[textLabel_, stylePrefix_String, appearanceID_String, opts : OptionsPattern[]] := createAppearance[textLabel, stylePrefix <> appearanceID, opts];
createAppearance[textLabel_, stylePrefix_String, appearanceID : "Pending", opts : OptionsPattern[]] := Overlay[{
	RawBoxes[progressSpinner[]],
	createAppearance[textLabel, stylePrefix <> appearanceID, opts]}, {2, 1}, 2, Alignment -> {Center, Center},
	FrameMargins -> 0, ImageMargins -> 0]


buttonAppearances[textLabel_, stylePrefix_, isSignInButton:True]  := buttonAppearances[textLabel, stylePrefix, {"Default", "Hover", "Pressed", "Disabled", "Pending"}];
buttonAppearances[textLabel_, stylePrefix_, isSignInButton:False] := buttonAppearances[textLabel, stylePrefix, {"Default", "Hover", "Pressed"}];
buttonAppearances[textLabel_, stylePrefix_, appearanceIDs_List]   := Table[appearanceIDs[[i]] -> createAppearance[textLabel, stylePrefix, appearanceIDs[[i]]], {i, Length[appearanceIDs]}]

buttonAppearances[textLabel_, isSignInButton:True] := buttonAppearances[textLabel, "ButtonRed1", isSignInButton];
buttonAppearances[textLabel_, isSignInButton:False] := buttonAppearances[textLabel, "ButtonGray1", isSignInButton];

buttonAppearances[isSignInButton:True] := buttonAppearances[tr["SignInButtonLabel"], isSignInButton];
buttonAppearances[isSignInButton:False] := buttonAppearances[tr["CancelButtonLabel"], isSignInButton]


btnAppearanceSelector[labelAppearances_, getButtonState:Dynamic[selector_]] := PaneSelector[labelAppearances, getButtonState]
btnAppearanceSelector[isSignInButton_?BooleanQ, getButtonState:Dynamic[selector_]] := btnAppearanceSelector[buttonAppearances[isSignInButton], getButtonState]


SetAttributes[custombutton, HoldRest]; 
custombutton[isSignInButton_?BooleanQ, function_, Dynamic[signInButtonEnabled_], btnOpts:OptionsPattern[Button]] := MouseAppearance[DynamicModule[{getButtonState, mouseState, inProgress},
	FE`Evaluate[FEPrivate`Set[mouseState, "Default"]];
	FE`Evaluate[FEPrivate`Set[inProgress, False]];
	getButtonState = If[isSignInButton,		
		Dynamic[
			If[signInButtonEnabled,
				Which[
					SameQ[inProgress, True],                                     "Pending",
					SameQ[mouseState, "Hover"],                                  "Hover",
					SameQ[mouseState, "Pressed"],                                "Pressed",
					True,                                                                  "Default"
				],
				"Disabled"
			]
		],
		Dynamic[Which[
			SameQ[mouseState, "Hover"],   "Hover",
			SameQ[mouseState, "Pressed"], "Pressed",
			True,                                   "Default"
		]]
	];
	
With[{
		custombtn = Button[btnAppearanceSelector[isSignInButton, getButtonState], function,
			btnOpts
		]
	},
	EventHandler[
		custombtn,
		{
			"MouseDown"    :> (Set[mouseState, "Pressed"]; Set[inProgress, signInButtonEnabled]),
			"MouseUp"      :> (Set[mouseState, "Hover"]),
			"MouseEntered" :> (Set[mouseState, "Hover"]),
			"MouseExited"  :> (Set[mouseState, "Default"])
		},
		PassEventsDown -> True
	]
]
	
], "LinkHand"]


(*SignInButton*)
custombutton[type:"SignIn", function_, dyn:Dynamic[signInButtonEnabled_], opts:OptionsPattern[Button]] := custombutton[True, function, dyn, Appearance -> {"Default" -> None, "ButtonType" -> "Default"}, Enabled -> dyn, opts];

(*CancelButton*)
custombutton[type:"Cancel", function_, dyn:Dynamic[signInButtonEnabled_], opts:OptionsPattern[Button]] := custombutton[False, function, dyn, Appearance -> {"Default" -> None, "ButtonType" -> "Cancel"}, opts]



signinbtns[Dynamic[logincreds_], dyn:Dynamic[signInButtonEnabled_]] := With[{
		signinBtn = ToBoxes @ custombutton["SignIn", DialogReturn[logincreds], dyn],
		cancelBtn = ToBoxes @ custombutton["Cancel", DialogReturn[MathLink`CallFrontEnd@FrontEnd`WolframCloud`ConnectionCancelled[]; logincreds = $Canceled], dyn]
	},
	RawBoxes @ StyleBox[DynamicBox[
		FEPrivate`FrontEndResource["FEExpressions",
			"ChoiceButtonsOrder"][{
				signinBtn,
				cancelBtn
		}]],
		DynamicUpdating -> True,
		Deployed -> True]]


embeddedStylesheet[] := Notebook[
	{
		Cell[StyleData[StyleDefinitions -> "SystemDialog.nb"]], 
		Cell[StyleData["WDLoginDialogButton"],
			FontFamily->"Source Sans Pro",
			FontSize->Dynamic[If[FEPrivate`$OperatingSystem === "MacOSX", 14, 15]],
			FrameBoxOptions->{FrameMargins->{{10, 10}, {5, 5}},
				ImageSize->{Automatic, 30},
				RoundingRadius->3}],			
		Cell[StyleData["ButtonGray1", StyleDefinitions -> StyleData["WDLoginDialogButton"]],
			FontColor->RGBColor[0.39215, 0.39215, 0.39215]],
		Cell[StyleData["ButtonGray1Default", StyleDefinitions -> StyleData["ButtonGray1"]],
			Background->RGBColor[0.898039, 0.898039, 0.898039],
			FrameBoxOptions->{FrameStyle->Directive[1, RGBColor[0.898039, 0.898039, 0.898039]]}],
		Cell[StyleData["ButtonGray1Hover", StyleDefinitions -> StyleData["ButtonGray1"]],
			Background->RGBColor[0.960784, 0.960784, 0.960784],
			FrameBoxOptions->{FrameStyle->Directive[1, RGBColor[0.898039, 0.898039, 0.898039]]}],
		Cell[StyleData["ButtonGray1Pressed", StyleDefinitions -> StyleData["ButtonGray1"]],
			Background->RGBColor[0.651, 0.651, 0.651],
			FrameBoxOptions->{FrameStyle->Directive[1, RGBColor[0.651, 0.651, 0.651]]}],
		Cell[StyleData["ButtonGray1Disabled", StyleDefinitions -> StyleData["ButtonGray1"]],
			FontOpacity->0.5,
			Background->RGBColor[0.898039, 0.898039, 0.898039, 0.5],
			FrameBoxOptions->{FrameStyle->Directive[1, RGBColor[0.898039, 0.898039, 0.898039, 0.5]]}],
		Cell[StyleData["ButtonGray1Pending", StyleDefinitions -> StyleData["ButtonGray1"]],
			FontOpacity->0.5,
			Background->RGBColor[0.651, 0.651, 0.651, 0.5],
			FrameBoxOptions->{FrameStyle->Directive[1, RGBColor[0.651, 0.651, 0.651, 0]]}],
		Cell[StyleData["ButtonRed1", StyleDefinitions -> StyleData["WDLoginDialogButton"]],
			FontColor->RGBColor[1, 1, 1]],
		Cell[StyleData["ButtonRed1Default", StyleDefinitions -> StyleData["ButtonRed1"]],
			Background->RGBColor[0.866666, 0.066666, 0.],
			FrameBoxOptions->{FrameStyle->Directive[1, RGBColor[0.866666, 0.066666, 0.]]}],
		Cell[StyleData["ButtonRed1Hover", StyleDefinitions -> StyleData["ButtonRed1"]],
			Background->RGBColor[0.996078431372549, 0., 0.],
			FrameBoxOptions->{FrameStyle->Directive[1, RGBColor[0.996078431372549, 0., 0.]]}],
		Cell[StyleData["ButtonRed1Pressed", StyleDefinitions -> StyleData["ButtonRed1"]],
			Background->RGBColor[0.6902, 0.0588, 0.],
			FrameBoxOptions->{FrameStyle->Directive[1, RGBColor[0.6902, 0.0588, 0.]]}],
		Cell[StyleData["ButtonRed1Disabled", StyleDefinitions -> StyleData["ButtonRed1"]],
			FontOpacity->0.5,
			Background->RGBColor[0.866666, 0.066666, 0., 0.5],
			FrameBoxOptions->{FrameStyle->Directive[1, RGBColor[0.866666, 0.066666, 0., 0.5]]}],
		Cell[StyleData["ButtonRed1Pending", StyleDefinitions -> StyleData["ButtonRed1"]],
			FontOpacity->0.5,
			Background->RGBColor[0.6902, 0.0588, 0., 0.5],
			FrameBoxOptions->{FrameStyle->Directive[1, RGBColor[0.6902, 0.0588, 0., 0.]]}]
	}, 
	Visible -> False
]


(* ::Subsection::Closed:: *)
(*Composites*)


inFldGrid[header_, inFld_, opts:OptionsPattern[]] := Grid[{
		{header},
		{inFld}
	},
	Alignment -> Left
];

inFldGrid[dyn:Dynamic[symbol_], header_, type:("password" | "username"), boxid_, opts:OptionsPattern[]] := inFldGrid[header, loginFld[dyn, boxid, type], opts]


chkbox[dyn_Dynamic] := Toggler[dyn,
		{
			True -> CloudDialogImage["CheckboxOn"],
			False -> CloudDialogImage["CheckboxOff"]
		}
]


(* ::Subsubsection::Closed:: *)
(*Messages*)


(*Certain error messages contain styled ButtonBoxes. To support both the legacy and v11+ styles, I added duplicate error messages for those messages that contain BBs. The style of the BB changed for v11. In v11+ errmssgTR[] converts the errorCode to a string and appends "v11" to any message that contains a button. The new ids are represented in FE/SystemFiles/TextResources/ErrorMessages.tr. At present, only error codes 1500 and 1600 contain BBs. So there replacement ids are "1500v11" and "1600v11", respectively.*)
SetAttributes[errmssgTR, HoldFirst];

errmssgTR[errorCode_] := PaneSelector[
	{
		True -> Dynamic[FrontEndResource["WolframCloudLoginErrors", "1600v11"], DestroyAfterEvaluation -> True],
		False -> Dynamic[FrontEndResource["WolframCloudLoginErrors", errorCode], DestroyAfterEvaluation -> True]
	},
	Dynamic[
		errorCode === 1600 && BoxForm`sufficientVersionQ[11]
	],
	Alignment -> {Center, Center},
	ImageSize -> {Full, Automatic}
]


SetAttributes[errmssg, HoldAll];
errmssg[message_, isWCUILogin_, stylesuffix_String, opts:OptionsPattern[]] := PaneSelector[
					{
						True -> errmssg[message, stylesuffix],
						False -> ""
					},
					Dynamic[isWCUILogin && errorCode != 0],
					(*ImageSize -> {Full, All},*)
					opts
				];

errmssg[message_, stylesuffix_String] := styledtxt[message, "errorIn"<>stylesuffix];


(* ::Subsubsection::Closed:: *)
(*Create Wolfram ID*)


createWolframID := Framed[
	Style[
		Row[
			{
				styledtxt[tr["CloudAccountQuery"], "wolframid", "leadin"], " ", button[mouseovertxt[tr["JoinNowButtonLabel"], "wolframidlink"], "hyperlink", "MouseClicked" :> FE`hyperlinkCoded["https://account.wolfram.com/create-account", "source=cloudlogindialog"]]
			}
		],
		LineBreakWithin -> False
	],
	ImageSize -> {Full, Automatic},
	Alignment -> {Center, Center},
	ImageMargins -> {{31, 31}, {0, 0}},
	FrameStyle -> None
]


(* ::Subsection::Closed:: *)
(*Title-Error Banner*)


titleBanner := CloudDialogImage["TopBanner"];


SetAttributes[errorPanel, HoldFirst];
errorPanel[errorMessage_] := Framed[errorMessage,
	ImageSize -> {Full, Automatic},
	Alignment -> {Center, Center},
	FrameMargins -> {{20, 20}, {16, 16}},
	FrameStyle -> None,
	Background -> $ErrorPanelBackgrounColor
];

errorPanel[errorCode_, styleSuffix_] := errorPanel[errmssg[errorCode, styleSuffix]]


SetAttributes[errorTitleColumn, HoldFirst];

errorTitleColumn[message_] := Column[
	{
		errorPanel[message],
		titleBanner
	},
	ItemSize -> {Automatic, Automatic},
	Spacings -> {{0, 0}, {0, 0}}
]

errorTitleColumn[errorCode_?(IntegerQ[#] && # =!= 0&), _] := errorTitleColumn[errmssg[errmssgTR[errorCode], "ErrorPanel"]]
errorTitleColumn[_?(# === 0&), notifications_List?(Length[#] > 0&)] := errorTitleColumn[Column[errmssg[#, "ErrorPanel"]& /@ notifications]
	]
errorTitleColumn[_, _] := titleBanner


SetAttributes[titlePlusErrorPanel, HoldAll];
titlePlusErrorPanel[errorCode_, isWCUILogin_, notifications_] := PaneSelector[
	{
		False -> titleBanner,
		True -> errorTitleColumn[errorCode, notifications]
	},
	Dynamic[(isWCUILogin && errorCode != 0) || Length[notifications] > 0],
	ImageSize -> {Full, Automatic}
]



(* ::Section::Closed:: *)
(*Dialog*)


(* ::Subsection::Closed:: *)
(*FrontEnd Version >= 11*)


loginDialog[username_String] := loginDialog[username, {}] /; BoxForm`sufficientVersionQ[11]
loginDialog[username_String, notificationIds_List] := With[{boxid = "username", pwdboxid =" passwd", cloudLoginTR = $CloudLoginTextResource},
	Block[
		{
			$loginCredentials
		},
		Module[
			{
				ctrlCol
			},

			Clear[$loginCredentials];

			errorCode = CurrentValue["WolframCloudLoginError"];
			isWCUILogin = TrueQ[CurrentValue["WolframCloudUILogin"]];
			notifications = Dynamic[FrontEndResource[$CloudLoginTextResource, #], DestroyAfterEvaluation -> True]& /@ notificationIds;
			

			If[Developer`UseFrontEnd[CurrentValue["UserInteractionEnabled"]],
				SetAttributes[ctrlCol, HoldAll];
				ctrlCol[uname_, pwd_] := Column[
					{
						styledtxt[tr["CloudSubheaderText"], "subheader"],

						spacr[26](*errmssg[errorCode, isWCUILogin, "ControlArea"]*),

						inFldGrid[Dynamic[uname],
							styledtxt[
								tr["WolframIDLabel"]
							],
							"username",
							boxid
						],

						spacr[28](*errmssg[errorCode, isWCUILogin, "ControlArea"]*),

						inFldGrid[Dynamic[pwd],
							styledtxt[tr["PasswordLabel"]],
							"password",
							pwdboxid
						],

						spacr[28](*errmssg[errorCode, isWCUILogin, "ControlArea"]*),

						DynamicWrapper[
							Grid[
								{
									{
										Grid[
											{
												{
													chkbox[Dynamic[CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"}, True]]],
													button[styledtxt[tr["RemeberMeLabel"], "rememberme"],
														"MouseClicked" :> (
															FEPrivate`Set[
																FrontEnd`CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"}],
																FEPrivate`Not[FrontEnd`CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"}, True]]
															]
														)
													]

												}
											},
											Alignment -> {Automatic, Center}
										]
									},
									{
										Grid[
											{
												{
													
													signinbtns[Dynamic[$loginCredentials],
														Dynamic[Not[uname === "" || pwd === ""]]
													]
													,
													Pane[
														button[mouseovertxt[tr["ForgotPasswordLabel"]], "hyperlink",
															"MouseClicked" :> (FrontEndExecute[FrontEnd`NotebookLocate[{URL["https://account.wolfram.com/auth/forgot-password"], None}]])
														], 
														Full, 
														FrameMargins -> {{0, 1}, {0, 0}}, 
														Alignment -> {Right, Baseline}]
												}
											},
											ItemSize -> {{Scaled[0.58], Scaled[0.42]}, Automatic},
											Spacings -> {0, 0},
											Alignment -> {{Left, Right}, Automatic}
										]
									}
								},
								Alignment -> {Left, Automatic},
								Spacings -> {Automatic, 1}
							],
							$loginCredentials = {uname, pwd}
						]
					},
					Alignment -> Left,
					Spacings -> {0, 0},
					ItemSize -> {Automatic, Automatic}
				];

				DialogInput[
					ExpressionCell[
						DynamicModule[{uname = username, pwd = ""},
							Framed[
								Column[
									{
										CloudObject`Private`titlePlusErrorPanel[CloudObject`Private`errorCode, CloudObject`Private`isWCUILogin, CloudObject`Private`notifications](*titleBanner*),
										spacr[34],
										Pane[ctrlCol[uname, pwd], {Full, All},
											FrameMargins -> {{$LeftDialogMargin, $RightDialogMargin}, {0, 0}}
										],
										spacr[26],
										dividerLine,
										spacr[13],
										createWolframID,
										spacr[26]
									},
									Spacings -> {0, 0}
								],
								ImageSize -> {Full, Full},
								FrameMargins -> 0,
								ImageMargins -> {{0, 0}, {-3, -1}},
								FrameStyle -> None
							]
						],
						CellMargins -> {{-1, -5}, {0, -2}},
						CellFrameMargins -> 0
					],
					StyleDefinitions -> embeddedStylesheet[],
					Background -> $DialogBackgroundColor,
					CellContext -> Notebook,
					DynamicUpdating -> True,
					DynamicEvaluationTimeout -> 100.,
					WindowTitle :> Dynamic[FEPrivate`FrontEndResource[cloudLoginTR, "WindowTitle"]],
					WindowSize -> {$DialogWidth, All}, 
					Modal -> True,
					NotebookDynamicExpression :> (
						Refresh[FrontEnd`MoveCursorToInputField[EvaluationNotebook[], If[username === "", boxid, pwdboxid]], None]
					)
				];
				$loginCredentials,
				Return[$Canceled](*Else, no interactive FE*)
			]
		]
	]
] /; BoxForm`sufficientVersionQ[11];

$loginDialog351950p2 = True;



(* ::Subsection::Closed:: *)
(*Legacy FrontEnd*)


loginDialog[username_String, _] := loginDialog[username] 
loginDialog[username_String] := With[{btnlblStyle = {"DialogStyle", "ControlStyle", FontSize -> (Inherited*0.95)}, boxid = "username", pwdboxid="passwd", cloudLoginTR = $CloudLoginTextResource},
  Block[{$loginCredentials}, 
   Module[{leftCol, rightCol, columns, errorCode = CurrentValue["WolframCloudLoginError"], isWCUILogin = TrueQ[CurrentValue["WolframCloudUILogin"]]
}, 
   	Clear[$loginCredentials];
    If[Developer`UseFrontEnd[CurrentValue["UserInteractionEnabled"]],
    
     SetAttributes[leftCol, HoldAll];
     leftCol[uname_, pwd_] := Column[{
        Column[{
          ExpressionCell[Row[{
             tr["WolframIDLabel1"](*"Wolfram ID "*)," ",
             Style[ tr["WolframIDLabel2"](*"(your email address)"*),FontSize -> (Inherited*0.85)]}], "DialogStyle","ControlStyle", FormatType -> TextForm],
             InputField[Dynamic[uname], String, ContinuousAction -> True,  System`BoxID -> boxid]
          }],
        Column[{
          TextCell[tr["PasswordLabel"](*"Password"*), "DialogStyle", "ControlStyle"],
          
          InputField[Dynamic[pwd], String, ContinuousAction -> True, 
           FieldMasked -> True, System`BoxID -> pwdboxid]
          }
         ],
		 DynamicWrapper[Grid[{
             {
			 EventHandler[
				PaneSelector[
					{
						False -> Style[tr["ForgotPasswordLabel"](*"Forgot your password?"*), btnlblStyle, Gray],
						True -> Style[tr["ForgotPasswordLabel"](*"Forgot your password?"*), btnlblStyle, RGBColor[0.878431,0.513725,0.133333]]
					},
					FrontEnd`CurrentValue["MouseOver"]
				],
				"MouseClicked" :> (FrontEndExecute[FrontEnd`NotebookLocate[{URL["https://account.wolfram.com/auth/forgot-password"], None}]])			
			 ]           
             },
             {
             Grid[{{
				Toggler[Dynamic[FrontEnd`CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"},True]],
				{
					True -> CloudDialogImage["CheckboxOn"],
					False -> CloudDialogImage["CheckboxOff"]
				}],
				EventHandler[Style[tr["RemeberMeLabel"], "DialogStyle", "ControlStyle"],
					"MouseClicked" :> (
						FEPrivate`Set[
							FrontEnd`CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"}],
							FEPrivate`Not[FrontEnd`CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"},True]]
						]
					)]
				}},
			 Alignment -> {Automatic, Center}]
             },
			{PaneSelector[
					{
						True -> Pane[
									Style[Dynamic[FrontEndResource["WolframCloudLoginErrors", errorCode], DestroyAfterEvaluation -> True], 
									LineIndent -> 0, RGBColor[0.9, 0.55, 0.32], LineSpacing -> {1, 0}], {Full, Full}, 
									Alignment -> {Left, Center},
									Scrollbars -> {False, Automatic},
									AppearanceElements -> {}             
								],
						False -> ""
					},
					Dynamic[isWCUILogin && errorCode != 0],
					ImageSize -> {Full(*220*), 50},
					ImageMargins -> {{0, 10}, {5, 0}}
				]
			}, (*Login error message display*)
			{
			 Grid[{{
             Button[
              
              Pane[Style[tr["SignInButtonLabel" ](*"Sign In"*), btnlblStyle, White], 
               ImageMargins -> {{10, 10}, {0, 0}}],
              DialogReturn[$loginCredentials],
              ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]],
              Appearance -> {
              	"Default" -> CloudDialogImage["SigninButton","Default"], 
              	"Hover" -> CloudDialogImage["SigninButton","Hover"], 
                "Pressed" -> CloudDialogImage["SigninButton","Pressed"],
				"ButtonType" -> "Default"}],

             Button[
              
              Pane[Style[tr["CancelButtonLabel" ](*"Sign In"*), btnlblStyle, RGBColor[0.266667,0.266667,0.266667]], 
               ImageMargins -> {{10, 10}, {0, 0}}],
              DialogReturn[MathLink`CallFrontEnd@FrontEnd`WolframCloud`ConnectionCancelled[]; $loginCredentials = $Canceled],
              ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]],
              Appearance -> {
              	"Default" -> CloudDialogImage["CancelButton","Default"], 
              	"Hover" -> CloudDialogImage["CancelButton","Hover"], 
                "Pressed" -> CloudDialogImage["CancelButton","Pressed"],
				"ButtonType" -> "Cancel"}]
			 }}]

			}},
		  Alignment -> {Left, Automatic},
		  Spacings -> {Automatic, 1}],
          $loginCredentials = {uname, pwd}]
        },
       Alignment -> Left,
       Spacings -> .5];
       
     SetAttributes[rightCol, HoldAll];
     rightCol[uname_, pwd_] := Pane[Column[{CloudDialogImage["CloudLogoIcon"],
        Style[tr["CloudAccountQuery"](*"Don't have a Wolfram Cloud account yet?"*), 
         "DialogStyle", "ControlStyle", FontSize -> (Inherited*0.9), LineSpacing -> {1, 0}],
		Button[
          
          Pane[Style[tr["JoinNowButtonLabel"](*"Join Now"*), btnlblStyle, White], 
           ImageMargins -> {{10, 10}, {0, 0}}],
          FE`hyperlinkCoded["https://account.wolfram.com/create-account", "source=cloudlogindialog"],
          ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]],
          Appearance -> {
          	"Default" -> CloudDialogImage["JoinNowButton","Default"], 
          	"Hover" -> CloudDialogImage["JoinNowButton","Hover"],
          	"Pressed" -> CloudDialogImage["JoinNowButton","Pressed"]
          	}]
        },
       Alignment -> Center,
       ItemSize -> Scaled[.6],
       Spacings -> 2.5],
	 FrameMargins -> {{0, 0}, {0, 5}}];
     
     SetAttributes[columns, HoldAll];
     columns[uname_, pwd_] := Grid[{
        {"", Pane[
          leftCol[uname, pwd],
          Full,
          Alignment -> Left
          ], Pane[
          rightCol[uname, pwd],
          Full,
          Alignment -> Center
          ]}
       } ,
       Dividers -> {{None, {3 -> Directive[RGBColor[0.74, 0.74, 0.74]]}}, None},
       Spacings -> {0, 1},
       ItemSize -> {{Automatic, 
          1 -> FEPrivate`If[
            FEPrivate`SameQ[FEPrivate`$OperatingSystem, "MacOSX"], 4, 
            3]}, Automatic},
       Alignment -> {{Left, {-1 -> Center}}, Top}];
     
     DialogInput[
      ExpressionCell[DynamicModule[{uname = username, pwd = ""},
       Framed[
        Column[{Panel["", Appearance -> {"Default" -> CloudDialogImage["TopBanner"]}, ImageSize -> {Full, Automatic},
           FrameMargins -> {{10, 10}, {0, 0}}, 
           Alignment -> {Left, Center}],
          
          Panel[
           Pane[
            columns[uname, pwd],
            {Full,All},
            ImageMargins -> {{0, 0}, {10, 15}}
            ],
           Appearance -> {"Default" -> CloudDialogImage["BackgroundImage"]}
           ]
          },
         Spacings -> {0, 0}
         ],
        ImageSize -> {Full, Full},
        FrameMargins -> 0,
        ImageMargins -> {{0, 0}, {-3, -1}},
        FrameStyle -> None
        ]],
       CellMargins -> {{-1, -5}, {0, -2}},
       CellFrameMargins -> 0
       ],
      CellContext -> Notebook,
      WindowTitle :> Dynamic[FEPrivate`FrontEndResource[cloudLoginTR,"WindowTitle"]](*"Enter Login Credentials"*),
      WindowSize -> {520, FitAll}, 
      Modal -> True,
      NotebookDynamicExpression :> (
         Refresh[FrontEnd`MoveCursorToInputField[EvaluationNotebook[], If[username === "", boxid, pwdboxid]], None]
 )
      ];
     $loginCredentials,
     (*Else,no interactive FE*)Return[$Canceled];]]
     ]
  ]


(* ::Section::Closed:: *)
(*Footer*)


End[];

EndPackage[];
