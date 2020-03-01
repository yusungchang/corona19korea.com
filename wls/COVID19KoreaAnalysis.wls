#!/usr/bin/env wolframscript -print -format JSON
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*South Korea*)


(* ::Subtitle:: *)
(*Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Initialization*)


apiKCDCURLBase="https://www.cdc.go.kr";
apiKCDCURLBoard="/board/board.es";
apiKCDCURL=apiKCDCURLBase<>apiKCDCURLBoard;
apiKCDCParamsCommon={"mid"->"a20501000000","bid"->"0015"};
apiKCDCParamKeyWord={"keyWord"->"\:cf54\:b85c\:b098\:bc14\:c774\:b7ec\:c2a4\:ac10\:c5fc\:c99d-19 \:ad6d\:b0b4 \:bc1c\:c0dd \:d604\:d669"};
apiKCDCParamPage[n_]:={"nPage"->n};
apiKCDCParamsPost[n_]:={"act"->"view","list_no"->n,"nPage"->1};


cacheDirectory=Directory[]<>"/../data/cache/";
outputDirectory=Directory[]<>"/../src/output/";


(* ::Section:: *)
(*Post to Cache*)


(*raw=URLExecute[apiKCDCURL,Join[apiKCDCParamsCommon,apiKCDCParamKeyWord,apiKCDCParamPage[#]],"Source"]&/@Range[5];*)


(*postList=Flatten[Flatten[StringCases[Quiet@Flatten[StringCases[ImportString[#,"Hyperlinks"],apiKCDCURLBoard<>__]],"list_no="~~(x:DigitCharacter..)~~"&":>x]]&/@raw];*)


(*crawlPosts[x_]:=Module[{html},
If[!FileExistsQ[cacheDirectory<>#<>".html"],
html=URLExecute[apiKCDCURL,Join[apiKCDCParamsCommon,apiKCDCParamsPost[#]],"Source"];
Export[cacheDirectory<>#<>".html",html,"Text",CharacterEncoding-> "UTF8"]]&/@x]*)


(*crawlPosts[postList]*)


(* ::Section:: *)
(*Parsing Caches*)


(* ::Subsection:: *)
(*Transform*)


(* ::Subsubsection:: *)
(*HTML to Data Function*)


dataRow[name_]:=
Module[{data,pos,rows},
data=Import[name,"Data"];
pos=Position[data,
{" \:ad6c\:bd84"," \:cd1d\:acc4"," \:d655\:c9c4\:d658\:c790"," \:ac80\:c0ac\:d604\:d669"}|{" \:ad6c\:bd84"," \:cd1d\:acc4"," \:d655\:c9c4\:d658\:c790\:d604\:d669"," \:ac80\:c0ac\:d604\:d669"}];
rows=Extract[data,Most[First[pos]]][[3;;-1]];
If[Length[rows]==6,(* \:c0ac\:b9dd\:c790 \:c804 \:d3ec\:ba67 2 rows *)
{
Join[rows[[1,1;;3]],rows[[2]],{"0"},rows[[1,4;;-1]]],
Join[rows[[3,1;;3]],rows[[4]],{"0"},rows[[3,4;;-1]]]
},
If[Length[rows[[1]]]==8,
{
Join[rows[[1,1;;5]],{"0"},rows[[1,6;;-1]]],
Join[rows[[2,1;;5]],{"0"},rows[[2,6;;-1]]]
},
Most[rows] (* \:c0ac\:b9dd\:c790 \:d6c4 \:d3ec\:ba67 *)
]
]
];


(* ::Subsubsection:: *)
(*Convert Cached HTMLs*)


dataPre1=Flatten[dataRow/@FileNames[cacheDirectory<>"*.html"],1];


(* ::Subsubsection:: *)
(*Convert Values to Expressions*)


dataPre2=Map[
Function[{x},
Prepend[
ToExpression[StringReplace[#,("*"|","|" ")->""]]&/@Rest[x],
Quiet@DateObject[StringReplace[First[x],{".( "~~_~~" ) "->" "," \:c2dc \:ae30\:c900"->":00"}]]
]],
dataPre1
];


(* ::Subsubsection:: *)
(*Remove Duplicated Entries*)


data=Sort[DeleteDuplicates[dataPre2,First[#1]==First[#2]&]];


(* ::Subsubsection:: *)
(*Define Extract Functions*)


colMap={"\:c2dc\:ac04"->1,"\:cd1d\:acc4"->2,"\:d655\:c9c4"->3,"\:aca9\:b9ac\:d574\:c81c"->4,"\:aca9\:b9ac\:c911"->5,"\:c0ac\:b9dd"->6,"\:ac80\:c0ac\:cd1d\:acc4"->7,"\:ac80\:c0ac\:c911"->8,"\:acb0\:acfc\:c74c\:c131"->9};
colMapR=Reverse[colMap,1];
getData[col_,range_:All,table_:data]:=table[[range,col/.colMap]];


(* ::Subsection:: *)
(*Style Definitions*)


(* ::Subsubsection:: *)
(*Color Test*)


colors=With[{hsb=ColorConvert[#,"HSB"]},ReplacePart[ColorConvert[#,"HSB"],{2->hsb[[2]]*.8,3->hsb[[3]]*1.1}]]&/@{RGBColor[{51,102,204}/255.],RGBColor[{220,57,18}/255.],RGBColor[{255,153,0}/255.],RGBColor[{16,150,24}/255.]};


ColorData[24,"ColorList"];


ColorData[4,"ColorList"];


(* ::Subsubsection:: *)
(*Color*)


RGBColor/@(ImageValue[#,{1,1}]&/@{Image[CompressedData["
1:eJxTTMoPSmNiYGAo5gASQYnljkVFiZXBAkBOaF5xZnpeaopnXklqemqRRRIz
UJAXilmA+CoT4/9RPIoHMwYAvAJDeA==
"], "Byte", ColorSpace -> "RGB", Interleaving -> True],Image[CompressedData["
1:eJxTTMoPSmNiYGAo5gASQYnljkVFiZXBAkBOaF5xZnpeaopnXklqemqRRRIz
UJAXilmA+Nt+9f+jeBQPZgwAED3vHA==
"], "Byte", ColorSpace -> "RGB", Interleaving -> True],Image[CompressedData["
1:eJxTTMoPSmNiYGAo5gASQYnljkVFiZXBAkBOaF5xZnpeaopnXklqemqRRRIz
UJAXilmA2Ln16/9RPIoHMwYA9szapQ==
"], "Byte", ColorSpace -> "RGB", Interleaving -> True],Image[CompressedData["
1:eJxTTMoPSmNiYGAo5gASQYnljkVFiZXBAkBOaF5xZnpeaopnXklqemqRRRIz
UJAXilmA2Ghb5f9RPIoHMwYAy8ud6Q==
"], "Byte", ColorSpace -> "RGB", Interleaving -> True],Image[CompressedData["
1:eJxTTMoPSmNiYGAo5gASQYnljkVFiZXBAkBOaF5xZnpeaopnXklqemqRRRIz
UJAXilmA+EMO0/9RPIoHMwYA+Emb7g==
"], "Byte", ColorSpace -> "RGB", Interleaving -> True]}); (* Source: Google Calendar colors *)


getColor["\:d655\:c9c4"]=RGBColor[{0.9411764705882353, 0.4235294117647059, 0.00784313725490196, 1.}];
getColor["\:aca9\:b9ac\:c911"]=RGBColor[{0.9647058823529412, 0.7490196078431373, 0.15294117647058825`, 1.}];
getColor["\:aca9\:b9ac\:d574\:c81c"]=RGBColor[{0.2627450980392157, 0.5215686274509804, 0.9607843137254902, 1.}];
getColor["\:c0ac\:b9dd"]=RGBColor[{0.8352941176470589, 0.00784313725490196, 0.00392156862745098, 1.}];
getColor["\:ac80\:c0ac\:cd1d\:acc4"]=RGBColor[{0.9411764705882353, 0.4235294117647059, 0.00784313725490196, 1.}];
getColor["\:ac80\:c0ac\:c911"]=RGBColor[{0.9647058823529412, 0.7490196078431373, 0.15294117647058825`, 1.}];
getColor["\:acb0\:acfc\:c74c\:c131"]=RGBColor[{0.19607843137254902`, 0.7137254901960784, 0.4745098039215686, 1.}];


(* ::Subsubsection:: *)
(*Plot Styles*)


imageWidth=860;


legendFrame=(Framed[Style[#,14],Background->GrayLevel[1,.9],FrameStyle->LightGray,RoundingRadius->3]&);
placedLegendFrame=(Placed[#,{Left,Top},legendFrame]&);


plotLabelFun[title_]:=Column[{
Style[title,32,GrayLevel[.4]],
Style[
StringReplace[DateString[{"MonthShort","/","DayShort"," (","DayNameShort",") ","Hour24Short","\:c2dc \:d604\:c7ac"}],{"Mon"->"\:c6d4","Tue"->"\:d654","Wed"->"\:c218","Thu"->"\:baa9","Fri"->"\:ae08","Sat"->"\:d1a0","Sun"->"\:c77c"}],
20,GrayLevel[.6]]
},Alignment->Center,Spacings->{0,2},BaseStyle->"Label"];
imageDimensions={ImageSize->{imageWidth,Automatic},ImagePadding->{{Automatic,Automatic},{Automatic,20}},AspectRatio->1/2};
frameTickStyle={FontSize->12};
barLabelStyle={FontSize->12};
barLabelStyleSmall={FontSize->10};


(* ::Section:: *)
(*Visualization*)


(* ::Subsection:: *)
(*\:c804\:ad6d \:cf54\:b85c\:b09819 \:c0c1\:d669*)


(* ::Input::Initialization:: *)
Pane[
Labeled[
Column[{
Spacer[20],
Row[Framed[Column[{#,Style[NumberForm[getData[#,-1],DigitBlock->3],40,Bold,Italic]},Alignment->Center],Background->getColor[#],Alignment->Center,ImageSize->{200,200},FrameStyle->None,RoundingRadius->10]&/@{"\:d655\:c9c4","\:aca9\:b9ac\:c911","\:aca9\:b9ac\:d574\:c81c","\:c0ac\:b9dd"},Spacer[10]],
Spacer[10],
Row[Framed[Column[{#,Style[NumberForm[getData[#,-1],DigitBlock->3],40,Bold,Italic]},Alignment->Center],Background->getColor[#],Alignment->Center,ImageSize->{273,140},FrameStyle->None,RoundingRadius->10]&/@{"\:ac80\:c0ac\:cd1d\:acc4","\:ac80\:c0ac\:c911","\:acb0\:acfc\:c74c\:c131"},Spacer[10]]
},BaseStyle->{FontFamily->"Overpass",FontSize->24,White},Spacings->0]
,plotLabelFun["\:c804\:ad6d \:cf54\:b85c\:b09819 \:c0c1\:d669"],Top],
ImageSize->{imageWidth,Automatic},FrameMargins->0]
