#!/usr/bin/env wolframscript -print -format SVG
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*South Korea: Status*)


(* ::Subtitle:: *)
(*\:c7a5\:c720\:c131 | Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Config*)


$outputDirectory=Directory[]<>"/../src/output/";


$dataFile=$outputDirectory<>"korea-corona19-data.csv";


(* ::Section:: *)
(*Initialization*)


(* ::Subsection:: *)
(*Data Handling*)


(* ::Subsubsection:: *)
(*Read Data & Convert Dates to DateObject*)


data=Cases[Import[$dataFile],Except[{}]];


data=Prepend[Rest[#],DateObject[First[#]]]&/@data;


(* ::Subsubsection:: *)
(*Extract Functions*)


colMap={"\:c2dc\:ac04"->1,"\:cd1d\:acc4"->2,"\:d655\:c9c4"->3,"\:aca9\:b9ac\:d574\:c81c"->4,"\:aca9\:b9ac\:c911"->5,"\:c0ac\:b9dd"->6,"\:ac80\:c0ac\:cd1d\:acc4"->7,"\:ac80\:c0ac\:c911"->8,"\:acb0\:acfc\:c74c\:c131"->9};
colMapR=Reverse[colMap,1];
getData[col_,range_:All,table_:data]:=table[[range,col/.colMap]];


(* ::Subsection:: *)
(*Style Definitions*)


(* ::Subsubsection:: *)
(*Color*)


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


plotLabelFun[title_,time_:CurrentTime[]]:=Column[{
Style[title,32,GrayLevel[.4]],
Style[
StringReplace[DateString[time,{"MonthShort","/","DayShort"," (","DayNameShort",") ","Hour24Short","\:c2dc  \:c5c5\:b370\:c774\:d2b8"}],{"Mon"->"\:c6d4","Tue"->"\:d654","Wed"->"\:c218","Thu"->"\:baa9","Fri"->"\:ae08","Sat"->"\:d1a0","Sun"->"\:c77c"}],
20,GrayLevel[.6]],
Spacer[{0,16}]
},Alignment->Center,Spacings->{0,2},BaseStyle->"Label"];


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
,plotLabelFun["\:c804\:ad6d \:cf54\:b85c\:b09819 \:c0c1\:d669",Last@getData["\:c2dc\:ac04"]],Top],
ImageSize->{imageWidth,Automatic},FrameMargins->0]
