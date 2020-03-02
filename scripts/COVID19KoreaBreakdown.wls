#!/usr/bin/env wolframscript -print -format SVG
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*South Korea: Breakdown*)


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


legendFrame=(Framed[Style[#,14],Background->GrayLevel[1,.9],FrameStyle->LightGray,RoundingRadius->3]&);
placedLegendFrame=(Placed[#,{Left,Top},legendFrame]&);


plotLabelFun[title_,time_:CurrentTime[]]:=Column[{
Style[title,32,GrayLevel[.4]],
Style[
StringReplace[DateString[time,{"MonthShort","/","DayShort"," (","DayNameShort",") ","Hour24Short","\:c2dc \:c5c5\:b370\:c774\:d2b8"}],{"Mon"->"\:c6d4","Tue"->"\:d654","Wed"->"\:c218","Thu"->"\:baa9","Fri"->"\:ae08","Sat"->"\:d1a0","Sun"->"\:c77c"}],
20,GrayLevel[.6]],
Spacer[{0,16}]
},Alignment->Center,Spacings->{0,2},BaseStyle->"Label"];


imageDimensions={ImageSize->{imageWidth,Automatic},AspectRatio->1/2};


frameTickStyle=Directive[FontSize->14];


barLabelStyle={FontSize->12};
barLabelStyleSmall={FontSize->10};


(* ::Section:: *)
(*Visualization*)


(* ::Subsection:: *)
(*\:c804\:ad6d \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:d604\:d669*)


(* ::Input::Initialization:: *)
Labeled[
Module[{
range9=Flatten[Position[#["Hour"]&/@getData["\:c2dc\:ac04"],x_/;x<12]],(* \:c624\:c804 \:c77c\:c77c \:d604\:d669 \:b9cc \:cd94\:cd9c *)
legends={"\:c0ac\:b9dd","\:aca9\:b9ac\:c911","\:aca9\:b9ac\:d574\:c81c"},
dateTicks,
legendInset,
labelFun
},

dateTicks=MapIndexed[{First@#2,DateString[#1,{"MonthShort","/","DayShort"}]}&,getData["\:c2dc\:ac04",range9]];
legendInset=legendFrame@SwatchLegend[getColor/@legends,legends,LegendMarkers->Graphics[{EdgeForm[Opacity[.1]],Rectangle[]}],LegendMarkerSize->14];
labelFun=(With[{i=#2[[2]]},
If[i==2 (* \:c911\:ac04\:bc14 *),
Placed[Style[If[#1<200,"",#1],White,barLabelStyleSmall,Bold],Top],
Placed[Style[If[#1<4,"",#1],(getColor/@legends)[[i]],barLabelStyle,Bold],Above]
]
]
&);

SetOptions[BarChart,imageDimensions];

BarChart[Transpose[getData[#,range9]&/@legends],
ChartLayout->"Stacked",
ChartStyle->getColor/@legends,
LabelingFunction->labelFun,
Frame->True,
FrameTicks->{{Automatic,None},{dateTicks,None}},
FrameTicksStyle->frameTickStyle,
GridLines->Automatic,
Epilog->Inset[legendInset,Scaled[{0.025,0.975}],{Left,Top}]
]
]
,plotLabelFun["\:c804\:ad6d \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:d604\:d669",Last@getData["\:c2dc\:ac04"]],Top]
