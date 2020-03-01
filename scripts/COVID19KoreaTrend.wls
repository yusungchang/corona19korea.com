#!/usr/bin/env wolframscript -print -format SVG
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


outputDirectory=Directory[]<>"/../src/output/";


(* ::Section:: *)
(*Parsing Caches*)


(* ::Subsection:: *)
(*Transform*)


(* ::Subsubsection:: *)
(*Remove Duplicated Entries*)


data=Cases[Import[outputDirectory<>"korea-corona19-data.csv"],Except[{}]];


data=Prepend[Rest[#],DateObject[First[#]]]&/@data;


(* ::Subsubsection:: *)
(*Define Extract Functions*)


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
(*\:c804\:ad6d \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:d604\:d669*)


(* ::Input::Initialization:: *)
Labeled[
Module[{
range=Span[10,-1],
legends={"\:d655\:c9c4","\:c0ac\:b9dd"},
labelInset,
todayInset,
currentInset
},

labelInset=Table[
With[{list=getData[{"\:c2dc\:ac04",d},20;;-1]},
(With[{x=AbsoluteTime@#[[1]],y=#[[2]]},
Inset[Style[If[y<4,"",y],barLabelStyle,Bold,getColor[d]],Offset[{-10,12},{x,y}]]]&)/@list[[-1;;1;;-2]]
],{d,legends}];

todayInset={
With[{t=AbsoluteTime@getData["\:c2dc\:ac04",-1]},{getColor["\:d655\:c9c4"],Dashed,Line[{{t,0},{t,getData["\:d655\:c9c4",-1]}}]}]
};

currentInset=Text[Framed[Style[DateString[getData["\:c2dc\:ac04",-1],{"MonthShort","/","DayShort"," ","Hour24Short","\:c2dc"}],White,Bold,14],FrameStyle->None,Background->getColor["\:d655\:c9c4"],FrameMargins->{{8,8},{4,4}},RoundingRadius->4],Scaled[{0.98,0.5}],Scaled[{1,0.5}]];

DateListPlot[
getData[{"\:c2dc\:ac04",#},range]&/@legends,
InterpolationOrder->2,
PlotStyle->getColor/@legends,
PlotMarkers->"OpenMarkers",
PlotRange->{Automatic,All},
PlotRangePadding->{Scaled[0.01],{Automatic,Scaled[0.08]}},
Filling->Bottom,
DateTicksFormat->{"MonthShort","/","DayShort"},
FrameTicks->{{Automatic,None},{Automatic,None}},
FrameTicksStyle->frameTickStyle,
GridLines->Automatic,
PlotLegends->placedLegendFrame[{"\:b204\:c801\:d655\:c9c4\:c790","\:b204\:c801\:c0ac\:b9dd\:c790"}],
Prolog->todayInset,
Epilog->{labelInset,currentInset},
imageDimensions
]
]
,plotLabelFun["\:c804\:ad6d \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:cd94\:c774"],Top]
