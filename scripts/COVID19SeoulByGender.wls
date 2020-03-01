#!/usr/bin/env wolframscript -print -format SVG
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*Seoul District*)


(* ::Subtitle:: *)
(*Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Initialization*)


outputDirectory=Directory[]<>"/../src/output/";


(* ::Section:: *)
(*Load Data*)


table=Cases[Import[outputDirectory<>"seoul-corona19-data.csv"],Except[{}]];


table=Prepend[Rest[#],DateObject[First[#]]]&/@table;


(* ::Section:: *)
(*Data Functions*)


seoulDistrictNames={"\:ac15\:b0a8\:ad6c","\:ac15\:b3d9\:ad6c","\:ac15\:bd81\:ad6c","\:ac15\:c11c\:ad6c","\:ad00\:c545\:ad6c","\:ad11\:c9c4\:ad6c","\:ad6c\:b85c\:ad6c","\:ae08\:cc9c\:ad6c","\:b178\:c6d0\:ad6c","\:b3c4\:bd09\:ad6c","\:b3d9\:b300\:bb38\:ad6c","\:b3d9\:c791\:ad6c","\:b9c8\:d3ec\:ad6c","\:c11c\:b300\:bb38\:ad6c","\:c11c\:cd08\:ad6c","\:c131\:b3d9\:ad6c","\:c131\:bd81\:ad6c","\:c1a1\:d30c\:ad6c","\:c591\:cc9c\:ad6c","\:c601\:b4f1\:d3ec\:ad6c","\:c6a9\:c0b0\:ad6c","\:c740\:d3c9\:ad6c","\:c885\:b85c\:ad6c","\:c911\:ad6c","\:c911\:b791\:ad6c"};


(* ::Subsubsection:: *)
(*Define Extract Functions*)


seoulDataColMap={"\:d655\:c9c4\:c77c"->1,"\:b098\:c774"->2,"\:c131\:bcc4"->3,"\:ac70\:c8fc\:c9c0"->4};
seoulDataColMapR=Reverse[seoulDataColMap,1];
seoulGetData[col_,range_:All,t_:table]:=t[[range,col/.seoulDataColMap]];


(* ::Section:: *)
(*Visualization Functions*)


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


(* ::Section:: *)
(*Visualization*)


(* ::Input::Initialization:: *)
Labeled[
Pane[
PieChart[Association[Rule@@#&/@Tally[seoulGetData["\:c131\:bcc4"]]],
ImageSize->{Automatic,400},
LabelingFunction->(Placed[Style[#,20,Bold,White],"RadialCenter"]&),
ChartStyle->{RGBColor[{0.9647058823529412, 0.7490196078431373, 0.15294117647058825`, 1.}],RGBColor[{0.2627450980392157, 0.5215686274509804, 0.9607843137254902, 1.}]},
ChartLegends->Placed[{"\:c5ec","\:b0a8"},Right,Style[#,16,GrayLevel[.3]]&]],
imageDimensions,Alignment->Center
],
plotLabelFun["\:c11c\:c6b8\:d2b9\:bcc4\:c2dc \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:c131\:bcc4"],Top]
