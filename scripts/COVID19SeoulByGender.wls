#!/usr/bin/env wolframscript -print -format SVG
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*\:c11c\:c6b8: \:d655\:c9c4\:c790 \:c131\:bcc4*)


(* ::Subtitle:: *)
(*\:c7a5\:c720\:c131 | Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Initialization*)


$outputDirectory=Directory[]<>"/../src/output/";


$dataFile=$outputDirectory<>"seoul-corona19-data.csv";


(* ::Section:: *)
(*Load Data*)


table=Cases[Import[$dataFile],Except[{}]];


table=Prepend[Rest[#],DateObject[First[#]]]&/@table;


(* ::Subsubsection:: *)
(*Extract Functions*)


dataColMap={"\:d655\:c9c4\:c77c"->1,"\:b098\:c774"->2,"\:c131\:bcc4"->3,"\:ac70\:c8fc\:c9c0"->4};
dataColMapR=Reverse[dataColMap,1];
getData[col_,range_:All,t_:table]:=t[[range,col/.dataColMap]];


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
StringReplace[DateString[{"MonthShort","/","DayShort"," (","DayNameShort",") ","Hour24Short","\:c2dc \:c5c5\:b370\:c774\:d2b8"}],{"Mon"->"\:c6d4","Tue"->"\:d654","Wed"->"\:c218","Thu"->"\:baa9","Fri"->"\:ae08","Sat"->"\:d1a0","Sun"->"\:c77c"}],
20,GrayLevel[.6]],
Spacer[{0,16}]
},Alignment->Center,Spacings->{0,2},BaseStyle->"Label"];


imageDimensions={ImageSize->{imageWidth,Automatic},AspectRatio->1/2};


(* ::Section:: *)
(*Visualization*)


(* ::Input::Initialization:: *)
Labeled[
Pane[
PieChart[Association[Rule@@#&/@Tally[getData["\:c131\:bcc4"]]],
ImageSize->{Automatic,400},
LabelingFunction->(Placed[Style[#,20,Bold,White],"RadialCenter"]&),
ChartStyle->{RGBColor[{0.9647058823529412, 0.7490196078431373, 0.15294117647058825`, 1.}],RGBColor[{0.2627450980392157, 0.5215686274509804, 0.9607843137254902, 1.}]},
ChartLegends->Placed[{"\:c5ec","\:b0a8"},Right,Style[#,16,GrayLevel[.3]]&]],
imageDimensions,Alignment->Center
],
plotLabelFun["\:c11c\:c6b8\:d2b9\:bcc4\:c2dc \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:c131\:bcc4"],Top]
