#!/usr/bin/env wolframscript -print -format SVG
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*\:acbd\:ae30\:b3c4: \:d655\:c9c4\:c790 \:d604\:d669*)


(* ::Subtitle:: *)
(*\:c7a5\:c720\:c131 | Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Initialization*)


$outputDirectory=Directory[]<>"/../src/output/";


$dataFile=$outputDirectory<>"gyunggi-corona19-data.csv";


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
DateHistogram[getData["\:d655\:c9c4\:c77c"],"Day","CumulativeCount",
imageDimensions,
Frame->True,
FrameTicksStyle->Directive[FontSize->14],
LabelingFunction->(Placed[Style[If[#>4,#,""],12,Bold,RGBColor[{0.9411764705882353, 0.4235294117647059, 0.00784313725490196, 1.}]],Above]&),
ChartStyle->Directive[{RGBColor[{0.9411764705882353, 0.4235294117647059, 0.00784313725490196, 1.}],EdgeForm[{White,Thick}]}],
GridLines->Automatic,
DateTicksFormat->{"MonthShort","/","DayShort"}],
plotLabelFun["\:acbd\:ae30\:b3c4 \:cf54\:b85c\:b098 19 \:d655\:c9c4\:c790 \:d604\:d669"],Top]
