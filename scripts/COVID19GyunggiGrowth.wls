#!/usr/bin/env wolframscript -print -format SVG
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*\:acbd\:ae30\:b3c4  \:d655\:c9c4\:c790 \:d604\:d669*)


(* ::Subtitle:: *)
(*\:c7a5\:c720\:c131 | Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Note*)


(* ::Section:: *)
(*Initialization*)


outputDirectory=Directory[]<>"/../src/output/";


(* ::Section:: *)
(*Load Data*)


table=Cases[Import[outputDirectory<>"gyunggi-corona19-data.csv"],Except[{}]];


table=Prepend[Rest[#],DateObject[First[#]]]&/@table;


(* ::Section:: *)
(*Data Functions*)


ggDistrictNames={"\:ad70\:d3ec\:c2dc","\:ad6c\:b9ac\:c2dc","\:c591\:d3c9\:ad70","\:ace0\:c591\:c2dc","\:c624\:c0b0\:c2dc","\:d558\:b0a8\:c2dc","\:c2dc\:d765\:c2dc","\:c758\:c655\:c2dc","\:c548\:c591\:c2dc","\:c5ec\:c8fc\:c2dc","\:c548\:c131\:c2dc","\:c131\:b0a8\:c2dc","\:c548\:c0b0\:c2dc","\:c6a9\:c778\:c2dc","\:b0a8\:c591\:c8fc\:c2dc","\:ae40\:d3ec\:c2dc","\:c218\:c6d0\:c2dc","\:bd80\:cc9c\:c2dc","\:acfc\:cc9c\:c2dc","\:c758\:c815\:bd80\:c2dc","\:c774\:cc9c\:c2dc","\:d3c9\:d0dd\:c2dc","\:c5f0\:cc9c\:ad70","\:ac00\:d3c9\:ad70","\:ad11\:ba85\:c2dc","\:b3d9\:b450\:cc9c\:c2dc","\:c591\:c8fc\:c2dc","\:d3ec\:cc9c\:c2dc","\:d654\:c131\:c2dc","\:d30c\:c8fc\:c2dc","\:ad11\:c8fc\:c2dc"};


(* ::Subsubsection:: *)
(*Define Extract Functions*)


ggDataColMap={"\:d655\:c9c4\:c77c"->1,"\:b098\:c774"->2,"\:c131\:bcc4"->3,"\:ac70\:c8fc\:c9c0"->4};
ggDataColMapR=Reverse[ggDataColMap,1];
ggGetData[col_,range_:All,t_:table]:=t[[range,col/.ggDataColMap]];


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
DateHistogram[ggGetData["\:d655\:c9c4\:c77c"],"Day","CumulativeCount",
imageDimensions,
Frame->True,
FrameTicksStyle->12,
LabelingFunction->(Placed[Style[If[#>4,#,""],12,Bold,RGBColor[{0.9411764705882353, 0.4235294117647059, 0.00784313725490196, 1.}]],Above]&),
ChartStyle->Directive[{RGBColor[{0.9411764705882353, 0.4235294117647059, 0.00784313725490196, 1.}],EdgeForm[{White,Thick}]}],
GridLines->Automatic,
DateTicksFormat->{"MonthShort","/","DayShort"}],
plotLabelFun["\:acbd\:ae30\:b3c4 \:cf54\:b85c\:b098 19 \:d655\:c9c4\:c790 \:d604\:d669"],Top]
