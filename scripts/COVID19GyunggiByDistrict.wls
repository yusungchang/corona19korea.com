#!/usr/bin/env wolframscript -print -format SVG
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*\:acbd\:ae30\:b3c4  \:d655\:c9c4\:c790 \:ac70\:c8fc\:c9c0*)


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


(* ::Subsection:: *)
(*VWorld API*)


vworldAPICred=Import[Directory[]<>"/../config/config.vworld.key.json","JSON"];


vworldAPIURL="http://api.vworld.kr/req/data";
vworldAPIParams={"service"->"data","request"->"GetFeature","data"->"LT_C_ADSIGG_INFO"};


(* \:c774\:b984 \:b9e4\:ce6d \:bb38\:c81c\:b85c :=: \:b300\:c2e0 :like: \:c0ac\:c6a9 *)


vworldAPICall[dist_]:=URLExecute[vworldAPIURL,Join[vworldAPICred,vworldAPIParams,{"attrFilter"->"sig_kor_nm:like:"<>dist}]];


(* ::Subsection:: *)
(*District Coordinates & Centers*)


Clear[districtCoords]


Clear[districtCenter]


districtCoords[dist_]:=districtCoords[dist]=Cases[vworldAPICall[dist],HoldPattern["coordinates"->x_]:>x,Infinity][[1,1,1]];


districtCenter[dist_]:=districtCenter[dist]=RegionCentroid[Polygon[districtCoords[dist]]]


Graphics[{Line/@districtCoords/@ggDistrictNames,Red,Point/@districtCenter/@ggDistrictNames}]


(* ::Subsection:: *)
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


(* ::Subsection:: *)
(*District Heatmap*)


Clear[districtHeatmap]
districtHeatmap[districts_,data_,OptionsPattern[{ImageSize->480,LegendLabel->"Legend",LabelStyle->{FontSize->12},Min->-1,Max->-1}]]:=
Module[{
min=OptionValue[Min],
max=OptionValue[Max],
imageW=OptionValue[ImageSize],
labelS=OptionValue[LabelStyle],
tally,
regions,
labels,
others
},
tally=Tally[Select[data,MemberQ[districts,#]&]];
others=Complement[districts,tally[[All,1]]];
min=If[min==-1,Min@tally[[All,2]],min];
max=If[max==-1,Max@tally[[All,2]],max];

{regions,labels}=Transpose@(
With[{s=#[[1]],n=#[[2]]},
With[{p=(n-min)/(max-min)},
{
{ColorData["CandyColors",1-p],Polygon[districtCoords[s]]},
Text[Style[Column[{s,Style[n,Smaller,Plain]},Alignment->Center],labelS,GrayLevel[If[p>0.5,1,0.2]]],districtCenter[s]]
}
]
]&/@tally);

Overlay[{
Graphics[{
regions,
GrayLevel[0,0.1],Line/@districtCoords/@districts,
Opacity[1],
labels,
Text[Style[#,labelS,GrayLevel[0.2]],districtCenter[#]]&/@others
},ImageSize->imageW
],
Framed[
Grid[{
{min,
Graphics[
Raster@Reverse[ColorData["CandyColors","Image"][[1,1,1]],2],
ImageSize->{imageW/4,imageW/40},AspectRatio->\!\(TraditionalForm\`
\*FractionBox[\(1\), \(10\)]\),PlotRange->{{0,100},{0,1}}],
max},
{"",OptionValue["LegendLabel"],""}
},Alignment->{Center,Center},BaseStyle->{"Text",FontSize->12}],
RoundingRadius->4,FrameStyle->LightGray,FrameMargins->{{5,5},{5,10}}
]
},Alignment->{Right,Top}]
]


(* ::Section:: *)
(*District Visualization*)


(* ::Input::Initialization:: *)
ggCorona19ByDistricts=Labeled[
Pane[
districtHeatmap[ggDistrictNames,ggGetData["\:ac70\:c8fc\:c9c0"],ImageSize->imageWidth*.5,LegendLabel->Style["\:d655 \:c9c4 \:c790",14]],
ImageSize->{imageWidth,Automatic},Alignment->Center],
plotLabelFun["\:acbd\:ae30\:b3c4 \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:ac70\:c8fc\:c9c0"],Top]
