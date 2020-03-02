#!/usr/bin/env wolframscript -print -format PNG
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*\:c11c\:c6b8: \:d655\:c9c4\:c790 \:ac70\:c8fc\:c9c0*)


(* ::Subtitle:: *)
(*\:c7a5\:c720\:c131 | Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Configuration*)


$outputDirectory=Directory[]<>"/../src/output/";


$vworldAPICred=Import[Directory[]<>"/../config/vworld.key.json","JSON"];


$districtMain="\:c11c\:c6b8\:d2b9\:bcc4\:c2dc";


$districtNames={"\:ac15\:b0a8\:ad6c","\:ac15\:b3d9\:ad6c","\:ac15\:bd81\:ad6c","\:ac15\:c11c\:ad6c","\:ad00\:c545\:ad6c","\:ad11\:c9c4\:ad6c","\:ad6c\:b85c\:ad6c","\:ae08\:cc9c\:ad6c","\:b178\:c6d0\:ad6c","\:b3c4\:bd09\:ad6c","\:b3d9\:b300\:bb38\:ad6c","\:b3d9\:c791\:ad6c","\:b9c8\:d3ec\:ad6c","\:c11c\:b300\:bb38\:ad6c","\:c11c\:cd08\:ad6c","\:c131\:b3d9\:ad6c","\:c131\:bd81\:ad6c","\:c1a1\:d30c\:ad6c","\:c591\:cc9c\:ad6c","\:c601\:b4f1\:d3ec\:ad6c","\:c6a9\:c0b0\:ad6c","\:c740\:d3c9\:ad6c","\:c885\:b85c\:ad6c","\:c911\:ad6c","\:c911\:b791\:ad6c"};


$centerFilName=$outputDirectory<>"seoul-district-centers.json"; (* RegionCentroid \:c2dc\:ac04\:c774 \:c624\:b798\:ac78\:b9ac\:b294 \:ad00\:acc4\:b85c \:acc4\:c0b0 \:acb0\:acfc\:b97c \:c800\:c7a5\:d568 *)


$imageWidth=860;


(* ::Section:: *)
(*Load Data*)


table=Cases[Import[$outputDirectory<>"seoul-corona19-data.csv"],Except[{}]];


table=Prepend[Rest[#],DateObject[First[#]]]&/@table;


(* ::Section:: *)
(*Data Functions*)


(* ::Subsubsection:: *)
(*Define Extract Functions*)


dataColMap={"\:d655\:c9c4\:c77c"->1,"\:b098\:c774"->2,"\:c131\:bcc4"->3,"\:ac70\:c8fc\:c9c0"->4};
dataColMapR=Reverse[dataColMap,1];
getData[col_,range_:All,t_:table]:=t[[range,col/.dataColMap]];


(* ::Section:: *)
(*District Visualization Functions*)


(* ::Subsection:: *)
(*VWorld \:c624\:d508 API*)


vworldAPIURL="http://api.vworld.kr/req/data";
vworldAPIParams={"service"->"data","request"->"GetFeature","data"->"LT_C_ADSIGG_INFO"};


(* \:c9c0\:c5ed\:ba85 \:c911\:bcf5 \:bb38\:c81c\:b85c full \:d589\:c815\:ad6c\:c5ed\:ba85 \:c0ac\:c6a9. \:adf8\:b9ac\:ace0 \:c77c\:bd80 \:d589\:c815\:ad6c\:c5ed\:c740 \:bd84\:b9ac\:b418\:c5b4 \:c788\:c73c\:bbc0\:b85c :like: \:c0ac\:c6a9 *)


vworldAPICall[dist_]:=URLExecute[vworldAPIURL,Join[$vworldAPICred,vworldAPIParams,{"attrFilter"->"full_nm:like:"<>$districtMain<>" "<>dist}]];


(* ::Subsection:: *)
(*District Coordinates & Centers*)


Clear[districtCoords]


Clear[districtShape]


Clear[districtCenter]


districtCoords[dist_]:=districtCoords[dist]=Cases[vworldAPICall[dist],HoldPattern["coordinates"->x_]:>x,Infinity];


districtShape[dist_]:=districtShape[dist]=Reverse@Sort@Flatten@Table[Polygon/@x,{x,districtCoords[dist]}]


If[!FileExistsQ[$centerFilName],
Export[$centerFilName,
MapThread[Rule,{$districtNames,(RegionCentroid@First@districtShape[#]&)/@$districtNames}]]
]


centerRules=Import[$centerFilName];


districtCenter[dist_]:=dist/.centerRules;


(* ::Subsection:: *)
(*Plot Styles*)


plotLabelFun[title_]:=Column[{
Style[title,32,GrayLevel[.4]],
Style[
StringReplace[DateString[{"MonthShort","/","DayShort"," (","DayNameShort",") ","Hour24Short","\:c2dc \:d604\:c7ac"}],{"Mon"->"\:c6d4","Tue"->"\:d654","Wed"->"\:c218","Thu"->"\:baa9","Fri"->"\:ae08","Sat"->"\:d1a0","Sun"->"\:c77c"}],
20,GrayLevel[.6]],
Spacer[{0,16}]
},Alignment->Center,Spacings->{0,2},BaseStyle->"Label"];


(* ::Subsection:: *)
(*District Heatmap*)


Clear[districtHeatmap]
districtHeatmap[districts_,data_,OptionsPattern[{ImageSize->480,LegendLabel->"Legend",LabelStyle->{FontSize->14,Bold},Min->-1,Max->-1}]]:=
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
{EdgeForm[],ColorData["CandyColors",1-p],First@districtShape[s]},
Text[Style[Column[{s,Style[n,Smaller,Plain]},Alignment->Center],labelS,GrayLevel[If[p>0.5,1,0.2]]],districtCenter[s]]
}
]
]&/@tally);

Overlay[{
Graphics[{
regions,
EdgeForm[GrayLevel[0,0.1]],FaceForm[],(First@districtShape[#]&)/@districts,
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
},Alignment->{Left,Top}]
]


(* ::Section:: *)
(*District Visualization*)


(* ::Input::Initialization:: *)
Magnify[Labeled[
districtHeatmap[
$districtNames,getData["\:ac70\:c8fc\:c9c0"],
ImageSize->$imageWidth,
LabelStyle->{FontSize->14,Bold},
LegendLabel->Style["\:d655 \:c9c4 \:c790",14]
],
plotLabelFun[$districtMain<>" \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:ac70\:c8fc\:c9c0"],Top],2]
