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


(* ::Subsection:: *)
(*VWorld API*)


vworldAPICred=Import[Directory[]<>"/../config/config.vworld.key.json","JSON"];


vworldAPIURL="http://api.vworld.kr/req/data";
vworldAPIParams={"service"->"data","request"->"GetFeature","data"->"LT_C_ADSIGG_INFO"};


vworldAPICall[dist_]:=URLExecute[vworldAPIURL,Join[vworldAPICred,vworldAPIParams,{"attrFilter"->"sig_kor_nm:=:"<>dist}]];


(* ::Subsection:: *)
(*District Coordinates & Centers*)


Clear[districtCoords]


Clear[districtCenter]


districtCoords[dist_]:=districtCoords[dist]=Cases[vworldAPICall[dist],HoldPattern["coordinates"->x_]:>x,Infinity][[1,1,1]];


districtCenter[dist_]:=districtCenter[dist]=RegionCentroid[Polygon[districtCoords[dist]]]


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
},Alignment->{Left,Top}]
]


(* ::Section:: *)
(*District Visualization*)


(* ::Input::Initialization:: *)
Labeled[
districtHeatmap[seoulDistrictNames,seoulGetData["\:ac70\:c8fc\:c9c0"],ImageSize->imageWidth,LegendLabel->Style["\:d655 \:c9c4 \:c790",14]],
plotLabelFun["\:c11c\:c6b8\:c2dc \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:ac70\:c8fc\:c9c0"],Top]
