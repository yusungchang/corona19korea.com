#!/usr/bin/env wolframscript -print -format PNG
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*\:acbd\:ae30\:b3c4  \:d655\:c9c4\:c790 \:ac70\:c8fc\:c9c0*)


(* ::Subtitle:: *)
(*\:c7a5\:c720\:c131 | Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Configuration*)


$outputDirectory=Directory[]<>"/../src/output/";


$vworldAPICred=Import[Directory[]<>"/../config/config.vworld.key.json","JSON"];


$districtMain="\:acbd\:ae30\:b3c4";


$districtNames={"\:ad70\:d3ec\:c2dc","\:ad6c\:b9ac\:c2dc","\:c591\:d3c9\:ad70","\:ace0\:c591\:c2dc","\:c624\:c0b0\:c2dc","\:d558\:b0a8\:c2dc","\:c2dc\:d765\:c2dc","\:c758\:c655\:c2dc","\:c548\:c591\:c2dc","\:c5ec\:c8fc\:c2dc","\:c548\:c131\:c2dc","\:c131\:b0a8\:c2dc","\:c548\:c0b0\:c2dc","\:c6a9\:c778\:c2dc","\:b0a8\:c591\:c8fc\:c2dc","\:ae40\:d3ec\:c2dc","\:c218\:c6d0\:c2dc","\:bd80\:cc9c\:c2dc","\:acfc\:cc9c\:c2dc","\:c758\:c815\:bd80\:c2dc","\:c774\:cc9c\:c2dc","\:d3c9\:d0dd\:c2dc","\:c5f0\:cc9c\:ad70","\:ac00\:d3c9\:ad70","\:ad11\:ba85\:c2dc","\:b3d9\:b450\:cc9c\:c2dc","\:c591\:c8fc\:c2dc","\:d3ec\:cc9c\:c2dc","\:d654\:c131\:c2dc","\:d30c\:c8fc\:c2dc","\:ad11\:c8fc\:c2dc"};


$centerFilName=$outputDirectory<>"gyunggido-district-centers.json"; (* RegionCentroid \:c2dc\:ac04\:c774 \:c624\:b798\:ac78\:b9ac\:b294 \:ad00\:acc4\:b85c \:acc4\:c0b0 \:acb0\:acfc\:b97c \:c800\:c7a5\:d568 *)


$imageWidth=860;


(* ::Section:: *)
(*Load Data*)


table=Cases[Import[$outputDirectory<>"gyunggi-corona19-data.csv"],Except[{}]];


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


legendFrame=(Framed[Style[#,14],Background->GrayLevel[1,.9],FrameStyle->LightGray,RoundingRadius->3]&);
placedLegendFrame=(Placed[#,{Left,Top},legendFrame]&);


plotLabelFun[title_]:=Column[{
Style[title,32,GrayLevel[.4]],
Style[
StringReplace[DateString[{"MonthShort","/","DayShort"," (","DayNameShort",") ","Hour24Short","\:c2dc \:d604\:c7ac"}],{"Mon"->"\:c6d4","Tue"->"\:d654","Wed"->"\:c218","Thu"->"\:baa9","Fri"->"\:ae08","Sat"->"\:d1a0","Sun"->"\:c77c"}],
20,GrayLevel[.6]],
Spacer[{0,16}]
},Alignment->Center,Spacings->{0,2},BaseStyle->"Label"];


imageDimensions={ImageSize->{$imageWidth,Automatic},ImagePadding->{{Automatic,Automatic},{Automatic,20}},AspectRatio->1/2};


(* ::Subsection:: *)
(*District Heatmap*)


Clear[districtHeatmap]
districtHeatmap[districts_,data_,OptionsPattern[{ImageSize->480,AspectRatio->1/2,LegendLabel->"Legend",LabelStyle->{FontSize->12},Min->-1,Max->-1}]]:=
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
Text[Style[Column[{s,Style[n,Smaller,Plain]},Alignment->Center,Spacings->0],labelS,GrayLevel[If[p>0.8,0.7,0.2]]],districtCenter[s]]
}
]
]&/@tally);

Overlay[{
Pane[
Graphics[{
regions,
EdgeForm[GrayLevel[0,0.1]],FaceForm[],(First@districtShape[#]&)/@districts,
labels,
Text[Style[#,labelS,GrayLevel[0.2]],districtCenter[#]]&/@others
},ImageSize->{Automatic,imageW*OptionValue[AspectRatio]}],
ImageSize->imageW,Alignment->Center
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
Magnify[Labeled[
districtHeatmap[
$districtNames,getData["\:ac70\:c8fc\:c9c0"],
ImageSize->$imageWidth,
LabelStyle->{FontSize->10},
LegendLabel->Style["\:d655 \:c9c4 \:c790",14]
],
plotLabelFun[$districtMain<>" \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:ac70\:c8fc\:c9c0"],Top],2]
