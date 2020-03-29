#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Visualization*)
(*Gyeonggi*)


(* ::Subtitle:: *)
(*\:c7a5\:c720\:c131 | Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section::Closed:: *)
(*Configuration*)


(* ::Subsection::Closed:: *)
(*Global*)


$baseDirectory=If[$Notebooks, NotebookDirectory[], Directory[]<>"/"];


$configDirectory=$baseDirectory<>"../config/";


$dataDirectory=$baseDirectory<>"../data/";


$outputDirectory=$baseDirectory<>"../src/output/";


(* ::Subsection::Closed:: *)
(*District Map*)


(* ::Text:: *)
(*VWorkd Open API: 2D Data API 2.0 (\:c2dc\:ad70\:ad6c)*)


(* http://www.vworld.kr/dev/v4dv_2ddataguide2_s002.do?svcIde=adsigg *)


$vworldAPICred=Import[$configDirectory<>"vworld.key.json","JSON"];


$districtMain="\:acbd\:ae30\:b3c4";


$districtNames={"\:ad70\:d3ec\:c2dc","\:ad6c\:b9ac\:c2dc","\:c591\:d3c9\:ad70","\:ace0\:c591\:c2dc","\:c624\:c0b0\:c2dc","\:d558\:b0a8\:c2dc","\:c2dc\:d765\:c2dc","\:c758\:c655\:c2dc","\:c548\:c591\:c2dc","\:c5ec\:c8fc\:c2dc","\:c548\:c131\:c2dc","\:c131\:b0a8\:c2dc","\:c548\:c0b0\:c2dc","\:c6a9\:c778\:c2dc","\:b0a8\:c591\:c8fc\:c2dc","\:ae40\:d3ec\:c2dc","\:c218\:c6d0\:c2dc","\:bd80\:cc9c\:c2dc","\:acfc\:cc9c\:c2dc","\:c758\:c815\:bd80\:c2dc","\:c774\:cc9c\:c2dc","\:d3c9\:d0dd\:c2dc","\:c5f0\:cc9c\:ad70","\:ac00\:d3c9\:ad70","\:ad11\:ba85\:c2dc","\:b3d9\:b450\:cc9c\:c2dc","\:c591\:c8fc\:c2dc","\:d3ec\:cc9c\:c2dc","\:d654\:c131\:c2dc","\:d30c\:c8fc\:c2dc","\:ad11\:c8fc\:c2dc"};


$centerFilName=$dataDirectory<>"gyeonggi-district-centers.json"; (* RegionCentroid \:c2dc\:ac04\:c774 \:c624\:b798\:ac78\:b9ac\:b294 \:ad00\:acc4\:b85c \:acc4\:c0b0 \:acb0\:acfc\:b97c \:c800\:c7a5\:d568 *)


(* ::Subsection::Closed:: *)
(*Output & Translation*)


$outputRules=Import[$configDirectory<>"gyeonggi-corona19.json"];


$translateRules=Import[$dataDirectory<>"translate.json"];


Clear[value,valueR];
valueR[keys_,assoc_]:=If[Rest@keys=={},Lookup[assoc,First@keys],valueR[Rest@keys,Lookup[assoc,First@keys]]];
value[keys___][assoc_]:=valueR[{keys},assoc];


outputValue[k___]:=value[k][$outputRules]
translateValue[k___]:=value[k][$translateRules]


(* ::Section::Closed:: *)
(*Source*)


Print["Read Gyeonggi province data..."];


(* ::Subsection::Closed:: *)
(*HTTP Request Setup*)


(* ::Text:: *)
(*\:acbd\:ae30\:b3c4 \:cf54\:b85c\:b09819 \:c0c1\:d669\:d310*)


ggBoardPostURL="https://www.gg.go.kr/ajax/board/getList.do";


(* Referer URL: https://www.gg.go.kr/bbs/board.do?bsIdx=722&menuId=2903 *)


(* Table header: {"\:c5f0\:bc88","\:c804\:ad6d\:bc88\:d638","\:c9c0\:c5ed","\:c131\:bcc4","\:cd9c\:c0dd\:c5f0\:b3c4","\:bc1c\:c0dd \:acbd\:c704","\:d655\:c9c4\:c77c","\:aca9\:b9ac\:bcd1\:c6d0"} *)


Clear[ggReq];
ggReq[offset_:0]:=HTTPRequest[
ggBoardPostURL, <|
	Method -> "POST", 
	"Body"-> {"bsIdx"->722,"bcIdx"->0,"menuId"->2903,"offset"->offset}
|>
];


ggPIDKey="ADD_COLUMN05"; (* Key value for PID (Patient ID) *)


(* ::Subsection::Closed:: *)
(*Request & Gather*)


ggDataRaw=URLExecute[ggReq[0]];


ggDataRawTotal=Lookup[ggDataRaw,"total"];
ggDataRawItems=Lookup[ggDataRaw,"items"];
ggDataRawStep=Length[Lookup[ggDataRaw,"items"]];


Do[
ggDataRawItems=SortBy[Union[ggDataRawItems,Lookup[URLExecute[ggReq[off]],"items"]],ToExpression@Lookup[#,ggPIDKey]&],
{off,ggDataRawStep,ggDataRawTotal,ggDataRawStep}]


ggDataRaw={"total"->ggDataRawTotal,"items"->ggDataRawItems};


(* ::Subsection::Closed:: *)
(*Save Cache*)


Export[$dataDirectory<>outputValue["cache","filename"],ggDataRaw];


(* ::Section:: *)
(*Preprocess Data*)


Print["Process data..."];


(* ::Subsection::Closed:: *)
(*Transform Function*)


ggDataProcess[row_,eventYear_]:=Module[{
dateKey="ADD_COLUMN09",genKey="ADD_COLUMN03",
ageKey="ADD_COLUMN02",districtKey="ADD_COLUMN01",
date,
gender,
age,
district
},
date=DateObject[First@StringSplit[Lookup[row,dateKey],"/"]<>"."<>ToString[eventYear]];(* \:d655\:c9c4 \:b0a0\:c790 \:cc98\:b9ac: \:c911\:bcf5\:b0a0\:c9dc\:c2dc \:ccab \:b0a0\:c9dc\:b9cc \:c120\:d0dd *)
gender=Lookup[row,genKey]; (* \:c131\:bcc4 \:cc98\:b9ac *)
age=eventYear-With[{a=ToExpression@StringReplace[Lookup[row,ageKey],Except[DigitCharacter]->""]},If[a<=20,2000+a,1900+a]]; (* \:b098\:c774 \:cc98\:b9ac *)
district=Replace[Lookup[row,districtKey],Missing[__]:>""]; (* \:ac70\:c8fc\:c9c0 \:cc98\:b9ac *)
{date,age,gender,district}
]


(* ::Subsection::Closed:: *)
(*Process Raw Data*)


table=Quiet[ggDataProcess[#,2020]&/@ggDataRawItems]; (* \:b9ac\:d3ec\:d2b8 \:c2dc\:c810: 2020\:b144 *)


(* ::Subsection::Closed:: *)
(*Extract Functions*)


Clear[getData];
dataColMap={"\:d655\:c9c4\:c77c"->1,"\:b098\:c774"->2,"\:c131\:bcc4"->3,"\:ac70\:c8fc\:c9c0"->4};
dataType={"\:d655\:c9c4\:c77c"->DateObject,"\:b098\:c774"->Integer,"\:c131\:bcc4"->String,"\:ac70\:c8fc\:c9c0"->String};
dataColMapR=Reverse[dataColMap,1];
getData[col_,range_:All,t_:table]:=t[[range,col/.dataColMap]];


(* ::Subsection::Closed:: *)
(*Verify Processed Data*)


If[And@@Flatten[Table[MatchQ[#,Blank[c/.dataType]]&/@getData[c], {c, dataColMap[[All,1]]}]]=!=True,
	Print["Data error: The data format might be changed in Gyunggi site.\n"];
	Quit[]];


(* ::Subsection::Closed:: *)
(*Export Processed Data*)


(* For CSV, convert DateObject to date strings *)


exportData=Normal@AssociationThread[
	dataColMap[[All,1]]->Prepend[Rest[#],DateString@First[#]]]&/@table;


(* ::Subsubsection:: *)
(*Korean*)


Export[$outputDirectory<>"kr/"<>outputValue["data","filename"],
	exportData,
	outputValue["data","type"]
];


(* ::Subsubsection:: *)
(*English*)


Export[$outputDirectory<>"en/"<>outputValue["data","filename"],
	exportData/.translateValue["en"],
	outputValue["data","type"]
];


(* ::Section:: *)
(*Visualization Functions*)


Print["Process district geometry..."];


(* ::Subsection::Closed:: *)
(*VWorld Open API*)


(* ::Text:: *)
(*VWorkd Open API: 2D Data API 2.0 (\:c2dc\:ad70\:ad6c)*)


vworldAPIURL="http://api.vworld.kr/req/data";
vworldAPIParams={"service"->"data","request"->"GetFeature","data"->"LT_C_ADSIGG_INFO"};


(* \:c9c0\:c5ed\:ba85 \:c911\:bcf5 \:bb38\:c81c\:b85c full \:d589\:c815\:ad6c\:c5ed\:ba85 \:c0ac\:c6a9. \:adf8\:b9ac\:ace0 \:c77c\:bd80 \:d589\:c815\:ad6c\:c5ed\:c740 \:bd84\:b9ac\:b418\:c5b4 \:c788\:c73c\:bbc0\:b85c :like: \:c0ac\:c6a9 *)


vworldAPICall[dist_]:=URLExecute[vworldAPIURL,Join[$vworldAPICred,vworldAPIParams,{"attrFilter"->"full_nm:like:"<>$districtMain<>" "<>dist}]];


(* ::Subsection::Closed:: *)
(*District Coordinates & Centers*)


Clear[districtCoords];
Clear[districtShape];
Clear[districtCenter];


districtCoords[dist_]:=districtCoords[dist]=Cases[vworldAPICall[dist],HoldPattern["coordinates"->x_]:>x,Infinity];


districtShape[dist_]:=districtShape[dist]=Reverse@Sort@Flatten@Table[Polygon/@x,{x,districtCoords[dist]}]


If[!FileExistsQ[$centerFilName],
Export[$centerFilName,
MapThread[Rule,{$districtNames,(RegionCentroid@First@districtShape[#]&)/@$districtNames}]]
]


centerRules=Import[$centerFilName];


districtCenter[dist_]:=dist/.centerRules;


(* ::Subsection::Closed:: *)
(*Color*)


getColor["\:d655\:c9c4"]=RGBColor[{0.9411764705882353, 0.4235294117647059, 0.00784313725490196, 1.}];
getColor["\:aca9\:b9ac\:c911"]=getColor["\:c5ec"]=RGBColor[{0.9647058823529412, 0.7490196078431373, 0.15294117647058825`, 1.}];
getColor["\:aca9\:b9ac\:d574\:c81c"]=getColor["\:b0a8"]=getColor["\:c99d\:ac00\:c218"]=RGBColor[{0.2627450980392157, 0.5215686274509804, 0.9607843137254902, 1.}];
getColor["\:c0ac\:b9dd"]=RGBColor[{0.8352941176470589, 0.00784313725490196, 0.00392156862745098, 1.}];
getColor["\:ac80\:c0ac\:cd1d\:acc4"]=RGBColor[{0.9411764705882353, 0.4235294117647059, 0.00784313725490196, 1.}];
getColor["\:ac80\:c0ac\:c911"]=RGBColor[{0.9647058823529412, 0.7490196078431373, 0.15294117647058825`, 1.}];
getColor["\:acb0\:acfc\:c74c\:c131"]=getColor["\:c5f0\:b839\:bd84\:d3ec"]=RGBColor[{0.19607843137254902`, 0.7137254901960784, 0.4745098039215686, 1.}];


(* ::Subsection:: *)
(*Plot Styles*)


imageWidth=860;


imageDimensions={ImageSize->{imageWidth,Automatic},AspectRatio->1/2};


(* ::Subsection:: *)
(*Label Function*)


$locale="kr";


plotLabelFun[title_,time_:CurrentDate[]]:=Column[{
	Style[title,32,GrayLevel[.6]],
	Style[Row[{
		StringReplace[DateString[time,{"MonthShort","/","DayShort"," (","DayNameShort",") ","Hour24Short"}],
			Switch[$locale,
				"en", {},
				"kr", {"Mon"->"\:c6d4","Tue"->"\:d654","Wed"->"\:c218","Thu"->"\:baa9","Fri"->"\:ae08","Sat"->"\:d1a0","Sun"->"\:c77c"}]
			],"\:c2dc"," ","\:c5c5\:b370\:c774\:d2b8"}],
		20,GrayLevel[.8]
	],
	Spacer[{0,16}]
},Alignment->Center,Spacings->{0,2},BaseStyle->{FontFamily->"Gothic A1",FontWeight->"Light"}];


(* ::Subsection:: *)
(*District Heat Map*)


atMost[list_,n_:30]:=list[[1;;Min[n,Length[list]]]];


Clear[districtHeatmap];
districtHeatmap[districts_,data_,OptionsPattern[{ImageSize->480,AspectRatio->1/2,LegendLabel->"Legend",LabelStyle->{FontSize->12},Min->-1,Max->-1}]]:=
Module[{
min=OptionValue[Min],
max=OptionValue[Max],
imageW=OptionValue[ImageSize],
labelS=OptionValue[LabelStyle],
tally,
regions,
labels,
others,
otherShapes
},
tally=Tally[Select[data,MemberQ[districts,#]&]];
others=Complement[districts,tally[[All,1]]];
min=If[min==-1,Min@tally[[All,2]],min];
max=If[max==-1,Max@tally[[All,2]],max];

{regions,labels}=Transpose@(
With[{s=#[[1]],n=#[[2]]},
With[{p=(n-min)/(max-min)},
{
{EdgeForm[],ColorData["CandyColors",1-p],atMost@districtShape[s]},
Text[Style[Column[{s,Style[n,Smaller,Plain]},Alignment->Center,Spacings->0],labelS,GrayLevel[If[p>0.5,1,0.4]]],districtCenter[s]]
}
]
]&/@tally);

otherShapes={EdgeForm[],GrayLevel[1,0.2],atMost@districtShape[#]}&/@others;

Overlay[{
Pane[
Graphics[{
regions,
otherShapes,
EdgeForm[GrayLevel[0,0.1]],FaceForm[],(atMost@districtShape[#]&)/@districts,
labels,
Text[Style[#,labelS,GrayLevel[0.2]],districtCenter[#]]&/@others
},ImageSize->{Automatic,imageW*OptionValue[AspectRatio]}],
ImageSize->imageW,Alignment->Center
],

Grid[{
{min,
Graphics[
Raster@Reverse[ColorData["CandyColors","Image"][[1,1,1]],2],
ImageSize->{imageW/4,imageW/40},AspectRatio->\!\(TraditionalForm\`
\*FractionBox[\(1\), \(10\)]\),PlotRange->{{0,100},{0,1}}],
max},
{"",OptionValue["LegendLabel"],""}
},Alignment->{Center,Center},BaseStyle->{"Text",FontColor->GrayLevel[.6],FontSize->12}]
},Alignment->{Right,Top}]
]


(* ::Section:: *)
(*\:acbd\:ae30\:b3c4 \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:ac70\:c8fc\:c9c0*)


out1:=Magnify[Labeled[
districtHeatmap[
$districtNames,getData["\:ac70\:c8fc\:c9c0"],
ImageSize->imageWidth,
LabelStyle->{FontSize->10},
LegendLabel->Style["\:d655 \:c9c4 \:c790",14]
],
plotLabelFun[outputValue["1","label"]],Top],2]


(* ::Subsection::Closed:: *)
(*Export*)


Print["Generate "<>outputValue["1","label"]<>"..."];


(* ::Subsubsection:: *)
(*Korean*)


$locale="kr";


Export[$outputDirectory<>"kr/"<>outputValue["1","filename"],out1,outputValue["1","type"],Background->None];


(* ::Subsubsection:: *)
(*English*)


$locale="en";


Export[$outputDirectory<>"en/"<>outputValue["1","filename"],out1/.translateValue["en"],outputValue["1","type"],Background->None];


(* ::Section:: *)
(*\:acbd\:ae30\:b3c4 \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:d604\:d669*)


dates=DateRange[Min@getData["\:d655\:c9c4\:c77c"],Max@getData["\:d655\:c9c4\:c77c"]];
evenQ=EvenQ[Length[dates]];
dateTicks={AbsoluteTime[#],DateString[#,{"MonthShort","/","DayShort"}]}&/@Cases[dates,x_/;(DateString[x,"DayName"]==="Monday")];


out2:=Labeled[
DateHistogram[getData["\:d655\:c9c4\:c77c"],"Day","CumulativeCount",
imageDimensions,
Frame->True,
FrameTicks->{{Automatic,None},{dateTicks,None}},
FrameTicksStyle->Directive[FontSize->14],
LabelingFunction->(Placed[If[#>4 && EvenQ[#2[[2]]]===evenQ,Overlay[{Style[#,14,Bold,Black],Style[#,14,getColor["\:d655\:c9c4"]]}],""],Above]&),
ChartStyle->Directive[{getColor["\:d655\:c9c4"],EdgeForm[{White,Thick}]}],
GridLines->Automatic,
DateTicksFormat->{"MonthShort","/","DayShort"}],
plotLabelFun[outputValue["2","label"]],Top]


(* ::Subsection::Closed:: *)
(*Export*)


Print["Generate "<>outputValue["2","label"]<>"..."];


(* ::Subsubsection:: *)
(*Korean*)


$locale="kr";


Export[$outputDirectory<>"kr/"<>outputValue["2","filename"],out2,outputValue["2","type"],Background->None];


(* ::Subsubsection:: *)
(*English*)


$locale="en";


Export[$outputDirectory<>"en/"<>outputValue["2","filename"],out2/.translateValue["en"],outputValue["2","type"],Background->None];


(* ::Section:: *)
(*\:acbd\:ae30\:b3c4 \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:c99d\:ac00\:c218*)


out3:=Labeled[
DateHistogram[getData["\:d655\:c9c4\:c77c"],"Day",
imageDimensions,
Frame->True,
FrameTicks->{{Automatic,None},{dateTicks,None}},
FrameTicksStyle->Directive[FontSize->14],
LabelingFunction->(Placed[Style[If[#>0,#,""],12,Bold,getColor["\:c99d\:ac00\:c218"]],Above]&),
ChartStyle->Directive[{getColor["\:c99d\:ac00\:c218"],EdgeForm[{White,Thick}]}],
GridLines->Automatic,
DateTicksFormat->{"MonthShort","/","DayShort"}],
plotLabelFun[outputValue["3","label"]],Top]


(* ::Subsection::Closed:: *)
(*Export*)


Print["Generate "<>outputValue["3","label"]<>"..."];


(* ::Subsubsection:: *)
(*Korean*)


$locale="kr";


Export[$outputDirectory<>"kr/"<>outputValue["3","filename"],out3,outputValue["3","type"],Background->None];


(* ::Subsubsection:: *)
(*English*)


$locale="en";


Export[$outputDirectory<>"en/"<>outputValue["3","filename"],out3/.translateValue["en"],outputValue["3","type"],Background->None];


(* ::Section:: *)
(*\:acbd\:ae30\:b3c4 \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:c5f0\:b839 \:bd84\:d3ec*)


out4:=Labeled[
Histogram[getData["\:b098\:c774"],{0,100,5},
imageDimensions,
Frame->True,
FrameTicksStyle->Directive[FontSize->14],
FrameLabel->{Style["\:c5f0 \:b839",14]},
LabelingFunction->(Placed[Style[#,14,getColor["\:c5f0\:b839\:bd84\:d3ec"],Bold],Above]&),
ChartStyle->Directive[{getColor["\:c5f0\:b839\:bd84\:d3ec"],EdgeForm[{White,Thick}]}],
GridLines->Automatic],
plotLabelFun[outputValue["4","label"]],Top]


(* ::Subsection::Closed:: *)
(*Export*)


Print["Generate "<>outputValue["4","label"]<>"..."];


(* ::Subsubsection:: *)
(*Korean*)


$locale="kr";


Export[$outputDirectory<>"kr/"<>outputValue["4","filename"],out4,outputValue["4","type"],Background->None];


(* ::Subsubsection:: *)
(*English*)


$locale="en";


Export[$outputDirectory<>"en/"<>outputValue["4","filename"],out4/.translateValue["en"],outputValue["4","type"],Background->None];


(* ::Section:: *)
(*\:acbd\:ae30\:b3c4 \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:c131\:bcc4*)


out5:=With[{data=Association[Rule@@#&/@(Sort@Tally[getData["\:c131\:bcc4"]])]},
Labeled[
Pane[
PieChart[data,
ImageSize->{Automatic,400},
LabelingFunction->(Placed[Style[Column[{#,Style[Row[{"(",Round[100*#/Plus@@Values[data],0.1],"%)"}],Plain,Smaller]},Alignment->Center],20,Bold,White],"RadialCenter"]&),
ChartStyle->{getColor["\:b0a8"],getColor["\:c5ec"]},
ChartLegends->Placed[{"\:b0a8","\:c5ec"},Right,Style[#,18,GrayLevel[.6]]&]],
imageDimensions,Alignment->Center
],
plotLabelFun[outputValue["5","label"]],Top]
]


(* ::Subsection::Closed:: *)
(*Export*)


Print["Generate "<>outputValue["5","label"]<>"..."];


(* ::Subsubsection:: *)
(*Korean*)


$locale="kr";


Export[$outputDirectory<>"kr/"<>outputValue["5","filename"],out5,outputValue["5","type"],Background->None];


(* ::Subsubsection:: *)
(*English*)


$locale="en";


Export[$outputDirectory<>"en/"<>outputValue["5","filename"],out5/.translateValue["en"],outputValue["5","type"],Background->None];
