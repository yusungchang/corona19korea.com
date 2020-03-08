#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Visualization*)
(*Gyunggi*)


(* ::Subtitle:: *)
(*\:c7a5\:c720\:c131 | Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Configuration*)


(* ::Subsection::Closed:: *)
(*Global*)


$baseDirectory=If[$Notebooks, NotebookDirectory[], Directory[]<>"/"];


$configDirectory=$baseDirectory<>"../config/";


$dataDirectory=$baseDirectory<>"../data/";


$cacheDirectory=$baseDirectory<>"../data/cache/";


$outputDirectory=$baseDirectory<>"../src/output/";


(* ::Subsection::Closed:: *)
(*District Map*)


(* ::Text:: *)
(*VWorkd Open API: 2D Data API 2.0 (\:c2dc\:ad70\:ad6c)*)


(* http://www.vworld.kr/dev/v4dv_2ddataguide2_s002.do?svcIde=adsigg *)


$vworldAPICred=Import[$configDirectory<>"vworld.key.json","JSON"];


$districtMain="\:acbd\:ae30\:b3c4";


$districtNames={"\:ad70\:d3ec\:c2dc","\:ad6c\:b9ac\:c2dc","\:c591\:d3c9\:ad70","\:ace0\:c591\:c2dc","\:c624\:c0b0\:c2dc","\:d558\:b0a8\:c2dc","\:c2dc\:d765\:c2dc","\:c758\:c655\:c2dc","\:c548\:c591\:c2dc","\:c5ec\:c8fc\:c2dc","\:c548\:c131\:c2dc","\:c131\:b0a8\:c2dc","\:c548\:c0b0\:c2dc","\:c6a9\:c778\:c2dc","\:b0a8\:c591\:c8fc\:c2dc","\:ae40\:d3ec\:c2dc","\:c218\:c6d0\:c2dc","\:bd80\:cc9c\:c2dc","\:acfc\:cc9c\:c2dc","\:c758\:c815\:bd80\:c2dc","\:c774\:cc9c\:c2dc","\:d3c9\:d0dd\:c2dc","\:c5f0\:cc9c\:ad70","\:ac00\:d3c9\:ad70","\:ad11\:ba85\:c2dc","\:b3d9\:b450\:cc9c\:c2dc","\:c591\:c8fc\:c2dc","\:d3ec\:cc9c\:c2dc","\:d654\:c131\:c2dc","\:d30c\:c8fc\:c2dc","\:ad11\:c8fc\:c2dc"};


$centerFilName=$dataDirectory<>"gyunggi-district-centers.json"; (* RegionCentroid \:c2dc\:ac04\:c774 \:c624\:b798\:ac78\:b9ac\:b294 \:ad00\:acc4\:b85c \:acc4\:c0b0 \:acb0\:acfc\:b97c \:c800\:c7a5\:d568 *)


(* ::Subsection::Closed:: *)
(*Output & Translation*)


$outputRules=Import[$dataDirectory<>"gyunggi-corona19.json"];


$translateRules=Import[$dataDirectory<>"translate.json"];


Clear[value,valueR];
valueR[keys_,assoc_]:=If[Rest@keys=={},Lookup[assoc,First@keys],valueR[Rest@keys,Lookup[assoc,First@keys]]];
value[keys___][assoc_]:=valueR[{keys},assoc];


outputValue[k___]:=value[k][$outputRules]
translateValue[k___]:=value[k][$translateRules]


(* ::Section:: *)
(*Source*)


(* ::Subsection::Closed:: *)
(*Source URL*)


(* ::Text:: *)
(*\:acbd\:ae30\:b3c4 \:cf54\:b85c\:b09819 \:c0c1\:d669\:d310*)


ggPageURL="https://www.gg.go.kr/bbs/boardView.do?bsIdx=464&bIdx=2296956&menuId=1535";


(* ::Subsection::Closed:: *)
(*Import Page*)


Print["Read Gyunggi district page..."];


raw=Import[ggPageURL,"Data"];


tableRaw=raw[[4,2,2,1,2,2]];


(* ::Section:: *)
(*Preprocess Data*)


Print["Process data..."];


(* ::Subsection::Closed:: *)
(*Transform Function*)


ggDataProcess[body_,eventYear_]:=Module[{
colDate=5,colGen=3,colAge=4,colDistrict=-1,
dates,
genders,
ages,
districts
},
dates=Quiet@DateObject[First@StringCases[ToString[#],RegularExpression["\\d+\\.\\d+"]]]&/@body[[All,colDate]];(* \:d655\:c9c4 \:b0a0\:c790 \:cc98\:b9ac *)
genders=body[[All,colGen]]; (* \:c131\:bcc4 \:cc98\:b9ac *)
ages=(eventYear-With[{a=ToExpression[StringTake[#,{2,-1}]]},If[a<=20,2000+a,1900+a]])&/@body[[All,colAge]]; (* \:b098\:c774 \:cc98\:b9ac *)
districts=(#<>"\:c2dc"&)/@(StringReplace[#,"("~~__~~")":>""]&/@body[[All,colDistrict]]); (* \:cd9c\:c0dd\:c9c0 \:cc98\:b9ac: \:ad6d\:c801\:c81c\:ac70 \:d6c4 "\:c2dc" \:cd94\:ac00 *)

Transpose[{dates,ages,genders,districts}]
]


(* ::Subsection::Closed:: *)
(*Process Raw Data*)


table=ggDataProcess[tableRaw,2020]; (* \:b9ac\:d3ec\:d2b8 \:c2dc\:c810: 2020\:b144 *)


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


Export[$outputDirectory<>outputValue["data","filename"],
	AssociationThread[
		dataColMap[[All,1]]->Prepend[Rest[#],DateString@First[#]]]&/@table, (* For CSV, convert DateObject to date strings *)
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


(* ::Subsection::Closed:: *)
(*Plot Styles*)


imageWidth=860;


legendFrame=(Framed[Style[#,14],Background->GrayLevel[1,.9],FrameStyle->LightGray,RoundingRadius->3]&);
placedLegendFrame=(Placed[#,{Left,Top},legendFrame]&);


imageDimensions={ImageSize->{imageWidth,Automatic},AspectRatio->1/2};


(* ::Subsection::Closed:: *)
(*Label Function*)


$locale="kr";


plotLabelFun[title_,time_:CurrentDate[]]:=Column[{
	Style[title,32,GrayLevel[.4]],
	Style[Row[{
		StringReplace[DateString[time,{"MonthShort","/","DayShort"," (","DayNameShort",") ","Hour24Short"}],
			Switch[$locale,
				"en", {},
				"kr", {"Mon"->"\:c6d4","Tue"->"\:d654","Wed"->"\:c218","Thu"->"\:baa9","Fri"->"\:ae08","Sat"->"\:d1a0","Sun"->"\:c77c"}]
			],"\:c2dc"," ","\:c5c5\:b370\:c774\:d2b8"}],
		20,GrayLevel[.6]
	],
	Spacer[{0,16}]
},Alignment->Center,Spacings->{0,2},BaseStyle->{FontFamily->"Nanum Gothic"}];


(* ::Subsection::Closed:: *)
(*District Heat Map*)


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
(*\:acbd\:ae30\:b3c4 \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:ac70\:c8fc\:c9c0*)


out1:=Magnify[Labeled[
districtHeatmap[
$districtNames,getData["\:ac70\:c8fc\:c9c0"],
ImageSize->imageWidth,
LabelStyle->{FontSize->10},
LegendLabel->Style["\:d655 \:c9c4 \:c790",14]
],
plotLabelFun[$districtMain<>" \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:ac70\:c8fc\:c9c0"],Top],2]


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


out2:=Labeled[
DateHistogram[getData["\:d655\:c9c4\:c77c"],"Day","CumulativeCount",
imageDimensions,
Frame->True,
FrameTicksStyle->Directive[FontSize->14],
LabelingFunction->(Placed[Style[If[#>4,#,""],12,Bold,getColor["\:d655\:c9c4"]],Above]&),
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
ChartLegends->Placed[{"\:b0a8","\:c5ec"},Right,Style[#,16,GrayLevel[.3]]&]],
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
