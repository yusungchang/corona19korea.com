#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Visualization*)
(*South Korea*)


(* ::Subtitle:: *)
(*\:c7a5\:c720\:c131 | Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Config*)


$baseDirectory=If[$Notebooks, NotebookDirectory[], Directory[]<>"/"];


$configDirectory=$baseDirectory<>"../config/";


$dataDirectory=$baseDirectory<>"../data/";


$cacheDirectory=$baseDirectory<>"../data/cache/";


$outputDirectory=$baseDirectory<>"../src/output/";


(* ::Subsection:: *)
(*Import Output Configuration*)


$outputRules=Import[$dataDirectory<>"korea-corona19.json"];


$translateRules=Import[$dataDirectory<>"translate.json"];


Clear[value,valueR];
valueR[keys_,assoc_]:=If[Rest@keys=={},Lookup[assoc,First@keys],valueR[Rest@keys,Lookup[assoc,First@keys]]];
value[keys___][assoc_]:=valueR[{keys},assoc];


outputValue[k___]:=value[k][$outputRules]
translateValue[k___]:=value[k][$translateRules]


(* ::Section::Closed:: *)
(*Source Config*)


(* \:c9c8\:bcd1\:ad00\:b9ac\:bcf8\:bd80 (KCDC) \:ac8c\:c2dc\:d310 *)


apiKCDCURLBase="https://www.cdc.go.kr";
apiKCDCURLBoard="/board/board.es";
apiKCDCURL=apiKCDCURLBase<>apiKCDCURLBoard;
apiKCDCParamsCommon={"mid"->"a20501000000","bid"->"0015"};
apiKCDCParamKeyWord={"keyWord"->"\:cf54\:b85c\:b098\:bc14\:c774\:b7ec\:c2a4\:ac10\:c5fc\:c99d-19 \:ad6d\:b0b4 \:bc1c\:c0dd \:d604\:d669"};
apiKCDCParamPage[n_]:={"nPage"->n};
apiKCDCParamsPost[n_]:={"act"->"view","list_no"->n,"nPage"->1};
apiKCDCMaxLookback:=5


(* ::Section:: *)
(*Cache Posts*)


Print["Read KCDC public board..."];


raw=URLExecute[
	apiKCDCURL,
	Join[apiKCDCParamsCommon, apiKCDCParamKeyWord, apiKCDCParamPage[#]
],"Source"]& /@ Range[apiKCDCMaxLookback];


postList=Flatten[
	Flatten[
		StringCases[
			Quiet@Flatten[StringCases[ImportString[#,"Hyperlinks"],apiKCDCURLBoard<>__]],
			"list_no="~~(x:DigitCharacter..)~~"&":>x]
	]&/@raw
];


Print["Read posts and cache..."];


crawlPosts[x_]:=Module[{html},
	If[!FileExistsQ[$cacheDirectory<>#<>".html"],
	html=URLExecute[apiKCDCURL,Join[apiKCDCParamsCommon,apiKCDCParamsPost[#]],"Source"];
	Export[$cacheDirectory<>#<>".html",html,"Text",CharacterEncoding-> "UTF8"]]&/@x
];


crawlPosts[postList];


(* ::Section:: *)
(*Preprocess Data*)


(* ::Subsection:: *)
(*Transform*)


Print["Process data..."];


(* ::Subsubsection:: *)
(*Extract and Process Table Rows*)


dataRow[name_]:=
Module[{data,pos,rows},
data=Import[name,"Data"];
pos=Position[data,
	{" \:ad6c\:bd84"," \:cd1d\:acc4"," \:d655\:c9c4\:d658\:c790"," \:ac80\:c0ac\:d604\:d669"}|{" \:ad6c\:bd84"," \:cd1d\:acc4"," \:d655\:c9c4\:d658\:c790\:d604\:d669"," \:ac80\:c0ac\:d604\:d669"}];
	rows=Extract[data,Most[First[pos]]][[3;;-1]];
	If[Length[rows]==6,(* \:c0ac\:b9dd\:c790 \:c804 \:d3ec\:ba67 2 rows *)
		{Join[rows[[1,1;;3]],rows[[2]],{"0"},rows[[1,4;;-1]]],Join[rows[[3,1;;3]],rows[[4]],{"0"},rows[[3,4;;-1]]]},
		If[Length[rows[[1]]]==8,
			{Join[rows[[1,1;;5]],{"0"},rows[[1,6;;-1]]],Join[rows[[2,1;;5]],{"0"},rows[[2,6;;-1]]]},
			Most[rows] (* \:c0ac\:b9dd\:c790 \:d6c4 \:d3ec\:ba67 *)
		]
	]
];


dataPre1=Flatten[dataRow/@FileNames[$cacheDirectory<>"*.html"],1];


(* ::Subsubsection:: *)
(*Convert Values to Expressions*)


dataPre2=Map[
	Function[{x},
		Prepend[
			ToExpression[StringReplace[#,("*"|","|" ")->""]]&/@Rest[x],
			Quiet@DateObject[StringReplace[First[x],{".( "~~_~~" ) "->" "," \:c2dc \:ae30\:c900"->":00"}]]
		]
	], dataPre1
];


(* ::Subsubsection:: *)
(*Remove Duplicated Entries*)


data=Sort[DeleteDuplicates[dataPre2,First[#1]==First[#2]&]];


(* ::Subsection:: *)
(*Define Data Functions*)


Clear[getData];
colMap={"\:c2dc\:ac04"->1,"\:cd1d\:acc4"->2,"\:d655\:c9c4"->3,"\:aca9\:b9ac\:d574\:c81c"->4,"\:aca9\:b9ac\:c911"->5,"\:c0ac\:b9dd"->6,"\:ac80\:c0ac\:cd1d\:acc4"->7,"\:ac80\:c0ac\:c911"->8,"\:acb0\:acfc\:c74c\:c131"->9};
colType={"\:c2dc\:ac04"->DateObject,"\:cd1d\:acc4"->Integer,"\:d655\:c9c4"->Integer,"\:aca9\:b9ac\:d574\:c81c"->Integer,"\:aca9\:b9ac\:c911"->Integer,
	"\:c0ac\:b9dd"->Integer,"\:ac80\:c0ac\:cd1d\:acc4"->Integer,"\:ac80\:c0ac\:c911"->Integer,"\:acb0\:acfc\:c74c\:c131"->Integer};
colMapR=Reverse[colMap,1];
getData[col_,range_:All,table_:data]:=table[[range,col/.colMap]];


(* ::Subsection:: *)
(*Verify Data Types*)


If[And@@Flatten[Table[MatchQ[#,Blank[c/.colType]]&/@getData[c], {c, colMap[[All,1]]}]]=!=True,
	Print["Data error: The data format might be changed in KCDC site.\n"];
	Quit[]];


(* ::Subsection:: *)
(*Export Data as CSV*)


Export[$outputDirectory<>outputValue["data","filename"],
	AssociationThread[
		colMap[[All,1]]->Prepend[Rest[#],DateString@First[#]]]&/@data, (* For CSV, convert DateObject to date strings *)
	outputValue["data","type"]
];


(* ::Section:: *)
(*Visualization Definitions*)


(* ::Subsection:: *)
(*Color*)


getColor["\:d655\:c9c4"]=RGBColor[{0.9411764705882353, 0.4235294117647059, 0.00784313725490196, 1.}];
getColor["\:aca9\:b9ac\:c911"]=RGBColor[{0.9647058823529412, 0.7490196078431373, 0.15294117647058825`, 1.}];
getColor["\:aca9\:b9ac\:d574\:c81c"]=RGBColor[{0.2627450980392157, 0.5215686274509804, 0.9607843137254902, 1.}];
getColor["\:c0ac\:b9dd"]=RGBColor[{0.8352941176470589, 0.00784313725490196, 0.00392156862745098, 1.}];
getColor["\:ac80\:c0ac\:cd1d\:acc4"]=RGBColor[{0.9411764705882353, 0.4235294117647059, 0.00784313725490196, 1.}];
getColor["\:ac80\:c0ac\:c911"]=RGBColor[{0.9647058823529412, 0.7490196078431373, 0.15294117647058825`, 1.}];
getColor["\:acb0\:acfc\:c74c\:c131"]=RGBColor[{0.19607843137254902`, 0.7137254901960784, 0.4745098039215686, 1.}];


(* ::Subsection:: *)
(*Plot Styles*)


imageWidth=860;


imageDimensions={ImageSize->{imageWidth,Automatic},AspectRatio->1/2};


frameTickStyle=Directive[FontSize->14];


barLabelStyle={FontSize->12};
barLabelStyleSmall={FontSize->10};


legendFrame=(Framed[Style[#,14],Background->GrayLevel[1,.9],FrameStyle->LightGray,RoundingRadius->3]&);
placedLegendFrame=(Placed[#,{Left,Top},legendFrame]&);


(* ::Subsection:: *)
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


(* ::Section:: *)
(*\:c804\:ad6d \:cf54\:b85c\:b09819 \:c0c1\:d669*)


Clear[out1];
out1:=Pane[
Labeled[
Column[{
Spacer[20],
Row[Framed[Column[{#,Style[NumberForm[getData[#,-1],DigitBlock->3],40,Bold,Italic]},Alignment->Center],
	Background->getColor[#],Alignment->Center,ImageSize->{200,200},FrameStyle->None,RoundingRadius->10]&
	/@{"\:d655\:c9c4","\:aca9\:b9ac\:c911","\:aca9\:b9ac\:d574\:c81c","\:c0ac\:b9dd"},Spacer[10]],
Spacer[10],
Row[Framed[Column[{#,Style[NumberForm[getData[#,-1],DigitBlock->3],40,Bold,Italic]},Alignment->Center],
	Background->getColor[#],Alignment->Center,ImageSize->{273,140},FrameStyle->None,RoundingRadius->10]&
	/@{"\:ac80\:c0ac\:cd1d\:acc4","\:ac80\:c0ac\:c911","\:acb0\:acfc\:c74c\:c131"},Spacer[10]]
},BaseStyle->{FontFamily->"Overpass",FontSize->24,White},Spacings->0]
,plotLabelFun[outputValue["1","label"],Last@getData["\:c2dc\:ac04"]],Top],
ImageSize->{imageWidth,Automatic},FrameMargins->0];


(* ::Subsection:: *)
(*Export*)


Print["Generate "<>outputValue["1","label"]<>"..."];


(* ::Subsubsection:: *)
(*Korean*)


$locale="kr";


Export[$outputDirectory<>"kr/"<>outputValue["1","filename"],out1,outputValue["1","type"]];


(* ::Subsubsection:: *)
(*English*)


$locale="en";


Export[$outputDirectory<>"en/"<>outputValue["1","filename"],out1/.translateValue["en"],outputValue["1","type"]];


(* ::Section:: *)
(*\:c804\:ad6d \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:cd94\:c774*)


Clear[out2];
out2=Labeled[
Module[{
range=Span[10,-1],
legends={"\:d655\:c9c4","\:c0ac\:b9dd"},
labelInset,
todayInset,
currentInset
},

labelInset=Table[
With[{list=getData[{"\:c2dc\:ac04",d},20;;-1]},
(With[{x=AbsoluteTime@#[[1]],y=#[[2]]},
Inset[Style[If[y<4,"",y],barLabelStyle,Bold,getColor[d]],Offset[{-10,12},{x,y}]]]&)/@list[[-1;;1;;-2]]
],{d,legends}];

{todayInset,currentInset}=
With[{t=AbsoluteTime@getData["\:c2dc\:ac04",-1], y=getData["\:d655\:c9c4",-1]},{
	{getColor["\:d655\:c9c4"],Dashed,Line[{{t,0},{t,y}}]},
	Text[Framed[Style[Row[{DateString[getData["\:c2dc\:ac04",-1],{"MonthShort","/","DayShort"," ","Hour24Short"}],"\:c2dc"}],White,Bold,14],FrameStyle->None,Background->getColor["\:d655\:c9c4"],FrameMargins->{{8,8},{4,4}},RoundingRadius->4],Offset[{-10,0},{t,y/2}],Scaled[{1,0.5}]]
}];

DateListPlot[
getData[{"\:c2dc\:ac04",#},range]&/@legends,
InterpolationOrder->2,
PlotStyle->getColor/@legends,
PlotMarkers->"OpenMarkers",
PlotRange->{Automatic,All},
PlotRangePadding->{Scaled[0.01],{Automatic,Scaled[0.08]}},
Filling->Bottom,
DateTicksFormat->{"MonthShort","/","DayShort"},
FrameTicks->{{Automatic,None},{Automatic,None}},
FrameTicksStyle->frameTickStyle,
GridLines->Automatic,
PlotLegends->placedLegendFrame[{"\:b204\:c801\:d655\:c9c4\:c790","\:b204\:c801\:c0ac\:b9dd\:c790"}],
Prolog->todayInset,
Epilog->{labelInset,currentInset},
imageDimensions
]
]
,plotLabelFun[outputValue["2","label"],Last@getData["\:c2dc\:ac04"]],Top];


(* ::Subsection:: *)
(*Export*)


Print["Generate "<>outputValue["2","label"]<>"..."];


(* ::Subsubsection:: *)
(*Korean*)


$locale="kr";


Export[$outputDirectory<>"kr/"<>outputValue["2","filename"],out2,outputValue["2","type"]];


(* ::Subsubsection:: *)
(*English*)


$locale="en";


Export[$outputDirectory<>"en/"<>outputValue["2","filename"],out2/.translateValue["en"],outputValue["2","type"]];


(* ::Section:: *)
(*\:c804\:ad6d \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:bd84\:c11d*)


Clear[out3];
out3:=Labeled[
Module[{
range9=Flatten[Position[#["Hour"]&/@getData["\:c2dc\:ac04"],x_/;x<12]],(* \:c624\:c804 \:c77c\:c77c \:d604\:d669 \:b9cc \:cd94\:cd9c *)
legends={"\:c0ac\:b9dd","\:aca9\:b9ac\:c911","\:aca9\:b9ac\:d574\:c81c"},
dateTicks,
legendInset,
labelFun
},

dateTicks=MapIndexed[{First@#2,Rotate[DateString[#1,{"MonthShort","/","DayShort"}],45Degree]}&,getData["\:c2dc\:ac04",range9]];
legendInset=legendFrame@SwatchLegend[getColor/@legends,legends,LegendMarkers->Graphics[{EdgeForm[Opacity[.1]],Rectangle[]}],LegendMarkerSize->14];
labelFun=(With[{i=#2[[2]]},
If[i==2 (* \:c911\:ac04\:bc14 *),
Placed[Style[If[#1<200,"",#1],White,barLabelStyleSmall,Bold],Top],
Placed[Style[If[#1<4,"",#1],(getColor/@legends)[[i]],barLabelStyle,Bold],Above]
]
]
&);

SetOptions[BarChart,imageDimensions];

BarChart[Transpose[getData[#,range9]&/@legends],
ChartLayout->"Stacked",
ChartStyle->getColor/@legends,
LabelingFunction->labelFun,
Frame->True,
FrameTicks->{{Automatic,None},{dateTicks,None}},
FrameTicksStyle->frameTickStyle,
GridLines->Automatic,
Epilog->Inset[legendInset,Scaled[{0.025,0.975}],{Left,Top}]
]
]
,plotLabelFun[outputValue["3","label"],Last@getData["\:c2dc\:ac04"]],Top];


(* ::Subsection:: *)
(*Export*)


Print["Generate "<>outputValue["3","label"]<>"..."];


(* ::Subsubsection:: *)
(*Korean*)


$locale="kr";


Export[$outputDirectory<>"kr/"<>outputValue["3","filename"],out3,outputValue["3","type"]];


(* ::Subsubsection:: *)
(*English*)


$locale="en";


Export[$outputDirectory<>"en/"<>outputValue["3","filename"],out3/.translateValue["en"],outputValue["3","type"]];


(* ::Section:: *)
(*\:c804\:ad6d \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:c99d\:ac00 \:cd94\:c774*)


Clear[out4];
out4:=Labeled[
Module[{
range9=Flatten[Position[#["Hour"]&/@getData["\:c2dc\:ac04"],x_/;x<12]],(* \:c624\:c804 \:c77c\:c77c \:d604\:d669 \:b9cc \:cd94\:cd9c *)
dateTicks,
legendInset,
todayInset,
currentInset,
labelInset,
diff,
marker=Graphics[{{GrayLevel[1],Disk[{0,0},Offset[{3,3}]]},AbsoluteThickness[1.5],Dashing[{}],Circle[{0,0},Offset[{3,3}]]}]
},
dateTicks=MapIndexed[{First@#2,Rotate[DateString[#1,{"MonthShort","/","DayShort"}],45Degree]}&,getData["\:c2dc\:ac04",range9]];
dateTicks=Most@dateTicks;
diff=Differences[getData["\:d655\:c9c4",range9]];
{todayInset,currentInset}=With[{t=Length[dateTicks],y=Last@diff},{
{getColor["\:d655\:c9c4"],Dashed,Line[{{t,0},{t,y}}]},
Text[Framed[Style[Row[{DateString[getData["\:c2dc\:ac04",-1],{"MonthShort","/","DayShort"," ","Hour24Short"}],"\:c2dc"}],White,Bold,14],FrameStyle->None,Background->getColor["\:d655\:c9c4"],FrameMargins->{{8,8},{4,4}},RoundingRadius->4],Offset[{-10,0},{t,y/2}],Scaled[{1,0.5}]]
}];
labelInset=Table[With[{y=diff[[x]]},
Inset[Style[If[y<4,"",y],barLabelStyle,Bold,getColor["\:d655\:c9c4"]],Offset[{0,15},{x,y}]]],
{x,Range[Length[dateTicks]]}];

SetOptions[ListLinePlot,imageDimensions];

ListLinePlot[diff,
PlotStyle->getColor["\:d655\:c9c4"],
Frame->True,
FrameTicks->{{Automatic,None},{dateTicks,None}},
FrameTicksStyle->frameTickStyle,
GridLines->{Automatic,Automatic},
InterpolationOrder->2,
PlotMarkers->{marker},
Filling->Bottom,
Prolog->todayInset,
Epilog->{currentInset,labelInset}
]
]
,plotLabelFun[outputValue["4","label"],Last@getData["\:c2dc\:ac04"]],Top]


(* ::Subsection:: *)
(*Export*)


Print["Generate "<>outputValue["4","label"]<>"..."];


(* ::Subsubsection:: *)
(*Korean*)


$locale="kr";


Export[$outputDirectory<>"kr/"<>outputValue["4","filename"],out4,outputValue["4","type"]];


(* ::Subsubsection:: *)
(*English*)


$locale="en";


Export[$outputDirectory<>"en/"<>outputValue["4","filename"],out4/.translateValue["en"],outputValue["4","type"]];