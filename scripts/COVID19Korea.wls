#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Visualization*)
(*South Korea*)


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


$cacheDirectory=$baseDirectory<>"../data/cache-cdc-go-kr/";


$outputDirectory=$baseDirectory<>"../src/output/";


(* ::Subsection::Closed:: *)
(*Province Map*)


(* ::Text:: *)
(*VWorkd Open API: 2D Data API 2.0 (\:ad11\:c5ed\:c2dc\:b3c4)*)


(* http://www.vworld.kr/dev/v4dv_2ddataguide2_s002.do?svcIde=adsido *)


$vworldAPICred=Import[$configDirectory<>"vworld.key.json","JSON"];


$provinceNames={"\:c11c\:c6b8","\:bd80\:c0b0","\:b300\:ad6c","\:c778\:cc9c","\:ad11\:c8fc","\:b300\:c804","\:c6b8\:c0b0","\:c138\:c885","\:acbd\:ae30","\:ac15\:c6d0","\:cda9\:bd81","\:cda9\:b0a8","\:c804\:bd81","\:c804\:b0a8","\:acbd\:bd81","\:acbd\:b0a8","\:c81c\:c8fc"};


$provinceFullNames={"\:c11c\:c6b8\:d2b9\:bcc4\:c2dc","\:bd80\:c0b0\:ad11\:c5ed\:c2dc","\:b300\:ad6c\:ad11\:c5ed\:c2dc","\:c778\:cc9c\:ad11\:c5ed\:c2dc","\:ad11\:c8fc\:ad11\:c5ed\:c2dc","\:b300\:c804\:ad11\:c5ed\:c2dc","\:c6b8\:c0b0\:ad11\:c5ed\:c2dc","\:c138\:c885\:d2b9\:bcc4\:c790\:ce58\:c2dc","\:acbd\:ae30\:b3c4",
	"\:ac15\:c6d0\:b3c4","\:cda9\:ccad\:bd81\:b3c4","\:cda9\:ccad\:b0a8\:b3c4","\:c804\:b77c\:bd81\:b3c4","\:c804\:b77c\:b0a8\:b3c4","\:acbd\:c0c1\:bd81\:b3c4","\:acbd\:c0c1\:b0a8\:b3c4","\:c81c\:c8fc\:d2b9\:bcc4\:c790\:ce58\:b3c4"};


$provinceRules=MapThread[#1->#2&,{$provinceNames,$provinceFullNames}];


$centerFilName=$dataDirectory<>"korea-province-centers.json"; (* RegionCentroid \:c2dc\:ac04\:c774 \:c624\:b798\:ac78\:b9ac\:b294 \:ad00\:acc4\:b85c \:acc4\:c0b0 \:acb0\:acfc\:b97c \:c800\:c7a5\:d568 *)


(* ::Subsection::Closed:: *)
(*Output & Translation*)


$outputRules=Import[$configDirectory<>"korea-corona19.json"];


$translateRules=Import[$dataDirectory<>"translate.json"];


Clear[value,valueR];
valueR[keys_,assoc_]:=If[Rest@keys=={},Lookup[assoc,First@keys],valueR[Rest@keys,Lookup[assoc,First@keys]]];
value[keys___][assoc_]:=valueR[{keys},assoc];


outputValue[k___]:=value[k][$outputRules]
translateValue[k___]:=value[k][$translateRules]


(* ::Section::Closed:: *)
(*Source*)


(* ::Subsection:: *)
(*Source URL*)


(* ::Text:: *)
(*\:c9c8\:bcd1\:ad00\:b9ac\:bcf8\:bd80 (KCDC) \:ac8c\:c2dc\:d310*)


apiKCDCURLBase="https://www.cdc.go.kr";
apiKCDCURLBoard="/board/board.es";
apiKCDCURL=apiKCDCURLBase<>apiKCDCURLBoard;
apiKCDCParamsCommon={"mid"->"a20501000000","bid"->"0015"};
apiKCDCParamKeyWord={"keyWord"->"\:cf54\:b85c\:b098\:bc14\:c774\:b7ec\:c2a4\:ac10\:c5fc\:c99d-19","keyField"->"TITLE"};
apiKCDCParamPage[n_]:={"nPage"->n};
apiKCDCParamsPost[n_]:={"act"->"view","list_no"->n,"nPage"->1};
apiKCDCMaxLookback:=5


(* ::Subsection:: *)
(*Read Boards, Import Posts and Cache*)


Print["Read KCDC public board..."];


newFileAdded=False;
newFileNames={};


raw=URLExecute[
	apiKCDCURL,
	Join[apiKCDCParamsCommon, apiKCDCParamKeyWord, apiKCDCParamPage[#]
],"Source",CharacterEncoding->"UTF8"]& /@ Range[apiKCDCMaxLookback];


postList=Flatten[
	Flatten[
		StringCases[
			Quiet@Flatten[StringCases[ImportString[#,"Hyperlinks"],apiKCDCURLBoard<>__]],
			"list_no="~~(x:DigitCharacter..)~~"&":>x]
	]&/@raw
];


Print["Read posts and cache..."];


crawlPosts[x_]:=Module[{html},(If[!FileExistsQ[$cacheDirectory<>#1<>".html"],newFileAdded=True;Print["New file added: "<>#1<>".html"];AppendTo[newFileNames,$cacheDirectory<>#1<>".html"];html=URLExecute[apiKCDCURL,Join[apiKCDCParamsCommon,apiKCDCParamsPost[#1]],"Source"];Export[$cacheDirectory<>#1<>".html",html,"Text",CharacterEncoding->"UTF8"]]&)/@x];


crawlPosts[postList];


(* ::Section::Closed:: *)
(*Preprocess Data*)


(* ::Subsection:: *)
(*Load All Caches*)


(* To manually process files already caches, add file names to newFineNames *)


(* And set newFileAdded to True *)


(* Eg: newFileNames=FileNames[All,$cacheDirectory][[-n;;-1]]; newFileAdded=True; *)


newCaches=Import[#,"String"]&/@newFileNames;


dataOldRaw=Import[$outputDirectory<>"kr/"<>outputValue["data","filename"],
	outputValue["data","type"]
];
dataOld=MapThread[{#1,Sequence@@#2}&,
	{DateObject/@dataOldRaw[[All,1,2]],dataOldRaw[[All,2;;-1,2]]}];


pvDataOldRaw=Import[$outputDirectory<>"kr/"<>outputValue["data-province","filename"],
	outputValue["data-province","type"]
];
pvDataOld=MapThread[{#1,Sequence@@#2}&,
	{DateObject/@pvDataOldRaw[[All,1,2]],pvDataOldRaw[[All,2;;-1,2]]}];


(* ::Subsection:: *)
(*Nationwide Data*)


Print["Process nationwide data..."];


(* ::Subsubsection:: *)
(*Transform Function*)


numerify[x_]:=ToExpression[StringReplace[x,("*"|","|" ")->""]]


Clear[dataRow];
dataRow[name_]:=
Module[{data,pos,rows,newflag=False},
	data=ImportString[name,"Data"];
	pos=Position[data,
	{" \:ad6c\:bd84"," \:cd1d\:acc4"," \:d655\:c9c4\:d658\:c790"," \:ac80\:c0ac\:d604\:d669"}|{" \:ad6c\:bd84"," \:cd1d\:acc4"," \:d655\:c9c4\:d658\:c790\:d604\:d669"," \:ac80\:c0ac\:d604\:d669"}];(* Two old formats *)
	If[pos=={},pos=Position[data,{" \:ad6c\:bd84"," \:cd1d\:acc4"," \:acb0\:acfc \:c591\:c131"," \:ac80\:c0ac \:c911"," \:acb0\:acfc \:c74c\:c131"}];newflag=True];(* They changed format again starting from 3/14 *)
	If[pos=={},Print["Data error: The data format might be changed in KCDC site.\n"];Quit[]];
	rows=Extract[data,Most[First[pos]]][[3;;-1]];
	If[Length[rows]==6,(* \:c0ac\:b9dd\:c790 \:c804 \:d3ec\:ba67 2 rows *)
		{Join[rows[[1,1;;3]],rows[[2]],{"0"},rows[[1,4;;-1]]],Join[rows[[3,1;;3]],rows[[4]],{"0"},rows[[3,4;;-1]]]},
		If[Length[rows[[1]]]==8,
			If[newflag,
				With[{sum=numerify/@Most[rows][[All,-1]]+numerify/@Most[rows][[All,-2]]}, (* Calculate total tested *)
					MapThread[Join[#1[[1;;6]],{ToString@#2},#1[[-2;;-1]]]&,{Most[rows],sum}]]
				,(* \:c0ac\:b9dd\:c790 \:d6c4 \:d3ec\:ba67 after 3/14 *)
				{Join[rows[[1,1;;5]],{"0"},rows[[1,6;;-1]]],Join[rows[[2,1;;5]],{"0"},rows[[2,6;;-1]]]}(* \:c0ac\:b9dd\:c790 \:c804 \:d3ec\:ba67 3 rows *)
			],
			Most[rows](* \:c0ac\:b9dd\:c790 \:d6c4 \:d3ec\:ba67 *)
		]	
	]
];


(* ::Subsubsection:: *)
(*Transform new cache data*)


If[newFileAdded,
	dataPre1=Flatten[dataRow/@newCaches,1];
	(* Convert values to expressions *)
	dataPre2=Map[
	Function[{x},
		Prepend[
			numerify/@Rest[x],
			Quiet@DateObject[StringReplace[First[x],{".( "~~_~~" ) "->" "," \:c2dc \:ae30\:c900"->":00"}]]
		]
	], dataPre1
	];
	(* Merge & remove duplicated entries *)
	data=Sort[DeleteDuplicates[Join[dataOld,dataPre2],First[#1]==First[#2]&]],
	(* No new data *)
	data=dataOld
];


(* ::Subsubsection:: *)
(*Extract Functions*)


Clear[getData];
colMap={"\:c2dc\:ac04"->1,"\:cd1d\:acc4"->2,"\:d655\:c9c4"->3,"\:aca9\:b9ac\:d574\:c81c"->4,"\:aca9\:b9ac\:c911"->5,"\:c0ac\:b9dd"->6,"\:ac80\:c0ac\:cd1d\:acc4"->7,"\:ac80\:c0ac\:c911"->8,"\:acb0\:acfc\:c74c\:c131"->9};
colType={"\:c2dc\:ac04"->DateObject,"\:cd1d\:acc4"->Integer,"\:d655\:c9c4"->Integer,"\:aca9\:b9ac\:d574\:c81c"->Integer,"\:aca9\:b9ac\:c911"->Integer,
	"\:c0ac\:b9dd"->Integer,"\:ac80\:c0ac\:cd1d\:acc4"->Integer,"\:ac80\:c0ac\:c911"->Integer,"\:acb0\:acfc\:c74c\:c131"->Integer};
colMapR=Reverse[colMap,1];
getData[col_,range_:All,table_:data]:=table[[range,col/.colMap]];


(* ::Subsubsection:: *)
(*Verify Processed Data*)


If[And@@Flatten[Table[MatchQ[#,Blank[c/.colType]]&/@getData[c], {c, colMap[[All,1]]}]]=!=True,
	Print["Data error: The data format might be changed in KCDC site.\n"];
	Quit[]];


(* ::Subsubsection:: *)
(*Export Processed Data*)


If[newFileAdded,
	(* For CSV, convert DateObject to date strings *)
	exportData=Normal@AssociationThread[
		colMap[[All,1]]->Prepend[Rest[#],DateString@First[#]]]&/@data;
	(* Korean *)
	Export[$outputDirectory<>"kr/"<>outputValue["data","filename"],
		exportData, outputValue["data","type"]];
	(* English *)
	Export[$outputDirectory<>"en/"<>outputValue["data","filename"],
		exportData/.translateValue["en"],outputValue["data","type"]];
];


(* ::Subsection:: *)
(*Province Data*)


Print["Process province data..."];


(* ::Subsubsection:: *)
(*Transform Function*)


Clear[pvFilter];
pvFilter[cache_]:=With[{title=ImportString[cache,"Title"]},
If[Length@StringCases[title,"\:c815\:b840\:be0c\:b9ac\:d551"]>0 || Length@StringCases[title,"0\:c2dc \:ae30\:c900"]>0,
Module[{date,do,data,pos,table,header,r,c,col,values,assoc},
date=First@StringCases[title,(x:DigitCharacter..)~~"\:c6d4"~~(" "...)~~(y:DigitCharacter..)~~"\:c77c":>x<>"-"<>y]; (* \:b0a0\:c9dc *)
do=DateObject@date;
(* \:d14c\:c774\:be14 *)
data=ImportString[cache,"Data"];
pos=Position[data,x_/;ContainsAll[{"\:d569\:acc4","\:ad6c\:bd84","\:ac80\:c5ed"}~Join~$provinceNames,StringReplace[x," ":>""]]]; (* Check horizontal table header *)
If[pos=={},(* Try vertical *)
pos=Position[data,x_/;ContainsAll[{"\:c9c0\:c5ed","\:d655\:c9c4\:d658\:c790","\:be44\:ace0","\:c8fc\:c694\:ae30\:d0c0\:c720\:d589"},StringReplace[x," ":>""]]];
If[pos=={},
None,
(* Process vertical table *)
pos=First@pos;
table=Part[data,Sequence@@Most@pos];
header=StringReplace[First@#," ":>""]&/@table;
values=ToExpression[StringReplace[Part[#,2],{" ":>"",",":>""}]]&/@table;(* Assuming that 2nd column is the values *)
assoc=AssociationThread[$provinceNames->Lookup[AssociationThread[header->values],$provinceNames]];
{do,Sequence@@Values[assoc]}
],
(* Process horizontal table *)
pos=First@pos;
table=Part[data,Sequence@@Most@pos];
header=StringReplace[Part[data,Sequence@@pos]," ":>""];
c=First@First@Position[header,"\:d569\:acc4"];
col=Quiet[ToExpression/@((StringReplace[Part[#,c],",":>""])&/@table)];
r=First@First@Position[col,First@Max[col]];
values=ToExpression[StringReplace[#,",":>""]]&/@table[[r]];
assoc=AssociationThread[$provinceNames->Lookup[AssociationThread[header->values],$provinceNames]];
{do,Sequence@@Values[assoc]}
]
],
None]
];


(* ::Subsubsection:: *)
(*Transform new cache data*)


If[newFileAdded,
	(* Clean up missing entries *)
	pvDataPre=DeleteDuplicates@ReplaceAll[DeleteCases[Quiet[pvFilter/@newCaches],None],_Missing:>0];
	(* Add "Total" column *)
	pvDataNew={First@#,Sequence@@(Rest@#),Total[Rest@#]}&/@pvDataPre;
	(* Merge old & new *)
	pvData=Join[pvDataOld,pvDataNew],
	(* No new data *)
	pvData=pvDataOld
];


pvData=Sort[pvData];


(* ::Subsubsection:: *)
(*Extract Functions*)


Clear[pvGetData];
pvColMap={"\:c2dc\:ac04"->1,Sequence@@MapIndexed[#1->(First@#2+1)&,$provinceNames],"\:c804\:ad6d"->-1};
pvGetData[col_,range_:All,table_:pvData]:=table[[range,col/.pvColMap]];


(* ::Subsubsection:: *)
(*Export Processed Data*)


If[newFileAdded,
	(* For CSV, convert DateObject to date strings *)
	exportPV=Normal@AssociationThread[
		pvColMap[[All,1]]->Prepend[Rest[#],DateString@First[#]]]&/@pvData;
	(* Korean *)
	Export[$outputDirectory<>"kr/"<>outputValue["data-province","filename"],
		exportPV,outputValue["data-province","type"]];
	(* English *)
	Export[$outputDirectory<>"en/"<>outputValue["data-province","filename"],
	exportPV/.translateValue["en"], outputValue["data-province","type"]]
];


(* ::Section::Closed:: *)
(*Visualization Functions*)


(* ::Subsection::Closed:: *)
(*VWorld Open API*)


vworldAPIURL="http://api.vworld.kr/req/data";
vworldAPIParams={"service"->"data","request"->"GetFeature","data"->"LT_C_ADSIDO_INFO"};


(* \:c9c0\:c5ed\:ba85 \:c911\:bcf5 \:bb38\:c81c\:b85c full \:d589\:c815\:ad6c\:c5ed\:ba85 \:c0ac\:c6a9. \:adf8\:b9ac\:ace0 \:c77c\:bd80 \:d589\:c815\:ad6c\:c5ed\:c740 \:bd84\:b9ac\:b418\:c5b4 \:c788\:c73c\:bbc0\:b85c :like: \:c0ac\:c6a9 *)


vworldAPICall[prov_]:=URLExecute[vworldAPIURL,Join[$vworldAPICred,vworldAPIParams,{"attrFilter"->"ctp_kor_nm:like:"<>prov}]];


(* ::Subsection::Closed:: *)
(*Province Coordinates & Centers*)


Clear[provinceCoords];
Clear[provinceShape];
Clear[provinceCenter];
Clear[provinceBBox];


provinceCoords[prov_]:=provinceCoords[prov]=Cases[vworldAPICall[prov/.$provinceRules],HoldPattern["coordinates"->x_]:>x,Infinity];


provinceShape[prov_]:=provinceShape[prov]=Reverse@Sort@Flatten@Table[Polygon/@x,{x,provinceCoords[prov]}];


provinceBBox[prov_]:=provinceBBox[prov]=RegionBounds[First@provinceShape[prov]];


If[!FileExistsQ[$centerFilName],
	provinceCenter[prov_]:=provinceCenter[prov]=.5*{Plus@@First@provinceBBox[prov],Plus@@Last@provinceBBox[prov]}+
		Switch[prov,"\:acbd\:ae30",{0.2,-0.2},"\:cda9\:bd81",{-0.2,0.2},_,{0,0}];
	Export[$centerFilName,MapThread[Rule,{$provinceNames,provinceCenter/@$provinceNames}]]
];


centerRules=Import[$centerFilName];


provinceCenter[dist_]:=dist/.centerRules;


(* ::Subsection::Closed:: *)
(*Colors*)


getColor["\:d655\:c9c4"]=RGBColor[{0.9647058823529412, 0.7490196078431373, 0.15294117647058825`, 1.}];
getColor["\:aca9\:b9ac\:c911"]=RGBColor[{0.9411764705882353, 0.4235294117647059, 0.00784313725490196, 1.}];
getColor["\:aca9\:b9ac\:d574\:c81c"]=RGBColor[{0.2627450980392157, 0.5215686274509804, 0.9607843137254902, 1.}];
getColor["\:c0ac\:b9dd"]=RGBColor[{0.8352941176470589, 0.00784313725490196, 0.00392156862745098, 1.}];
getColor["\:ac80\:c0ac\:cd1d\:acc4"]=getColor["\:aca9\:b9ac\:d574\:c81c"];
getColor["\:ac80\:c0ac\:c911"]=getColor["\:d655\:c9c4"];
getColor["\:acb0\:acfc\:c74c\:c131"]=RGBColor[{0.19607843137254902`, 0.7137254901960784, 0.4745098039215686, 1.}];


(* ::Subsection::Closed:: *)
(*Plot Styles*)


imageWidth=860;


imageDimensions={ImageSize->{imageWidth,Automatic},AspectRatio->1/2};


frameTickStyle=Directive[FontSize->14];


barLabelStyle={FontSize->12};
barLabelStyleSmall={FontSize->10};


legendFrame=(Framed[Style[#,14,FontColor->GrayLevel[0.6]],Background->GrayLevel[1,.1],FrameStyle->GrayLevel[1,.15],RoundingRadius->3]&);
placedLegendFrame=(Placed[LineLegend[#,LegendFunction->legendFrame],{Scaled[{0.025,0.975}],{Left,Top}}]&);


(* ::Subsection::Closed:: *)
(*Label Function*)


$locale="kr";


plotLabelFun[title_,time_:CurrentDate[]]:=Column[{
	Style[title,32,GrayLevel[.6]],
	Style[Row[{
		StringReplace[DateString[time,{"MonthShort","/","DayShort"," (","DayNameShort",") "}],
			Switch[$locale,
				"en", {},
				"kr", {"Mon"->"\:c6d4","Tue"->"\:d654","Wed"->"\:c218","Thu"->"\:baa9","Fri"->"\:ae08","Sat"->"\:d1a0","Sun"->"\:c77c"}]
			],"\:c5c5\:b370\:c774\:d2b8"}],
		20,GrayLevel[.8]
	],
	Spacer[{0,16}]
},Alignment->Center,Spacings->{0,2},BaseStyle->{FontFamily->"Gothic A1",FontWeight->"Light"}];


(* ::Subsection::Closed:: *)
(*Province Heat Map*)


Clear[provinceHeatmap]
provinceHeatmap[col_,OptionsPattern[{
	ScalingFunctions->Log,
	ImageSize->480,AspectRatio->1/2,LegendLabel->"Legend",
	LabelStyle->{FontSize->12},Min->-1,Max->-1}]]:=
Module[{
min=OptionValue[Min],
max=OptionValue[Max],
imageW=OptionValue[ImageSize],
labelS=OptionValue[LabelStyle],
f=OptionValue[ScalingFunctions],
tally,
regions,
labels,
others
},
tally=MapThread[{#1,#2}&,{$provinceNames,pvData[[col,2;;-2]]}];
others=Complement[$provinceNames,tally[[All,1]]];
min=If[min==-1,Min@tally[[All,2]],min];
max=If[max==-1,Max@tally[[All,2]],max];

regions=With[{s=#[[1]],n=#[[2]]},
With[{p=(f[n]-f[min])/(f[max]-f[min])},
Module[{shape},
	shape={EdgeForm[],ColorData["CandyColors",1-p],First@provinceShape[s]};
	If[s==="\:c81c\:c8fc",
		AppendTo[shape,{GrayLevel[.4],Line[{{126.162,33.75},{127.1,33.75},{127.1,33.1}}]}];
		shape=Translate[shape,{-1.5,1}]
	];
	shape
]
]
]&/@tally;

labels=With[{s=#[[1]],n=#[[2]]},
With[{p=(f[n]-f[min])/(f[max]-f[min])},
Module[{shape},
	shape={Text[Style[Column[{s,Style[n,Smaller,Plain]},Alignment->Center,Spacings->0],
			labelS,GrayLevel[If[p>0.4,1,0.2]]],provinceCenter[s]]};
	If[s==="\:c81c\:c8fc",
		shape=Translate[shape,{-1.5,1}]
	];
	shape
]
]
]&/@tally;

Overlay[{
Pane[
Graphics[{
regions,
EdgeForm[GrayLevel[0,0.1]],FaceForm[],(First@provinceShape[#]&)/@Cases[$provinceNames,Except["\:c81c\:c8fc"]],
labels,
Text[Style[#,labelS,GrayLevel[0.2]],provinceCenter[#]]&/@others
},ImageSize->{Automatic,imageW*OptionValue[AspectRatio]}],
ImageSize->imageW,Alignment->Center
],

Grid[{
{min,
Show[
DensityPlot[1-((f[x]-f[min])/(f[max]-f[min])),{x,min,max},{y,0,1},
	PlotRange->{{min,max},{0,1}},AspectRatio->\!\(TraditionalForm\`
\*FractionBox[\(1\), \(10\)]\),ColorFunction->"CandyColors",MaxRecursion->0,PlotPoints->200,Frame->False],
ImageSize->{imageW/4,imageW/40}],
max},
{"",OptionValue["LegendLabel"],""}
},Alignment->{Center,Center},BaseStyle->{"Text",FontColor->GrayLevel[.6],FontSize->12}]
},Alignment->{Right,Top}]
]


(* ::Section::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Section::Closed:: *)
(*\:c804\:ad6d \:cf54\:b85c\:b09819 \:ac80\:c0ac*)


Clear[out2];
out2:=Labeled[
Module[{
range=Flatten[Position[#["Hour"]&/@getData["\:c2dc\:ac04"],x_/;x<12]],(* \:c624\:c804 \:c77c\:c77c \:d604\:d669 \:b9cc \:cd94\:cd9c *)
legends={"\:ac80\:c0ac\:cd1d\:acc4","\:ac80\:c0ac\:c911"},
labelInset,
todayInset,
currentInset,
dateTicks
},
dateTicks=Cases[getData["\:c2dc\:ac04",range],x_/;(DateString[x,"DayName"]==="Monday")];

labelInset=Table[
With[{list=getData[{"\:c2dc\:ac04",d},range]},
(With[{x=AbsoluteTime@#[[1]],y=#[[2]]},
{Inset[Style[If[y<55,"",y],Bold,barLabelStyle,getColor[d]],Offset[{-10,15},{x,y}]],
AbsolutePointSize[4],getColor[d],Point[{x,y}]}
]&)/@list[[-1;;-50;;-4]]
],{d,legends}];(* Labeling cutoff increased to 55 *)

{todayInset,currentInset}=
With[{t=AbsoluteTime@getData["\:c2dc\:ac04",-1], y=getData[legends[[1]],-1],yr=getData[legends[[2]],-1]},{
	{getColor[legends[[1]]],Dashed,Line[{{t,0},{t,y}}]},
	Text[Framed[Style[DateString[getData["\:c2dc\:ac04",-1],{"MonthShort","/","DayShort"}],White,Bold,14],
		FrameStyle->None,Background->GrayLevel[.1],
		FrameMargins->{{8,8},{4,4}},
		RoundingRadius->4],Offset[{-5,0},{t,(y+yr)/2}],Scaled[{1,0.5}]]
}];

DateListPlot[
getData[{"\:c2dc\:ac04",#},range]&/@legends,
InterpolationOrder->2,
PlotStyle->getColor/@legends,
PlotRange->{Automatic,All},
PlotRangePadding->{Scaled[0.01],{Automatic,Scaled[0.08]}},
PlotRangeClipping->False,
DateTicksFormat->{"MonthShort","/","DayShort"},
FrameTicks->{{Automatic,None},{dateTicks,None}},
FrameTicksStyle->frameTickStyle,
GridLines->Automatic,
PlotLegends->placedLegendFrame[legends],
Prolog->todayInset,
Epilog->{labelInset,currentInset},
Filling->{1->{2},2->Bottom},
imageDimensions
]
]
,plotLabelFun[outputValue["2","label"],Last@getData["\:c2dc\:ac04"]],Top];


(* ::Subsection::Closed:: *)
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


(* ::Section::Closed:: *)
(*\:c804\:ad6d \:cf54\:b85c\:b09819 \:d655\:c9c4*)


Clear[out3];
out3:=Labeled[
Module[{
range=Flatten[Position[#["Hour"]&/@getData["\:c2dc\:ac04"],x_/;x<12]],(* \:c624\:c804 \:c77c\:c77c \:d604\:d669 \:b9cc \:cd94\:cd9c *)
legends={"\:d655\:c9c4","\:aca9\:b9ac\:c911","\:c0ac\:b9dd"},
labelInset,
todayInset,
currentInset,
dateTicks
},
dateTicks=Cases[getData["\:c2dc\:ac04",range],x_/;(DateString[x,"DayName"]==="Monday")];

labelInset=Table[
With[{list=getData[{"\:c2dc\:ac04",d},range]},
(With[{x=AbsoluteTime@#[[1]],y=#[[2]]},
{Inset[Style[If[y<55,"",y],Bold,barLabelStyle,getColor[d]],Offset[{-10,15},{x,y}]],
AbsolutePointSize[4],getColor[d],Point[{x,y}]}
]&)/@list[[-1;;-40;;-4]]
],{d,legends}];(* Labeling cutoff increased to 55 *)

{todayInset,currentInset}=
With[{t=AbsoluteTime@getData["\:c2dc\:ac04",-1], y=getData[legends[[1]],-1],yr=getData[legends[[2]],-1]},{
	{getColor[legends[[1]]],Dashed,Line[{{t,0},{t,y}}]},
	Text[Framed[Style[DateString[getData["\:c2dc\:ac04",-1],{"MonthShort","/","DayShort"}],White,Bold,14],
		FrameStyle->None,Background->GrayLevel[.1],
		FrameMargins->{{8,8},{4,4}},
		RoundingRadius->4],Offset[{-5,0},{t,(y+yr)/2}],Scaled[{1,0.5}]]
}];

DateListPlot[
getData[{"\:c2dc\:ac04",#},range]&/@legends,
InterpolationOrder->2,
PlotStyle->getColor/@legends,
PlotRange->{Automatic,All},
PlotRangePadding->{Scaled[0.01],{Automatic,Scaled[0.08]}},
PlotRangeClipping->False,
DateTicksFormat->{"MonthShort","/","DayShort"},
FrameTicks->{{Automatic,None},{dateTicks,None}},
FrameTicksStyle->frameTickStyle,
GridLines->Automatic,
PlotLegends->placedLegendFrame[legends],
Prolog->todayInset,
Epilog->{labelInset,currentInset},
Filling->{1->{2},2->{3},3->Bottom},
imageDimensions
]
]
,plotLabelFun[outputValue["3","label"],Last@getData["\:c2dc\:ac04"]],Top];


(* ::Subsection::Closed:: *)
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


(* ::Section::Closed:: *)
(*\:c804\:ad6d \:cf54\:b85c\:b09819 \:d655\:c9c4\:c790 \:c99d\:ac00*)


Clear[out4];
out4:=Labeled[
Module[{
range=Flatten[Position[#["Hour"]&/@getData["\:c2dc\:ac04"],x_/;x<12]],(* \:c624\:c804 \:c77c\:c77c \:d604\:d669 \:b9cc \:cd94\:cd9c *)
legend="\:d655\:c9c4",
labelInset,
todayInset,
currentInset,
dateTicks,
data
},
data=Transpose@{getData["\:c2dc\:ac04",range],Prepend[Differences[getData[legend,range]],0]};
dateTicks=Cases[getData["\:c2dc\:ac04",range],x_/;(DateString[x,"DayName"]==="Monday")];

labelInset=
(With[{x=AbsoluteTime@data[[All,1]][[#[[1]]]],y=#[[2]]},
{Inset[Style[y,Bold,barLabelStyle,getColor[legend]],Offset[{0,15},{x,y}]],
AbsolutePointSize[4],getColor[legend],Point[{x,y}]}
]&)/@Cases[FindPeaks[data[[All,2]],0,0,-Infinity],{i_,y_}/;IntegerQ[i]==True];

{todayInset,currentInset}=
With[{t=AbsoluteTime@getData["\:c2dc\:ac04",-1], y=data[[-1,2]],yr=data[[-1,2]]+500},{
	{GrayLevel[.7],Dashed,Line[{{t,0},Scaled[{0,1},{t,0}]}]},
	Text[Framed[Style[DateString[getData["\:c2dc\:ac04",-1],{"MonthShort","/","DayShort"}],White,Bold,14],
		FrameStyle->None,Background->GrayLevel[.1],
		FrameMargins->{{8,8},{4,4}},
		RoundingRadius->4],Offset[{-5,0},Scaled[{0,0.5},{t,0}]],Scaled[{1,0.5}]]
}];

DateListPlot[
Transpose@{getData["\:c2dc\:ac04",range],Prepend[Differences[getData[legend,range]],0]},
InterpolationOrder->2,
PlotStyle->getColor[legend],
PlotRange->{Automatic,All},
PlotRangePadding->{Scaled[0.01],{Automatic,Scaled[0.08]}},
PlotRangeClipping->False,
DateTicksFormat->{"MonthShort","/","DayShort"},
FrameTicks->{{Automatic,None},{dateTicks,None}},
FrameTicksStyle->frameTickStyle,
GridLines->Automatic,
Prolog->todayInset,
Epilog->{labelInset,currentInset},
Filling->{1->Bottom},
imageDimensions
]
]
,plotLabelFun[outputValue["4","label"],Last@getData["\:c2dc\:ac04"]],Top];


(* ::Subsection::Closed:: *)
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


(* ::Section::Closed:: *)
(*\:c804\:ad6d \:cf54\:b85c\:b09819 \:c9c0\:c5ed \:d655\:c9c4\:c790*)


out5:=Magnify[Labeled[
provinceHeatmap[
-1,
ImageSize->imageWidth,
LabelStyle->{FontSize->10},
LegendLabel->Style["\:d655 \:c9c4 \:c790",14]
],
plotLabelFun[outputValue["5","label"],Last@pvGetData["\:c2dc\:ac04"]],Top],2]


(* ::Subsection:: *)
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
