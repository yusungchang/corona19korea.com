#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Visualization*)
(*South Korea Provinces*)


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
(*District Map Configuration*)


(* ::Text:: *)
(*VWorkd Open API: 2D Data API 2.0 (\:ad11\:c5ed\:c2dc\:b3c4)*)


(* http://www.vworld.kr/dev/v4dv_2ddataguide2_s002.do?svcIde=adsido *)


$vworldAPICred=Import[$configDirectory<>"vworld.key.json","JSON"];


$provinceNames={"\:c11c\:c6b8","\:bd80\:c0b0","\:b300\:ad6c","\:c778\:cc9c","\:ad11\:c8fc","\:b300\:c804","\:c6b8\:c0b0","\:c138\:c885","\:acbd\:ae30","\:ac15\:c6d0","\:cda9\:bd81","\:cda9\:b0a8","\:c804\:bd81","\:c804\:b0a8","\:acbd\:bd81","\:acbd\:b0a8","\:c81c\:c8fc"};


(* ::Subsection:: *)
(*Import Output Configuration*)


$outputRules=Import[$dataDirectory<>"seoul-corona19.json"];


$translateRules=Import[$dataDirectory<>"translate.json"];


Clear[value,valueR];
valueR[keys_,assoc_]:=If[Rest@keys=={},Lookup[assoc,First@keys],valueR[Rest@keys,Lookup[assoc,First@keys]]];
value[keys___][assoc_]:=valueR[{keys},assoc];


outputValue[k___]:=value[k][$outputRules]
translateValue[k___]:=value[k][$translateRules]


(* ::Section:: *)
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
