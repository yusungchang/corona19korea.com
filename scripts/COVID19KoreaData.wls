#!/usr/bin/env wolframscript -print -format CSV
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*South Korea*)


(* ::Subtitle:: *)
(*Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Initialization*)


apiKCDCURLBase="https://www.cdc.go.kr";
apiKCDCURLBoard="/board/board.es";
apiKCDCURL=apiKCDCURLBase<>apiKCDCURLBoard;
apiKCDCParamsCommon={"mid"->"a20501000000","bid"->"0015"};
apiKCDCParamKeyWord={"keyWord"->"\:cf54\:b85c\:b098\:bc14\:c774\:b7ec\:c2a4\:ac10\:c5fc\:c99d-19 \:ad6d\:b0b4 \:bc1c\:c0dd \:d604\:d669"};
apiKCDCParamPage[n_]:={"nPage"->n};
apiKCDCParamsPost[n_]:={"act"->"view","list_no"->n,"nPage"->1};


cacheDirectory=Directory[]<>"/../data/cache/";
outputDirectory=Directory[]<>"/../src/output/";


(* ::Section:: *)
(*Post to Cache*)


raw=URLExecute[apiKCDCURL,Join[apiKCDCParamsCommon,apiKCDCParamKeyWord,apiKCDCParamPage[#]],"Source"]&/@Range[5];


postList=Flatten[Flatten[StringCases[Quiet@Flatten[StringCases[ImportString[#,"Hyperlinks"],apiKCDCURLBoard<>__]],"list_no="~~(x:DigitCharacter..)~~"&":>x]]&/@raw];


crawlPosts[x_]:=Module[{html},
If[!FileExistsQ[cacheDirectory<>#<>".html"],
html=URLExecute[apiKCDCURL,Join[apiKCDCParamsCommon,apiKCDCParamsPost[#]],"Source"];
Export[cacheDirectory<>#<>".html",html,"Text",CharacterEncoding-> "UTF8"]]&/@x];


crawlPosts[postList];


(* ::Section:: *)
(*Parsing Caches*)


(* ::Subsection:: *)
(*Transform*)


(* ::Subsubsection:: *)
(*HTML to Data Function*)


dataRow[name_]:=
Module[{data,pos,rows},
data=Import[name,"Data"];
pos=Position[data,
{" \:ad6c\:bd84"," \:cd1d\:acc4"," \:d655\:c9c4\:d658\:c790"," \:ac80\:c0ac\:d604\:d669"}|{" \:ad6c\:bd84"," \:cd1d\:acc4"," \:d655\:c9c4\:d658\:c790\:d604\:d669"," \:ac80\:c0ac\:d604\:d669"}];
rows=Extract[data,Most[First[pos]]][[3;;-1]];
If[Length[rows]==6,(* \:c0ac\:b9dd\:c790 \:c804 \:d3ec\:ba67 2 rows *)
{
Join[rows[[1,1;;3]],rows[[2]],{"0"},rows[[1,4;;-1]]],
Join[rows[[3,1;;3]],rows[[4]],{"0"},rows[[3,4;;-1]]]
},
If[Length[rows[[1]]]==8,
{
Join[rows[[1,1;;5]],{"0"},rows[[1,6;;-1]]],
Join[rows[[2,1;;5]],{"0"},rows[[2,6;;-1]]]
},
Most[rows] (* \:c0ac\:b9dd\:c790 \:d6c4 \:d3ec\:ba67 *)
]
]
];


(* ::Subsubsection:: *)
(*Convert Cached HTMLs*)


dataPre1=Flatten[dataRow/@FileNames[cacheDirectory<>"*.html"],1];


(* ::Subsubsection:: *)
(*Convert Values to Expressions*)


dataPre2=Map[
Function[{x},
Prepend[
ToExpression[StringReplace[#,("*"|","|" ")->""]]&/@Rest[x],
Quiet@DateObject[StringReplace[First[x],{".( "~~_~~" ) "->" "," \:c2dc \:ae30\:c900"->":00"}]]
]],
dataPre1
];


(* ::Subsubsection:: *)
(*Remove Duplicated Entries*)


data=Sort[DeleteDuplicates[dataPre2,First[#1]==First[#2]&]];


Prepend[Rest[#],DateString@First[#]]&/@data (* For CSV output *)
