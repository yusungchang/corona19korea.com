#!/usr/bin/env wolframscript -print -format CSV
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


seoulPageURL="http://www.seoul.go.kr/coronaV/coronaStatus.do?tab=1";


cacheDirectory=Directory[]<>"/../data/cache/";
outputDirectory=Directory[]<>"/../src/output/";


(* ::Section:: *)
(*Import Page*)


raw=Import[seoulPageURL,"Data"];


tableRaw=raw[[2,2,1,3]];


(* ::Section:: *)
(*Data Functions*)


seoulDistrictNames={"\:ac15\:b0a8\:ad6c","\:ac15\:b3d9\:ad6c","\:ac15\:bd81\:ad6c","\:ac15\:c11c\:ad6c","\:ad00\:c545\:ad6c","\:ad11\:c9c4\:ad6c","\:ad6c\:b85c\:ad6c","\:ae08\:cc9c\:ad6c","\:b178\:c6d0\:ad6c","\:b3c4\:bd09\:ad6c","\:b3d9\:b300\:bb38\:ad6c","\:b3d9\:c791\:ad6c","\:b9c8\:d3ec\:ad6c","\:c11c\:b300\:bb38\:ad6c","\:c11c\:cd08\:ad6c","\:c131\:b3d9\:ad6c","\:c131\:bd81\:ad6c","\:c1a1\:d30c\:ad6c","\:c591\:cc9c\:ad6c","\:c601\:b4f1\:d3ec\:ad6c","\:c6a9\:c0b0\:ad6c","\:c740\:d3c9\:ad6c","\:c885\:b85c\:ad6c","\:c911\:ad6c","\:c911\:b791\:ad6c"};


seoulDataProcess[body_,eventYear_]:=Module[{
colNum=1,colDate=3,colGen=4,colAge=4,colDistrict=5,
dates,
genders,
ages,
districts
},
dates=Quiet@DateString@DateObject[First@StringCases[ToString[#],RegularExpression["\\d+\\.\\d+"]]]&/@body[[All,colDate]];(* \:d655\:c9c4 \:b0a0\:c790 \:cc98\:b9ac *)
genders=StringPart[#,1]&/@body[[All,colGen]]; (* \:c131\:bcc4 \:cc98\:b9ac *)
ages=(eventYear-With[{a=ToExpression[StringTake[#,{4,5}]]},If[a<=20,2000+a,1900+a]])&/@body[[All,colAge]]; (* \:b098\:c774 \:cc98\:b9ac *)
districts=body[[All,colDistrict]]; (* \:cd9c\:c0dd\:c9c0 \:cc98\:b9ac *)
Transpose[{dates,ages,genders,districts}]
]


(* ::Subsubsection:: *)
(*Process Raw Data*)


table=seoulDataProcess[tableRaw,2020] (* \:b9ac\:d3ec\:d2b8 \:c2dc\:c810: 2020\:b144 *)
