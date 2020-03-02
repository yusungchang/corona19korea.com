#!/usr/bin/env wolframscript -print -format CSV
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*\:acbd\:ae30\:b3c4*)


(* ::Subtitle:: *)
(*\:c7a5\:c720\:c131 | Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Note*)


(* ::Section:: *)
(*Initialization*)


ggPageURL="https://www.gg.go.kr/bbs/boardView.do?bsIdx=464&bIdx=2296956&menuId=1535";


cacheDirectory=Directory[]<>"/../data/cache/";
outputDirectory=Directory[]<>"/../src/output/";


(* ::Section:: *)
(*Import Page*)


raw=Import[ggPageURL,"Data"];


tableHeader=raw[[4,2,2,1,2,1]];


tableRaw=raw[[4,2,2,1,2,2]];


(* ::Section:: *)
(*Data Functions*)


ggDistrictNames={"\:ad70\:d3ec\:c2dc","\:ad6c\:b9ac\:c2dc","\:c591\:d3c9\:ad70","\:ace0\:c591\:c2dc","\:c624\:c0b0\:c2dc","\:d558\:b0a8\:c2dc","\:c2dc\:d765\:c2dc","\:c758\:c655\:c2dc","\:c548\:c591\:c2dc","\:c5ec\:c8fc\:c2dc","\:c548\:c131\:c2dc","\:c131\:b0a8\:c2dc","\:c548\:c0b0\:c2dc","\:c6a9\:c778\:c2dc","\:b0a8\:c591\:c8fc\:c2dc","\:ae40\:d3ec\:c2dc","\:c218\:c6d0\:c2dc","\:bd80\:cc9c\:c2dc","\:acfc\:cc9c\:c2dc","\:c758\:c815\:bd80\:c2dc","\:c774\:cc9c\:c2dc","\:d3c9\:d0dd\:c2dc","\:c5f0\:cc9c\:ad70","\:ac00\:d3c9\:ad70","\:ad11\:ba85\:c2dc","\:b3d9\:b450\:cc9c\:c2dc","\:c591\:c8fc\:c2dc","\:d3ec\:cc9c\:c2dc","\:d654\:c131\:c2dc","\:d30c\:c8fc\:c2dc","\:ad11\:c8fc\:c2dc"};


ggDataProcess[body_,eventYear_]:=Module[{
colDate=5,colGen=3,colAge=4,colDistrict=-1,
dates,
genders,
ages,
districts
},
dates=Quiet@DateString@DateObject[First@StringCases[ToString[#],RegularExpression["\\d+\\.\\d+"]]]&/@body[[All,colDate]];(* \:d655\:c9c4 \:b0a0\:c790 \:cc98\:b9ac *)
genders=body[[All,colGen]]; (* \:c131\:bcc4 \:cc98\:b9ac *)
ages=(eventYear-With[{a=ToExpression[StringTake[#,{2,-1}]]},If[a<=20,2000+a,1900+a]])&/@body[[All,colAge]]; (* \:b098\:c774 \:cc98\:b9ac *)
districts=(#<>"\:c2dc"&)/@(StringReplace[#,"("~~__~~")":>""]&/@body[[All,colDistrict]]); (* \:cd9c\:c0dd\:c9c0 \:cc98\:b9ac: \:ad6d\:c801\:c81c\:ac70 \:d6c4 "\:c2dc" \:cd94\:ac00 *)

Transpose[{dates,ages,genders,districts}]
]


(* ::Subsubsection:: *)
(*Process Raw Data*)


table=ggDataProcess[tableRaw,2020] (* \:b9ac\:d3ec\:d2b8 \:c2dc\:c810: 2020\:b144 *)
