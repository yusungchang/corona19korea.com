#!/usr/bin/env wolframscript -print -format CSV
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*\:acbd\:ae30\:b3c4: Generate Data*)


(* ::Subtitle:: *)
(*\:c7a5\:c720\:c131 | Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Config*)


ggPageURL="https://www.gg.go.kr/bbs/boardView.do?bsIdx=464&bIdx=2296956&menuId=1535";


(* ::Section:: *)
(*Import Page*)


raw=Import[ggPageURL,"Data"];


tableRaw=raw[[4,2,2,1,2,2]];


(* ::Section:: *)
(*Parse Page*)


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
