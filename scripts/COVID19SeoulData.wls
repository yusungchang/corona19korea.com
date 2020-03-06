#!/usr/bin/env wolframscript -print -format CSV
(* ::Package:: *)

(* ::Title:: *)
(*COVID-19 Data Analysis*)
(*\:c11c\:c6b8: Generate Data*)


(* ::Subtitle:: *)
(*\:c7a5\:c720\:c131 | Yu-Sung Chang*)
(*CTO, Digital Innovation Group*)
(*SSG.COM*)


(* ::Section:: *)
(*Config*)


seoulPageURL="http://www.seoul.go.kr/coronaV/coronaStatus.do?tab=1";


(* ::Section:: *)
(*Import Page*)


raw=Import[seoulPageURL,"Data"];


tableRaw=raw[[2,2,1,2,3]];


(* ::Section:: *)
(*Parse Page*)


seoulDataProcess[body_,eventYear_]:=Module[{
colNum=1,colDate=3,colGen=4,colAge=4,colDistrict=5,
dates,
genders,
ages,
districts
},
dates=Quiet@DateString@DateObject[First@StringCases[ToString[#],RegularExpression["\\d+\\.\\d+"]]]&/@body[[All,colDate]];(* \:d655\:c9c4 \:b0a0\:c790 \:cc98\:b9ac *)
genders=StringPart[#,1]&/@body[[All,colGen]]; (* \:c131\:bcc4 \:cc98\:b9ac *)
ages=(eventYear-With[{a=ToExpression[First@StringCases[#,x:DigitCharacter..:>x]]},If[a<=20,2000+a,1900+a]])&/@body[[All,colAge]]; (* \:b098\:c774 \:cc98\:b9ac *)
districts=body[[All,colDistrict]]; (* \:cd9c\:c0dd\:c9c0 \:cc98\:b9ac *)
Transpose[{dates,ages,genders,districts}]
]


(* ::Subsubsection:: *)
(*Process Raw Data*)


table=seoulDataProcess[tableRaw,2020] (* \:b9ac\:d3ec\:d2b8 \:c2dc\:c810: 2020\:b144 *)
