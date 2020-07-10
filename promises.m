(* ::Package:: *)

Promise::usage="Promise[Function[{accept,reject},CustomLogic[accept[\"Success!\"],reject[\"Oh no! Error!\"]]]]"
Promise := 
 Module[{local, success, failure, listen, counter = 0, Promise},
  success[x_] := (local["s"] = x;(*Print[{"I've been executed",x,
    local}];*)Through[local["t"][local["s"]]]; local["t"] = {}; 
    local["resolved"] = True);
  failure[x_] := (local["f"] = x; Null);
  Promise[
    f_] := (local = <|"s" -> Null, "f" -> Null, "c" -> {}, "t" -> {}, 
      "resolved" -> False, "counter" -> counter++|>;
    local["edit"] := 
     Function[{key, value}, (local[key] = value; local)];
    local["debug"] := local;
    local[
      "_whenThened"] := (If[local["resolved"], 
       Through[local["t"][local["s"]]]; local["t"] = {}, Null]);
    f[success, failure]; local); Promise]
	
Module[{p, accept, reject},
 Promisify[
   f_] := ((p := 
     Promise[Function[{a, r}, (accept := a; 
        reject := r)]]); {(Function[x, accept[f[x]]]), p})
 ]


SetAttributes[PromiseThen, HoldAll]

PromiseThen[promise_?AssociationQ, 
   f_] := (promise["edit"]["t", Join[promise["t"], {#[[1]]}]]; 
     promise["_whenThened"]; promise = promise["debug"]; #[[2]]) &[
   Promisify[f]];
   
  