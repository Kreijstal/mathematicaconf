(* Mathematica package *)
BeginPackage["CloudObject`"]

System`Delayed;

Begin["`Private`"]

Unprotect[Delayed];

Options[Delayed] = sortOptions @ {UpdateInterval -> Infinity, CachePersistence -> 0};
SetAttributes[Delayed, {HoldFirst, ReadProtected}];

Protect[Delayed]

End[]

EndPackage[]
