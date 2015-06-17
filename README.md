w[n_, x_] := If[n >= x, "win", n]
\[Kappa][n_] := Join[Flatten@Drop[#, -1], {First@Last@#}] &@Split@n
\[CapitalKappa][n_] := 
 If[EvenQ@Length@# == True, "Vicky", "Jason"] &@\[Kappa][n]
\[Lambda][n_] := If[# < 2, 2, If[# > 9, 9, #]] &@n
\[CapitalLambda][list_, nn_] := 
 If[Times @@ list*9 < nn, Times @@ list*9, 1]
ra[n_] := {1}~Join~(Reverse@Range[2, n]~Join~ConstantArray[2, 9 - n])
accT[list_] := Times @@ # & /@ (Take[list, #] & /@ Range@Length@list)
\[CapitalRho][n_] := RandomInteger[{2, 9}, 1][[1]]
\[Tau][n_] := If[n == 2, 2, n - 1]
\[CapitalTau][n_] := 
 n /. Flatten[{#[[1]] -> #[[2]]} & /@ 
    Transpose@{Range[2, 9], Reverse@Range[2, 9]}]
gameJ[nn_] := 
 With[{j = 324, v1 = \[CapitalRho][n], v2 = \[CapitalRho][n], 
   v3 = \[CapitalRho][n], v4 = 9, j4 = 9}, 
  With[{j1 = 
     If[j < nn <= 2 j, 2, 
        If[2 j < nn <= 4 j, 4, 
         If[4 j < nn <= 8 j, 8, #]]] &@\[Lambda]@\[LeftFloor](nn - 1)/
        9\[RightFloor]},
   With[{j2 = \[Lambda]@\[LeftFloor](
        nn - 1)/\[CapitalLambda][{j1, v1}, nn]\[RightFloor]}, 
    With[{j3 = \[Lambda]@\[LeftFloor](
         nn - 1)/\[CapitalLambda][{j1, v1, j2, v2}, nn]\[RightFloor]},
      With[{out = (w[#, 
            nn] & /@ (accT@# &@{j1, v1, j2, v2, j3, v3, j4, v4}))},
      {{\[CapitalKappa]@#, \[Kappa]@#} &@out, {j1, v1, j2, v2, j3, v3}}
      ]]]]]
gameV[nn_] := 
 With[{j1 = \[CapitalRho][n], j2 = \[CapitalRho][n], 
   j3 = \[CapitalRho][n], j4 = 9, v4 = 9}, With[{v1 =
     If[nn < 
       2917, \[Lambda]@\[LeftFloor](
        nn - 1)/\[CapitalLambda][{j1}, nn]\[RightFloor],
      If[2917 <= nn <= 3240, (ra@5)[[j1]],
       If[3241 <= nn <= 3888, (ra@6)[[j1]],
        If[3889 <= nn <= 4536, (ra@7)[[j1]],
         If[4537 <= nn <= 5184, (ra@8)[[j1]],
          If[5185 <= nn <= 5832, (ra@9)[[j1]]]]]]]]},
   With[{v2 = \[Lambda]@\[LeftFloor](
        nn - 1)/\[CapitalLambda][{j1, v1, j2}, nn]\[RightFloor]}, 
    With[{v3 = \[Lambda]@\[LeftFloor](
         nn - 1)/\[CapitalLambda][{j1, v1, j2, v2, j3}, 
          nn]\[RightFloor]}, 
     With[{out = (w[#, 
            nn] & /@ (accT@# &@{j1, v1, j2, v2, j3, v3, j4, v4}))},
      {{\[CapitalKappa]@#, \[Kappa]@#} &@out, {j1, v1, j2, v2, j3, v3}}
      ]]]]]
      gameJV[nn_] := 
 With[{j = 324, j4 = 9, v4 = 9}, 
  With[{j1 = 
     If[j < nn <= 2 j, 2, 
        If[2 j < nn <= 4 j, 4, 
         If[4 j < nn <= 8 j, 8, #]]] &@\[Lambda]@\[LeftFloor](nn - 1)/
        9\[RightFloor]},
   With[{v1 =
      If[nn < 
        2917, \[Lambda]@\[LeftFloor](
         nn - 1)/\[CapitalLambda][{j1}, nn]\[RightFloor],
       If[2917 <= nn <= 3240, (ra@5)[[j1]],
        If[3241 <= nn <= 3888, (ra@6)[[j1]],
         If[3889 <= nn <= 4536, (ra@7)[[j1]],
          If[4537 <= nn <= 5184, (ra@8)[[j1]],
           If[5185 <= nn <= 5832, (ra@9)[[j1]]
            ]]]]]]}, 
    With[{j2 = \[Lambda]@\[LeftFloor](
         nn - 1)/\[CapitalLambda][{j1, v1}, nn]\[RightFloor]},
     With[{v2 = \[Lambda]@\[LeftFloor](
          nn - 1)/\[CapitalLambda][{j1, v1, j2}, nn]\[RightFloor]}, 
      With[{j3 = \[Lambda]@\[LeftFloor](
           nn - 1)/\[CapitalLambda][{j1, v1, j2, v2}, 
            nn]\[RightFloor]}, 
       With[{v3 = \[Lambda]@\[LeftFloor](
            nn - 1)/\[CapitalLambda][{j1, v1, j2, v2, j3}, 
             nn]\[RightFloor]}, 
        With[{out = (w[#, 
               nn] & /@ (accT@# &@{j1, v1, j2, v2, j3, v3, j4, v4}))},
         {{\[CapitalKappa]@#, \[Kappa]@#} &@out, {j1, v1, j2, v2, j3, 
           v3}, j1}
         ]]]]]]]]
         gameV2[#] & /@ RandomInteger[{f2@2 + 1, f2@3}, 1]
(*IMPLIMENTATION
gameJ2[#] & /@ RandomInteger[{f2@3 + 1, f2@4}, 1]
gameV2[#] & /@ RandomInteger[{f2@4 + 1, f2@5}, 1]
gameJ2[#] & /@ RandomInteger[{f2@5 + 1, f2@6}, 1]
gameV2[#] & /@ RandomInteger[{f2@6 + 1, f2@7}, 1]

TRIAL RUNS

mm = 1000;
DeleteDuplicates@(gameV[#] & /@ RandomInteger[{f2@2 + 1, f2@3}, 1] & /@
     Range@mm)[[All, 1, 1, 1]]
DeleteDuplicates@(gameJ[#] & /@ RandomInteger[{f2@3 + 1, f2@4}, 1] & /@
     Range@mm)[[All, 1, 1, 1]]
DeleteDuplicates@(gameV[#] & /@ RandomInteger[{f2@4 + 1, f2@5}, 1] & /@
     Range@mm)[[All, 1, 1, 1]]
DeleteDuplicates@(gameJ[#] & /@ RandomInteger[{f2@5 + 1, f2@6}, 1] & /@
     Range@mm)[[All, 1, 1, 1]]
DeleteDuplicates@(gameV[#] & /@ RandomInteger[{f2@6 + 1, f2@7}, 1] & /@
     Range@mm)[[All, 1, 1, 1]]
*)
