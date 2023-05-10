(* ::Package:: *)

BeginPackage["Matrix`"]
(*
prodottoMatrici::usage = ""

inputMatrice::usage = ""

askKindOfMatrix::usage = ""

askForDimensions::usage = ""

generateRandomMatrix::usage = ""
*)

prodottoMatrici[A_, B_] := Module[{m, n, q, p, resultMatrix, i, j, k}, 
{m, n} = Dimensions[A]; 
{q, p} = Dimensions[B]; 
    If[m != p, Print["Le dimensioni delle matrici non sono compatibili per il prodotto."]; Return[]]; resultMatrix = ConstantArray[0, {m, p}]; 
    Do[resultMatrix[[i,j]] = Sum[Print["Calcolo ", A[[i,k]], " * ", B[[k,j]], " = ", A[[i,k]]*B[[k,j]]]; A[[i,k]]*B[[k,j]], {k, n}]; 
      Print["La somma \[EGrave]: ", resultMatrix[[i,j]]], {i, m}, {j, p}]; resultMatrix]

(*
A = inputMatrice["A"]; 
B = inputMatrice["B"];
*)

isValidDimension[n_]:= n != Null && IntegerQ[n] && 0 < n <= 10

manipulateMatrixProduct[] := DynamicModule[{
	 rowsA = 3,
     colA = 3,
     rowsB = 3,
     colB = 3,
	 matriceA = ConstantArray[0, {3, 3}],
     matriceB = ConstantArray[0, {3, 3}],
     risultato = "",
     randomFill = False,
     justUpdated = False,
     seed = 0},
    Manipulate[
        If[randomFill,
            SeedRandom[seed];
            If[!justUpdated,
                justUpdated = True;
                rowsA = RandomChoice[{2, 3, 4, 5}];
                colA = RandomChoice[{2, 3, 4, 5}];
                rowsB = RandomChoice[{2, 3, 4, 5}];
                colB = rowsA;
            ];
            matriceA = RandomInteger[{-10, 10}, {rowsA, colA}];
            matriceB = RandomInteger[{-10, 10}, {rowsB, rowsA}],
            seed="";
            justUpdated = False;
            matriceA = ConstantArray[0, {rowsA, colA}];
            matriceB = ConstantArray[0, {rowsB, colB}]
        ];
        
        Column[{
            Row[{"Dimensione Matrice A:",
            (*
	            Chiedere come validare il range, 
	            If[isValidDimension[rowsA], rowsA = #] &, 
	            non funziona, lo stesso vale per il seed: InputField non accetta range
            *)
                InputField[Dynamic[rowsA], 
                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill],
                "x",
                InputField[Dynamic[colA], 
                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill]
            }],
            Row[{"Dimensione Matrice B:",
                InputField[Dynamic[rowsB], 
                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill],
                "x",
                InputField[Dynamic[colB], 
                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill]
            }, Alignment->Center],
            Row[Spacer[20]{
	            Column[{
	                "Matrice A:",
	                Dynamic@Grid[
	                    If[randomFill,
	                        matriceA,
	                        Table[
	                            With[{i = i, j = j},
	                                InputField[Dynamic[matriceA[[i, j]]], Number, FieldSize -> {3, 1}, Alignment -> Center]
	                            ],
	                            {i, rowsA}, {j, colA}
	                        ]
	                    ],
	                    Frame -> All, Spacings -> {1, 1}, ItemSize -> {3, 2}
	                ]
	            }, Alignment->Center],
	            Column[{
	                "Matrice B:",
	                Dynamic@Grid[
	                    If[randomFill,
	                        matriceB,
	                        Table[
	                            With[{i = i, j = j},
	                                InputField[Dynamic[matriceB[[i, j]]], Number, FieldSize -> {3, 1}, Alignment -> Center]
	                            ],
	                            {i, rowsB}, {j, colB}
	                        ]
	                    ],
	                    Frame -> All, Spacings -> {1, 1}, ItemSize -> {3, 2}
	                ]
	            }, Alignment->Center]
            }],
		
		Row[{
			Button["Calcola prodotto", 
				risultato=MatrixForm[matriceA . matriceB]
			],
			Button["Reset",
				seed="";
				randomFill=False;
				matriceA=ConstantArray[0,{rowsA,colA}];
				matriceB=ConstantArray[0,{rowsB,colB}];
				risultato=""
			]
		}],
		Row[{"Risultato:",risultato}]
	}],
	Row[ Spacer[20]{
		Control[{
			{randomFill,False,"Riempi randomicamente"},{False,True}
		}],
		Dynamic@If[randomFill,
			Column[{Text["Random Seed : " Green], InputField[Dynamic[seed], Number, FieldSize -> {10, 1.5}]}],
            Column[{Text["Random Seed : " Red], InputField[Dynamic[seed], Number, FieldSize -> {10, 1.5}, Enabled -> False]}]
        ]
	}],
	
	TrackedSymbols:>{rowsA, colA, rowsB, colB, randomFill, seed},
	SynchronousUpdating->True
	]
]

manipulateMatrixProduct[]
EndPackage[]





(* ::Input:: *)
(*Manipulate[If[Matrix`randomFill, Matrix`seed$$ = ""; *)
(*     If[ !Matrix`justUpdated$$, Matrix`justUpdated$$ = True; *)
(*       Matrix`rowsA$$ = RandomChoice[{2, 3, 4, 5}]; Matrix`colA$$ = *)
(*        RandomChoice[{2, 3, 4, 5}]; Matrix`rowsB$$ = *)
(*        RandomChoice[{2, 3, 4, 5}]; Matrix`colB$$ = Matrix`rowsA$$; ]; *)
(*     Matrix`matriceA$$ = RandomInteger[{-10, 10}, {Matrix`rowsA$$, *)
(*        Matrix`colA$$}]; Matrix`matriceB$$ = RandomInteger[{-10, 10}, *)
(*       {Matrix`rowsB$$, Matrix`rowsA$$}], Matrix`justUpdated$$ = False; *)
(*     Matrix`matriceA$$ = ConstantArray[0, {Matrix`rowsA$$, Matrix`colA$$}]; *)
(*     Matrix`matriceB$$ = ConstantArray[0, {Matrix`rowsB$$, Matrix`colB$$}]]; *)
(*   Column[{Row[{"Dimensione Matrice A:", InputField[Dynamic[Matrix`rowsA$$, *)
(*         If[#1 != Null && NumericQ[#1] && 0 < #1 < 10, Matrix`rowsA$$ = *)
(*            #1] & ], Number, FieldSize -> {2, 1}, Alignment -> Center, *)
(*        Enabled ->  !Matrix`randomFill], "x", InputField[*)
(*        Dynamic[Matrix`colA$$, If[#1 != Null && NumericQ[#1] && 0 < #1 < 10, *)
(*           Matrix`colA$$ = #1] & ], Number, FieldSize -> {2, 1}, *)
(*        Alignment -> Center, Enabled ->  !Matrix`randomFill]}], *)
(*     Row[{"Dimensione Matrice B:", InputField[Dynamic[Matrix`rowsB$$, *)
(*         If[#1 != Null && NumericQ[#1] && 0 < #1 < 10, Matrix`rowsB$$ = *)
(*            #1] & ], Number, FieldSize -> {2, 1}, Alignment -> Center, *)
(*        Enabled ->  !Matrix`randomFill], "x", InputField[*)
(*        Dynamic[Matrix`colB$$, If[#1 != Null && NumericQ[#1] && 0 < #1 < 10, *)
(*           Matrix`colB$$ = #1] & ], Number, FieldSize -> {2, 1}, *)
(*        Alignment -> Center, Enabled ->  !Matrix`randomFill]}, *)
(*      Alignment -> Center], *)
(*     Row[Spacer[20]*{Column[{"Matrice A:", Dynamic[*)
(*           Grid[If[Matrix`randomFill, Matrix`matriceA$$, *)
(*             Table[With[{Matrix`i$ = Matrix`i, Matrix`j$ = Matrix`j}, *)
(*               InputField[Dynamic[Matrix`matriceA$$[[Matrix`i$,Matrix`j$]]], *)
(*                Number, FieldSize -> {3, 1}, Alignment -> Center]], *)
(*              {Matrix`i, Matrix`rowsA$$}, {Matrix`j, Matrix`colA$$}]], *)
(*            Frame -> All, Spacings -> {1, 1}, ItemSize -> {3, 2}]]}, *)
(*         Alignment -> Center], Column[{"Matrice B:", *)
(*          Dynamic[Grid[If[Matrix`randomFill, Matrix`matriceB$$, *)
(*             Table[With[{Matrix`i$ = Matrix`i, Matrix`j$ = Matrix`j}, *)
(*               InputField[Dynamic[Matrix`matriceB$$[[Matrix`i$,Matrix`j$]]], *)
(*                Number, FieldSize -> {3, 1}, Alignment -> Center]], *)
(*              {Matrix`i, Matrix`rowsB$$}, {Matrix`j, Matrix`colB$$}]], *)
(*            Frame -> All, Spacings -> {1, 1}, ItemSize -> {3, 2}]]}, *)
(*         Alignment -> Center]}], *)
(*     Row[{Button["Calcola prodotto", Matrix`risultato$$ = *)
(*         MatrixForm[Matrix`matriceA$$ . Matrix`matriceB$$]], *)
(*       Button["Azzera", Matrix`matriceA$$ = ConstantArray[0, *)
(*           {Matrix`rowsA$$, Matrix`colA$$}]; Matrix`matriceB$$ = *)
(*          ConstantArray[0, {Matrix`rowsB$$, Matrix`colB$$}]; *)
(*         Matrix`risultato$$ = ""]}], *)
(*     Row[{"Risultato:", Matrix`risultato$$}]}], *)
(*  {{Matrix`randomFill, False, "Riempi randomicamente"}, {False, True}, *)
(*   ControlPlacement -> 1}, Row[{Manipulate`Place[1]*Spacer[20], *)
(*    Dynamic[If[Matrix`randomFill, Column[{Text["Random Seed : "*Green], *)
(*         InputField[Dynamic[Matrix`seed$$], String, FieldSize -> *)
(*           {10, 1.5}]}], Column[{Text["Random Seed : "*Red], *)
(*         InputField[Dynamic[Matrix`seed$$], String, FieldSize -> {10, 1.5}, *)
(*          Enabled -> False]}]]]*Spacer[20]}], *)
(*  TrackedSymbols :> {Matrix`rowsA$$, Matrix`colA$$, Matrix`rowsB$$, *)
(*    Matrix`colB$$, Matrix`randomFill}, SynchronousUpdating -> False]*)


(* ::Input:: *)
(*Manipulate[If[Matrix`randomFill, Matrix`seed$$ = ""; *)
(*     If[ !Matrix`justUpdated$$, Matrix`justUpdated$$ = True; *)
(*       Matrix`rowsA$$ = RandomChoice[{2, 3, 4, 5}]; Matrix`colA$$ = *)
(*        RandomChoice[{2, 3, 4, 5}]; Matrix`rowsB$$ = *)
(*        RandomChoice[{2, 3, 4, 5}]; Matrix`colB$$ = Matrix`rowsA$$; ]; *)
(*     Matrix`matriceA$$ = RandomInteger[{-10, 10}, {Matrix`rowsA$$, *)
(*        Matrix`colA$$}]; Matrix`matriceB$$ = RandomInteger[{-10, 10}, *)
(*       {Matrix`rowsB$$, Matrix`rowsA$$}], Matrix`justUpdated$$ = False; *)
(*     Matrix`matriceA$$ = ConstantArray[0, {Matrix`rowsA$$, Matrix`colA$$}]; *)
(*     Matrix`matriceB$$ = ConstantArray[0, {Matrix`rowsB$$, Matrix`colB$$}]]; *)
(*   Column[{Row[{"Dimensione Matrice A:", InputField[Dynamic[Matrix`rowsA$$, *)
(*         If[#1 != Null && NumericQ[#1] && 0 < #1 < 10, Matrix`rowsA$$ = *)
(*            #1] & ], Number, FieldSize -> {2, 1}, Alignment -> Center, *)
(*        Enabled ->  !Matrix`randomFill], "x", InputField[*)
(*        Dynamic[Matrix`colA$$, If[#1 != Null && NumericQ[#1] && 0 < #1 < 10, *)
(*           Matrix`colA$$ = #1] & ], Number, FieldSize -> {2, 1}, *)
(*        Alignment -> Center, Enabled ->  !Matrix`randomFill]}], *)
(*     Row[{"Dimensione Matrice B:", InputField[Dynamic[Matrix`rowsB$$, *)
(*         If[#1 != Null && NumericQ[#1] && 0 < #1 < 10, Matrix`rowsB$$ = *)
(*            #1] & ], Number, FieldSize -> {2, 1}, Alignment -> Center, *)
(*        Enabled ->  !Matrix`randomFill], "x", InputField[*)
(*        Dynamic[Matrix`colB$$, If[#1 != Null && NumericQ[#1] && 0 < #1 < 10, *)
(*           Matrix`colB$$ = #1] & ], Number, FieldSize -> {2, 1}, *)
(*        Alignment -> Center, Enabled ->  !Matrix`randomFill]}, *)
(*      Alignment -> Center], *)
(*     Row[Spacer[20]*{Column[{"Matrice A:", Dynamic[*)
(*           Grid[If[Matrix`randomFill, Matrix`matriceA$$, *)
(*             Table[With[{Matrix`i$ = Matrix`i, Matrix`j$ = Matrix`j}, *)
(*               InputField[Dynamic[Matrix`matriceA$$[[Matrix`i$,Matrix`j$]]], *)
(*                Number, FieldSize -> {3, 1}, Alignment -> Center]], *)
(*              {Matrix`i, Matrix`rowsA$$}, {Matrix`j, Matrix`colA$$}]], *)
(*            Frame -> All, Spacings -> {1, 1}, ItemSize -> {3, 2}]]}, *)
(*         Alignment -> Center], Column[{"Matrice B:", *)
(*          Dynamic[Grid[If[Matrix`randomFill, Matrix`matriceB$$, *)
(*             Table[With[{Matrix`i$ = Matrix`i, Matrix`j$ = Matrix`j}, *)
(*               InputField[Dynamic[Matrix`matriceB$$[[Matrix`i$,Matrix`j$]]], *)
(*                Number, FieldSize -> {3, 1}, Alignment -> Center]], *)
(*              {Matrix`i, Matrix`rowsB$$}, {Matrix`j, Matrix`colB$$}]], *)
(*            Frame -> All, Spacings -> {1, 1}, ItemSize -> {3, 2}]]}, *)
(*         Alignment -> Center]}], *)
(*     Row[{Button["Calcola prodotto", Matrix`risultato$$ = *)
(*         MatrixForm[Matrix`matriceA$$ . Matrix`matriceB$$]], *)
(*       Button["Azzera", Matrix`matriceA$$ = ConstantArray[0, *)
(*           {Matrix`rowsA$$, Matrix`colA$$}]; Matrix`matriceB$$ = *)
(*          ConstantArray[0, {Matrix`rowsB$$, Matrix`colB$$}]; *)
(*         Matrix`risultato$$ = ""]}], *)
(*     Row[{"Risultato:", Matrix`risultato$$}]}], *)
(*  {{Matrix`randomFill, False, "Riempi randomicamente"}, {False, True}, *)
(*   ControlPlacement -> 1}, Row[{Manipulate`Place[1]*Spacer[20], *)
(*    Dynamic[If[Matrix`randomFill, Column[{Text["Random Seed : "*Green], *)
(*         InputField[Dynamic[Matrix`seed$$], String, FieldSize -> *)
(*           {10, 1.5}]}], Column[{Text["Random Seed : "*Red], *)
(*         InputField[Dynamic[Matrix`seed$$], String, FieldSize -> {10, 1.5}, *)
(*          Enabled -> False]}]]]*Spacer[20]}], *)
(*  TrackedSymbols :> {Matrix`rowsA$$, Matrix`colA$$, Matrix`rowsB$$, *)
(*    Matrix`colB$$, Matrix`randomFill}, SynchronousUpdating -> False]*)


33


(* ::Input:: *)
(*Manipulate[If[Matrix`randomFill, *)
(*    Matrix`matriceA$$ = RandomInteger[{0, 10}, {Matrix`dimensioneA, *)
(*        Matrix`dimensioneA}]; Matrix`matriceB$$ = RandomInteger[{0, 10}, *)
(*       {Matrix`dimensioneB, Matrix`dimensioneB}]; Matrix`inputEnabled$$ = *)
(*      False; , Matrix`matriceA$$ = ConstantArray[0, {Matrix`dimensioneA, *)
(*        Matrix`dimensioneA}]; Matrix`matriceB$$ = ConstantArray[0, *)
(*       {Matrix`dimensioneB, Matrix`dimensioneB}]; Matrix`inputEnabled$$ = *)
(*      True; ]; Column[{Row[{"Matrice A:", Grid[Matrix`matriceA$$, *)
(*        Frame -> All]}], Row[{"Matrice B:", Grid[Matrix`matriceB$$, *)
(*        Frame -> All]}], Row[{Button["Calcola prodotto", *)
(*        Matrix`risultato$$ = MatrixForm[Matrix`matriceA$$ . *)
(*           Matrix`matriceB$$]], Button["Reset", *)
(*        Matrix`matriceA$$ = ConstantArray[0, {Matrix`dimensioneA, *)
(*            Matrix`dimensioneA}]; Matrix`matriceB$$ = ConstantArray[0, *)
(*           {Matrix`dimensioneB, Matrix`dimensioneB}]; Matrix`risultato$$ = *)
(*          ""]}], Row[{"Risultato:", Matrix`risultato$$}]}], *)
(*  {{Matrix`dimensioneA, 2}, {2, 3, 4}, ControlType -> SetterBar}, *)
(*  {{Matrix`dimensioneB, 2}, {2, 3, 4}, ControlType -> SetterBar}, *)
(*  {{Matrix`randomFill, True, "Riempi randomicamente"}, {False, True}}, *)
(*  ControlPlacement -> Left, TrackedSymbols :> {Matrix`dimensioneA, *)
(*    Matrix`dimensioneB, Matrix`randomFill}]*)


(* ::Input:: *)
(*"Matrix`"*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)
