(* ::Package:: *)

BeginPackage["Matrix`"]


manipulateMatrixProduct::usage = "La funzione pi\[UGrave] bella del mondo"


manipulateMatrixProduct[] := DynamicModule[{
	 rowsA = 3,
     colA = 3,
     rowsB = 3,
     colB = 3,
	 matriceA = ConstantArray[0, {3, 3}],
     matriceB = ConstantArray[0, {3, 3}],
     matriceAB = ConstantArray[0, {3, 3}],
     inputUtente = ConstantArray["", {3, 3}],
     currentElement = 0,
     randomFill = False,
     justUpdated = False,
     showErrors = False,
     userTry = False,
     seed = ""
    },
    Manipulate[
        If[randomFill && seed != "",
            SeedRandom[seed];
            If[!justUpdated,
            (*AB ha rowsA righe e colB colonne sse colA=rowsB*)
                justUpdated = True;
			    showErrors=False;
				currentElement=0;
                rowsA = RandomChoice[{2, 3, 4, 5}];
                colA = RandomChoice[{2, 3, 4, 5}];
                rowsB = colA;
                colB = RandomChoice[{2, 3, 4, 5}]; 
            ];
            matriceA = RandomInteger[{-10, 10}, {rowsA, colA}];
            matriceB = RandomInteger[{-10, 10}, {rowsB, colB}];
            userTry=True;
            justUpdated = False;
            ,
            If[!userTry,
	            seed="";
	            justUpdated = False;
	            rowsA=3;
	            colA=3;
	            rowsB=3;
	            colB=3;
	            matriceA = ConstantArray[0, {rowsA, colA}];
	            matriceB = ConstantArray[0, {rowsB, colB}];
            ]
        ];
        If[userTry,
	        matriceAB = Dot[matriceA, matriceB];
	        inputUtente = ConstantArray["", {Dimensions[matriceAB][[1]], Dimensions[matriceAB][[2]]}];
        ];
        Column[{
            Row[{"Dimensione Matrice A: ",
            (*
	            Chiedere come validare il range, 
	            If[isValidDimension[rowsA], rowsA = #] &, 
	            non funziona, lo stesso vale per il seed: InputField non accetta range
            *)
                InputField[Dynamic[rowsA, 
	                If[IntegerQ[#] && # 1 <= # <= 6 , rowsA = #, 
	                   If[StringMatchQ[ToString[#], "*.*"], 
	                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
	                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&], 
	                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill
                ],
                " x ", 
                InputField[Dynamic[colA,
                If[IntegerQ[#] && # 1 <= # <= 6 , colA = #, 
                   If[StringMatchQ[ToString[#], "*.*"], 
                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&],
                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill]
            }],
            Row[{"Dimensione Matrice B: ",
                InputField[Dynamic[rowsB, 
                If[IntegerQ[#] && # 1 <= # <= 6 , rowsB = #, 
                   If[StringMatchQ[ToString[#], "*.*"], 
                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&],  
                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill],
                " x ",
                InputField[Dynamic[colB, 
                If[IntegerQ[#] && # 1 <= # <= 6 , colB = #, 
                   If[StringMatchQ[ToString[#], "*.*"], 
                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&],
                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill]
            }, Alignment->Center],
            Spacer[30],
            Column[{
	            Row[Spacer[10]{
		            Row[{
			            Column[{
			                "Matrice A:",
			                Dynamic@Grid[
		                        Table[
		                            With[{i = i, j = j},
		                                InputField[Dynamic[matriceA[[i, j]]], 
			                                Number, FieldSize -> {3, 1}, 
			                                Alignment -> Center, 
			                                Background-> Dynamic@If[currentElement > -1 && currentElement < (Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]])+1 
			                                    && i == Quotient[(currentElement - 1), colB] + 1, 
			                                       RGBColor[0, 255, 0, .2], White],
		                                    Appearance -> Dynamic@If[userTry || randomFill, Frameless],
			                                Enabled->Dynamic[Not[userTry]]
		                                ]
		                            ],
		                            {i, rowsA}, {j, colA}
		                        ],
			                    Frame -> All, Spacings -> {1, 1}, ItemSize -> {3, 2}
			                ]
			            }, Alignment->Center
			            ],    
			        Spacer[20],
			        Column[{"*"}]
		            }],
		            Row[{
			            Column[{
			                "Matrice B:",
			                Dynamic@Grid[
		                        Table[
		                            With[{i = i, j = j},
		                                InputField[
		                                    Dynamic[matriceB[[i, j]]],  
			                                Number, FieldSize -> {3, 1}, 
			                                Alignment -> Center,
			                                Background-> Dynamic@If[currentElement > 0 && currentElement < (Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]])+1
			                                    && j == Mod[(currentElement - 1), colB] + 1, 
			                                        RGBColor[0, 255, 0, .2], White],
			                                Appearance -> Dynamic@If[userTry || randomFill, Frameless],
			                                Enabled -> Dynamic[Not[userTry]]
			                            ]
		                            ],
		                            {i, rowsB}, {j, colB}
		                        ],
			                    Frame -> All, Spacings -> {1, 1}, ItemSize -> {3, 2}
			                ]
			            }, Alignment->Center], 
			            Spacer[20],
			            Column[{"="}]	   
		            }]        
	            }],
				Dynamic@Column[{
					Spacer[50],
					Dynamic@If[!randomFill, 
						Button["Inizia", 
							userTry=True,
							ImageSize->60
						],
						Invisible[placeholder]
					]
					,
					Spacer[50],
	                "Matrice A*B:",
	                If[colA != rowsB,
		                Framed[Text[Style["Le colonne di A e le righe di B devono avere 
dimensioni uguali per poter generare una matrice", TextAlignment->Center, FontColor->Red]], Background->LightGray],
						Column[{Dynamic@Grid[
		                    Table[
						        With[{i = i, j = j, indice = (i - 1) * Dimensions[matriceAB][[2]] + j},
						            If[indice <= currentElement,
						                If[showErrors && matriceAB[[i, j]] != inputUtente[[i, j]],
							                If[!NumberQ[inputUtente[[i,j]]],
							                (*FIXME: Inserendo degli spazi vuoti si rompe*)
								                Style[Text[Dynamic[matriceAB[[i, j]]]], FontColor -> Red],
						                        Style[Text[inputUtente[[i,j]] "->" Dynamic[matriceAB[[i, j]]]], FontColor -> Red]
						                    ],
						                        Style[Text[Dynamic[matriceAB[[i, j]]]]]   
					                    ],
							            InputField[
										    Dynamic[inputUtente[[i,j]]],
										    Number, (*Otherwise it breaks on check result with Number and " " as input*)
										    FieldSize -> {Automatic, 2},
										    Alignment -> Center,
										    DefaultBaseStyle -> {ShowStringCharacters -> False, ShowStringCharactersStyle -> "Placeholder"},
										    FieldHint -> StringTemplate["\!\(\*SubsuperscriptBox[\(\[Sum]\), \(k = 1\), \(`1`\)]\) `2` * `3`"
										    ][ToString[rowsA], ToString[StandardForm[Subscript[a, i, k]]], ToString[StandardForm[Subscript[b, k, j]]]],
										    ImageSize -> {Full, Automatic},
										    BaseStyle -> Bold
 										]
						            ]
						        ],
						        {i, Dimensions[matriceAB][[1]]}, {j, Dimensions[matriceAB][[2]]}
						    ],
			                  Frame -> All, Spacings -> {1, 1}, ItemSize -> {10, 2}
				            ], 
				            Button[
				                "Mostra il prossimo elemento",
							    If[currentElement < Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]],
							        currentElement++
							    ],
								Enabled->userTry
							],
							Button[
				                "Torna indietro",
							    If[currentElement > 0, currentElement--],
								Enabled->userTry
							]
			            }]
	                ]
		        }, Alignment->Center]
            }, Alignment->Center]
            ,
			Row[{
				Button["Verifica Risultato",
					If[currentElement != Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]],
						showErrors = True;
						currentElement=(Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]])+1;
					],
					Enabled->userTry
				],
				Spacer[10], 
				Button["Mostra Soluzione", 
					currentElement=(Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]])+1,
					Enabled->userTry
				],
				Spacer[10],
				Button["Reset",
					userTry=False;
					showErrors=False;
					currentElement=0;
					seed="";
					rowsA = 3; 
					colA = 3; 
					rowsB = 3; 
					colB = 3;
					randomFill = False;
					matriceA = ConstantArray[0,{rowsA,colA}];
					matriceB = ConstantArray[0,{rowsB,colB}];
					matriceAB = matriceA . matriceB;
					inputUtente = ConstantArray["", {rowsA, colB}];
				]
			}]
		}],
		Row[ Spacer[20]{
			Control[{
				{randomFill,False,"Riempi randomicamente"},{False,True}
			}],
			Dynamic@If[randomFill,
			    Column[{
			        Text["Random Seed : " Green],
			        InputField[Dynamic[seed,
			            If[IntegerQ[#] && # >= 1,
			                seed = #,
			                If[StringMatchQ[ToString[#], "*-*"],
			                    MessageDialog["Il carattere \"-\" non pu\[OGrave] essere inserito"],
			                    If[StringMatchQ[ToString[#], "*.*"],
			                        MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"],
			                        MessageDialog["Inserire un numero intero positivo maggiore o uguale a 1"]
			                    ]
			                ]
			            ]&],
			            Number, FieldSize -> {10, 1.5}
			        ]
			    }],
			    Column[{
			        Text["Random Seed : " Red],
			        InputField[Dynamic@seed, Number, FieldSize -> {10, 1.5}, Enabled -> False]
			    }]
			]
		}],
		TrackedSymbols:>{rowsA, colA, rowsB, colB, randomFill, seed, inputUtente, showErrors, userTry, matriceA, matriceB},
		SynchronousUpdating->True
	]
]

manipulateMatrixProduct[]
EndPackage[]

(*
da aggiungere la storia dei colori se ce la facciamo,
aggiungere come si fa lo svolgimento con una finestra pop up,






(* ::Output:: *)
(*Manipulate[If[Matrix`randomFill && Matrix`seed$$ != "", *)
(*    SeedRandom[Matrix`seed$$]; If[ !Matrix`justUpdated$$, *)
(*      Matrix`justUpdated$$ = True; Matrix`showErrors$$ = False; *)
(*       Matrix`currentElement$$ = 0; Matrix`rowsA$$ = *)
(*        RandomChoice[{2, 3, 4, 5}]; Matrix`colA$$ = *)
(*        RandomChoice[{2, 3, 4, 5}]; Matrix`rowsB$$ = Matrix`colA$$; *)
(*       Matrix`colB$$ = RandomChoice[{2, 3, 4, 5}]; ]; *)
(*     Matrix`matriceA$$ = RandomInteger[{-10, 10}, {Matrix`rowsA$$, *)
(*        Matrix`colA$$}]; Matrix`matriceB$$ = RandomInteger[{-10, 10}, *)
(*       {Matrix`rowsB$$, Matrix`colB$$}]; Matrix`userTry$$ = True; *)
(*     Matrix`justUpdated$$ = False; , If[ !Matrix`userTry$$, *)
(*     Matrix`seed$$ = ""; Matrix`justUpdated$$ = False; Matrix`rowsA$$ = 3; *)
(*      Matrix`colA$$ = 3; Matrix`rowsB$$ = 3; Matrix`colB$$ = 3; *)
(*      Matrix`matriceA$$ = ConstantArray[0, {Matrix`rowsA$$, Matrix`colA$$}]; *)
(*      Matrix`matriceB$$ = ConstantArray[0, {Matrix`rowsB$$, *)
(*         Matrix`colB$$}]; ]]; If[Matrix`userTry$$, *)
(*    Matrix`matriceAB$$ = Matrix`matriceA$$ . Matrix`matriceB$$; *)
(*     Matrix`inputUtente$$ = ConstantArray["", *)
(*       {Dimensions[Matrix`matriceAB$$][[1]], Dimensions[Matrix`matriceAB$$][[*)
(*         2]]}]; ]; Column[{Row[{"Dimensione Matrice A:", *)
(*       InputField[Dynamic[Matrix`rowsA$$, If[IntegerQ[#1] && #1*1 <= #1 <= 6, *)
(*           Matrix`rowsA$$ = #1, If[StringMatchQ[ToString[#1], "*.*"], *)
(*            MessageDialog[*)
(*             "Inserire un numero maggiore o uguale a 1 e senza virgola"], *)
(*            MessageDialog[*)
(*             "Inserire un numero intero positivo compreso tra 1 e 6"]]] & ], *)
(*        Number, FieldSize -> {2, 1}, Alignment -> Center, *)
(*        Enabled ->  !Matrix`randomFill], "x", InputField[*)
(*        Dynamic[Matrix`colA$$, If[IntegerQ[#1] && #1*1 <= #1 <= 6, *)
(*           Matrix`colA$$ = #1, If[StringMatchQ[ToString[#1], "*.*"], *)
(*            MessageDialog[*)
(*             "Inserire un numero maggiore o uguale a 1 e senza virgola"], *)
(*            MessageDialog[*)
(*             "Inserire un numero intero positivo compreso tra 1 e 6"]]] & ], *)
(*        Number, FieldSize -> {2, 1}, Alignment -> Center, *)
(*        Enabled ->  !Matrix`randomFill]}], *)
(*     Row[{"Dimensione Matrice B:", InputField[Dynamic[Matrix`rowsB$$, *)
(*         If[IntegerQ[#1] && #1*1 <= #1 <= 6, Matrix`rowsB$$ = #1, *)
(*           If[StringMatchQ[ToString[#1], "*.*"], MessageDialog[*)
(*             "Inserire un numero maggiore o uguale a 1 e senza virgola"], *)
(*            MessageDialog[*)
(*             "Inserire un numero intero positivo compreso tra 1 e 6"]]] & ], *)
(*        Number, FieldSize -> {2, 1}, Alignment -> Center, *)
(*        Enabled ->  !Matrix`randomFill], "x", InputField[*)
(*        Dynamic[Matrix`colB$$, If[IntegerQ[#1] && #1*1 <= #1 <= 6, *)
(*           Matrix`colB$$ = #1, If[StringMatchQ[ToString[#1], "*.*"], *)
(*            MessageDialog[*)
(*             "Inserire un numero maggiore o uguale a 1 e senza virgola"], *)
(*            MessageDialog[*)
(*             "Inserire un numero intero positivo compreso tra 1 e 6"]]] & ], *)
(*        Number, FieldSize -> {2, 1}, Alignment -> Center, *)
(*        Enabled ->  !Matrix`randomFill]}, Alignment -> Center], Spacer[30], *)
(*     Column[{Row[Spacer[10]*{Row[{Column[{"Matrice A:", Dynamic[Grid[*)
(*                Table[With[{Matrix`i$ = Matrix`i, Matrix`j$ = Matrix`j}, *)
(*                  InputField[Dynamic[Matrix`matriceA$$[[Matrix`i$,*)
(*                     Matrix`j$]]], Number, FieldSize -> {3, 1}, Alignment -> *)
(*                    Center, Background -> Dynamic[If[*)
(*                      Matrix`currentElement$$ > -1 && *)
(*                       Matrix`currentElement$$ < Dimensions[*)
(*                          Matrix`matriceAB$$][[1]]*Dimensions[*)
(*                          Matrix`matriceAB$$][[2]] + 1 && Matrix`i$ == *)
(*                        Quotient[Matrix`currentElement$$ - 1, *)
(*                          Matrix`colB$$] + 1, RGBColor[0, 255, 0, 0.2], *)
(*                      White]], Appearance -> Dynamic[If[Matrix`userTry$$ || *)
(*                       Matrix`randomFill, Frameless]], Enabled -> Dynamic[*)
(*                      !Matrix`userTry$$]]], {Matrix`i, Matrix`rowsA$$}, *)
(*                 {Matrix`j, Matrix`colA$$}], Frame -> All, Spacings -> *)
(*                 {1, 1}, ItemSize -> {3, 2}]]}, Alignment -> Center], *)
(*            Spacer[20], Column[{"*"}]}], *)
(*          Row[{Column[{"Matrice B:", Dynamic[Grid[Table[With[{Matrix`i$ = *)
(*                    Matrix`i, Matrix`j$ = Matrix`j}, InputField[Dynamic[*)
(*                    Matrix`matriceB$$[[Matrix`i$,Matrix`j$]]], Number, *)
(*                   FieldSize -> {3, 1}, Alignment -> Center, Background -> *)
(*                    Dynamic[If[Matrix`currentElement$$ > 0 && *)
(*                       Matrix`currentElement$$ < Dimensions[*)
(*                          Matrix`matriceAB$$][[1]]*Dimensions[*)
(*                          Matrix`matriceAB$$][[2]] + 1 && Matrix`j$ == *)
(*                        Mod[Matrix`currentElement$$ - 1, Matrix`colB$$] + 1, *)
(*                      RGBColor[0, 255, 0, 0.2], White]], Appearance -> *)
(*                    Dynamic[If[Matrix`userTry$$ || Matrix`randomFill, *)
(*                      Frameless]], Enabled -> Dynamic[ !Matrix`userTry$$]]], *)
(*                 {Matrix`i, Matrix`rowsB$$}, {Matrix`j, Matrix`colB$$}], *)
(*                Frame -> All, Spacings -> {1, 1}, ItemSize -> {3, 2}]]}, *)
(*             Alignment -> Center], Spacer[20], Column[{"="}]}]}], *)
(*       Dynamic[Column[{Spacer[50], Dynamic[If[ !Matrix`randomFill, *)
(*            Button["Inizia", Matrix`userTry$$ = True, ImageSize -> 60], *)
(*            Invisible[Matrix`placeholder]]], Spacer[50], "Matrice A*B:", *)
(*          If[Matrix`colA$$ != Matrix`rowsB$$, Framed[*)
(*            Text[Style["Le colonne di A e le righe di B devono avere \*)
(*\ndimensioni uguali per poter generare una matrice", TextAlignment -> Center, *)
(*              FontColor -> Red]], Background -> LightGray], *)
(*           Column[{Dynamic[Grid[Table[With[{Matrix`i$ = Matrix`i, Matrix`j$ = *)
(*                   Matrix`j, Matrix`indice$ = (Matrix`i - 1)*Dimensions[*)
(*                       Matrix`matriceAB$$][[2]] + Matrix`j}, *)
(*                 If[Matrix`indice$ <= Matrix`currentElement$$, *)
(*                  If[Matrix`showErrors$$ && Matrix`matriceAB$$[[Matrix`i$,*)
(*                      Matrix`j$]] != Matrix`inputUtente$$[[Matrix`i$,*)
(*                      Matrix`j$]], If[ !NumberQ[Matrix`inputUtente$$[[*)
(*                       Matrix`i$,Matrix`j$]]], Style[Text[Dynamic[*)
(*                       Matrix`matriceAB$$[[Matrix`i$,Matrix`j$]]]], *)
(*                     FontColor -> Red], Style[Text[Matrix`inputUtente$$[[*)
(*                        Matrix`i$,Matrix`j$]]*"->"*Dynamic[*)
(*                        Matrix`matriceAB$$[[Matrix`i$,Matrix`j$]]]], *)
(*                     FontColor -> Red]], Style[Text[Dynamic[*)
(*                      Matrix`matriceAB$$[[Matrix`i$,Matrix`j$]]]]]], *)
(*                  InputField[Dynamic[Matrix`inputUtente$$[[Matrix`i$,*)
(*                     Matrix`j$]]], Number, FieldSize -> {Automatic, 2}, *)
(*                   Alignment -> Center, DefaultBaseStyle -> *)
(*                    {ShowStringCharacters -> False, *)
(*                     Matrix`ShowStringCharactersStyle -> "Placeholder"}, *)
(*                   FieldHint -> StringTemplate["\!\(\*SubsuperscriptBox[\(\[Sum]\)\*)
(*, \(k = 1\), \(`1`\)]\) `2` * `3`"][ToString[Matrix`rowsA$$], ToString[*)
(*                      StandardForm[Subscript[Matrix`a, Matrix`i$, *)
(*                        Matrix`k]]], ToString[StandardForm[Subscript[*)
(*                        Matrix`b, Matrix`k, Matrix`j$]]]], ImageSize -> *)
(*                    {Full, Automatic}]]], {Matrix`i, Dimensions[*)
(*                   Matrix`matriceAB$$][[1]]}, {Matrix`j, Dimensions[*)
(*                   Matrix`matriceAB$$][[2]]}], Frame -> All, Spacings -> *)
(*                {1, 1}, ItemSize -> {10, 2}]], Button[*)
(*              "Mostra il prossimo elemento", If[Matrix`currentElement$$ < *)
(*                Dimensions[Matrix`matriceAB$$][[1]]*Dimensions[*)
(*                   Matrix`matriceAB$$][[2]], Matrix`currentElement$$++], *)
(*              Enabled -> Matrix`userTry$$], Button["Torna indietro", *)
(*              If[Matrix`currentElement$$ > 0, Matrix`currentElement$$--], *)
(*              Enabled -> Matrix`userTry$$]}]]}, Alignment -> Center]]}, *)
(*      Alignment -> Center], Row[{Button["Verifica Risultato", *)
(*        If[Matrix`currentElement$$ != Dimensions[Matrix`matriceAB$$][[1]]**)
(*           Dimensions[Matrix`matriceAB$$][[2]], Matrix`showErrors$$ = True; *)
(*          Matrix`currentElement$$ = Dimensions[Matrix`matriceAB$$][[1]]**)
(*             Dimensions[Matrix`matriceAB$$][[2]] + 1; ], *)
(*        Enabled -> Matrix`userTry$$], Spacer[10], Button["Mostra Soluzione", *)
(*        Matrix`currentElement$$ = Dimensions[Matrix`matriceAB$$][[1]]**)
(*           Dimensions[Matrix`matriceAB$$][[2]] + 1, *)
(*        Enabled -> Matrix`userTry$$], Spacer[10], Button["Reset", *)
(*        Matrix`userTry$$ = False; Matrix`showErrors$$ = False; *)
(*         Matrix`currentElement$$ = 0; Matrix`seed$$ = ""; Matrix`rowsA$$ = 3; *)
(*         Matrix`colA$$ = 3; Matrix`rowsB$$ = 3; Matrix`colB$$ = 3; *)
(*         Matrix`randomFill = False; Matrix`matriceA$$ = ConstantArray[0, *)
(*           {Matrix`rowsA$$, Matrix`colA$$}]; Matrix`matriceB$$ = *)
(*          ConstantArray[0, {Matrix`rowsB$$, Matrix`colB$$}]; *)
(*         Matrix`matriceAB$$ = Matrix`matriceA$$ . Matrix`matriceB$$; *)
(*         Matrix`inputUtente$$ = ConstantArray["", {Matrix`rowsA$$, *)
(*            Matrix`colB$$}]; ]}]}], *)
(*  {{Matrix`randomFill, False, "Riempi randomicamente"}, {False, True}, *)
(*   ControlPlacement -> 1}, Row[{Manipulate`Place[1]*Spacer[20], *)
(*    Dynamic[If[Matrix`randomFill, Column[{Text["Random Seed : "*Green], *)
(*         InputField[Dynamic[Matrix`seed$$, If[IntegerQ[#1] && #1 >= 1, *)
(*             Matrix`seed$$ = #1, If[StringMatchQ[ToString[#1], "*-*"], *)
(*              MessageDialog["Il carattere \"-\" non pu\[OGrave] essere inserito"], *)
(*              If[StringMatchQ[ToString[#1], "*.*"], MessageDialog[*)
(*                "Inserire un numero maggiore o uguale a 1 e senza virgola"], *)
(*               MessageDialog[*)
(*                "Inserire un numero intero positivo maggiore o uguale a 1"]]]] \*)
(*& ], Number, FieldSize -> {10, 1.5}]}], Column[{Text["Random Seed : "*Red], *)
(*         InputField[Dynamic[Matrix`seed$$], Number, FieldSize -> {10, 1.5}, *)
(*          Enabled -> False]}]]]*Spacer[20]}], *)
(*  TrackedSymbols :> {Matrix`rowsA$$, Matrix`colA$$, Matrix`rowsB$$, *)
(*    Matrix`colB$$, Matrix`randomFill, Matrix`seed$$, Matrix`inputUtente$$, *)
(*    Matrix`showErrors$$, Matrix`userTry$$, Matrix`matriceA$$, *)
(*    Matrix`matriceB$$}, SynchronousUpdating -> True]*)
