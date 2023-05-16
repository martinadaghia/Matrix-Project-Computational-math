(* ::Package:: *)

BeginPackage["Matrix`"]
(*
prodottoMatrici::usage = ""

inputMatrice::usage = ""

askKindOfMatrix::usage = ""

askForDimensions::usage = ""

generateRandomMatrix::usage = ""
*)

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
            Row[{"Dimensione Matrice A:",
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
                "x", 
                InputField[Dynamic[colA,
                If[IntegerQ[#] && # 1 <= # <= 6 , colA = #, 
                   If[StringMatchQ[ToString[#], "*.*"], 
                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&],
                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill]
            }],
            Row[{"Dimensione Matrice B:",
                InputField[Dynamic[rowsB, 
                If[IntegerQ[#] && # 1 <= # <= 6 , rowsB = #, 
                   If[StringMatchQ[ToString[#], "*.*"], 
                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&],  
                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill],
                "x",
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
			                    If[randomFill,
			                        matriceA,
			                        Table[
			                            With[{i = i, j = j},
			                                InputField[Dynamic[matriceA[[i, j]]], 
				                                Number, FieldSize -> {3, 1}, 
				                                Alignment -> Center, 
				                                Background-> Dynamic@If[currentElement > -1 && currentElement < (Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]])+1 
				                                    && i == Quotient[(currentElement - 1), colB] + 1, 
				                                       Green, White],
			                                    Appearance -> Dynamic@If[userTry, Frameless],
				                                Enabled->Dynamic[Not[userTry]]
			                                ]
			                            ],
			                            {i, rowsA}, {j, colA}
			                        ]
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
			                    If[randomFill,
			                        matriceB,
			                        Table[
			                            With[{i = i, j = j},
			                                InputField[
			                                    Dynamic[matriceB[[i, j]]],  
				                                Number, FieldSize -> {3, 1}, 
				                                Alignment -> Center,
				                                Background-> Dynamic@If[currentElement > 0 && currentElement < (Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]])+1
				                                    && j == Mod[(currentElement - 1), colB] + 1, 
				                                        Green, White],
				                                Appearance -> Dynamic@If[userTry, Frameless],
				                                Enabled -> Dynamic[Not[userTry]]
				                            ]
			                            ],
			                            {i, rowsB}, {j, colB}
			                        ]
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
										    ImageSize -> {Full, Automatic}
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







