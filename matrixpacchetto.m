(* ::Package:: *)

(* :Title: Matrix *)
(* :Context: matrixpacchetto` *)
(* :Author: Martina Daghia, Matrina Zauli, Riccardo Spini, Gabriele Fogu*)
(* :Summary: implementazione del gioco didattico Matrix, con annessa la spiegazione del prodotto tra due matrici *)
(* :Copyright: Matrix 2023 *)
(* :Package Version: 19 Maggio 2023 *)
(* :Mathematica Version: 13.2.1.0 *)
(* :Sources: biblio *)

BeginPackage["matrixpacchetto`"]


matrici::usage = "matrici[] 
	Funzione che permette di creare l'interfaccia. Essa contiene diverse funzionalit\[AGrave], ovvero  
	permette di creare un esercizio randomicamente, permette all'utente di inserire personalmente  valori. 
	Inoltre, attraverso dei bottoni permette di iniziare il gioco, verificare il risultato inserito dall'utente,
	mostrare la soluzione generata dalla funzione e resettare l'ambiente di lavoro.";
	
Begin["`Private`"];

matrici[]:= DynamicModule[{
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
       If[randomFill && seed!="",
          SeedRandom[seed];
          If[!justUpdated,
            (*AB ha rowsA righe e colB colonne sse colA=rowsB*)
                justUpdated = True;
			    showErrors = False;
				currentElement=0;
                rowsA = RandomChoice[{2, 3, 4, 5}];
                colA = RandomChoice[{2, 3, 4, 5}];
                rowsB = colA;
                colB = RandomChoice[{2, 3, 4, 5}]; 
            ];
            matriceA = RandomInteger[{-10, 10}, {rowsA, colA}];
            matriceB = RandomInteger[{-10, 10}, {rowsB, colB}];
            matriceAB = Dot[matriceA, matriceB];
			inputUtente = ConstantArray["", {rowsA, colB}];
            justUpdated = False;
       ];
         (*If[!randomFill, userTry=False];
            Print["not rF"];*)
        (*  
       
        If[randomFill && seed != "",
        
            Print["rF and seed"];
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
            Print["not uT"];
	            seed="";
	            justUpdated = False;
	            (*rowsA=3;
	            colA=3;
	            rowsB=3;
	            colB=3;*);
	            matriceA = ConstantArray[0, {rowsA, colA}];
	            matriceB = ConstantArray[0, {rowsB, colB}];
            ]
        ];
        If[userTry,
            Print["uT"];
            Print[matriceA];
            Print[matriceB];
	        matriceAB = Dot[matriceA, matriceB];
	        inputUtente = ConstantArray["", {Dimensions[matriceAB][[1]], Dimensions[matriceAB][[2]]}];
        ];
        *);
        Column[{
            Row[{"Dimensione Matrice A: ",
            (*
	            Chiedere come validare il range, 
	            If[isValidDimension[rowsA], rowsA = #] &, 
	            non funziona, lo stesso vale per il seed: InputField non accetta range
            *)
                InputField[Dynamic[rowsA, 
	                If[IntegerQ[#] && # 1 <= # <= 6 , 
	                rowsA = #; 
	                matriceA = ConstantArray[0, {rowsA, colA}];
	                userTry = False, 
	                   If[StringMatchQ[ToString[#], "*.*"], 
	                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
	                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&], 
	                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill
                ],
                " x ", 
                InputField[Dynamic[colA,
                If[IntegerQ[#] && # 1 <= # <= 6 , 
                   colA = #; 
                   matriceA = ConstantArray[0, {rowsA, colA}];
                   userTry = False, 
                   If[StringMatchQ[ToString[#], "*.*"], 
                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&],
                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill]
            }],
            Row[{"Dimensione Matrice B: ",
                InputField[Dynamic[rowsB, 
                If[IntegerQ[#] && # 1 <= # <= 6 , 
	               rowsB = #; 
	               matriceB = ConstantArray[0, {rowsB, colB}];
	               userTry = False, 
                   If[StringMatchQ[ToString[#], "*.*"], 
                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&],  
                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill],
                " x ",
                InputField[Dynamic[colB, 
                If[IntegerQ[#] && # 1 <= # <= 6 , 
	               colB = #;
	               matriceB = ConstantArray[0, {rowsB, colB}];
	               userTry = False, 
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
							If[colA == rowsB,
								matriceAB = Dot[matriceA, matriceB];
							    inputUtente = ConstantArray["", {Dimensions[matriceAB][[1]], Dimensions[matriceAB][[2]]}];
								userTry=True
							],
							ImageSize->60,
							Enabled -> !userTry
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
						                    (*Elemento non corretto*)
						                    Print[inputUtente[[i,j]] ,"wrong", indice];
							                If[!MissingQ[inputUtente[[i,j]]] && NumberQ[inputUtente[[i,j]]],
							                    (*Errore di calcolo commesso*)
						                        Style[
						                            StringForm["`1` -> `2`", ToString[inputUtente[[i,j]]], ToString[matriceAB[[i, j]]]],
						                            FontColor -> Red
						                        ],
							                    (*Elemento nullo*)
								                Style[Dynamic[matriceAB[[i, j]]], FontColor -> Red]
								                
						                    ],
						                    (*Elemento inserito corretto*)
					                        Style[Dynamic[matriceAB[[i, j]]], 
						                        Background -> Dynamic@If[currentElement > -1 && currentElement < (Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]])+1 
			                                    && indice == currentElement, 
			                                       RGBColor[0, 255, 0, .2], White],
			                                    FontColor -> RGBColor["#32aa52"]
		                                    ]   
					                    ],
							            InputField[
										    Dynamic[inputUtente[[i,j]]],
										    Number, (*Otherwise it breaks on check result with Number and " " as input*)
										    FieldSize -> {Automatic, 2},
										    Alignment -> Center,
										    Enabled-> userTry,
										    Appearance-> If[!userTry, Frameless], 
										    DefaultBaseStyle -> {ShowStringCharacters -> False, ShowStringCharactersStyle -> "Placeholder"},
											FieldHint -> StringJoin[
											    "\!\(\*SubsuperscriptBox[\(\[Sum]\), \(k = 1\), \(",
											    ToString[rowsA],
											    "\)]\)",
											    ToString[TraditionalForm[Subscript[a, HoldForm[i], HoldForm[k]]]],
											    "*",
											    ToString[TraditionalForm[Subscript[b, HoldForm[k], HoldForm[j]]]]
											],
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
								Enabled->userTry && currentElement < Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]]
							],
							Button[
				                "Torna indietro",
							    If[currentElement > 0, currentElement--],
								Enabled->userTry && currentElement < Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]]
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
					matriceAB = Dot[matriceA, matriceB];
					inputUtente = ConstantArray["", {rowsA, colB}];
				]
			}]
		}],
		Row[ Spacer[20]{
			Row[{
				
				Dynamic@If[randomFill, 
					Button["Riempi manualmente", 	
						seed="";				
						rowsA = 3; 
						colA = 3; 
						rowsB = 3; 
						colB = 3;
						matriceA = ConstantArray[0,{rowsA,colA}];
						matriceB = ConstantArray[0,{rowsB,colB}];
						randomFill = False;
						matriceAB = Dot[matriceA, matriceB];
						userTry = False;
					], 
					Button["Riempi randomicamente",
						randomFill = True;
						userTry = True;
					]
				]
			}],
			Dynamic@If[randomFill,
			    Column[{
			        Text["Random Seed : " Green],
			        InputField[Dynamic[seed,
			            If[IntegerQ[#] && # >= 1,
			                seed = #,
			                If[StringMatchQ[ToString[#], "*-*"],
			                    MessageDialog["Il seed deve essere positivo."],
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

End[]		
EndPackage[]

(*
	input <=9999 && Null = 0
	mostra soluzione e mostra prossimo illuminano di verde i numeri, ok?
	modifica di tutte le dimensioni => generare AB (pare fatto)
*)
matrici[]



