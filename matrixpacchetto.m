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


mamatrici::usage = "matrici[] 
	Funzione che permette di creare un'interfaccia interattiva e dinamica. Essa contiene diverse funzionalit\[AGrave], ovvero  
	permette di creare un esercizio randomicamente, permette all'utente di inserire personalmente  valori. 
	Inoltre, attraverso dei bottoni permette di iniziare il gioco, verificare il risultato inserito dall'utente,
	mostrare la soluzione generata dalla funzione e resettare l'ambiente di lavoro.";
	
Begin["`Private`"];

mamatrici[]:= DynamicModule[{
		 rowsA = 3,
	     colA = 3,
	     rowsB = 3,
	     colB = 3,
	     (*creiamo le matrici 3x3 con tutti valori 0*)
		 matriceA = ConstantArray[0, {3, 3}], 
	     matriceB = ConstantArray[0, {3, 3}],
	     matriceAB = ConstantArray[0, {3, 3}],
	     (*creiamo la matrice 3x3 con tutti valori vuoti*)
	     inputUtente = ConstantArray["", {3, 3}],
	     currentElement = 0, (*elemento della matrice AB in cui si effettua il calcolo del prodotto*)
	     randomFill = False, (*variabile bool che diventer\[AGrave] true se l'utente sceglie l'opzione random, se no rimarr\[AGrave] false*)
	     justUpdated = False, (*variabile bool di controllo per sapere se le matrici random sono state generate oppure no*)
	     showErrors = False, (*variabile bool di controllo per vedere se sono presenti errori o no sul bottone verifica risultato*)
	     userTry = False, (*variabile bool di controllo per verificare se l'utente ha iniziato il suo tentativo oppure no*)
	     seed = "" (*inizializziamo il valore del seed*)
    },
    Manipulate[
    
    (*Caso randomico*)
       If[randomFill && seed!="",
          SeedRandom[seed];
          
          (* Definite le dimensioni delle matrici
          Non sono state generate le matrici random*)
          If[!justUpdated, 
            (*AB ha rowsA righe e colB colonne se colA=rowsB*)
                justUpdated = True;
			    showErrors = False;
				currentElement=0;
				(*le righe e le colonne avranno dei valori compresi tra questa lista*)
                rowsA = RandomChoice[{1, 2, 3, 4, 5, 6}]; 
                colA = RandomChoice[{1, 2, 3, 4, 5, 6}];
                rowsB = colA;
                colB = RandomChoice[{1, 2, 3, 4, 5, 6}]; 
            ];
            (*Riempimento matrici randomiche di numeri solo interi compresi tra -10 e 10*)
            matriceA = RandomInteger[{-10, 10}, {rowsA, colA}];
            matriceB = RandomInteger[{-10, 10}, {rowsB, colB}];
            matriceAB = Dot[matriceA, matriceB]; (*prodotto tra matrice A e B*)
			inputUtente = ConstantArray["", {rowsA, colB}]; (*matrice che mantiene in memoria l'input dell'utente su AB*)
            justUpdated = False;
       ];
         
        Column[{
        (*Attraverso l'inputfield prendo le dimensioni delle matrici, effettuo dei controlli e aggiorno*)
            Row[{"Dimensione Matrice A: ",
                InputField[Dynamic[rowsA, 
	                If[IntegerQ[#] && 1 <= # <= 6 , 
	                rowsA = #; 
	                matriceA = ConstantArray[0, {rowsA, colA}];
	                userTry = False, (*resetta matrici se si verificano cambiamenti delle dimensioni*)
	                   If[StringMatchQ[ToString[#], "*.*"], 
	                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
	                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&], 
	                
	                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill
                ],
                " x ", 
                InputField[Dynamic[colA,
                If[IntegerQ[#] && 1 <= # <= 6 , 
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
                If[IntegerQ[#] && 1 <= # <= 6 , 
	               rowsB = #; 
	               matriceB = ConstantArray[0, {rowsB, colB}];
	               userTry = False, 
                   If[StringMatchQ[ToString[#], "*.*"], 
                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&],  
                Number, FieldSize -> {2, 1}, Alignment -> Center, Enabled -> !randomFill],
                " x ",
                InputField[Dynamic[colB, 
                If[IntegerQ[#] && 1 <= # <= 6 , 
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
		                        (*Per ogni elemento (i,j ovvero per ogn rigaA e colonnaA) della matrice A creo un inputField con queste caratteristiche*)
		                            With[{i = i, j = j},
		                                InputField[Dynamic[matriceA[[i, j]]], 
			                                Number, FieldSize -> {3, 1}, 
			                                Alignment -> Center, 
			                                (* Il background si attiva solamente quando siamo nella correzione della matrice AB.
			                                Se l'indice in analisi \[EGrave] fra 1 e la cardinalit\[AGrave] di AB usiamo una formula per risalire alla riga che l'ha generato e la illuminiamo di verde*)
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
			                                Background-> Dynamic@If[currentElement > -1 && currentElement < (Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]])+1
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
								(*Matrice che ci serve per memorizzare i valori inseriti dall'utente*)
							    inputUtente = ConstantArray["", {Dimensions[matriceAB][[1]], Dimensions[matriceAB][[2]]}];
								userTry=True
							],
							ImageSize->60,
							Enabled -> !userTry
						],
						Invisible[placeholder] (*quando le matrici sono generate randomicamente l'utente potr\[AGrave] solamente iniziare l'esercizio senza cliccare il bottone "Inizia"*)
					]
					,
					Spacer[50],
	                "Matrice A*B:",
	                If[colA != rowsB,
		                Framed[Text[Style["Le colonne di A e le righe di B devono avere 
dimensioni uguali per poter generare una matrice", TextAlignment->Center, FontColor->Red]], Background->LightGray],
						Column[{Dynamic@Grid[
		                    Table[
		                    
		                    (*Mostramo la matrice AB*)
						        With[{i = i, j = j, indice = (i - 1) * Dimensions[matriceAB][[2]] + j},
						        
						        (*Se stiamo generando l'elemento indice che \[EGrave] prima dell'ultimo elemento di cui si \[EGrave] richiesta la correzione*)
						            If[indice <= currentElement,
						            
						            (*Se siamo in fase di verifica degli errori commessi e l'elemento inserito dell'utente \[EGrave] sbagliato*)
						                If[showErrors && matriceAB[[i, j]] != inputUtente[[i, j]],
						                    (*ELEMENTO NON CORRETTO*)
						                    
						                    (*Se l'inputfield non era vuoto e era un numero *)
							                If[!MissingQ[inputUtente[[i,j]]] && NumberQ[inputUtente[[i,j]]],
							                    (*ERRORE DI CALCOLO COMMESSO*)
							               
							                    (*Mostriamo l'input dell'utente, --> con poi il valore corretto*)
						                        Style[
						                            StringForm[ToString[inputUtente[[i,j]]], "->", ToString[matriceAB[[i, j]]]],
						                            FontColor -> Red
						                        ],
							                    (*ELEMENTO NULLO*)
								                Style[Dynamic[matriceAB[[i, j]]], FontColor -> Red]
								                
						                    ],
						                    
						                    (*ELEMENTO INSERITO CORRETTO*)
					                        Style[Dynamic[matriceAB[[i, j]]], 
						                        Background -> Dynamic@If[currentElement > -1 && currentElement < (Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]])+1 
			                                    && indice == currentElement, 
			                                       RGBColor[0, 255, 0, .2], White],
			                                    FontColor -> RGBColor["#32aa52"] (*colore verde*)
		                                    ]   
					                    ],
					                    (*Inputfield della matrice AB vuoto*)
							            InputField[
										    Dynamic[inputUtente[[i,j]]],
										    Number, 
										    FieldSize -> {Automatic, 2},
										    Alignment -> Center,
										    Enabled-> userTry,
										    Appearance-> If[!userTry, Frameless], (*feedback visivo inputfield per quando l'utente non ha ancora premuto "inizia"*)
				
										    DefaultBaseStyle -> {ShowStringCharacters -> False, ShowStringCharactersStyle -> "Placeholder"},
											(* Mostriamo la formula parametrica per calcolare il valore di quella cella *)
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
				Button["Pulisci",
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
				(*Se l'esercizio \[EGrave] impostato per generare matrici random mostro il bottone per passare alla modalit\[AGrave] manuale e viceversa*)
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
			(*Se siamo in modalit\[AGrave] random do la possibilit\[AGrave] di isnerire il seed e controllo che sia valido*)
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
		(*Variabili che scatenano l'aggiornamento dell'interfaccia dinamica quando vengono modificati*)
		TrackedSymbols:>{rowsA, colA, rowsB, colB, randomFill, seed, inputUtente, showErrors, userTry, matriceA, matriceB},
		(*Aggiornamento sincrono*)
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
mamatrici[]
(*
da aggiungere la storia dei colori se ce la facciamo,
aggiungere come si fa lo svolgimento con una finestra pop up
*)





