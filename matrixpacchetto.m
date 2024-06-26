(* ::Package:: *)

(* :Title: Matrix *)
(* :Context: matrixpacchetto` *)
(* :Author: Martina Daghia, Matrina Zauli, Riccardo Spini, Gabriele Fogu*)
(* :Summary: implementazione del gioco didattico Matrix, con annessa la spiegazione del prodotto tra due matrici *)
(* :Copyright: Matrix 2023 *)
(* :Package Version: 21 Maggio 2023 *)
(* :Mathematica Version: 13.2.1.0 *)
(* :Sources: biblio *)


(*Definamo il nostro pacchetto*)
BeginPackage["matrixpacchetto`"];


(*Definiamo la nostra funzione*)
GeneraInterfaccia::usage="GeneraInterfaccia []
	Funzione che permette di creare un'interfaccia interattiva e dinamica. Essa contiene diverse funzionalit\[AGrave], ovvero  
	permette di creare un esercizio randomicamente, permette all'utente di inserire personalmente  valori. 
	Inoltre, attraverso dei bottoni permette di iniziare il gioco, verificare il risultato inserito dall'utente,
	mostrare la soluzione generata dalla funzione e resettare l'ambiente di lavoro.";


(*Definiamo il nostro pacchetto/contesto privato*)
Begin["`Private`"]


(*Implementiamo la nostra funzione GeneraInterfaccia*)
GeneraInterfaccia[]:= DynamicModule[{
		 rowsA = 3, (*varibile che mi rappresenta le righe della matrice A, che fissiamo come valore di default 3*)
	     colA = 3, (*varibile che mi rappresenta le colonne della matrice A, che fissiamo come valore di default 3*)
	     rowsB = 3, (*varibile che mi rappresenta le righe della matrice B, che fissiamo come valore di default 3*)
	     colB = 3, (*varibile che mi rappresenta le colonne della matrice B, che fissiamo come valore di default 3*)
		 matriceA = ConstantArray[0, {3, 3}], (*creiamo la matrice A con dimensioni 3 (righe) x 3(colonne) con tutti valori 0*)
	     matriceB = ConstantArray[0, {3, 3}], (*creiamo la matrice B con dimensioni 3 (righe) x 3(colonne) con tutti valori 0*)
	     matriceAB = ConstantArray[0, {3, 3}], (*creiamo la matrice AB (risultante) con dimensioni 3 (righe) x 3(colonne) con tutti valori 0*)
	     inputUtente = ConstantArray["", {3, 3}], (*creiamo la matrice 3x3 con tutti valori vuoti per contenere l'input utente*)
	     currentElement = 0, (*elemento della matrice input attualmente in verifica con AB*)
	     randomFill = False, (*variabile bool che diventer\[AGrave] true se l'utente sceglie l'opzione random, se no rimarr\[AGrave] false*)
	     justUpdated = False, (*variabile bool di controllo per sapere se le matrici random sono state generate oppure no*)
	     showErrors = False, (*variabile bool di controllo per attivare la visualizzazione di errori*)
	     userTry = False, (*variabile bool di controllo per verificare se l'utente ha iniziato il suo tentativo oppure no*)
	     seed = "", (*inizializziamo il valore del seed*)
	     inputValue=""
    },
    
    Manipulate[ (*creiamo una interfaccia interattiva*)
    
    (*Caso randomico*)
    
    (*Se la variabile randomFill \[EGrave] true (che significa che l'utente ha premuto sul bottone per il caso randomico) e il seed \[EGrave] diverso da vuoto (\[EGrave] pieno)*)
       If[randomFill && seed!="",
       (*utilizziamo la funzione SeedRandom per inizializzare il generatore di numeri casuali con quel determinato valore di seed*)
          SeedRandom[seed]; 
          
          (* Caso in cui vengono definite le dimensioni delle matrici *)
          
          (*Se non sono state generate le matrici random*)
          If[!justUpdated, 
            (*AB ha rowsA righe e colB colonne se colA=rowsB*)
                justUpdated = True; (*setto la variabile booleana true poich\[EGrave] una volta che si entra in questo if, vengono generate le variabili random*)
			    showErrors = False; (**)
				currentElement=0; (**)
                rowsA = RandomChoice[{1, 2, 3, 4, 5, 6}]; (*funzione che permette di selezionare casualmente un numero di righe per la matrice A dalla lista {1, 2, 3, 4, 5, 6}*)
                colA = RandomChoice[{1, 2, 3, 4, 5, 6}]; (*funzione che permette di selezionare casualmente un numero di colonne per la matrice A dalla lista {1, 2, 3, 4, 5, 6}*)
                rowsB = colA; (*assegno al numero di righe della matrice B lo stesso valore del numero di colonne della matrice A in quanto devono essere uguali per consentire il prodotto tra le 2 matrici*)
                colB = RandomChoice[{1, 2, 3, 4, 5, 6}]; (*funzione che permette di selezionare casualmente un numero di colonne per la matrice B dalla lista {1, 2, 3, 4, 5, 6}*)
            ];
            matriceA = RandomInteger[{-10, 10}, {rowsA, colA}]; (*Riempimento matrice A randomica di numeri soltanto interi compresi tra -10 e 10*)
            matriceB = RandomInteger[{-10, 10}, {rowsB, colB}]; (*Riempimento matrice B randomica di numeri soltanto interi compresi tra -10 e 10*)
            matriceAB = Dot[matriceA, matriceB]; (*la matrice AB rappresenta il prodotto tra matrice A e B, il prodtto viene effettuato dalla bult-in Dot*)
			inputUtente = ConstantArray["", {rowsA, colB}]; (*matrice che mantiene in memoria l'input dell'utente sulla matrice AB*)
            justUpdated = False; (*risettiamo la variabile a false una volta finiti tutti i passaggi per il caso randomico*)
       ];
         
        Column[{
        (*Attraverso l'inputfield prendo le dimensioni delle matrici, effettuo dei controlli e aggiorno*)
        
        (*Faccio inserire all'utente le dimensioni della matrice A attraverso il campo di input interattivo, 
           il font del testo viene messo in tutto il codice come stile "Helvetica"*)
            Row[{Style["Dimensione Matrice A: ", FontFamily -> "Helvetica"], 
                InputField[Dynamic[rowsA, (*rowsA verr\[AGrave] aggiornata dinamicamente ogni volta che il valore nel campo di input viene modificato*)
                
					(*Controllo che il numero che l'utente ha inserito sia un intero e sia un numero compreso tra 1 e 6,
					se non lo \[EGrave] viene creata una finestra in cui viene dato un errore all'utente*)
	                If[IntegerQ[#] && 1 <= # <= 6 , 
	                rowsA = #;  (*assegno il valore inserito dell'utente nella variabile rowsA che rappresenta le righe di A*)
	                matriceA = ConstantArray[0, {rowsA, colA}]; (*creo la matrice A con tutti valori 0 in base alle righe e le colonne che gli passo come variabili*)
	                userTry = False, (*resetta matrici se si verificano cambiamenti delle dimensioni*)
			
						(*Controllo che il numero inserito non contenga il punto, nel caso lo contenga mando all'utente un messaggio di errore,
						in quanto le dimensioni delle matrici devono essere per forza un numero intero senza virgola*)
	                   If[StringMatchQ[ToString[#], "*.*"], 
	                         MessageDialog["Inserire un numero intero, le dimensioni delle matrici non possono avere numeri decimali con il punto.\n Ad esempio dimensioni come 2.1, 4.5 non sono accettate, ma sono accettate dimensioni come 2, 4, 3."], 
	                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&], 
	                
	                (*viene creato come campo di input di tipo numerico con una dimensione di visualizzazione {4, 2} e viene allineato al centro.
	                  questo campo viene abilitato o disabilitato in base al valore della variabile randomFill (se \[EGrave] true allora viene disabilitato o viceversa).
	                  viene applicato uno stile utilizzando "Helvetica"*)
	                Number, FieldSize -> {4, 2}, Alignment -> Center, Enabled -> !randomFill, BaseStyle -> {FontFamily -> "Helvetica"}

                ],
                
                Style[" x ", FontFamily -> "Helvetica"], 

                InputField[Dynamic[colA, (*colA verr\[AGrave] aggiornata dinamicamente ogni volta che il valore nel campo di input viene modificato*)
                
                (*Controllo che il numero che l'utente ha inserito sia un intero e sia un numero compreso tra 1 e 6,
				se non lo \[EGrave] viene creata una finestra in cui viene dato un errore all'utente*)
                If[IntegerQ[#] && 1 <= # <= 6 , 
                   colA = #; (*assegno il valore inserito dell'utente nella variabile colA che rappresenta le righe di A*)
                   matriceA = ConstantArray[0, {rowsA, colA}]; (*creo la matrice A con tutti valori 0 in base alle righe e le colonne che gli passo come variabili*)
                   userTry = False, (*resetta matrici se si verificano cambiamenti delle dimensioni*)
                   
                   (*Controllo che il numero inserito non contenga il punto, nel caso lo contenga mando all'utente un messaggio di errore,
				   in quanto le dimensioni delle matrici devono essere per forza un numero intero senza virgola*)
                   If[StringMatchQ[ToString[#], "*.*"], 
                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&],
                
                (*viene creato come campo di input di tipo numerico con una dimensione di visualizzazione {4, 2} e viene allineato al centro.
	              questo campo viene abilitato o disabilitato in base al valore della variabile randomFill (se \[EGrave] true allora viene disabilitato o viceversa).
	              viene applicato uno stile utilizzando "Helvetica"*)
                Number, FieldSize -> {4, 2}, Alignment -> Center, Enabled -> !randomFill, BaseStyle -> {FontFamily -> "Helvetica"}]
            }],
            
            (*Faccio inserire all'utente le dimensioni della matrice B attraverso il campo di input interattivo, 
            il font del testo viene messo in tutto il codice come stile "Helvetica"*)
            Row[{Style["Dimensione Matrice B: ", FontFamily -> "Helvetica"],
                InputField[Dynamic[rowsB, (*rowsB verr\[AGrave] aggiornata dinamicamente ogni volta che il valore nel campo di input viene modificato*)
                
                (*Controllo che il numero che l'utente ha inserito sia un intero e sia un numero compreso tra 1 e 6,
				se non lo \[EGrave] viene creata una finestra in cui viene dato un errore all'utente*)
                If[IntegerQ[#] && 1 <= # <= 6 , 
	               rowsB = #; (*assegno il valore inserito dell'utente nella variabile rowsB che rappresenta le righe di B*)
	               matriceB = ConstantArray[0, {rowsB, colB}]; (*creo la matrice B con tutti valori 0 in base alle righe e le colonne che gli passo come variabili*)
	               userTry = False, (*resetta matrici se si verificano cambiamenti delle dimensioni*)
	               
	               (*Controllo che il numero inserito non contenga il punto, nel caso lo contenga mando all'utente un messaggio di errore,
				   in quanto le dimensioni delle matrici devono essere per forza un numero intero senza virgola*)
                   If[StringMatchQ[ToString[#], "*.*"], 
                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&],  
                
                (*viene creato come campo di input di tipo numerico con una dimensione di visualizzazione {4, 2} e viene allineato al centro.
	              questo campo viene abilitato o disabilitato in base al valore della variabile randomFill (se \[EGrave] true allora viene disabilitato o viceversa).
	              viene applicato uno stile utilizzando "Helvetica"*)
                Number, FieldSize -> {4, 2}, Alignment -> Center, Enabled -> !randomFill, BaseStyle -> {FontFamily -> "Helvetica"}],
                
                Style[" x ", FontFamily -> "Helvetica"],
                
                InputField[Dynamic[colB, (*colB verr\[AGrave] aggiornata dinamicamente ogni volta che il valore nel campo di input viene modificato*)
                
                (*Controllo che il numero che l'utente ha inserito sia un intero e sia un numero compreso tra 1 e 6,
				se non lo \[EGrave] viene creata una finestra in cui viene dato un errore all'utente*)
                If[IntegerQ[#] && 1 <= # <= 6 , 
	               colB = #; (*assegno il valore inserito dell'utente nella variabile colB che rappresenta le colonne di B*)
	               matriceB = ConstantArray[0, {rowsB, colB}]; (*creo la matrice B con tutti valori 0 in base alle righe e le colonne che gli passo come variabili*)
	               userTry = False, (*resetta matrici se si verificano cambiamenti delle dimensioni*)
	               
	               (*Controllo che il numero inserito non contenga il punto, nel caso lo contenga mando all'utente un messaggio di errore,
				   in quanto le dimensioni delle matrici devono essere per forza un numero intero senza virgola*)
                   If[StringMatchQ[ToString[#], "*.*"], 
                         MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"], 
                MessageDialog["Inserire un numero intero positivo compreso tra 1 e 6"]]]&],
                
                (*viene creato come campo di input di tipo numerico con una dimensione di visualizzazione {4, 2} e viene allineato al centro.
	              questo campo viene abilitato o disabilitato in base al valore della variabile randomFill (se \[EGrave] true allora viene disabilitato o viceversa).
	              viene applicato uno stile utilizzando "Helvetica"*)
                Number, FieldSize -> {4, 2}, Alignment -> Center, Enabled -> !randomFill, BaseStyle -> {FontFamily -> "Helvetica"}]
                
            }, Alignment->Center], (*allineo tutta la riga al centro*)
            
            Spacer[30],  (*utilizzato per inserire uno spazio vuoto di lunghezza 30 all'interno della disposizione grafica*)
            Column[{
	            Row[Spacer[10]{ (*utilizzato per inserire uno spazio vuoto di lunghezza 10 all'interno della disposizione grafica*)
		            Row[{
			            Column[{
			                Style["Matrice A: ", FontFamily -> "Helvetica"],
			                Dynamic@Grid[
		                        Table[
		                        (*Per ogni elemento (i,j ovvero per ogn rigaA e colonnaA) della matrice A creo un inputField con queste caratteristiche*)
		                            With[{i = i, j = j},
		                                InputField[Dynamic[matriceA[[i, j]], 
						(*I numeri che l'utente pu\[OGrave] inserire nelle matrici (A e B) devono stare dentro al range di -9999 e 9999,
							nel caso il numero inserito non sia interno al range viene mostrato all'utente un messaggio di errore e riazzerata la casella.*)
		                                If[!(-9999 <= # <= 9999), 
		                                    MessageDialog["Il numero che hai inserito non \[EGrave] corretto. Inserire numeri da -9999 a 9999"], 
		                                    matriceA[[i, j]] = #
		                                ]&], 
			                                Number, FieldSize -> {5, 2},
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
			        Spacer[20], (*utilizzato per inserire uno spazio vuoto di lunghezza 20 all'interno della disposizione grafica*)
			        Column[{ Style["\[CenterDot]", Bold, FontFamily -> "Helvetica"]}]
		            }],
		            Row[{
			            Column[{
			                Style["Matrice B: ", FontFamily -> "Helvetica"],
			                Dynamic@Grid[
		                        Table[
		                            With[{i = i, j = j},
		                                InputField[
		                                    Dynamic[matriceB[[i, j]], 
			                                If[!(-9999 <= # <= 9999), 
			                                    MessageDialog["Il numero che hai inserito non \[EGrave] corretto. Inserire numeri da -9999 a 9999"], 
			                                    matriceB[[i, j]] = #
			                                ]&],  
			                                Number, FieldSize -> {5, 2}, 
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
			            Spacer[20], (*utilizzato per inserire uno spazio vuoto di lunghezza 20 all'interno della disposizione grafica*)
			            Column[{Style["=", FontFamily -> "Helvetica"]}]	
   
		            }]        
	            }],
				Dynamic@Column[{
					Spacer[50], (*utilizzato per inserire uno spazio vuoto di lunghezza 50 all'interno della disposizione grafica*)
					Dynamic@If[!randomFill, 
					(*Se le colonne della matrice A sono uguali alle righe della matrice B, allora \[EGrave] possibile fare il prodotto tra le due matrici*)
						Button["Inizia", 
							If[colA == rowsB,
								matriceAB = Dot[matriceA, matriceB]; (*prodotto tra matrice A e matrice B*)
								(*Matrice che ci serve per memorizzare i valori inseriti dall'utente*)
							    inputUtente = ConstantArray["", {Dimensions[matriceAB][[1]], Dimensions[matriceAB][[2]]}];
								userTry=True
							],
							BaseStyle->{FontFamily -> "Helvetica", FontSize->30},
							Enabled -> !userTry
						],
						Invisible[""] (*quando le matrici sono generate randomicamente l'utente potr\[AGrave] solamente iniziare l'esercizio senza cliccare il bottone "Inizia"*)
					]
					,
					Spacer[50], (*utilizzato per inserire uno spazio vuoto di lunghezza 50 all'interno della disposizione grafica*)
					Row[{
					Column[{
		                Style["Matrice A\[CenterDot]B:", FontFamily -> "Helvetica"],
		                If[colA != rowsB,
			                Framed[Text[Style["Le colonne di A e le righe di B devono avere 
	dimensioni uguali per poter generare una matrice", FontFamily -> "Helvetica", TextAlignment->Center, FontColor->Red]], Background->LightGray],
							Column[{Dynamic@Grid[
			                    Table[
			                    
			                    (*Mostramo la matrice AB*)
							        With[{i = i, j = j, indice = (i - 1) * Dimensions[matriceAB][[2]] + j},
							        
							        (*Se stiamo generando l'elemento indice che \[EGrave] prima dell'ultimo elemento di cui si \[EGrave] richiesta la correzione*)
							            If[indice <= currentElement,
							            If[inputUtente[[i,j]]== Null, inputUtente[[i,j]] = ""];
							            (*Se siamo in fase di verifica degli errori commessi e l'elemento inserito dell'utente \[EGrave] sbagliato*)
							                If[showErrors && matriceAB[[i, j]] != inputUtente[[i, j]],
							                    (*ELEMENTO NON CORRETTO*)
							                    
							                    (*Se l'inputfield non era vuoto e era un numero *)
								                If[!MissingQ[inputUtente[[i,j]]] && NumberQ[inputUtente[[i,j]]],
								                    (*ERRORE DI CALCOLO COMMESSO*)
								                    (*Mostriamo l'input dell'utente, --> con poi il valore corretto*)
							                        Style[
							                            (*ToString[inputUtente[[i,j]]]<> " ->"<> ToString[matriceAB[[i, j]]],*)
							                            ToString[inputUtente[[i,j]]],
							                            FontColor -> Red
							                        ],
							                    	(*ELEMENTO NULLO*)
									                (*Style[Dynamic[matriceAB[[i, j]]], FontColor -> Red]*)
									                InputField[
													    Dynamic[inputUtente[[i,j]]],
													    Number, 
													    FieldSize -> {Automatic, 3},
													    Alignment -> Center,
													    Enabled-> userTry,
													    Appearance-> If[!userTry, Frameless], (*feedback visivo inputfield per quando l'utente non ha ancora premuto "inizia"*)
							
													    DefaultBaseStyle -> {ShowStringCharacters -> False, ShowStringCharactersStyle -> "Placeholder"},
														(* Mostriamo la formula parametrica per calcolare il valore di quella cella *)
														FieldHint ->  
														"\!\(\*SubsuperscriptBox[\(\[Sum]\), \(k = 1\), \("<>ToString[rowsA]<>"\)]\)" 
														<> ToString[TraditionalForm[Subscript[Symbol["a"], i, Symbol["k"]]], InputForm]
														<> "*"
														<> ToString[TraditionalForm[Subscript[Symbol["b"], Symbol["k"], j]], InputForm]
														 ,
													    ImageSize -> {Full, Automatic},
													    BaseStyle -> Bold
			 										]
							                    ],
							                    
							                    (*ELEMENTO INSERITO CORRETTO*)
						                        Style[Dynamic[matriceAB[[i, j]]], 
							                        Background->White,
				                                    FontColor -> RGBColor["#32aa52"] (*colore verde*)
			                                    ]   
						                    ],
						                    (*Inputfield della matrice AB vuoto*)
								            InputField[
											    Dynamic[inputUtente[[i,j]]],
											    Number, 
											    FieldSize -> {Automatic, 3},
											    Alignment -> Center,
											    Enabled-> userTry,
											    Appearance-> If[!userTry, Frameless], (*feedback visivo inputfield per quando l'utente non ha ancora premuto "inizia"*)
								
											    DefaultBaseStyle -> {ShowStringCharacters -> False, ShowStringCharactersStyle -> "Placeholder"},
												(* Mostriamo la formula parametrica per calcolare il valore di quella cella *)
												FieldHint -> 
												"\!\(\*SubsuperscriptBox[\(\[Sum]\), \(k = 1\), \("<>ToString[rowsA]<>"\)]\)" 
												<> ToString[TraditionalForm[Subscript[Symbol["a"], i, Symbol["k"]]], InputForm]
												<> "*"
												<> ToString[TraditionalForm[Subscript[Symbol["b"], Symbol["k"], j]], InputForm]
												 ,
											    ImageSize -> {Full, Automatic},
											    BaseStyle -> Bold
	 										]
							            ]
							        ],
							        {i, Dimensions[matriceAB][[1]]}, {j, Dimensions[matriceAB][[2]]}
							    ],
				                Frame -> All, Spacings -> {1, 1}, ItemSize -> {10, 2}
					            ]
					          
				            }, Alignment->Center]
		                ]
	                }, Alignment->Center],
	                Spacer[80], (*utilizzato per inserire uno spazio vuoto di lunghezza 80 all'interno della disposizione grafica*)
	                Column[{
			(*I bottoni risolvi precedente e risolvi successivo sono una "pseudo guida" per l'utente in cui si evidenziano le righe e le colonne delle matrici e la casellina corrispondente nella matrice AB in cui andr\[AGrave] inserito il calcolo.
				Risolvi precedente ti consente di tornare indietro al passaggio precedente evidenziandoti il passo successivo, invece
				Risolvi successivo ti permette di andare avanti e continuare la guida per l'utente illuminandoti le caselline delle matrici nel modo corretto.*)
					            Button[
					                "Risolvi successivo",
								    If[currentElement < (Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]])+1,
								        currentElement++
								    ],
									Enabled->userTry && currentElement < Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]],
									BaseStyle->{FontFamily -> "Helvetica", FontSize->30}
								],
					            Spacer[100],
					            Button[
					                "Risolvi precedente",
								    If[currentElement > 0, currentElement--],
									Enabled->userTry && currentElement < (Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]])+1,
									BaseStyle->{FontFamily -> "Helvetica", FontSize->30}
								] 
							}, 
							Alignment->Center],
							Spacer[80] (*utilizzato per inserire uno spazio vuoto di lunghezza 80 all'interno della disposizione grafica*)
	                }]
		        }, Alignment->Center]
            }, Alignment->Center],
            Spacer[20], (*utilizzato per inserire uno spazio vuoto di lunghezza 20 all'interno della disposizione grafica*)
			Row[{
			(*Verifica risultato
				verifica il risultato inserito dall'utente mostrando gli errori che ha effettuato in rosso e di fianco la correzione e mostrando le risposte corrette in verde.
			  
			  Mostra soluzione
			  	Stampa la soluzione del prodotto tra le due matrici e colora i valori dentro alle celle di verde.
			  
			  Pulisci (=pulisci soluzione)
			  	Resetta tutta l'interfaccia utente azzerando ogni casellina e ripristinandola.
			*)
				Dynamic@Button["Verifica Risultato",
					If[currentElement != Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]],
						showErrors = True;
						currentElement=(Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]])+1;
					],

					Enabled->userTry && currentElement < Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]],
					BaseStyle->{FontFamily -> "Helvetica", FontSize->30}
				],
				Spacer[20], (*utilizzato per inserire uno spazio vuoto di lunghezza 20 all'interno della disposizione grafica*)
				Dynamic@Button["Mostra Soluzione", 
					currentElement=(Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]])+1,
					Enabled->userTry && currentElement < Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]],
					BaseStyle->{FontFamily -> "Helvetica", FontSize->30}
				],
				Spacer[20], (*utilizzato per inserire uno spazio vuoto di lunghezza 20 all'interno della disposizione grafica*)
				Dynamic@Button["Ritenta Soluzione", 
					showErrors = False;
					currentElement = 0;
					(*ciclo su tutti gli elementi della matrice di input(input(tente), paragonarli alla matriceAB e se sono diversi in input matrice mettere = "" *)
					Do[
				        If[inputUtente[[i, j]] != matriceAB[[i, j]],
				            inputUtente[[i, j]] = "";
				        ],
				        {i, Dimensions[inputUtente][[1]]},
				        {j, Dimensions[inputUtente][[2]]}
				    ],
									
					Enabled -> Not[currentElement < Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]]], BaseStyle->{FontFamily -> "Helvetica", FontSize->30}
				],
				Spacer[350], (*utilizzato per inserire uno spazio vuoto di lunghezza 350 all'interno della disposizione grafica*)
				Button["Pulisci",
					userTry=False;
					showErrors=False;
					currentElement=0;
					seed="";
					inputValue="";
					rowsA = 3; 
					colA = 3; 
					rowsB = 3; 
					colB = 3;
					randomFill = False;
					matriceA = ConstantArray[0,{rowsA,colA}];
					matriceB = ConstantArray[0,{rowsB,colB}];
					matriceAB = Dot[matriceA, matriceB];
					inputUtente = ConstantArray["", {rowsA, colB}];
					ImageSize->500,
					BaseStyle->{FontFamily -> "Helvetica", FontSize->30}
				]
			}, Alignment->Center]
		}, Alignment->Center],
		Dynamic@If[randomFill, Column[{Style[Text["Modalit\[AGrave] Matrici Random", BaseStyle->{FontFamily -> "Helvetica", FontSize->60}]], Spacer[60]}], Column[{Style[Text["Modalit\[AGrave] Matrici Custom", BaseStyle->{FontFamily -> "Helvetica", FontSize->60}]], Spacer[60]}]],
		Row[ Spacer[20]{ (*utilizzato per inserire uno spazio vuoto di lunghezza 20 all'interno della disposizione grafica*)
			Row[{Style[Text["Passa a: ", BaseStyle->{FontFamily -> "Helvetica", FontSize->38}]],
				(*Se l'esercizio \[EGrave] impostato per generare matrici random mostro il bottone per passare alla modalit\[AGrave] manuale e viceversa*)
				Dynamic@If[randomFill, 
					Button["Modalit\[AGrave] matrici personalizzate", 			
						rowsA = 3; 
						colA = 3; 
						rowsB = 3; 
						colB = 3;
						matriceAB = ConstantArray[0, {rowsB, colA}];
						inputUtente = ConstantArray["", {rowsB, colA}];
						seed="";	
						inputValue="";
						matriceA = ConstantArray[0,{rowsA, colA}];
						matriceB = ConstantArray[0,{rowsB, colB}];
						randomFill = False;
						userTry = False,
						ImageSize->500,
						BaseStyle->{FontFamily -> "Helvetica", FontSize->30},
						Enabled-> currentElement < Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]]
					], 
					Button["Modalit\[AGrave] matrici randomiche",
						randomFill = True;
						userTry = True,
						ImageSize->500,
						BaseStyle->{FontFamily -> "Helvetica", FontSize->30},
						Enabled-> currentElement < Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]] (*WUAAAA*)
					]
				]
			}],
			Spacer[500], (*utilizzato per inserire uno spazio vuoto di lunghezza 500 all'interno della disposizione grafica*)
			(*Se siamo in modalit\[AGrave] random do la possibilit\[AGrave] di isnerire il seed e controllo che sia valido*)
			Column[{
			Dynamic@If[randomFill,
			    Column[{
			        Style[Text["Random Seed: " InputField[Dynamic@inputValue,
			            Number, FieldSize -> {16, 2},  
			            BaseStyle->{FontFamily -> "Helvetica", FontSize->30}
			        ] Green], FontFamily -> "Helvetica", FontSize->30]
			    }],
			    Column[{
			        Style[Text["Random Seed: " InputField[Dynamic@inputValue, 
			            Number, FieldSize -> {16, 2}, 
			            Enabled -> False, 
			            BaseStyle->{FontFamily -> "Helvetica", FontSize->30}
			            ] Red], ShowSyntaxStyles->False, FontFamily -> "Helvetica", FontSize->30]
			    }]
			],
			Spacer[20],
			Dynamic@If[randomFill, 
			 Button["Conferma seed", 
		      If[IntegerQ[inputValue] && inputValue >= 1,
			                seed = inputValue,
			                If[StringMatchQ[ToString[inputValue], "*-*"],
			                    MessageDialog["Il seed deve essere positivo."],
			                    If[StringMatchQ[ToString[inputValue], "*.*"],
			                        MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"],
			                        MessageDialog["Inserire un numero intero positivo maggiore o uguale a 1"]
			                    ]
			                ]
			            ], 
			            ImageSize->{300,50}, 
			            Enabled->True, 
						BaseStyle->{FontFamily -> "Helvetica", FontSize->30}
 		      ],
 		       Button["Conferma seed", 
		      If[IntegerQ[inputValue] && inputValue >= 1,
			                seed = inputValue,
			                If[StringMatchQ[ToString[inputValue], "*-*"],
			                    MessageDialog["Il seed deve essere positivo."],
			                    If[StringMatchQ[ToString[inputValue], "*.*"],
			                        MessageDialog["Inserire un numero maggiore o uguale a 1 e senza virgola"],
			                        MessageDialog["Inserire un numero intero positivo maggiore o uguale a 1"]
			                    ]
			                ]
			            ], 
			            ImageSize->{300,50}, 
			            Enabled->False, 
						BaseStyle->{FontFamily -> "Helvetica", FontSize->30}
 		      ]
 		   ]
 		      }]
		}],
		(*Variabili che scatenano l'aggiornamento dell'interfaccia dinamica quando vengono modificati*)
		TrackedSymbols:>{rowsA, colA, rowsB, colB, randomFill, seed, inputUtente, showErrors, userTry, matriceA, matriceB},
		(*Aggiornamento sincrono
		
		currentElement < Dimensions[matriceAB][[1]]*Dimensions[matriceAB][[2]]*)
		SynchronousUpdating->True
	]
]



(*chiusura del contesto privato*)
End[]


(*chiusura del pacchetto*)
EndPackage[]
