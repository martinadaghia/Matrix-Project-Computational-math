(* ::Package:: *)

(* ::Input:: *)
(**)
(**)
(**)
(*(**)
(*ome del pacchetto,la versione,l'autore e la data di creazione*)
(**)*)
(**)
(*(*intestazioni*)*)
(*BeginPackage["Matrix`"] (*definisco il pacchetto*)*)
(*Matrix::usage ="Matrix[DIFFICULTY] crea un'istanza del gioco Sudoku con vari livelli di difficolt\[AGrave] selezionabile dal client ponendo DIFFICULTY='easy', 'medium', 'hard'"*)
(**)
(*(**)
(*non \[EGrave] necessario utilizzare un contesto separato nel pacchetto per il prodotto tra 2 matrici.*)
(*utilizzare un contesto separato \[EGrave] utile per organizzare e isolare il codice in contesti logici distinti --> per il nostro progetto semplice non importa*)
(**)
(*Begin["`Private`"]*)
(**)
(**)*)
(**)
(*(* DIRETTIVE*)
(*	Inserire nella prima coppia di graffe tutte le variabili che usiamo nel codice. TUTTE. Inserire all'interno delle seconde graffe il codice vero e proprio*)
(**)*)
(**)
(*(* di seguito sono elencate le funzioni facenti parte del pacchetto Matrix *)*)
(**)
(*(* FUNZIONE PER PRODOTTO SCALARE TRA 2 MATRICI *)*)
(*prodottoMatrici[A_,B_]:=Module[{m,n,q,p,resultMatrix},*)
(*{m,n}=Dimensions[A];(* restituisce le dimensioni dela matrice come numero righe e numero colonne *)*)
(*{q,p}=Dimensions[B];*)
(*If[m!=p,  (* controllo il numero di riga della matrice A con il numero di colonne della matrice B *)*)
(*Print["Le dimensioni delle matrici non sono compatibili per il prodotto."];*)
(*Return[]*)
(*];*)
(*resultMatrix=ConstantArray[0,{m,p}]; (* creo la matrice C con m (numero di righe) e p (numero di colonne), imposto gli elementi a 0 *)*)
(*Do[resultMatrix[[i,j]]=Sum[A[[i,k]]*B[[k,j]],{k,n}],{i,m},{j,p}]; (*i da 1 a m, j da 1 a p, k da 1 a n*)*)
(*resultMatrix*)
(*]*)
(**)
(*(* FUNZIONE PER FAR INSERIRE LA MATRICE ALL'UTENTE *)*)
(*inputMatrice[name_]:=Module[{n,m, messaggio,inserimentoRighe,inserimentoColonne, valore, matrice}, (*dentro al module ci vanno le varibili locali*)*)
(*	inserimentoRighe = "Inserisci il numero di righe della matrice "<>name <> ": " ;inserimentoColonne = "Inserisci il numero di colonne della matrice "<>name <> ": " ;*)
(**)
(*While[!NumericQ[n],*)
(*	Print["Errore: Inserisci un valore numerico."];*)
(*			n=Input[inserimentoRighe];*)
(*	];*)
(*	While[!NumericQ[m],*)
(*	Print["Errore: Inserisci un valore numerico."];*)
(*		m=Input[inserimentoColonne];*)
(*	];*)
(**)
(*	Print["Numero di righe della matrice "<>name <> " che hai inserito: ", n];*)
(*	Print["Numero di colonne della matrice "<>name <> " che hai inserito: ",m];*)
(*	*)
(*	Print["Creazione della matrice vuota"];*)
(*	matrice=ConstantArray[0,{n,m}];*)
(*	*)
(*	Do[messaggio="Inserisci il valore per l'elemento ("<>ToString[i]<>","<>ToString[j]<>"): ";*)
(*		valore=Input[messaggio];*)
(*		While[!NumericQ[valore],*)
(*		Print["Errore: Inserisci un valore numerico."];*)
(*			valore=Input[messaggio];*)
(*	];*)
(*		matrice[[i,j]]=valore;,{i,n},{j,m}*)
(*	];*)
(**)
(*	matrice;*)
(*	Print["Matrice "<>name <> " creata."];*)
(*	Print[matrice];*)
(*	Return[matrice]*)
(*]*)
(**)
(*(**)
(*	A=inputMatrice["A"];*)
(*	B=inputMatrice["B"];*)
(*	result=prodottoMatrici[A,B];*)
(*	Print[result];*)
(*	*)
(*	Grid[{{Grid[A,Frame->All],"x",Grid[B,Frame->All], "=", Grid[result,Frame->All]}}]*)
(**)*)
(**)
(* *)
(*(* FUNZIONE CHE GENERA DUE MATRICI RANDOMICHE CON IL SEED DATO IN INPUT DALL'UTENTE*)*)
(*randomMatrici[n_,m_] :=Module[{seed,matriceA,matriceB},*)
(*seed=Input["Inserisci il valore del seed (compreso tra 0 e 10000): "];*)
(*While[seed<0||seed>10000,*)
(*seed=Input["Valore non valido. Inserisci un seed compreso tra 0 e 10000: "];*)
(*]; *)
(*SeedRandom[seed];*)
(*Print["Il seed \[EGrave]: "];*)
(*Print[seed];*)
(**)
(*matriceA=RandomInteger[{0,10},{n,m}];*)
(*matriceB=RandomInteger[{0,10},{n,m}];*)
(**)
(*Print["Matrice randomica A: "];*)
(*Print[matriceA];*)
(*Print["Stampa griglia della matrice A: "];*)
(*Grid[matriceA,Frame->All];*)
(**)
(*Print["Matrice randomica B: "];*)
(*Print[matriceB];*)
(*Print["Stampa griglia della matrice B: "];*)
(*Grid[matriceB,Frame->All];*)
(**)
(*{matriceA,matriceB}*)
(**)
(*]*)
(**)
(*(**)
(*{n,m}={4,4};*)
(*{matrix1,matrix2}=randomMatrici[n,m];*)
(*Grid[matrix1,Frame->All];*)
(*Grid[matrix2,Frame->All];*)
(*result=prodottoMatrici[matrix1,matrix2];*)
(*	Print[result];*)
(*Grid[{{Grid[matrix1,Frame->All],"x",Grid[matrix2,Frame->All], "=", Grid[result,Frame->All]}}]*)
(**)
(**)*)
(**)
(*(**)
(*End[]*)
(**)*)
(*EndPackage[]*)
(**)
(**)
(**)
(**)


4











?C










