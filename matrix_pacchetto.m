(* ::Package:: *)

(* ::Input:: *)
(*(*intestazioni*)*)
(*BeginPackage["Matrix`"]*)
(*Matrix::usage ="Matrix[DIFFICULTY] crea un'istanza del gioco Sudoku con vari livelli di difficolt\[AGrave] selezionabile dal client ponendo DIFFICULTY='easy', 'medium', 'hard'"*)
(**)
(*Solve::usage =*)
(*"Risolve il prodotto tra due matrici"*)
(*(*Se creiamo altre funzioni da usare nel pacchetto vanno descritte qui*)*)
(**)
(*Begin["`Private`"]*)
(*(* DIRETTIVE*)
(*Inserire nella prima coppia di graffe tutte le variabili che usiamo nel codice. TUTTE. Inserire all'interno delle seconde graffe il codice vero e proprio*)
(**)*)
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
(*inputMatrice[name_]:=Module[{n,m, messaggio,valore, matrice}, (*dentro al module ci vanno le varibili locali*)*)
(*	n=Input["Inserisci il numero di righe della matrice "<>name <> ": "];*)
(*	m=Input["Inserisci il numero di colonne della matrice" <>name <> ": "];*)
(**)
(*	Print["Numero di righe della matrice "<>name <> " che hai inserito: ", n];*)
(*	Print["Numero di colonne della matrice "<>name <> " che hai inserito: ",m];*)
(*	*)
(*	Print["Creazione della matrice vuota"];*)
(*	matrice=ConstantArray[0,{n,m}];*)
(*	*)
(*	Do[messaggio="Inserisci il valore per l'elemento ("<>ToString[i]<>","<>ToString[j]<>"): ";*)
(*		valore=Input[messaggio];*)
(*		While[!NumericQ[valore],Print["Errore: Inserisci un valore numerico."];*)
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
(*A=inputMatrice["A"];*)
(*B=inputMatrice["B"];*)
(*result=prodottoMatrici[A,B];*)
(*Print[result];*)
(**)
(*Grid[{{Grid[A,Frame->All],"x",Grid[B,Frame->All], "=", Grid[result,Frame->All]}}]*)
(**)
(*End[]*)
(*EndPackage[]*)
(**)
(**)
(**)
(**)











?C











