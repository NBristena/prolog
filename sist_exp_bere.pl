/* SET WORKING DIRECTORY
:-use_module(library(file_systems),[]),file_systems:current_directory(_,'D:/0. Facultate/[3] Sem II/Expert/Proiect').
*/


/*-----------------------------------------------------------------------------
 * 		INITIALIZARE
 *-----------------------------------------------------------------------------*/
close_all :- current_stream(_,_,S), close(S), fail ; true.
curata_bc :- current_predicate(P), abolish(P,[force(true)]), fail ; true.

:- discontiguous trad/3, executa/1, citeste_cuvant/3, interogheaza/4, realizare_scop/3, cum/1.

:- use_module(library(lists)).
:- use_module(library(file_systems)).
:- use_module(library(system)).

:- dynamic scop/1.
:- dynamic regula/3.
:- dynamic intrebare/3.
:- dynamic fapt/3.
:- dynamic deja_intrebat/1.

:- op(900,fy,not).
not(P) :- P, !, fail.
not(_).


/*--------------------------------------------------------------------------------
 * 		   PORNIRE SISTEM
 *--------------------------------------------------------------------------------*/
go :-
	retractall(deja_intrebat(_)),
	retractall(fapt(_,_,_)),
	repeat,
	nl, write('  Alegeti una din urmatoarele optiuni: '),
	nl, write(' (Incarca | Consulta | Afiseaza_fapte | Cum.. | Exit)'),
	nl, citeste_linie([H|T]),nl,
	executa([H|T]), H == exit.

% --------------- CITIRE DIN CONSOLA -------------------------------------------
citeste_linie([Cuv|Lista_cuv]) :- 
	get_code(Char),
	citeste_cuvant(Char, Cuv, Char1), 
	rest_cuvinte_linie(Char1, Lista_cuv).
 
	rest_cuvinte_linie(-1, []) :- !. % a gasit EOF, se opreste
		
	rest_cuvinte_linie(Char,[]) :- (Char == 13 ; Char == 10), !. % a gasit ENTER(\n ; \r), se opreste

	rest_cuvinte_linie(Char, [Cuv1|Lista_cuv]) :-
		citeste_cuvant(Char, Cuv1, Char1),      
		rest_cuvinte_linie(Char1, Lista_cuv).


/*--------------------------------------------------------------------------------
 * 		   INCARCA SI PROCESEAZA REGULI
 *--------------------------------------------------------------------------------*/
executa([incarca]) :- incarca, !, write('~*~ Fisier incarcat cu succes ~*~'),nl.
	
incarca :- F = 'reguli_bere.txt',
%	write('Introduceti numele fisierului intre apostroafe, urmat de un punct:'),nl, read(F),
	file_exists(F), !, incarca(F).

incarca :- nl,
	write(' !X! Fisierul introdus nu exista !X!'),nl,nl,
	incarca.

% --------------- INCARCARE FISIER ---------------------------------------------
incarca(F) :-
	retractall(deja_intrebat(_)), retractall(fapt(_,_,_)),
	retractall(scop(_)), retractall(regula(_,_,_)), retractall(intrebare(_,_,_)),
	see(F), incarca_reguli, seen, !.

	incarca_reguli :-
		repeat,
			citeste_propozitie(L),
			proceseaza(L),
		L == [end_of_file].
	
% --------------- CITIRE DIN FISIER --------------------------------------------
citeste_propozitie([Cuv|Lista_cuv]) :-
	get_code(Char), citeste_cuvant(Char, Cuv, Char1), 
	rest_cuvinte_propozitie(Char1, Lista_cuv).
	
	rest_cuvinte_propozitie(-1, []) :- !. % EOF
		
	rest_cuvinte_propozitie(Char,[]) :- Char == 46, !. %PUNCT

	rest_cuvinte_propozitie(Char,[Cuv1|Lista_cuv]) :-
		citeste_cuvant(Char,Cuv1,Char1),      
		rest_cuvinte_propozitie(Char1,Lista_cuv).
	
% -------------- PROCESARE PROPOZITII ------------------------------------------
proceseaza([end_of_file]) :- !.
proceseaza(L) :- trad(R,L,[]),assertz(R), !.

/*   SCOP   */
trad(scop(X)) --> [scop,'@',X].

/* INTREBARI */
trad(intrebare(Atr,M,P)) --> [intreaba,'@',Atr], afiseaza(Atr,P), lista_optiuni(M).
	 
	afiseaza(_,P) -->  [enunt,'@',P].
	afiseaza(P,P) -->  [].
	 
	lista_optiuni(M) --> [variante,'@','('], lista_de_optiuni(M).
		 
		lista_de_optiuni([Element]) -->  [Element,')'].
		lista_de_optiuni([Element|T]) --> [Element],['|','|'], lista_de_optiuni(T).
	
/*  REGULI  */
trad( regula(N,premise(Daca),concluzie(Atunci,F)) ) --> identificator(N), daca(Daca), atunci(Atunci,F).
	 
	identificator(N) --> [regula,'@',N].
	 
	daca(Daca) --> [lista_premise,'@','('], lista_premise(Daca).
		 
		lista_premise([Daca]) --> propoz_daca(Daca), [')',concluzie,'@','('].
		lista_premise([Prima|Celalalte]) --> propoz_daca(Prima), lista_premise(Celalalte).
				
			propoz_daca(av(Atr,Val)) --> [Atr,'@',Val,'@'].
			propoz_daca(av(Atr,da)) --> ['@',Atr,'@'].
%			propoz_daca(not av(Atr,da)) --> ['@','!',Atr,'@'].
			propoz_daca(av(Atr,nu)) --> ['@','!',Atr,'@'].

	atunci(Atunci,FC) --> propoz_atunci(Atunci), [',',fc,'@'], [FC], ['@',')'].
	atunci(Atunci,100) --> propoz_atunci(Atunci), [')'].
			
		propoz_atunci(av(Atr,Val)) --> [Atr,'@','=',Val].
		propoz_atunci(av(Atr,da)) --> ['@',Atr].
%		propoz_atunci(not av(Atr,da)) --> ['@','!',Atr].
		propoz_atunci(av(Atr,nu)) --> ['@','!',Atr].

/*   ERORI   */
trad('-X- Eroare la parsare -X-'-L,L,_).

/* TRY IN CONSOLE:
trad(X,[regula,1.0,daca,buget_disponibil,este,redus,atunci,in_romania,fc,90.0],[]). 
trad(X,[regula,6.0,daca,in_romania,si,la_mare,si,tip_oferta,este,sejur_1_luna,si,buget_disponibil,este,mare,si,anotimp,este,vara,atunci,loc_concediu,este,neptun,fc,80.0],[]).

trad(X,[regula,'@',1.0,lista_premise,'@','(',buget_disponibil,'@',redus,'@',')',concluzie,'@','(','@',in_romania,',',fc,'@',90,'@',')'],[]).
trad(X,[regula,'@',6.0,lista_premise,'@','(','@',in_romania,'@','@',la_mare,'@',tip_oferta,'@',sejur_1_luna,'@',buget_disponibil,'@',mare,'@',anotimp,'@',vara,'@',')',concluzie,'@','(',loc_concediu,'@','=',neptun,',',fc,'@',80.0,'@',')'],[]).
*/


/*--------------------------------------------------------------------------------
 * 		   CONSULTA SISTEMUL
 *--------------------------------------------------------------------------------*/
executa([consulta]) :- 
	retractall(deja_intrebat(_)),
	retractall(fapt(_,_,_)),
	scopuri_princ,nl,nl, !.

	scopuri_princ :-
		scop(Atr),
		determina(Atr), 
		write(' ____________________'),nl,
		write('|'),nl,
		( fapt(av(Atr,_),_,_) -> 
			ordoneaza_solutii(Atr),
			write('| Optiunile dumneavoastra pentru '), write(Atr), write(' sunt:'),nl, 
			write('|'), 
			afiseaza_scop(Atr)
		;
			write('| Nu avem nicio '), write(Atr),write(' care sa se'),nl,
			write('| potriveasca cu preferintele tale.'),nl
		),
		write('|____________________'),nl,
		fail.
		
	scopuri_princ.

% --------------- INCEPE INTEROGAREA -------------------------------------------
/* realizare NOT fapt cu NotFC-> realizare fapt cu FC si NotFC = -FC
 * realizare fapt existent -> all good
 * realizare fapt ce poate fi interogat (not deja_intrebat, exista intrebare) -> interogheaza
 */
determina(Atr) :- realizare_scop(av(Atr,_),_,[scop(Atr)]),!.
determina(_).

%---#NOT FAPT 
	realizare_scop(not Scop, Not_FC, Istorie) :-
		realizare_scop(Scop, FC, Istorie),
		Not_FC is - FC, !.

%---#FAPT EXISTENT
	realizare_scop(Scop, FC, _) :-
		fapt(Scop, FC, _), !.

%---#FAPT DE INTREBAT
	realizare_scop(Scop, FC, Istorie) :-
		pot_interoga(Scop, Istorie),
		!,realizare_scop(Scop, FC, Istorie).

		pot_interoga(av(Atr,_), Istorie) :-
			not deja_intrebat(av(Atr,_)),
			intrebare(Atr, Optiuni, Mesaj),
			interogheaza(Atr, Mesaj, Optiuni, Istorie),nl,
			asserta( deja_intrebat(av(Atr,_)) ).

%INTREBARI YES/NO 
			interogheaza(Atr, Mesaj, [da,nu], Istorie) :-
				!, write('Q: '), write(Mesaj),nl,
				citeste_opt([da,nu]),
				de_la_utiliz(X, Istorie, [da,nu]),
				det_val_fc(X, Val, FC),
				asserta( fapt(av(Atr,Val), FC, [utiliz]) ).

%INTREBARI OPTIUNI
			interogheaza(Atr, Mesaj, Optiuni, Istorie) :-
				write('Q: '), write(Mesaj),nl,
				citeste_opt(Optiuni),
				de_la_utiliz(X, Istorie, Optiuni),
				assert_fapt(Atr, X).

%	%AFISEAZA OPTIUNI
				citeste_opt(Optiuni) :-
					%append(['('], Optiuni, Opt1),
					%append(Opt1, [')'], Opt),
					write('( '),
					scrie_lista_optiuni(Optiuni),
					write(' )\n').

%		%CITESTE RASPUNS
					de_la_utiliz(X, Istorie, Lista_opt) :-
						repeat,write(': '), citeste_linie(X),
						proceseaza_raspuns(X, Istorie, Lista_opt).

%			%PROCESEAZA RASPUNS
						proceseaza_raspuns([de_ce], Istorie, _) :- nl,
							write('|   Pentru regula:'),nl,
							afis_istorie(Istorie), !, fail.

						proceseaza_raspuns([X], _, Lista_opt):-
							member(X, Lista_opt).

						proceseaza_raspuns([X, fc, FC], _, Lista_opt):-
							member(X, Lista_opt), float(FC).
			
%---#FAPT DE DEMONSTRAT
	realizare_scop(Scop, FC_curent, Istorie) :-
		fg(Scop, FC_curent, Istorie).
		
		fg(Scop, FC_curent, Istorie) :-
			regula(N, premise(Lista), concluzie(Scop,FC)),
			demonstreaza(N, Lista, FC_premise, Istorie),
			ajusteaza(FC, FC_premise, FC_nou),
			actualizeaza(Scop, FC_nou, FC_curent, N),
			FC_curent == 100,!.
		fg(Scop, FC, _) :- fapt(Scop, FC, _).
			
% 	DEMONSTREAZA PREMISE			
			demonstreaza(N, ListaPremise, Val_finala, Istorie) :-
				dem(ListaPremise, 100, Val_finala, [N|Istorie]),!.

				dem([], Val_finala, Val_finala, _).

				dem([H|T], Val_actuala, Val_finala, Istorie) :-
					realizare_scop(H, FC, Istorie),
					Val_interm is min(Val_actuala, FC),
					Val_interm >= 20,
					dem(T, Val_interm, Val_finala, Istorie).

% 	AJUSTEAZA FC = FC_regula * FC_premise / 100
			ajusteaza(FC1,FC2,FC) :-
				X is FC1 * FC2 / 100,
				FC is round(X).

% 	ACTUALIZEAZA FAPTE
			actualizeaza(Scop, FC_nou, FC, RegulaN) :-
				fapt(Scop, FC_vechi, _),
				combina(FC_nou, FC_vechi, FC),
				retract( fapt(Scop, FC_vechi, Reguli_vechi) ),
				asserta( fapt(Scop, FC, [RegulaN | Reguli_vechi]) ), !.
			
			actualizeaza(Scop, FC, FC, RegulaN) :-
				asserta( fapt(Scop, FC, [RegulaN]) ).
/* * * * * * * * * * * * * * * * * * * * *
 * 		CALCUL FACTOR CERTITUDINE 		 *
 * * * * * * * * * * * * * * * * * * * * */
				combina(FC1 ,FC2, FC) :-
/* 2 pozitive */	FC1 >= 0, FC2 >= 0,
					X is FC2 * (100 - FC1) / 100 + FC1,
					FC is round(X).

				combina(FC1, FC2, FC) :-
/* 2 negative */	FC1 < 0, FC2 < 0,
					X is - ( -FC1 -FC2 * (100 + FC1) / 100 ),
					FC is round(X).

				combina(FC1, FC2, FC) :-
/* 1 pozitiva */	(FC1 < 0 ; FC2 < 0),
/* 1 negativa */	(FC1 > 0 ; FC2 > 0),
					FCM1 is abs(FC1), 
					FCM2 is abs(FC2),
					MFC is min(FCM1, FCM2),
					X is 100 * (FC1 + FC2) / (100 - MFC),
					FC is round(X).

% --------------- AFISEAZA CONCLUZII -------------------------------------------
ordoneaza_solutii(Atr) :- setof(sol(FC,X,I), Atr^retract(fapt(av(Atr,X),FC,I)), L), adauga_sol_ord(Atr,L).
				
	adauga_sol_ord(Atr,[sol(FC,X,I)|T]):- asserta(fapt(av(Atr,X),FC,I)), adauga_sol_ord(Atr,T).
	adauga_sol_ord(_,[]).

afiseaza_scop(Atr) :-nl,
	fapt(av(Atr,Val),FC,_),
	FC >= 20, scrie_scop(av(Atr,Val),FC),nl,fail.

afiseaza_scop(_).

scrie_scop(av(_,Val),FC) :-
	write('| --> '), write(Val), 
	write('  ~  factor de certitudine '), FC1 is integer(FC),write(FC1).


/*--------------------------------------------------------------------------------
 * 		   AFISEAZA FAPTE STIUTE
 *--------------------------------------------------------------------------------*/
executa([afiseaza_fapte]) :-
	afisare_fapte,!.
	
afisare_fapte :-
	    write(' ____________________'),
	nl,	write('|'),
	nl, write('| Fapte existente in baza de cunostinte:'),
	nl, write('|'),
	nl, write('|  (Atribut,Valoare) ~ FactorCertitudine'),
	nl, listeaza_fapte,
	    write('|____________________'),
	nl,nl.

listeaza_fapte:-  
	(fapt(av(Atr,Val),FC,_) ; fapt(not av(Atr,_),FC,_), Val = 'nu'), 
	write('|('), write(Atr), write(','), write(Val), write(') '),
	write('~'), write(' certitudine '), FC1 is integer(FC),write(FC1),
	nl,fail.
	
listeaza_fapte.


/*--------------------------------------------------------------------------------
 * 		   CUM S-A AJUNS LA O CONCLUZIE - ISTORIC
 *--------------------------------------------------------------------------------*/
executa([cum|L]) :- 
	write(' ____________________'),nl,
	write('|     Demonstratie:'),nl,
	cum(L),!,
	write('|____________________'),nl.
	
cum([]) :- nl,write('Cum ce? '),nl,
	citeste_linie(Linie),nl,
	transforma_scop(Scop,Linie), cum(Scop).

cum(L) :- transforma_scop(Scop,L), cum(Scop).

cum(not Scop) :- 
	fapt(Scop,FC,Reguli),
	lista_float_int(Reguli,Reguli1),
	FC < -20,
	transforma_scop(not Scop,PG),
	write('|'),nl,
	append(['|->'],PG,L),
	append(L,[a,fost,derivat,cu, ajutorul, 'regulilor:'|Reguli1],LL),
	scrie_lista_cu_spatiu(LL),
	afis_reguli(Reguli),fail.

cum(Scop) :-
	fapt(Scop,FC,Reguli),
	lista_float_int(Reguli,Reguli1),
	FC > 20,
	transforma_scop(Scop,PG),
	write('|'),nl,
	append(['|->'],PG,L),
	append(L,[a,fost,derivat,cu,ajutorul,'regulilor:'|Reguli1],LL),
	scrie_lista_cu_spatiu(LL),
	afis_reguli(Reguli),fail.

	lista_float_int([],[]).
		
	lista_float_int([Regula|Reguli],[Regula1|Reguli1]):-
		(Regula \== utiliz,
		Regula1 is integer(Regula);
		Regula ==utiliz, Regula1=Regula),
		lista_float_int(Reguli,Reguli1).

	transforma_scop(av(A,da),[A]) :- !.
	transforma_scop(not av(A,da), [nu,A]) :- !.
	transforma_scop(av(A,nu), [nu,A]) :- !.
	transforma_scop(av(A,V),[A,este,V]).
		
	afis_reguli([]).
	afis_reguli([N|X]) :-
		afis_regula(N),
		premisele(N),
		afis_reguli(X).

		afis_regula(N) :-
			regula(N, premise(Lista_premise), concluzie(Scop,FC)),
			NR is integer(N),
			write('|\n'),
			scrie_lista_fara_spatiu(['| regula@',NR]),
			write('| lista_premise@(\n'),
			scrie_lista_premise(Lista_premise),
			scrie_lista_fara_spatiu(['| concluzie@']),
			transforma_concluzie(Scop,Scop_tr),
			FC1 is integer(FC),
			append(['|     ('],Scop_tr,L1),
			append(L1,[' , fc@'],L2),
			append(L2,[FC1],L3),
			append(L3,['@)'],Concluzie),
			scrie_lista_fara_spatiu(Concluzie).

		premisele(N) :-
			regula(N, premise(Lista_premise), _),
			!, cum_premise(Lista_premise).

			cum_premise([]).
			cum_premise([Scop|X]) :-
				cum(Scop),
				cum_premise(X).
cum(_).

/*--------------------------------------------------------------------------------
 * 		   IESIRE SISTEM
 *--------------------------------------------------------------------------------*/
executa([exit]):-!.


/*--------------------------------------------------------------------------------
 * 		   EROARE 
 *--------------------------------------------------------------------------------*/
executa([_|_]) :- write('-X- Comanda incorecta -X-'),nl.
	
	
%_____________________________________________________________________________________________________________________________________________




scrie_lista_premise([]) :- write('| )\n').

scrie_lista_premise([H|T]) :-
	transforma_premisa(H,H_tr),
	write('|'), spatii(5), scrie_lista_fara_spatiu(H_tr),
	scrie_lista_premise(T).


transforma_premisa(av(A,da),['@',A,'@']) :- !.
transforma_premisa(not av(A,da), ['@!',A,'@']) :- !.
transforma_premisa(av(A,nu), ['@!',A,'@']) :- !.
transforma_premisa(av(A,V),[A,'@',V,'@']).

transforma_concluzie(av(A,da),['@',A]) :- !.
transforma_concluzie(not av(A,da), ['@!',A]) :- !.
transforma_concluzie(av(A,nu), ['@!',A]) :- !.
transforma_concluzie(av(A,V),[A,'@','=',V]).

% APARUT FIINDCA NU SE MAI ADAUGA SEPARAT ATRIBUTELE BOOLEENE
/*assert_fapt(Atr,[nu,fc,FC]) :-
	!,NFC is -FC, asserta( fapt(av(Atr,da),NFC,[utiliz]) ).*/

assert_fapt(Atr,[Val,fc,FC]) :-
	!,asserta( fapt(av(Atr,Val),FC,[utiliz]) ).

% APARUT FIINDCA NU SE MAI ADAUGA SEPARAT ATRIBUTELE BOOLEENE
/*assert_fapt(Atr,[nu]) :-
	asserta( fapt(av(Atr,da),-100,[utiliz])).*/

assert_fapt(Atr,[Val]) :-
	asserta( fapt(av(Atr,Val),100,[utiliz])).


/*det_val_fc([nu],da,-100).

det_val_fc([nu,FC],da,NFC) :- NFC is -FC.

det_val_fc([nu,fc,FC],da,NFC) :- NFC is -FC.*/

det_val_fc([Val,FC],Val,FC).

det_val_fc([Val,fc,FC],Val,FC).

det_val_fc([Val],Val,100).

        
afis_istorie([]) :-nl.

afis_istorie([scop(X)|T]) :-
	write('|\n'),scrie_lista_cu_spatiu(['|','-->',scopul,este,X]),!,
	afis_istorie(T).

afis_istorie([N|T]) :-
	afis_regula(N),!,afis_istorie(T).




/*--------------------------------------------------------------------------------
 * 		   FUNCTII "GLOBALE" 
 *--------------------------------------------------------------------------------*/

spatii(0).
spatii(N):- N > 0, write(' '), N1 is N-1, spatii(N1).

scrie_lista_cu_spatiu([]):-nl.
scrie_lista_cu_spatiu([H|T]) :- write(H), spatii(1), scrie_lista_cu_spatiu(T).

scrie_lista_fara_spatiu([]):-nl.
scrie_lista_fara_spatiu([H|T]) :- write(H), scrie_lista_fara_spatiu(T).

scrie_lista_optiuni([H|[]]) :- write(H).
scrie_lista_optiuni([H|T]) :- write(H), write(' / '), scrie_lista_optiuni(T).


% --------------------- CITIRE -------------------------------------------------
/* OPTIUNI citeste_cuvant
 * -> 1. end of file
 * -> 2. caracter_cuvant
 * -> 3. numar
 * -> 4. propozitie intre apostroafe
 * -> 5. cuvant normal
 * -> 6. orice altceva (ignora)
 */
% ------------ 1. END OF FILE
citeste_cuvant(-1,end_of_file,-1):-!. 

% ------------ 2. CARACTER ACCEPTAT
citeste_cuvant(Character,Cuvant,Character1) :-   
	caracter_cuvant(Character),!, 
	name(Cuvant, [Character]),get_code(Character1).
	
% ------------ 3. CAND DA DE UN NUMAR
citeste_cuvant(Character, Numar, Character1) :-
	caracter_numar(Character),!, /*nu citeste negative sau cu virgula*/
	citeste_tot_numarul(Character, Numar, Character1).
 /*daca ii dai 123abc citeste nr 123 si abc e urmatorul cuvant*/
 /*daca ii dai abc123 citeste cuvantul abc123 */

% ---- CITESTE TOATE CIFRELE
citeste_tot_numarul(Character,Numar,Character1):-
	determina_lista(Lista1,Character1),
	append([Character],Lista1,Lista),
	transforma_lista_numar(Lista,Numar).

determina_lista(Lista,Character1):-
	get_code(Character), 
	(caracter_numar(Character),
	determina_lista(Lista1,Character1),
	append([Character],Lista1,Lista); 
	\+(caracter_numar(Character)),
	Lista=[],Character1=Character).
 
% ---- TRANSFORMA LISTA DE CIFRE IN NUMAR
transforma_lista_numar([],0).

transforma_lista_numar([H|T],N):-
	transforma_lista_numar(T,NR), 
	lungime(T,L), Aux is exp(10,L),
	HH is H-48,N is HH*Aux+NR.

lungime([],0).
lungime([_|T],L) :- lungime(T,L1), L is L1+1.

% ------------ 4. CITESTE INTRE APOSTROAFE
citeste_cuvant(Character,Cuvant,Character1) :-
	Character == 39,!, % 39 este codul ASCII pt apostrof
	pana_la_urmatorul_apostrof(Lista_caractere),
	L=[Character|Lista_caractere],
	name(Cuvant, L),get_code(Character1).

pana_la_urmatorul_apostrof(Lista_caractere):-
	get_code(Character),
	(Character == 39,Lista_caractere=[Character];
	Character \== 39,
	pana_la_urmatorul_apostrof(Lista_caractere1),
	Lista_caractere=[Character|Lista_caractere1]).

% ------------ 5. CITIRE CUVANT NORMAL
citeste_cuvant(Character,Cuvant,Character1) :- /*dai Iasi si retine iasi */
	caractere_in_interiorul_unui_cuvant(Character),!,              
	((Character>64,Character<91),!,
	Character_modificat is Character+32;
	Character_modificat is Character),                              
	citeste_intreg_cuvantul(Charactere,Character1),
	name(Cuvant,[Character_modificat|Charactere]).

citeste_intreg_cuvantul(Lista_Charactere,Character1) :-
	get_code(Character),
	(caractere_in_interiorul_unui_cuvant(Character),
	((Character>64,Character<91),!, 
	Character_modificat is Character+32;
	Character_modificat is Character),
	citeste_intreg_cuvantul(Lista_Charactere1, Character1),
	Lista_Charactere=[Character_modificat|Lista_Charactere1]; \+(caractere_in_interiorul_unui_cuvant(Character)),
	Lista_Charactere=[], Character1=Character).

% ------------ 6. ORICE ALTCEVA ESTE IGNORAT
citeste_cuvant(_,Cuvant,Character1) :- get_code(Character),       
	citeste_cuvant(Character,Cuvant,Character1).
 

% ------ CARACTERE ACCEPTATE   !   (   )   ,   .   ?   =   @    |
caracter_cuvant(C):-member(C,[33, 40, 41, 44, 46, 63, 61, 64, 124]).

caracter_numar(C):- C >= 48,C =< 57. % [0,9]

caractere_in_interiorul_unui_cuvant(C):-
	C >= 48,C =< 57;  % [0,9]
	C >= 65,C =< 90;  % [A,Z]
	C >= 97,C =< 122; % [a,z]
	C == 45;		  % [-]
	C == 95.		  % [_]





/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
			EXEMPLU INTREBARI

*/