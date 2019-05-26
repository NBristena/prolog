/*-----------------------------------------------------------------------------
 * 		INITIALIZARE
 *-----------------------------------------------------------------------------*/
close_all :- current_stream(_,_,S), close(S), fail ; true.
curata_bc :- current_predicate(P), abolish(P,[force(true)]), fail ; true.

:- discontiguous executa1/1, executa2/1, citeste_cuvant/3, trad/3, realizare_scop/3, interogheaza/4, proceseaza_raspuns/3, cum/1.

:- use_module(library(file_systems),[]),file_systems:current_directory(_,'D:/0. Facultate/[3] Sem II/Expert/Proiect').
%:- use_module(library(file_systems),[]),file_systems:current_directory(_,'D:/Faculate/Anul_III/Info/Sistema expert - PROLOG/Proiect/Proiect - propriuzis/prolog_last_stage').

:- use_module(library(lists)).
:- use_module(library(file_systems)).
:- use_module(library(system)).

:- dynamic scop/1.
:- dynamic regula/3.
:- dynamic intrebare/3.
:- dynamic fapt/3.
:- dynamic deja_intrebat/1.
:- dynamic solutie/4.

:- op(900,fy,not).
not(P) :- P, !, fail.
not(_).





/*--------------------------------------------------------------------------------
 * 		   PORNIRE SISTEM
 *--------------------------------------------------------------------------------*/
go :-
	retractall(deja_intrebat(_)),
	retractall(fapt(_,_,_)),
	once(executa1([incarca])),nl,nl,
	executa1([consulta]),
	repeat,
	nl, write('     Meniu principal: '),
	nl, write(' (Incarca | Consulta | Afiseaza_fapte | Cum.. | Exit)'),
	nl, citeste_linie([H|T]),nl,
	executa1([H|T]), H == exit.

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
executa1([incarca]) :- incarca, !, write('~*~ Fisier incarcat cu succes ~*~'),nl.
	
incarca :- 
	F1 = 'reguli_bere.txt',
	%write('Introduceti numele fisierului intre apostroafe, urmat de un punct:'),nl, read(F),
	file_exists(F1), !, incarca(1,F1),
	F2 = 'solutii_bere.txt',
	file_exists(F2), !, incarca(2,F2).
	
incarca :- nl,
	write(' !X! Fisierul introdus nu exista !X!'),nl,nl,
	incarca.

% --------------- INCARCARE FISIERE ---------------------------------------------
incarca(1,F) :-
	retractall(deja_intrebat(_)), retractall(fapt(_,_,_)),
	retractall(scop(_)), retractall(regula(_,_,_)), retractall(intrebare(_,_,_)),
	see(F), incarca_fisier, seen, !.
	
incarca(2,F) :-
	retractall(solutie(_,_,_,_)),
	see(F), incarca_fisier, seen, !.

incarca(_,F) :-
	retractall(statistica(_,_,_)),
	see(F), incarca_fisier, seen, !.

	incarca_fisier :-
		repeat,
			citeste_propozitie(L),
			proceseaza(L),
		L == [end_of_file],!.
	
% --------------- CITIRE DIN FISIER --------------------------------------------
citeste_propozitie([Cuv|Lista_cuv]) :-
	get_code(Char), citeste_cuvant(Char, Cuv, Char1), 
	rest_cuvinte_propozitie(Char1, Lista_cuv).
	
	rest_cuvinte_propozitie(-1, []) :- !. % EOF
		
	rest_cuvinte_propozitie(Char,[]) :- Char == 46, !. %PUNCT

	rest_cuvinte_propozitie(Char,[]) :- Char == 47, citeste_separator(19), !. %SLASH

	rest_cuvinte_propozitie(Char,[Cuv1|Lista_cuv]) :-
		citeste_cuvant(Char,Cuv1,Char1),      
			rest_cuvinte_propozitie(Char1,Lista_cuv).

citeste_separator(0):-!.
citeste_separator(N) :- get_code(_), N1 is N-1, citeste_separator(N1).
	
% -------------- PROCESARE PROPOZITII ------------------------------------------
proceseaza([end_of_file]) :- !.
proceseaza(L) :- trad(R,L,[]),
	(R = intrebare(Atr,M,P) ->
		append(M,[nu_stiu,nu_conteaza],Mfinal),
		assertz(intrebare(Atr,Mfinal,P))
	;
		(R = solutie(Nume,Desc,Img,Props) ->
			atom_codes(Img,ImgCodes),
			append([39],ImgCodes,ApostrofImg),
			append(ApostrofImg,[39],ApostrofImgApostrof),
			atom_codes(CaleImg,ApostrofImgApostrof),
			assertz(solutie(Nume,Desc,CaleImg,Props))
		;
			assertz(R)
		)		
	), !.

/*   SOLUTII   */
trad( solutie(Nume,Desc,Img,Props) ) --> sol(Nume), descriere(Desc), imagine(Img), lista_proprietati(Props), ['/'].
	
	sol(Nume) --> ['[',valoare,scop,'-->',Nume,']'].

	descriere(Desc) --> ['[',descriere,'-->',Desc].

	imagine(Img) --> ['[',imagine,solutie,'-->',Img,']'].

	lista_proprietati(Props) --> ['[',proprietati,'-->'], lista_de_proprietati(Props).
		 
		lista_de_proprietati([Prop]) -->  propoz_props(Prop),[']'].
		lista_de_proprietati([Prima|Celelalte]) --> propoz_props(Prima),[';'], lista_de_proprietati(Celelalte).
			
			propoz_props(av(Atr,Val)) --> [Atr,'=',Val].
			
/*   SCOP   */
trad(scop(X)) --> [scop,'@',X].

/*  INTREBARI  */
trad(intrebare(Atr,M,P)) --> [intreaba,'@',Atr], afiseaza(Atr,P), lista_optiuni(M).
	 
	afiseaza(_,P) -->  [enunt,'@',P].
	afiseaza(P,P) -->  [].
	 
	lista_optiuni(M) --> [variante,'@','('], lista_de_optiuni(M).
		 
		lista_de_optiuni([Element]) -->  [Element,')'].
		lista_de_optiuni([Element|T]) --> [Element],['|','|'], lista_de_optiuni(T).
	
/*   REGULI   */
trad( regula(N,premise(Daca),concluzie(Atunci,F)) ) --> identificator(N), daca(Daca), atunci(Atunci,F).
	 
	identificator(N) --> [regula,'@',N].
	 
	daca(Daca) --> [lista_premise,'@','('], lista_premise(Daca).
		 
		lista_premise([Daca]) --> propoz_daca(Daca), [')',concluzie,'@','('].
		lista_premise([Prima|Celalalte]) --> propoz_daca(Prima), lista_premise(Celalalte).
				
			propoz_daca(av(Atr,Val)) --> [Atr,'@',Val,'@'].
			propoz_daca(av(Atr,da)) --> ['@',Atr,'@'].
			propoz_daca(av(Atr,nu)) --> ['@','!',Atr,'@'].

	atunci(Atunci,FC) --> propoz_atunci(Atunci), [',',fc,'@'], [FC], ['@',')'].
	atunci(Atunci,100) --> propoz_atunci(Atunci), [')'].
			
		propoz_atunci(av(Atr,Val)) --> [Atr,'@','=',Val].
		propoz_atunci(av(Atr,da)) --> ['@',Atr].
		propoz_atunci(av(Atr,nu)) --> ['@','!',Atr].

/*   ERORI   */
trad('-X- Eroare la parsare -X-'-L,L,_).





/*--------------------------------------------------------------------------------
 * 		   CONSULTA SISTEMUL
 *--------------------------------------------------------------------------------*/
executa1([consulta]) :- 
	retractall(deja_intrebat(_)),
	retractall(fapt(_,_,_)),
	scopuri_princ,nl,nl, !.

	scopuri_princ :-
		scop(Atr),
		determina(Atr), 
		
			 write(' ___________________________________________\n|\n'),
		( fapt(av(Atr,_),_,_) -> 
			ord_sol_fc(Atr),
			format('|   Optiunile dumneavoastra pentru ~a sunt:~n',[Atr]),
			format('|~`_t~14|',[]), 
			afiseaza_scop(Atr)
		;
			format('|   Nu avem nicio ~a care sa se~n',[Atr]),
			format('|   potriveasca cu preferintele tale.~n',[]),
			 write('|___________________________________________\n'),
			cere_date(0,_)
		),
		fail.
		
	scopuri_princ.


% --------------- INCEPE INTEROGAREA -------------------------------------------
/* realizare NOT fapt cu NotFC-> realizare fapt cu FC si NotFC = -FC
 * realizare fapt existent -> all good
 * realizare fapt ce poate fi interogat (not deja_intrebat, exista intrebare) -> interogheaza
 * realizare fapt ce trebuie demonstrat (exsta o regula) -> demonstreaza premise, ajusteaza factor in regula, actualizeaza KB
 */
determina(Atr) :- realizare_scop(av(Atr,_),_,[scop(Atr)]),!.
determina(_).

%--# NOT FAPT 
	realizare_scop(not Scop, Not_FC, Istorie) :-
		realizare_scop(Scop, FC, Istorie),
		Not_FC is - FC, !.

%--# FAPT EXISTENT
	realizare_scop(Scop, FC, _) :-
		fapt(Scop, FC, _), !.

%--# FAPT DE INTREBAT
	realizare_scop(Scop, FC, Istorie) :-
		pot_interoga(Scop, Istorie),
		!,realizare_scop(Scop, FC, Istorie).

		pot_interoga(av(Atr,_), Istorie) :-
			not deja_intrebat(av(Atr,_)),
			intrebare(Atr, Optiuni, Mesaj),
			interogheaza(Atr, Mesaj, Optiuni, Istorie),nl,
			asserta( deja_intrebat(av(Atr,_)) ).

%INTREBARI OPTIUNI
			interogheaza(Atr, Mesaj, Optiuni, Istorie) :-
				format('Q: ~a',[Mesaj]),nl,
				citeste_opt(Optiuni),
				de_la_utiliz(X, Istorie, Optiuni),
				(X == [nu_conteaza] ->
					append(Opt,[nu_stiu,nu_conteaza],Optiuni),
					assert_fapt(Atr, Opt)
				;
					(X == [nu_stiu] ->
						true
					;
						assert_fapt(Atr, X)
					)
				).

%   AFISEAZA OPTIUNI
				citeste_opt(Optiuni) :-
					write('( '), scrie_lista_optiuni(Optiuni).

%		CITESTE RASPUNS
					de_la_utiliz(X, Istorie, Lista_opt) :-
						repeat,
							nl,write(': '), 
							citeste_linie(X),
						proceseaza_raspuns(X, Istorie, Lista_opt).

%			PROCESEAZA RASPUNS
						proceseaza_raspuns([de_ce], Istorie, _) :- nl,
							write('|   Pentru regula:'),nl,
							afis_istorie(Istorie), !, fail.

							afis_istorie([]) :-nl.
							afis_istorie([scop(X)|T]) :- write('|\n'),scrie_lista_cu_spatiu(['|','-->',scopul,este,X]),nl,!, afis_istorie(T).
							afis_istorie([N|T]) :- afis_regula(N),!,afis_istorie(T).
						
						proceseaza_raspuns([X], _, Lista_opt):-
							member(X, Lista_opt).

						proceseaza_raspuns([X, fc, FC], _, Lista_opt):-
							member(X, Lista_opt), float(FC).

%				ADAUGA RASPUNS IN KB
							assert_fapt(Atr,[Val,fc,FC]) :- !,
								asserta( fapt(av(Atr,Val),FC,[utiliz]) ).

							%pentru raspunsul nu_conteaza adaugam fapte pentru toate optiunile atributului
							assert_fapt(Atr,[Val1|RestValori]) :- !,
								asserta( fapt(av(Atr,Val1),100,[utiliz]) ),
								assert_fapt(Atr,RestValori).

							assert_fapt(_,[]).

			
%--# FAPT DE DEMONSTRAT
	realizare_scop(Scop, FC_curent, Istorie) :-
		fg(Scop, FC_curent, Istorie).
		
		fg(Scop, FC_curent, Istorie) :-
			regula(N, premise(Lista), concluzie(Scop,FC)),
			demonstreaza(N, Lista, FC_premise, Istorie),
			ajusteaza(FC, FC_premise, FC_nou),
			actualizeaza(Scop, FC_nou, FC_curent, N),
			FC_curent == 100,!.
		fg(Scop, FC, _) :- fapt(Scop, FC, _).
			
%	DEMONSTREAZA PREMISE			
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
 * 		CALCUL FACTOR PENTRU 2 CAI 		 *
 * * * * * * * * * * * * * * * * * * * * */
				combina(FC1 ,FC2, FC) :-
/* 2 pozitive */	%FC1 >= 0, FC2 >= 0,
					X is FC2 * (100 - FC1) / 100 + FC1,
					FC is round(X).



% --------------- AFISEAZA CONCLUZII -------------------------------------------
ord_sol_fc(Atr) :- setof(sol(FC,X,I), Atr^retract(fapt(av(Atr,X),FC,I)), L), adauga_sol_ord(fc,Atr,L).
ord_sol_alfa(Atr) :- setof(sol(X,FC,I), Atr^retract(fapt(av(Atr,X),FC,I)), L), adauga_sol_ord(al,Atr,L).
	
	adauga_sol_ord(fc,Atr,[sol(FC,X,I)|T]):- asserta(fapt(av(Atr,X),FC,I)), adauga_sol_ord(fc,Atr,T).
	adauga_sol_ord(al,Atr,[sol(X,FC,I)|T]):- adauga_sol_ord(al,Atr,T), asserta(fapt(av(Atr,X),FC,I)).
	adauga_sol_ord(_,_,[]).


afiseaza_scop(Atr) :-nl,
	fapt(av(Atr,Val),FC,_),
	solutie(Val,Desc,Img,_),
	FC >= 20, scrie_scop(av(Atr,Val),FC,Desc,Img),nl,
%############################
	scrie_dem_in_fisier(av(Atr,Val)),
%############################
	fail.

afiseaza_scop(_) :- meniu_secundar,!.


scrie_scop(av(_,Val),FC,Desc,Img) :-
	FC1 is integer(FC),
	format('|~n',[]),
	format('| -> ~a  ####  avand fc egal cu ~d ~n|~n',[Val,FC1]),
	format('|   [~a]',[Img]), nl,
	format('|    Despre solutie: ~a',[Desc]), nl, 
	 write('|___________________________________________').


/****************************************************************************
 *--------------- Scrie demonstratie in fisier -----------------------------*/
	scrie_dem_in_fisier(Scop):- 
		ia_director('demonstratii_solutii'),
		gen_nume_fisier(Scop,NumeFisier),
		atom_concat( 'demonstratii_solutii/', NumeFisier, Path),
		telling(Curent_input),
		tell(Path),
			transforma_scop(Scop,L),
			executa1([cum|L]),
		told,
		tell(Curent_input),!.
		
		ia_director(Nume):-
			(directory_exists(Nume) -> 
				true
			; 
				make_directory(Nume)
			).
		
		gen_nume_fisier(av(Atr,Solutie),NumeFisier):-
			fapt(av(Atr,Solutie),FC,_),
			now(Timestamp),
			creaza_nume_fisier(Timestamp,Solutie,FC,NumeFisier).

			creaza_nume_fisier(Timestamp, Solutie, FC, NumeFisier):-
				conversie_nr_atom(Timestamp, Time),
				conversie_nr_atom(FC, FcNr),
				atom_concat( 'demonstratie[', Time, DemTime),
				atom_concat( DemTime, '][', DemTimeParanteze),
				atom_concat( DemTimeParanteze, Solutie, DemTimeSol),
				atom_concat( DemTimeSol, '][', DemTimeSolParanteze),
				atom_concat( DemTimeSolParanteze, FcNr, DemTimeSolFc),
				atom_concat( DemTimeSolFc, '].txt', NumeFisier).
				
				conversie_nr_atom(Nr,Atom):-
					number_chars(Nr,Lchr),
					atom_chars(Atom,Lchr).



% --------------------------------------------------------------------------
% --------------- MENIU SECUNDAR -------------------------------------------
% --------------------------------------------------------------------------
meniu_secundar :- 
	repeat,
	nl, write('     Meniu secundar: '),
	nl, write(' (Afis_alfabetic | Afis_prop | Exit)'),
	nl, citeste_linie([H]),nl,
	executa2([H]), H == exit.

%%
executa2([afis_alfabetic]) :- scop(Atr), 
	write(' ___________________________________________'),nl,
	write('|'),nl,
	write('| [ Afisare in ordine alfabetica ] '),nl,
	write('|  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'), 
	ord_sol_alfa(Atr),
	afiseaza_scop_secundar(Atr), !.

	afiseaza_scop_secundar(Atr) :-nl,
		fapt(av(Atr,Val),FC,_),
		solutie(Val,Desc,Img,_),
		FC >= 20, scrie_scop(av(Atr,Val),FC,Desc,Img),nl,fail.

	afiseaza_scop_secundar(_) :- !.

%%
executa2([afis_prop]) :- scop(Atr),
	write(' ___________________________________________'),nl,
	write('|'),nl,
	write('| [ Lista de proprietati ale solutiilor ] '),nl,
	write('|  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'), 
	afiseaza_props(Atr), !.

	afiseaza_props(Atr) :-nl,
		fapt(av(Atr,Val),FC,_),
		solutie(Val,_,_,Props),
		FC >= 20, scrie_scop(av(Atr,Val),FC,Props),nl,fail.

	afiseaza_props(_) :- !.

		scrie_scop(av(_,Val),FC,Props) :-
			FC1 is integer(FC),
			format('|~n| -> ~a  ####  avand fc egal cu ~d',[Val,FC1]),nl,
			write('| Proprietati: '), 
			scrie_props(Props),nl,
			write('|___________________________________________').

			scrie_props([av(A1,V1)|Props]):-
				nl, format('|     ~a : ~a;',[A1,V1]),
				scrie_props(Props).
			scrie_props([]).

%%
executa2([exit]):-
	numara_solutii(NrSol,SolList),
	cere_date(NrSol,SolList),!.

	numara_solutii(NrSol,SolList):- scop(Atr), 
		findall(Sol,(fapt(av(Atr,Sol),FC,_), FC > 19),List), 
		append(List,[niciuna],SolList), 
		length(List,NrSol).

	cere_date(NrSol,SolList) :- ( NrSol =:= 0 -> Rasp = fail ;
		(NrSol =:= 1 ->
			nl, write(' Alegi berea pe care ti-am recomandat-o?'),
			nl, write('( da | nu )'),nl,
			repeat,
			citeste_linie([DaNu]),
			(DaNu = da ->
				[Rasp|_] = SolList,!
			;
				(DaNu = nu ->
					Rasp = niciuna,!
				;
					fail
				)
			)
		;
			nl, write(' Ce bere alegi din cele recomandate?'),
			nl, citeste_opt(SolList),nl,
			repeat,
			citeste_linie([Rasp]),
			member(Rasp,SolList)
		)),
		((Rasp \== niciuna , Rasp \== fail) ->
			scop(Atr),
			fapt(av(Atr,Rasp),FC,_)
		;
			FC = 0
		),
		scrie_rez_in_fisier(Rasp,FC),!.

		scrie_rez_in_fisier(Rasp,FC):- 
			open('rezultate.txt',append,Stream),
			datime(datime(Y,M,D,H,Mi,S)),
			format(Stream,'~d_~d_~d_~d_~d_~d~t~19| --> ~a fc ~d.~n',[Y,M,D,H,Mi,S,Rasp,FC]),		
			close(Stream),!.

%%
executa2([_]) :- write('-X- Comanda incorecta -X-'),nl,!.
			


/*--------------------------------------------------------------------------------
 * 		   AFISEAZA FAPTE STIUTE
 *--------------------------------------------------------------------------------*/
executa1([afiseaza_fapte]) :-
	afisare_fapte,!.
	
	afisare_fapte :-
		write(' ___________________________________________'), nl,
		write('|'), nl,
		write('|  Fapte existente in baza de cunostinte:'), nl,
		write('|'), nl,
		write('|  (Atribut,Valoare) ~ FactorCertitudine'), nl,
		write('|'), nl, listeaza_fapte,
		write('|___________________________________________'),
		nl,nl.

		listeaza_fapte:-  
			(	fapt(av(Atr,Val),FC,_) 
			; 
				fapt(not av(Atr,_),FC,_), 
				Val = 'nu'
			), 
			FC1 is integer(FC),
			format('|(~a,~a) ~~ certitudine ~d',[Atr,Val,FC1]),
			nl,fail.
			
		listeaza_fapte.





/*--------------------------------------------------------------------------------
 * 		   CUM S-A AJUNS LA O CONCLUZIE - ISTORIC
 *--------------------------------------------------------------------------------*/
executa1([cum|L]) :- 
	write(' ___________________________________________'),nl,
	write('|     Demonstratie:'),nl,
	cum(L),!,
	write('|___________________________________________'),nl.
	
cum([]) :- nl,write('Cum ce? '),nl,
	citeste_linie(Linie),nl,
	transforma_scop(Scop,Linie), cum(Scop).

cum(L) :- transforma_scop(Scop,L), cum(Scop).

cum(not Scop) :- 
	fapt(Scop,FC,Reguli),
	lista_float_int(Reguli,Reguli1),
	FC < -20,
	transforma_scop(not Scop,PG),
	format('|~n|-> ',[]), scrie_lista_cu_spatiu(PG),
	format('a fost derivat cu ajutorul regulilor: ~w~n',[Reguli1]),
	afis_reguli(Reguli),fail.

cum(Scop) :-
	fapt(Scop,FC,Reguli),
	lista_float_int(Reguli,Reguli1),
	FC > 20,
	transforma_scop(Scop,PG),
	format('|~n|-> ',[]), scrie_lista_cu_spatiu(PG),
	format('a fost derivat cu ajutorul regulilor: ~w~n',[Reguli1]),
	afis_reguli(Reguli),fail.
		
	lista_float_int([Regula|Reguli],[Regula1|Reguli1]):-
		(Regula \== utiliz,
		Regula1 is integer(Regula);
		Regula ==utiliz, Regula1=Regula),
		lista_float_int(Reguli,Reguli1).

	lista_float_int([],[]).

	transforma_scop(av(A,da),[A]) :- !.
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
			format('|~n',[]),
			format('| regula@ ~d~n',[NR]),
			format('| lista_premise@ (~n',[]), 
			scrie_lista_premise(Lista_premise),
			format('| concluzie@ ( ',[]),
			transforma_concluzie(Scop,Concluzie),
			scrie_lista_fara_spatiu(Concluzie),
			FC1 is integer(FC),
			format(', fc@ ~d @ )~n',[FC1]).

			scrie_lista_premise([H|T]) :-
				transforma_premisa(H,H_tr),
				write('|'), spatii(4), scrie_lista_fara_spatiu(H_tr), nl,
				scrie_lista_premise(T).

			scrie_lista_premise([]) :- write('| )\n').

				transforma_premisa(av(A,da),['@ ',A,' @']) :- !.
				transforma_premisa(av(A,nu), ['@! ',A,' @']) :- !.
				transforma_premisa(av(A,V),[A,' @ ',V,' @ ']).

				transforma_concluzie(av(A,da),['@ ',A]) :- !.
				transforma_concluzie(av(A,nu), ['@! ',A]) :- !.
				transforma_concluzie(av(A,V),[A,' @','= ',V]).


		premisele(N) :-
			regula(N, premise(Lista_premise), _),
			!, cum_premise(Lista_premise).

			cum_premise([]).
			cum_premise([Scop|X]) :-
				cum(Scop),
				cum_premise(X).
cum(_).

/*--------------------------------------------------------------------------------*
 * 		   IESIRE SISTEM
 *--------------------------------------------------------------------------------*/
executa1([exit]):-!.

/*--------------------------------------------------------------------------------*
 * 		   EROARE 
 *--------------------------------------------------------------------------------*/
executa1([_|_]) :- write('-X- Comanda incorecta -X-'),nl.

% _____________________________________________________________________________________________________________________________________________	
% _____________________________________________________________________________________________________________________________________________
% XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX





/*--------------------------------------------------------------------------------*
 * 		   			FUNCTII "GLOBALE" 											  *
 *--------------------------------------------------------------------------------*/

spatii(0).
spatii(N):- N > 0, write(' '), N1 is N-1, spatii(N1).

scrie_lista_cu_spatiu([]).
scrie_lista_cu_spatiu([H|T]) :- write(H), spatii(1), scrie_lista_cu_spatiu(T).

scrie_lista_fara_spatiu([]).
scrie_lista_fara_spatiu([H|T]) :- write(H), scrie_lista_fara_spatiu(T).

scrie_lista_optiuni([H|[]]) :- format('~a )',[H]).
scrie_lista_optiuni([H|T]) :- format('~a | ',[H]), scrie_lista_optiuni(T).


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
citeste_cuvant(Caracter,Cuvant,Caracter1) :-   
	caracter_cuvant(Caracter),!, 
	name(Cuvant, [Caracter]),
	get_code(Caracter1).
	
% ------------ 3. CAND DA DE UN NUMAR
citeste_cuvant(Caracter, Numar, Caracter1) :-
	caracter_numar(Caracter),!, /*nu citeste negative sau cu virgula*/
	citeste_tot_numarul(Caracter, Numar, Caracter1).

% ---- CITESTE TOATE CIFRELE
citeste_tot_numarul(Caracter,Numar,Caracter1):-
	determina_lista(Lista1,Caracter1),
	append([Caracter],Lista1,Lista),
	transforma_lista_numar(Lista,Numar).

determina_lista(Lista,Caracter1):-
	get_code(Caracter), 
	(caracter_numar(Caracter) ->
		determina_lista(Lista1,Caracter1),
		append([Caracter],Lista1,Lista)
	; 
		Lista = [],
		Caracter1 = Caracter
	).
 
% ---- TRANSFORMA LISTA DE CIFRE IN NUMAR
transforma_lista_numar([],0).

transforma_lista_numar([H|T],N):-
	transforma_lista_numar(T,NR), 
	lungime(T,L), 
	Aux is exp(10,L),
	HH is H-48,
	N is HH*Aux+NR.

lungime([],0).
lungime([_|T],L) :- lungime(T,L1), L is L1+1.

% ------------ 4. CITESTE INTRE APOSTROAFE
citeste_cuvant(Caracter,Cuvant,Caracter1) :-
	Caracter == 39,!, % 39 este codul ASCII pt apostrof
	pana_la_urmatorul_apostrof(Lista_caractere),
	%L = [Caracter|Lista_caractere],
	%name(Cuvant, L),
	name(Cuvant, Lista_caractere),
	get_code(Caracter1).

pana_la_urmatorul_apostrof(Lista_caractere):-
	get_code(Caracter),
	(Caracter == 39 -> 
		Lista_caractere = []
	;
		pana_la_urmatorul_apostrof(Lista_caractere1),
		Lista_caractere = [Caracter|Lista_caractere1]
	).

% ------------ 5. CITIRE CUVANT NORMAL
citeste_cuvant(Caracter,Cuvant,Caracter1) :- /*dai Iasi si retine iasi */
	caractere_in_interiorul_unui_cuvant(Caracter),!,              
	((Caracter > 64 , Caracter < 91) ->
		Caracter_modificat is Caracter+32
	;
		Caracter_modificat is Caracter
	),                              
	citeste_intreg_cuvantul(Caractere,Caracter1),
	name(Cuvant,[Caracter_modificat|Caractere]).

citeste_intreg_cuvantul(Lista_Caractere,Caracter1) :-
	get_code(Caracter),
	(caractere_in_interiorul_unui_cuvant(Caracter) ->
		((Caracter > 64 , Caracter < 91) ->
			Caracter_modificat is Caracter+32
		;
			Caracter_modificat is Caracter
		),
		citeste_intreg_cuvantul(Lista_Caractere1, Caracter1),
		Lista_Caractere = [Caracter_modificat|Lista_Caractere1]
	;
		Lista_Caractere = [], 
		Caracter1 = Caracter
	).

% ------------ 6. ORICE ALTCEVA ESTE IGNORAT
citeste_cuvant(_,Cuvant,Caracter1) :- 
	get_code(Caracter),       
	citeste_cuvant(Caracter,Cuvant,Caracter1).


% ------ CARACTERE ACCEPTATE   !   (   )   ,   .   ?   =   @   |    [   ]   ;   /
caracter_cuvant(C):-member(C,[33, 40, 41, 44, 46, 63, 61, 64, 124, 91, 93, 59, 47]).

caracter_numar(C):- C >= 48,C =< 57. % [0,9]

caractere_in_interiorul_unui_cuvant(C):-
	C >= 48,C =< 57;  % [0,9]
	C >= 65,C =< 90;  % [A,Z]
	C >= 97,C =< 122; % [a,z]
	C == 45;		  % [-]
	C == 95;		  % [_]
	C == 62.		  % [>]





/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Nr dati cand SE da fail
Nr dati cand user nu a fost multumit

----------------------------------
NumeSOl | NrAparitii | MedieFactor
----------------------------------

*/