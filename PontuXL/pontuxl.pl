

%PARTIE BOT

:- use_module(library(lists)).

/* --------------------------------------------------------------------- */
/*                                                                       */
/*        PRODUIRE_REPONSE(L_Mots,L_strings) :                           */
/*                                                                       */
/*        Input : une liste de mots L_Mots representant la question      */
/*                de l'utilisateur                                       */
/*        Output : une liste de strings donnant la                       */
/*                 reponse fournie par le bot                            */
/*                                                                       */
/*        NB Par défaut le predicat retourne dans tous les cas           */
/*            [  "Je ne sais pas.", "Les étudiants",                     */
/*               "vont m'aider, vous le verrez !" ]                      */
/*                                                                       */
/*        Je ne doute pas que ce sera le cas ! Et vous souhaite autant   */
/*        d'amusement a coder le predicat que j'ai eu a ecrire           */
/*        cet enonce et ce squelette de solution !                       */
/*                                                                       */
/* --------------------------------------------------------------------- */


produire_reponse([fin],[L1]) :-
    L1 = [merci, de, m, '\'', avoir, consulte], !.

produire_reponse(L,Rep) :-
    mclef(M,_), member(M,L),
    clause(regle_rep(M,_,Pattern,Rep),Body),
    match_pattern(Pattern,P),
    call(Body), !.

produire_reponse(_,[S1,S2]) :-
    S1 = "Je ne sais pas. ",
    S2 = "Les étudiants vont m'aider, vous le verrez".

match_pattern(Pattern,Lmots) :-
    sublist(Pattern,L_mots).

match_pattern(LPatterns,Lmots) :-
    match_pattern_dist([100|LPatterns],Lmots).

match_pattern_dist([],_).
match_pattern_dist([N,Pattern|Lpatterns],Lmots) :-
    within_dist(N,Pattern,Lmots,Lmots_rem),
    match_pattern_dist(Lpatterns,Lmots_rem).

within_dist(_,Pattern,Lmots,Lmots_rem) :-
    prefixrem(Pattern,Lmots,Lmots_rem).
within_dist(N,Pattern,[_|Lmots],Lmots_rem) :-
    N > 1, Naux is N-1,
    within_dist(Naux,Pattern,Lmots,Lmots_rem).

sublist(SL,L) :-
    prefix(SL,L), !.
sublist(SL,[_|T]) :- sublist(SL,T).

sublistrem(SL,L,Lr) :-
    prefixrem(SL,L,Lr), !.
sublistrem(SL,[_|T],Lr) :- sublistrem(SL,T,Lr).

prefixrem([],L,L).
prefixrem([H|T],[H|L],Lr) :- prefixrem(T,L,Lr).



% ----------------------------------------------------------------%

nb_lutins(4).
nb_equipes(4).

mclef(commence, 10).       % mot central
mclef(debute, 10).          
mclef(demarre, 10).
mclef(entame, 10).
mclef(jeu, 5).
mclef(partie, 5).
mclef(equipe, 10).
mclef(quipe, 10).       
mclef(joueur, 5).
mclef(combien, 10).
mclef(lutin, 10).
mclef(lutins, 10).
mclef(compte, 5).
mclef(possede, 5).
mclef(a, 5).
mclef(deplacer, 10).
mclef(placer, 10).
mclef(case, 10).
mclef(occupee, 10).
mclef(autre, 5).
mclef(deja, 5).
mclef(placee, 5).
mclef(puis, 5).
mclef(peux, 2).
mclef(estce, 2).
mclef(quel, 10).
mclef(pont, 10).
mclef(retirer, 10).
mclef(supprimer, 5).
mclef(enlever, 5).
mclef(apres, 5).
mclef(conseil, 10).
mclef(jouer, 10).
mclef(choisir, 10).
mclef(conseillez, 10).
mclef(moi, 5).
mclef(je, 5).
mclef(couleur, 5).
mclef(mouvement, 5).
mclef(controle, 5).







% --------------------------------------------------------------- %

reponse_commence(["par convention, c'est au joueur en charge des lutins verts de commencer la partie."]).


regle_rep(commence, 1,
 [ qui, commence, le, jeu ],
 R) :- reponse_commence(R).

regle_rep(commence, 1,
 [ le, jeu, est, commence, par, quelle, equipe ],
 R) :- reponse_commence(R).

regle_rep(debute, 1,
 [ qui, debute, la, partie ],
 R) :- reponse_commence(R).

regle_rep(demarre, 1,
 [ quelle, equipe, demarre, le, jeu ],
 R) :- reponse_commence(R).

regle_rep(entame, 1,
 [ par, qui, la, partie, est, entamee ],
 R) :- reponse_commence(R).

% Variante souple avec mots-clés espacés
regle_rep(commence, 1,
 [5, [commence, debute, demarre, entame], 5, [jeu, partie] ],
 R) :- reponse_commence(R).

% ----------------------------------------------------------------% 
reponse_nb_lutins(["chaque equipe compte 4 lutins."]).

regle_rep(equipe, 1,
 [ combien, de, lutins, compte, chaque, equipe ],
 R) :- reponse_nb_lutins(R).

regle_rep(equipe, 1,
 [ chaque, equipe, possede, combien, de, lutins ],
 R) :- reponse_nb_lutins(R).

regle_rep(equipe, 1,
 [ une, equipe, a, combien, de, lutins ],
 R) :- reponse_nb_lutins(R).

regle_rep(quipe, 1,
 [ combien, de, lutins, par, equipe ],
 R) :- reponse_nb_lutins(R).

regle_rep(equipe, 1,
 [5, [equipe, equipes], 5, [combien, nombre, compte, possede, a], 5, [lutin, lutins] ],
 R) :- reponse_nb_lutins(R).

% ----------------------------------------------------------------% 
reponse_case_occupee(["non, vous ne pouvez pas déplacer un lutin sur une case occupée par un autre."]).


regle_rep(deplacer, 1,
 [ puis, je, deplacer, un, lutin, sur, une, case, occupee, par, un, autre, lutin ],
 R) :- reponse_case_occupee(R).

regle_rep(deplacer, 1,
 [ estce, que, je, peux, deplacer, un, lutin, sur, une, case, deja, occupee ],
 R) :- reponse_case_occupee(R).

regle_rep(occupee, 1,
 [ un, lutin, peutil, aller, sur, une, case, occupee, par, un, autre ],
 R) :- reponse_case_occupee(R).

regle_rep(placer, 1,
 [ peuton, placer, un, lutin, sur, la, meme, case, quun, autre ],
 R) :- reponse_case_occupee(R).

regle_rep(deplacer, 1,
 [5, [deplacer, poser, placer], 5, [lutin], 5, [occupee, occupee, deja] ],
 R) :- reponse_case_occupee(R).


% ----------------------------------------------------------------% 

reponse_retrait_pont(["il est permis de retirer le pont emprunté ou tout autre pont."]).

regle_rep(retirer, 1,
 [ quel, pont, puis, je, retirer, apres, avoir, deplace, un, lutin ],
 R) :- reponse_retrait_pont(R).

regle_rep(enlever, 1,
 [ quel, pont, estce, que, je, peux, enlever, apres, un, deplacement ],
 R) :- reponse_retrait_pont(R).

regle_rep(deplace, 1,
 [ apres, avoir, deplace, un, lutin, puis, je, supprimer, un, pont ],
 R) :- reponse_retrait_pont(R).

regle_rep(mouvement, 1,
 [ apres, un, mouvement, quel, pont, peuton, retirer ],
 R) :- reponse_retrait_pont(R).

regle_rep(retirer, 1,
 [5, [retirer, supprimer, enlever], 5, [pont], 5, [apres], 5, [deplacement, deplace, mouvement] ],
 R) :- reponse_retrait_pont(R).



% ----------------------------------------------------------------% 

regle_rep(conseillez, 5,
 [ je, joue, pour, les, lutins, Couleur, quel, lutin, me, conseillez, vous ],
 R) :-
    nom_equipe(Couleur, Equipe),
    jouer_ia_heuristique1(Equipe, Action),
    action_to_message(Action, R).

regle_rep(joue, 5,
 [ je, joue, les, lutins, Couleur, quel, lutin, dois, je, jouer ],
 R) :-
    nom_equipe(Couleur, Equipe),
    jouer_ia_heuristique1(Equipe, Action),
    action_to_message(Action, R).

regle_rep(recommandez, 5,
 [ quel, lutin, des, lutins, Couleur, me, recommandez, vous ],
 R) :-
    nom_equipe(Couleur, Equipe),
    jouer_ia_heuristique1(Equipe, Action),
    action_to_message(Action, R).

regle_rep(deplacer, 5,
 [ quel, lutin, dois, je, deplacer, si, je, suis, les, Couleur ],
 R) :-
    nom_equipe(Couleur, Equipe),
    jouer_ia_heuristique1(Equipe, Action),
    action_to_message(Action, R).

regle_rep(controle, 5,
 [ je, controle, les, lutins, Couleur, qui, dois, je, jouer ],
 R) :-
    nom_equipe(Couleur, Equipe),
    jouer_ia_heuristique1(Equipe, Action),
    action_to_message(Action, R).



nom_equipe(verte, vert).
nom_equipe(verts, vert).
nom_equipe(vert, vert).
nom_equipe(bleue, bleue).
nom_equipe(bleus, bleue).
nom_equipe(rouge, rouge).
nom_equipe(jaune, jaune).


action_to_message(deplacer_et_retirer(l(_, Num), X0, Y0, X1, Y1, pont(X0, Y0, X1, Y1)), [R]) :-
    format(string(R), "le lutin numero ~w doit aller vers ~w-~w et ensuite vous pourrez retirer le pont ~w-~w-~w-~w", [Num, X1, Y1, X0, Y0, X1, Y1]).




/* --------------------------------------------------------------------- */
/*                                                                       */
/*          CONVERSION D'UNE QUESTION DE L'UTILISATEUR EN                */
/*                        LISTE DE MOTS                                  */
/*                                                                       */
/* --------------------------------------------------------------------- */

% lire_question(L_Mots)

lire_question(Input, LMots) :- read_atomics(Input, LMots).



/*****************************************************************************/
% my_char_type(+Char,?Type)
%    Char is an ASCII code.
%    Type is whitespace, punctuation, numeric, alphabetic, or special.

my_char_type(46,period) :- !.
my_char_type(X,alphanumeric) :- X >= 65, X =< 90, !.
my_char_type(X,alphanumeric) :- X >= 97, X =< 123, !.
my_char_type(X,alphanumeric) :- X >= 48, X =< 57, !.
my_char_type(X,whitespace) :- X =< 32, !.
my_char_type(X,punctuation) :- X >= 33, X =< 47, !.
my_char_type(X,punctuation) :- X >= 58, X =< 64, !.
my_char_type(X,punctuation) :- X >= 91, X =< 96, !.
my_char_type(X,punctuation) :- X >= 123, X =< 126, !.
my_char_type(_,special).


/*****************************************************************************/
% lower_case(+C,?L)
%   If ASCII code C is an upper-case letter, then L is the
%   corresponding lower-case letter. Otherwise L=C.

lower_case(X,Y) :-
    X >= 65,
    X =< 90,
    Y is X + 32, !.

lower_case(X,X).


/*****************************************************************************/
% read_lc_string(-String)
%  Reads a line of input into String as a list of ASCII codes,
%  with all capital letters changed to lower case.

read_lc_string(String) :-
    get0(FirstChar),
    lower_case(FirstChar,LChar),
    read_lc_string_aux(LChar,String).

    read_lc_string_aux(10,[]) :- !.  % end of line

read_lc_string_aux(-1,[]) :- !.  % end of file

read_lc_string_aux(LChar,[LChar|Rest]) :- read_lc_string(Rest).


/*****************************************************************************/
% extract_word(+String,-Rest,-Word) (final version)
%  Extracts the first Word from String; Rest is rest of String.
%  A word is a series of contiguous letters, or a series
%  of contiguous digits, or a single special character.
%  Assumes String does not begin with whitespace.

extract_word([C|Chars],Rest,[C|RestOfWord]) :-
    my_char_type(C,Type),
    extract_word_aux(Type,Chars,Rest,RestOfWord).

    extract_word_aux(special,Rest,Rest,[]) :- !.
% if Char is special, don't read more chars.

extract_word_aux(Type,[C|Chars],Rest,[C|RestOfWord]) :-
    my_char_type(C,Type), !,
extract_word_aux(Type,Chars,Rest,RestOfWord).

extract_word_aux(_,Rest,Rest,[]).   % if previous clause did not succeed.


/*****************************************************************************/
% remove_initial_blanks(+X,?Y)
%   Removes whitespace characters from the
%   beginning of string X, giving string Y.

remove_initial_blanks([C|Chars],Result) :-
    my_char_type(C,whitespace), !,
remove_initial_blanks(Chars,Result).

remove_initial_blanks(X,X).   % if previous clause did not succeed.


/*****************************************************************************/
% digit_value(?D,?V)
%  Where D is the ASCII code of a digit,
%  V is the corresponding number.

digit_value(48,0).
digit_value(49,1).
digit_value(50,2).
digit_value(51,3).
digit_value(52,4).
digit_value(53,5).
digit_value(54,6).
digit_value(55,7).
digit_value(56,8).
digit_value(57,9).


/*****************************************************************************/
% string_to_number(+S,-N)
%  Converts string S to the number that it
%  represents, e.g., "234" to 234.
%  Fails if S does not represent a nonnegative integer.

string_to_number(S,N) :-
    string_to_number_aux(S,0,N).

    string_to_number_aux([D|Digits],ValueSoFar,Result) :-
    digit_value(D,V),
    NewValueSoFar is 10*ValueSoFar + V,
string_to_number_aux(Digits,NewValueSoFar,Result).

string_to_number_aux([],Result,Result).


/*****************************************************************************/
% string_to_atomic(+String,-Atomic)
%  Converts String into the atom or number of
%  which it is the written representation.

string_to_atomic([C|Chars],Number) :-
    string_to_number([C|Chars],Number), !.

string_to_atomic(String,Atom) :- atom_codes(Atom,String).
% assuming previous clause failed.


/*****************************************************************************/
% extract_atomics(+String,-ListOfAtomics) (second version)
%  Breaks String up into ListOfAtomics
%  e.g., " abc def  123 " into [abc,def,123].

extract_atomics(String,ListOfAtomics) :-
    remove_initial_blanks(String,NewString),
    extract_atomics_aux(NewString,ListOfAtomics).

    extract_atomics_aux([C|Chars],[A|Atomics]) :-
    extract_word([C|Chars],Rest,Word),
    string_to_atomic(Word,A),       % <- this is the only change
extract_atomics(Rest,Atomics).

extract_atomics_aux([],[]).


/*****************************************************************************/
% clean_string(+String,-Cleanstring)
%  removes all punctuation characters from String and return Cleanstring

clean_string([C|Chars],L) :-
    my_char_type(C,punctuation),
    clean_string(Chars,L), !.
clean_string([C|Chars],[C|L]) :-
    clean_string(Chars,L), !.
clean_string([C|[]],[]) :-
    my_char_type(C,punctuation), !.
clean_string([C|[]],[C]).


/*****************************************************************************/
% read_atomics(-ListOfAtomics)
%  Reads a line of input, removes all punctuation characters, and converts
%  it into a list of atomic terms, e.g., [this,is,an,example].

read_atomics(Input, ListOfAtomics) :-

    clean_string(Input,Cleanstring),
    extract_atomics(Cleanstring,ListOfAtomics).



/* --------------------------------------------------------------------- */
/*                                                                       */
/*        PRODUIRE_REPONSE : ecrit la liste de strings                   */
/*                                                                       */
/* --------------------------------------------------------------------- */

transformer_reponse_en_string(Li,Lo) :- flatten_strings_in_sentences(Li,Lo).

flatten_strings_in_sentences([],[]).
flatten_strings_in_sentences([W|T],S) :-
    string_as_list(W,L1),
    flatten_strings_in_sentences(T,L2),
    append(L1,L2,S).

% Pour SWI-Prolog
% string_as_list(W,L) :- string_to_list(W,L).


% Pour tau-Prolog
string_as_list(W,W).

%PARTIE REGLE DU JEU


:- dynamic(lutin/3).
:- dynamic(pont/4).

% Initialisation automatique des ponts horizontaux et verticaux
initialiser_ponts :-
    forall((between(0,4,X), between(0,5,Y)),
           (X2 is X + 1, assertz(pont(X,Y,X2,Y)))),
    forall((between(0,5,X), between(0,4,Y)),
           (Y2 is Y + 1, assertz(pont(X,Y,X,Y2)))).

% Vérifie qu'une case est libre (aucun lutin dessus)
case_libre(X, Y) :-
    \+ lutin(_, _, (X, Y)).

% Placer un lutin si sa case est libre et non déjà placé
placer_lutin(Equipe, Num, X, Y) :-
    integer(Num),
    Num >= 1, Num =< 4,
    \+ lutin(l(Equipe, Num), _, _),
    assertz(lutin(l(Equipe, Num), Equipe, (X,Y))).

% Vérifie si tous les lutins d'une équipe sont placés
tous_les_lutins_places(Equipe) :-
    forall(between(1,4,Num), lutin(l(Equipe,Num),_,_)).

% Retirer un pont entre deux positions (dans les deux sens possibles)
retirer_pont((X1,Y1)-(X2,Y2)) :-
    (retract(pont(X1,Y1,X2,Y2)) ; 
     retract(pont(X2,Y2,X1,Y1))).


% Vérifie l'existence d'un pont entre deux points (dans un sens ou l'autre)
pont_existe(X1,Y1,X2,Y2) :-
    (pont(X1,Y1,X2,Y2) ; pont(X2,Y2,X1,Y1)).


% Déplacer un lutin et retirer un pont en une action
deplacer_et_retirer(Equipe, Num, X, Y, pont(X1,Y1,X2,Y2)) :-
    retract(lutin(l(Equipe, Num), _, (X0, Y0))),
    assertz(lutin(l(Equipe, Num), Equipe, (X, Y))),
    retirer_pont((X1,Y1)-(X2,Y2)).

% Vérifie si une case (A,B) est une des extrémités d’un pont entre (X1,Y1) et (X2,Y2)
case_de_rotation_correspondante(X1, Y1, X2, Y2, A, B) :-
    (A = X1, B = Y1);
    (A = X2, B = Y2).


%roter un pont
roter_pont(Direction, X1, Y1, X2, Y2, A, B) :-
    
    (
        Direction = gauche, X1 = X2,
        NewX1 is A - 1, NewY1 is B,
        NewX2 is A,     NewY2 is B
        
    ;
        Direction = droite, X1 = X2,
        NewX1 is A,     NewY1 is B,
        NewX2 is A + 1, NewY2 is B
    ;
        Direction = gauche, Y1 = Y2,
        NewX1 is A,     NewY1 is B,
        NewX2 is A,     NewY2 is B + 1
    ;
        Direction = droite, Y1 = Y2,
        NewX1 is A,     NewY1 is B - 1,
        NewX2 is A,     NewY2 is B
    ),
    between(0, 5, NewX1), between(0, 5, NewY1),
    between(0, 5, NewX2), between(0, 5, NewY2),
    \+ pont_existe(NewX1, NewY1, NewX2, NewY2), 
    retirer_pont((X1,Y1)-(X2,Y2)),
    assertz(pont(NewX1, NewY1, NewX2, NewY2)),
    assertz(pont(NewX2, NewY2, NewX1, NewY1)) .

% Déplacer un lutin et roter un pont en une action
deplacer_et_roter(Equipe, Num, Direction, X, Y, X1, Y1, X2, Y2, A, B) :-
    retract(lutin(l(Equipe, Num), _, (_,_))),
    assertz(lutin(l(Equipe, Num), Equipe, (X, Y))),
    roter_pont(Direction, X1, Y1, X2, Y2, A, B).
    

% vérifie si un lutin n'a plus de ponts
lutin_totalement_isole(l(Equipe, Num)) :-
    lutin(l(Equipe, Num), _, (X,Y)),
    \+ (pont(X,Y,_,_)),
    \+ (pont(_,_,X,Y)).

% vérifie si une equipe n'a plus de pont c'est a dire est éliminé

equipe_eliminee(Equipe) :-
    tous_les_lutins_places(Equipe),
    forall(between(1,4,Num), lutin_totalement_isole(l(Equipe, Num))).

% vérifie si uun lutin peut se déplacer

lutin_peut_bouger(l(Equipe, Num)) :-
    lutin(l(Equipe, Num), _, (X, Y)),
    (
        (pont(X,Y,X2,Y2) ; pont(X2,Y2,X,Y)),
        case_libre(X2,Y2)
    ).


% vérifie si les lutins d'une équipe sont bloqué cad ne peuvent se deplacer malgrer les ponts disponibles

equipe_bloquee_sans_etre_eliminee(Equipe) :-
    tous_les_lutins_places(Equipe),
    \+ equipe_eliminee(Equipe),
    \+ (between(1,4,Num), lutin_peut_bouger(l(Equipe, Num))).




% === Représentation complète de l'état de jeu ===
etat_jeu(etat(Lutins, Ponts)) :-
    findall(l(Equipe, Num, X, Y), lutin(l(Equipe, Num), Equipe, (X, Y)), Lutins),
    findall(pont(X1, Y1, X2, Y2), pont(X1, Y1, X2, Y2), Ponts).



% Applique une action à un état et produit un nouvel état ceci sont des simulations
transition(etat(Lutins, Ponts), placer(l(Equipe, Num), X, Y), etat(NouveauxLutins, Ponts)) :-
    \+ member(l(Equipe, Num, _, _), Lutins),
    \+ member(l(_, _, X, Y), Lutins),  % Case libre
    NouveauxLutins = [l(Equipe, Num, X, Y) | Lutins].



transition(etat(Lutins, Ponts), deplacer_et_retirer(l(Equipe, Num), X0, Y0, X, Y, pont(X1, Y1, X2, Y2)), etat(NouveauxLutins, NouveauxPonts)) :-
    select(l(Equipe, Num, X0, Y0), Lutins, LutinsRestants),
    \+ member(l(_, _, X, Y), Lutins),
    (member(pont(X0, Y0, X, Y), Ponts) ; member(pont(X, Y, X0, Y0), Ponts)),
    select(pont(X1, Y1, X2, Y2), Ponts, PontsRestants), % pont à retirer
    NouveauxLutins = [l(Equipe, Num, X, Y) | LutinsRestants],
    NouveauxPonts = PontsRestants.
    


transition(etat(Lutins, Ponts), deplacer_et_roter(l(Equipe, Num), X0, Y0, X, Y, pont(X1, Y1, X2, Y2), Direction, A, B), etat(NouveauxLutins, [NewPont | PontsRestants])) :-
    select(l(Equipe, Num, X0, Y0), Lutins, LutinsRestants),
    \+ member(l(_, _, X, Y), Lutins),
    (member(pont(X0, Y0, X, Y), Ponts) ; member(pont(X, Y, X0, Y0), Ponts)),
    select(pont(X1, Y1, X2, Y2), Ponts, PontsRestants),  % ancien pont retiré
    rotate(Direction, X1, Y1, X2, Y2, A, B, NewPont),    % applique la rotation
    NouveauxLutins = [l(Equipe, Num, X, Y) | LutinsRestants].
    


transition(etat(Lutins, Ponts), retirer_pont(X1, Y1, X2, Y2), etat(Lutins, NouveauxPonts)) :-
    select(pont(X1, Y1, X2, Y2), Ponts, NouveauxPonts).
    

transition(etat(Lutins, Ponts), roter_pont(pont(X1, Y1, X2, Y2), Direction, A, B), etat(Lutins, [NewPont | PontsRestants])) :-
    select(pont(X1, Y1, X2, Y2), Ponts, PontsRestants),
    rotate(Direction, X1, Y1, X2, Y2, A, B, NewPont).

rotate(gauche, X1, Y1, X2, Y2, A, B, pont(NewX1, NewY1, NewX2, NewY2)) :-
    (
        X1 = X2, NewX1 is A - 1, NewY1 is B, NewX2 is A,     NewY2 is B;
        Y1 = Y2, NewX1 is A,     NewY1 is B, NewX2 is A,     NewY2 is B + 1
    ).
    
rotate(droite, X1, Y1, X2, Y2, A, B, pont(NewX1, NewY1, NewX2, NewY2)) :-
    (
        X1 = X2, NewX1 is A,     NewY1 is B, NewX2 is A + 1, NewY2 is B;
        Y1 = Y2, NewX1 is A,     NewY1 is B - 1, NewX2 is A, NewY2 is B
    ),
    between(0, 5, NewX1), between(0, 5, NewY1),
    between(0, 5, NewX2), between(0, 5, NewY2).


actions_possibles(etat(Lutins, Ponts), Equipe, Actions) :-
    % 🔹 Cas 1 : Tous les lutins ne sont pas encore placés
    findall(placer(l(Equipe, Num), X, Y),
            (between(1, 4, Num),
             \+ member(l(Equipe, Num, _, _), Lutins),
             between(0, 5, X), between(0, 5, Y),
             \+ member(l(_, _, X, Y), Lutins)
            ),
            Placements),
    Placements \= [], !,
    Actions = Placements.

actions_possibles(etat(Lutins, Ponts), Equipe, Actions) :-
    % 🔹 Cas 2 : Tous les lutins sont placés ET au moins un peut bouger
    (
        member(l(Equipe, Num, X0, Y0), Lutins),
        (member(pont(X0, Y0, X1, Y1), Ponts); member(pont(X1, Y1, X0, Y0), Ponts)),
        \+ member(l(_, _, X1, Y1), Lutins)
    ), !,
    % 🔸 Générer déplacements avec retrait de n'importe quel pont
    findall(deplacer_et_retirer(l(Equipe, Num), X0, Y0, X1, Y1, pont(XR1, YR1, XR2, YR2)),
            (
                member(l(Equipe, Num, X0, Y0), Lutins),
                (member(pont(X0, Y0, X1, Y1), Ponts); member(pont(X1, Y1, X0, Y0), Ponts)),
                \+ member(l(_, _, X1, Y1), Lutins),
                member(pont(XR1, YR1, XR2, YR2), Ponts)
            ),
            Retraits),
    % 🔸 Générer déplacements avec rotation de n'importe quel pont
    findall(deplacer_et_roter(l(Equipe, Num), X0, Y0, X1, Y1, pont(XR1, YR1, XR2, YR2), Direction, A, B),
            (
                member(l(Equipe, Num, X0, Y0), Lutins),
                (member(pont(X0, Y0, X1, Y1), Ponts); member(pont(X1, Y1, X0, Y0), Ponts)),
                \+ member(l(_, _, X1, Y1), Lutins),
                member(pont(XR1, YR1, XR2, YR2), Ponts),
                (A = XR1, B = YR1; A = XR2, B = YR2),
                (Direction = gauche ; Direction = droite),
                rotate(Direction, XR1, YR1, XR2, YR2, A, B, pont(NewX1, NewY1, NewX2, NewY2)),
                \+ member(pont(NewX1, NewY1, NewX2, NewY2), Ponts)
            ),
            Rotations),
    append(Retraits, Rotations, Actions).

actions_possibles(etat(Lutins, Ponts), Equipe, Actions) :-
    % 🔹 Cas 3 : Tous les lutins sont placés, mais bloqués, et équipe non éliminée
    forall(between(1, 4, Num), member(l(Equipe, Num, _, _), Lutins)),
    \+ (member(l(Equipe, Num, X, Y), Lutins),
        (member(pont(X, Y, NX, NY), Ponts); member(pont(NX, NY, X, Y), Ponts)),
        \+ member(l(_, _, NX, NY), Lutins)
    ), !,
    % 🔸 Générer actions de retrait de pont
    findall(retirer_pont(X1, Y1, X2, Y2),
            member(pont(X1, Y1, X2, Y2), Ponts),
            RetraitsPonts),
    % 🔸 Générer actions de rotation de pont
    findall(roter_pont(pont(X1, Y1, X2, Y2), Direction, A, B),
            (
                member(pont(X1, Y1, X2, Y2), Ponts),
                (A = X1, B = Y1; A = X2, B = Y2),
                (Direction = gauche ; Direction = droite),
                rotate(Direction, X1, Y1, X2, Y2, A, B, pont(NewX1, NewY1, NewX2, NewY2)),
                \+ member(pont(NewX1, NewY1, NewX2, NewY2), Ponts)
            ),
            RotationsPonts),
    append(RetraitsPonts, RotationsPonts, Actions).

actions_possibles(_, _, []).



% Compte les mouvements possibles pour tous les lutins d’une équipe dans un état
mobilite(Equipe, etat(Lutins, Ponts), Score) :-
    findall(1,
        (
            member(l(Equipe, _, X, Y), Lutins),
            (member(pont(X, Y, NX, NY), Ponts) ; member(pont(NX, NY, X, Y), Ponts)),
            \+ member(l(_, _, NX, NY), Lutins)
        ),
        Mouvements),
    length(Mouvements, Score).

% Heuristique 1 = Mobilité nette (moi - les autres)

heuristique_1(Etat, Equipe, ScoreFinal) :-
    mobilite(Equipe, Etat, MonScore),

    findall(ScoreAutre,
        (
            member(AutreEquipe, [bleue, rouge, verte, jaune]),
            AutreEquipe \= Equipe,
            mobilite(AutreEquipe, Etat, ScoreAutre)
        ),
        AutresScores),

    sum_list(AutresScores, ScoreAdverse),
    ScoreFinal is MonScore - ScoreAdverse.



max_profondeur(P) :- P = 2.




% === Ordre de jeu cyclique entre les 4 équipes ===
prochain_joueur(bleue, rouge).
prochain_joueur(rouge, verte).
prochain_joueur(verte, jaune).
prochain_joueur(jaune, bleue).

% === Sélectionne la meilleure action selon score ===
% Feuille atteinte : on retourne juste le score de l'état
minimax_multi(Etat, _, JoueurIA, 0, Score-none) :-
    heuristique_1(Etat, JoueurIA, Score), !.

% Joueur bloqué : on saute au suivant
minimax_multi(Etat, JoueurCourant, JoueurIA, Profondeur, Score-Action) :-
    actions_possibles(Etat, JoueurCourant, Actions),
    Actions == [],
    prochain_joueur(JoueurCourant, Suivant),
    minimax_multi(Etat, Suivant, JoueurIA, Profondeur, Score-Action), !.

% Cas général : on explore les actions du joueur courant
minimax_multi(Etat, JoueurCourant, JoueurIA, Profondeur, MeilleurScore-MeilleureAction) :-
    actions_possibles(Etat, JoueurCourant, Actions),
    Actions \= [],
    ProchainProfondeur is Profondeur - 1,
    prochain_joueur(JoueurCourant, ProchainJoueur),
    
    findall(Score-Action,
        (
            member(Action, Actions),
            transition(Etat, Action, NouvelEtat),
            minimax_multi(NouvelEtat, ProchainJoueur, JoueurIA, ProchainProfondeur, Score-_)
        ),
        ScoreActions),

    max_score_action(ScoreActions, MeilleurScore, MeilleureAction).



% --- Feuille : retourne la valeur heuristique ---
alphabeta_multi(Etat, _, JoueurIA, 0, _, _, Score-none) :-
    heuristique_1(Etat, JoueurIA, Score), !.

% --- Joueur bloqué : passer au joueur suivant ---
alphabeta_multi(Etat, JoueurCourant, JoueurIA, Profondeur, Alpha, Beta, Score-Action) :-
    actions_possibles(Etat, JoueurCourant, Actions),



    Actions == [],
    prochain_joueur(JoueurCourant, Suivant),
    alphabeta_multi(Etat, Suivant, JoueurIA, Profondeur, Alpha, Beta, Score-Action), !.

% --- Cas général avec élagage Alpha-Beta ---
alphabeta_multi(Etat, JoueurCourant, JoueurIA, Profondeur, Alpha, Beta, MeilleurScore-MeilleureAction) :-
    actions_possibles(Etat, JoueurCourant, Actions),
    limiter_actions(Actions, 5, ActionsLimitees),

    Actions \= [],
    ProchainProfondeur is Profondeur - 1,
    prochain_joueur(JoueurCourant, JoueurSuivant),
    alphabeta_loop(ActionsLimitees, Etat, JoueurCourant, JoueurIA, JoueurSuivant, ProchainProfondeur, Alpha, Beta, -10000, none, MeilleurScore-MeilleureAction).


alphabeta_loop([], _, _, _, _, _, _, _, Score, Action, Score-Action) :- !.

alphabeta_loop([Act | Rest], Etat, JoueurCourant, JoueurIA, JoueurSuivant, Profondeur, Alpha, Beta, ScoreBest, ActionBest, FinalScore-FinalAction) :-
    transition(Etat, Act, NouvelEtat),
    alphabeta_multi(NouvelEtat, JoueurSuivant, JoueurIA, Profondeur, Alpha, Beta, Score-_),
    
    (
        Score > ScoreBest
        -> NewScore = Score, NewAction = Act
        ;  NewScore = ScoreBest, NewAction = ActionBest
    ),
    
    NewAlpha is max(Alpha, NewScore),
    
    (
        Beta =< NewAlpha
        -> FinalScore = NewScore, FinalAction = NewAction  % ✂️ Coupure Alpha-Beta
        ;  alphabeta_loop(Rest, Etat, JoueurCourant, JoueurIA, JoueurSuivant, Profondeur, NewAlpha, Beta, NewScore, NewAction, FinalScore-FinalAction)
    ).
    

limiter_actions([], _, []).
limiter_actions(_, 0, []).
limiter_actions([A|R], N, [A|R2]) :-
    N > 0,
    N2 is N - 1,
    limiter_actions(R, N2, R2).


% deplacement_heuristique(+Etat, +Equipe, -Action)

% --- Heuristique 1 : sauver le lutin le plus en danger
deplacement_heuristique2(etat(Lutins, Ponts), Equipe, Action) :-
    % Trouver le lutin avec le moins de ponts disponibles
    findall((NbPonts, l(Equipe, Num, X, Y)),
        (
            member(l(Equipe, Num, X, Y), Lutins),
            findall(_, (member(pont(X,Y,_,_), Ponts); member(pont(_,_,X,Y), Ponts)), PontsDisponibles),
            length(PontsDisponibles, NbPonts)
        ),
        LutinsAvecPonts),
    
    % Trier pour trouver celui avec le moins de ponts
    sort(LutinsAvecPonts, [(MinPonts, l(Equipe, Num, X, Y))|_]),
    
    % Trouver un pont disponible pour ce lutin
    (member(pont(X, Y, NX, NY), Ponts); member(pont(NX, NY, X, Y), Ponts)),
    
    % Vérifier que la case destination est libre
    \+ member(l(_, _, NX, NY), Lutins),
    
    % Créer l'action
    Action = deplacer_et_retirer(l(Equipe, Num), X, Y, NX, NY, pont(X, Y, NX, NY)).

% --- Heuristique 2 : privilégier un lutin très mobile
deplacement_heuristique(etat(Lutins, Ponts), Equipe, Action) :- 
    % Trouver le lutin avec le plus de mobilité
    findall((NbPonts, l(Equipe, Num, X, Y)),
        (
            member(l(Equipe, Num, X, Y), Lutins),
            findall(_, (member(pont(X,Y,NX,NY), Ponts), \+ member(l(_, _, NX, NY), Lutins)), PontsUtilisables),
            length(PontsUtilisables, NbPonts)
        ),
        LutinsAvecMobilite),
    
    % Trier pour trouver celui avec le plus de mobilité
    sort(0, @>=, LutinsAvecMobilite, [(Max, l(Equipe, Num, X, Y))|_]),
    
    % Trouver un pont utilisable
    member(pont(X, Y, NX, NY), Ponts),
    \+ member(l(_, _, NX, NY), Lutins),
    
    % Créer l'action
    Action = deplacer_et_retirer(l(Equipe, Num), X, Y, NX, NY, pont(X, Y, NX, NY)).


% === IA stratégique (Alpha-Beta) ===
alphabeta_ia(Equipe, Action) :-
    etat_jeu(E),
    max_profondeur(P),
    alphabeta_multi(E, Equipe, Equipe, P, -10000, 10000, _-Action).
    

% === IA simple : heuristique 1 (lutin le plus vulnérable)
jouer_ia_heuristique1(Equipe, Action) :-
    etat_jeu(E),
    deplacement_heuristique(E, Equipe, Action).

% === IA simple : heuristique 2 (lutin le plus mobile)
jouer_ia_heuristique2(Equipe, Action) :-
    etat_jeu(E),
    deplacement_heuristique2(E, Equipe, Action).
    
