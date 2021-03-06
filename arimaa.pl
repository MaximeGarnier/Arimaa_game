%ensemble des pièces

board([[0,0,rabbit,silver],[0,1,rabbit,silver],[1,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[2,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

%cases_plateau().


%base fait force

force(6,elephant).
force(5,camel).
force(4,horse).
force(3,dog).
force(2,cat).
force(1,rabbit).



%prédicat plus_fort

plus_fort(X,Y):-force(A,X),force(B,Y),A>B.
plus_fort([X,Y],[U,V]):-type([X,Y],T1), type([U,V],T2), plus_fort(T1,T2).


%Cases alentours

sup([X,Y],[X1,Y]):- X>0, X1 is X-1.
inf([X,Y],[X1,Y]):- X<7, X1 is X+1.
gauche([X,Y],[X,Y1]):- Y>0, Y1 is Y-1.
droite([X,Y],[X,Y1]):- Y<7, Y1 is Y+1.



%prédicat proche pour savoir si deux cases sont proches ou de trouver toutes les cases proches

proche(CASE1,CASE2):-sup(CASE1,CASE2).
proche(CASE1,CASE2):-inf(CASE1,CASE2).
proche(CASE1,CASE2):-gauche(CASE1,CASE2).
proche(CASE1,CASE2):-droite(CASE1,CASE2).

%get_voisins retourne liste des cases voisines

get_voisins(P,L):-setof(P2,proche(P,P2),L).


piece_existante([X,Y]):-board(B), member([X,Y,_,_],B).
piece_existante([X,Y,T,C]):-board(B), member([X,Y,T,C],B).
case_vide(C):- \+piece_existante(C).
couleur([X,Y],C):-board(B), member([X,Y,_,C],B),!.
type([X,Y],T):-board(B), member([X,Y,T,_],B),!.

%piece_voisine

piece_voisine([X,Y],P):-get_voisins([X,Y],L), member(P,L), piece_existante(P).
pieces_voisines([X,Y],L):- setof(Z, piece_voisine([X,Y],Z), L).

%Voisin_allier

voisin_allier([X,Y]):-pieces_voisines([X,Y],L),member(P,L), couleur(P,C),couleur([X,Y],C).
voisin_allier([X,Y],P):-pieces_voisines([X,Y],L),member(P,L), couleur(P,C),couleur([X,Y],C).
voisins_alliers([X,Y],L):-setof(Z,voisin_allier([X,Y],Z),L).

%voisin_ennemi

voisin_ennemi([X,Y]):-pieces_voisines([X,Y],L),member(P,L), couleur(P,C1),couleur([X,Y],C2), C1\=C2.
voisin_ennemi([X,Y],P):-pieces_voisines([X,Y],L),member(P,L), couleur(P,C1),couleur([X,Y],C2), C1\=C2.
voisins_ennemis([X,Y],L):-setof(Z,voisin_ennemi([X,Y],Z),L).

voisin_ennemi_plus_fort(X):-voisin_ennemi(X,P), type(X,T1), type(P,T2), plus_fort(T2,T1).

%empecher lapin reculer
%faire boucle sur les coups possibles en retirant mouvement sup si lapin

coup_possible(P,CASE):- \+type(P,rabbit), piece_silver(P),get_voisins(P,L1), member(CASE,L1), case_vide(CASE).
coup_possible(P,CASE):-type(P,rabbit), piece_silver(P),sup(P,C), get_voisins(P,L1), member(CASE,L1), CASE\=C, case_vide(CASE).
coups_possibles(P,L):-setof(X,coup_possible(P,X),L).

%pieces suivant leur couleur
piece_gold([X,Y]):-board(B), member([X,Y,_,gold],B),!.
piece_silver([X,Y]):-board(B), member([X,Y,_,silver],B),!.

%Récupérer toutes les pièces, toutes les pièces gold et toutes les pièces silver

toutes_pieces(L):-setof(X,piece_existante(X),L).
toutes_pieces_gold(L):-setof(X,piece_gold(X),L).
toutes_pieces_silver(L):-setof(X,piece_silver(X),L).

%pousser
peut_pousser_tirer(X,Y):-plus_fort(X,Y), voisin_ennemi(X,Y).
peut_pousser(X,Y):-peut_pousser_tirer(X,Y), sup(X,Y), sup(Y,Z), case_vide(Z),!.
peut_pousser(X,Y):-peut_pousser_tirer(X,Y), inf(X,Y), inf(Y,Z), case_vide(Z),!.
peut_pousser(X,Y):-peut_pousser_tirer(X,Y), gauche(X,Y), gauche(Y,Z), case_vide(Z),!.
peut_pousser(X,Y):-peut_pousser_tirer(X,Y), droite(X,Y), droite(Y,Z), case_vide(Z),!.

%tirer
peut_tirer(X,Y):-peut_pousser_tirer(X,Y), sup(X,Y), sup(Z,X), case_vide(Z),!.
peut_tirer(X,Y):-peut_pousser_tirer(X,Y), inf(X,Y), inf(Z,X), case_vide(Z),!.
peut_tirer(X,Y):-peut_pousser_tirer(X,Y), gauche(X,Y), gauche(Z,X), case_vide(Z),!.
peut_tirer(X,Y):-peut_pousser_tirer(X,Y), droite(X,Y), droite(Z,X), case_vide(Z),!.


%gele

gele(P):-piece_existante(P), \+voisin_allier(P,_), voisin_ennemi_plus_fort(P),!.

%tomber
cases_pieges([[2,2],[2,6],[6,2],[6,6]]).
tomber(X):-cases_pieges(L), member(X,L), \+voisin_allier(X),!.

deplacement1(X,M1,[M1|L],B):-coup_possible(X,M1), remove(X,B,L).
deplacement4(X,[M1,M2,M3,M4],NewB,B):- deplacement1(X,M1,L,B), deplacement1(M1,M2,L1,L),deplacement1(M2,M3,L2,L1), deplacement1(M3,M4,NewB,L2).

%Est-ce que les pièces sur les pièges disparaissent automatiquement du board ??
%Comment on accède au board??
remove([X,Y],B,NewBoard):-delete(B,[X,Y,_,_],NewBoard).
%update_board(B,L).

%get_rabbits(Couleur, Board,L) : retourne les lapins d'une couleur dans L 
get_rabbits(_,[],[]).
get_rabbits(C,[[X,Y,rabbit,C]|Q],[[X,Y]|L]):-get_rabbits(C,Q,L),!.
get_rabbits(C,[_|Q],L):- get_rabbits(C,Q,L).

%get_cats(Couleur, Board,L) : retourne les chats d'une couleur dans L 
get_cats(_,[],[]).
get_cats(C,[[X,Y,cat,C]|Q],[[X,Y]|L]):-get_cats(C,Q,L),!.
get_cats(C,[_|Q],L):- get_cats(C,Q,L).

%get_chiens(Couleur, Board,L) : retourne les chiens d'une couleur dans L 
get_dogs(_,[],[]).
get_dogs(C,[[X,Y,dog,C]|Q],[[X,Y]|L]):-get_dogs(C,Q,L),!.
get_dogs(C,[_|Q],L):- get_dogs(C,Q,L).



%Fonctions outils éventuellement utile
concat([],L,L).
concat([T|Q],L,[T|R]):-concat(Q,L,R).


inter([], _, []).
inter([H1|T1], L2, [H1|Res]) :- member(H1, L2),inter(T1, L2, Res).
inter([_|T1], L2, Res) :- inter(T1, L2, Res).

% declare the dynamic fact
:- dynamic moves/1.

% predicat to add a new move to the list of moves
add_move(NewMove) :- moves(M), retract(moves(M)), asserta(moves([NewMove|M])).

% init moves with an empty list, add a new move to this list, return the new moves with the added move
test(M) :- asserta(moves([])), add_move([[1,0],[2,0]]), moves(M).




%IA couleur=silver







%pédicat gele, renvoie vraie si la piece est gelé (A REVOIR)

%gele([X,Y,TYPE,COULEUR]):-proche([X,Y],[X1,Y1]), piece([X1,Y1],[[X1,Y1,TYPE1,COULEUR1]|Q]), COULEUR\=COULEUR1.

%push
%pull