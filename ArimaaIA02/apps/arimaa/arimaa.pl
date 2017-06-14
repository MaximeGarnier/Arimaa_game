:- module(bot,
      [  get_moves/3
      ]).
	
% A few comments but all is explained in README of github

% get_moves signature
% get_moves(Moves, gamestate, board).

% Exemple of variable
% gamestate: [side, [captured pieces] ] (e.g. [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ]) 
% board: [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]

% Call exemple:
% get_moves(Moves, [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

:- dynamic board/1.
% declare the dynamic fact
:- dynamic moves/1.

% predicat to add a new move to the list of moves
add_move(NewMove) :- moves(M), retract(moves(M)), asserta(moves([NewMove|M])).

% init moves with an empty list, add a new move to this list, return the new moves with the added move
test(M) :- asserta(moves([])), add_move([[1,0],[2,0]]), moves(M).

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

coup_possible(P,CASE):- \+type(P,rabbit),\+gele(P), piece_silver(P),get_voisins(P,L1), member(CASE,L1), case_vide(CASE).
coup_possible(P,CASE):-type(P,rabbit),\+gele(P), piece_silver(P),sup(P,C), get_voisins(P,L1), member(CASE,L1), CASE\=C, case_vide(CASE).
coups_possibles(P,L):-setof(X,coup_possible(P,X),L).
coup_possible([[X,Y,_,_]|_],[X,Y],CASE):- coup_possible([X,Y],CASE),!.
coup_possible([[_,_,_,_]|Q],P,CASE):-coup_possible(Q,P,CASE).

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

%get_horses(Couleur, Board,L) : retourne les chevaux d'une couleur dans L 
get_horses(_,[],[]).
get_horses(C,[[X,Y,horse,C]|Q],[[X,Y]|L]):-get_horses(C,Q,L),!.
get_horses(C,[_|Q],L):- get_horses(C,Q,L).

%get_camels(Couleur, Board,L) : retourne les chameaux d'une couleur dans L 
get_camels(_,[],[]).
get_camels(C,[[X,Y,camel,C]|Q],[[X,Y]|L]):-get_camels(C,Q,L),!.
get_camels(C,[_|Q],L):- get_camels(C,Q,L).

%get_elephants(Couleur, Board,L) : retourne les elephants d'une couleur dans L 
get_elephants(_,[],[]).
get_elephants(C,[[X,Y,elephants,C]|Q],[[X,Y]|L]):-get_elephants(C,Q,L),!.
get_elephants(C,[_|Q],L):- get_elephants(C,Q,L).


%gele

gele(P):-piece_existante(P), \+voisin_allier(P,_), voisin_ennemi_plus_fort(P),!.

%tomber
cases_pieges([[2,2],[2,6],[6,2],[6,6]]).
tomber(X):-cases_pieges(L), member(X,L), \+voisin_allier(X),!.

%deplacement1(X,M1,[M1|L],B):-coup_possible(X,M1), remove(X,B,L).
%deplacement4(X,[M1,M2,M3,M4],NewB,B):- deplacement1(X,M1,L,B), deplacement1(M1,M2,L1,L),deplacement1(M2,M3,L2,L1), deplacement1(M3,M4,NewB,L2).

%Est-ce que les pièces sur les pièges disparaissent automatiquement du board ??
%Comment on accède au board??
remove([X,Y],B,NewBoard):-delete(B,[X,Y,_,_],NewBoard).

deplacement(0,_,_).
deplacement(N,[P1|P],[C1|C]):-N>0, board(Board), coup_possible(Board,P1,C1), updateBoard(P1,C1), M is N-1, deplacement(M,P,C).

move(0,[]).
move(N,[[X1,Y1]|Q]):-N>0, board(Board), coup_possible(Board,X1,Y1), updateBoard(X1,Y1), M is N-1, move(M,Q).
%move(N,[[X1,Y1]|Q]):-N>0, board(Board), get_rabbits(silver,Board,B), coup_possible(B,X1,Y1), updateBoard(X1,Y1), M is N-1, move(M,Q).

% default call
%get_moves([[[1,0],C]], Gamestate, Board) :- asserta(board(Board)), coup_possible([1,0],C).
get_moves1([[P,C]], Gamestate, Board) :- asserta(board(Board)), coup_possible(Board,P,C).
get_move_qui_marche(Old,New,Gamestate,Board):- asserta(board(Board)), updateBoard(Old,New).
get_move11(Old,New,Gamestate,Board):- asserta(board(Board)), deplacement(1,[Old],[New]).
get_moves2(Old,New,Gamestate,Board):- asserta(board(Board)), deplacement(4,Old,New).
get_moves(Moves,Gamestate,Board):-asserta(board(Board)), move(4,Moves).


% predicat to add a new move to the list of moves
%updateBoard(Old,New) :- board(Board), remove(Old,Board,NewBoard), retract(board(Board)), asserta(board([New|NewBoard])).
updateBoard([X,Y],[X1,Y1]) :- board(Board), get_piece([X,Y],[X,Y,T,C]), remove([X,Y],Board,NewBoard), retract(board(Board)), asserta(board([[X1,Y1,T,C]|NewBoard])).
%updateBoard(_,_) :- board(Board).
test1(UB) :- asserta(board([[0,0,rabbit,silver],[0,1,rabbit,silver],[1,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[2,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]])), 
updateBoard([6,1],[6,2]), board(UB).

%piece_existante([X,Y]):-board(B), member([X,Y,_,_],B).
get_piece([X,Y],[X,Y,T,C]):-board(B), member([X,Y,T,C],B).




