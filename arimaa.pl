%ensemble des pièces

board([[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

%base fait force

force(6,elephant).
force(5,chameau).
force(4,cheval).
force(3,chien).
force(2,chat).
force(1,lapin).



%prédicat plus_fort

plus_fort(X,Y):-force(A,X),force(B,Y),A>B.



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
couleur([X,Y],C):-board(B), member([X,Y,_,C],B).

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

%empecher lapin reculer
%faire boucle sur les coups possibles en retirant mouvement sup si lapin


toutes_pieces(_):-setof(X,piece_existante(X),L), write(L).



%silver






%prédicat piece, qui permet de savoir si une pièce se trouve en position [X,Y] dans l ensemble des pièces

piece([X,Y],[[X,Y,_,_]|Q]).
piece([X,Y],[_|Q]):-piece([X,Y],Q).




%piece_proche (A REVOIR)

piece_proche([X,Y,TYPE,COULEUR],[X1,Y1,TYPE1,COULEUR1]):-proche([X,Y],[X1,Y1]),piece([X,Y],board(B)), piece([X1,Y1],board(B)).



%pédicat gele, renvoie vraie si la piece est gelé (A REVOIR)

%gele([X,Y,TYPE,COULEUR]):-proche([X,Y],[X1,Y1]), piece([X1,Y1],[[X1,Y1,TYPE1,COULEUR1]|Q]), COULEUR\=COULEUR1.

%push
%pull