padre(juan, carlos). padre(luis, pablo).
padre(juan, luis).  padre(luis, manuel).
padre(carlos, daniel). padre(luis, ramiro).
padre(carlos, diego). abuelo(X, Y) :-  padre(X,Z), padre(Z, Y).
hijo(X, Y) :- padre(Y, X). 
hermano(X, Y) :- padre(Z, Y), padre(Z, X), X \= Y.
descendiente(X, Y) :- padre(Y, X).
descendiente(X, Y) :- padre(Y, Z), descendiente(X, Z).

/*
    ?-abuelo(X, manuel):- X:= Juan, Z:= luis 
        padre(X, Z)
            X := Juan, Z := carlos
            --X := Juan, Z := luis
            X := Carlos, Z := daniel
            X := Carlos, Z := diego
            X := luis, Z := pablo
            X := luis, Z := manuel 
            X := luis, Z := ramiro 
        padre(Z, manuel)
            Z := luis 

?- hermano(manuel, Y)

    C1 padre(Z, Y), padre(Z, manuel), manuel \= Y  nuevo goal

    G1 Z:= juan , Y := carlos --> !falso padre(juan, manuel) 
    G'1 Z:= juan , Y := luis --> !false padre(juan, manuel)
    
    G2 Z:= carlos, Y:= daniel --> !false padre(carlos, manuel)
    G'2 Z:= carlos, Y:= diego --> !false padre(carlos, manuel)

    G3 Z:= luis,  Y:= pablo -->  padre(luis, pablo) , Y := pablo
    G'3 Z:= luis,  Y:= ramiro -->  padre(luis, ramiro), Y:= ramiro


?- descendiente(X, juan)

c1 padre(juan, X)
    X:= carlos, X := luis

c2 padre(juan, Z), descendiente(Z, X2)
    Z:= carlos
        descendiente(carlos, X2)
            c1 padre(carlos, X2)
                X2:= daniel, X2:= diego
            c2 padre(carlos, Z2), descendiente(Z2, X3)
                Z2:= daniel, Z2:= diego
                    c1 Z2 = daniel no machea
                    c1 padre(diego, X3) no machea
    Z:= luis 
        descendiente(luis, X2)
            c1 padre(luis, x2)
                X2 := pablo. X2 := manuel, X2:= ramiro.
    

*/

natural(0).
natural(suc(X)):- natural(X).

menorOIgual(X, X) :- natural(X).
menorOIgual(X, suc(Y)):- X \= 0, menorOIgual(X, Y).


/*
   ?- menorOIgual(0, X)
    c1 
     
*/


/*juntar(L1,L2,L3) */

juntar([], Xs, Xs).
juntar([X|Xs], Ys, [X|Zs]) :-juntar(Xs, Ys, Zs).


/* last(?L, ?U)*/
last(Xs, U):- append(Ys, [U], Xs).

/* reverse(+L, ?R)*/

reverse([], []).
reverse([X], Xs) :- append([], X, Xs).
reverse([X|Xs], Ys):- reverse(Xs,Zs), append(Zs, [X], Ys).

/* prefijo(?P, L)*/
prefijo([], []).
prefijo(P, L) :- append(P, M, L).

/* sufijo()*/
sufijo([], []).
sufijo(S, L) :- append(M, S, L).

/* Sublista */

sublista([], []).
sublista(S, L) :- prefijo(P, L), sufijo(Su, L), append(P, S, Xs), append(Xs, Su, L).

/*pertenece*/

pertenece(X, [X|_]).
pertenece(X, [_|Xs]):- pertenece(X, Xs).

/* aplanar*/

es_lista([]).
es_lista([_|_]).


aplanar([], []).
aplanar([A|Xs], [A|L1]) :- not(es_lista(A)), aplanar(Xs, L1).
aplanar([A|Xs], L) :- es_lista(A), aplanar(A, As), aplanar(Xs, LXs), append(As, LXs, L).


/*interseccion*/
interseccion([], _, []).

interseccion([A|L1s], L2, L3) :-
    interseccion(L1s, L2, M),
    (   pertenece(A, L2),        % A está en L2
        \+ pertenece(A, M)       % y todavía no está en el resultado de la cola
    ->  L3 = [A|M]               % lo agrego
    ;   L3 = M                   % si no, dejo todo como estaba
    ).


/*Borrar */
borrar([], _, []).
borrar([A|L], X, Ls) :- append([A], M, Ls), borrar(L, X, M), A \= X.
borrar([X|L], X, Ls) :- borrar(L, X, Ls).  

/*SacarDuplicados*/
sacarDuplicados([], []).
sacarDuplicados([A|Ls], Ys) :- 
    borrar(Ls, A, Zs),
    append([A], M, Ys),
    sacarDuplicados(Zs, M).

/*permutacion*/

cuantasVeces([], _, 0).
cuantasVeces([A|Xs], X, Res):- cuantasVeces(Xs, X, Res), X \= A.
cuantasVeces([X|Xs], X, Res):- cuantasVeces(Xs, X, N), Res is N+1.

mismaCantidadDeApariciones([], _, _).
mismaCantidadDeApariciones([Z|Zs], Xs, Ys):-
    cuantasVeces(Xs, Z, Res),
    cuantasVeces(Ys, Z, Res),
    mismaCantidadDeApariciones(Zs, Xs, Ys).


esPermutacion(L1, L2) :-
    length(L1, X),
    length(L2, X),
    sacarDuplicados(L1, X1),
    sacarDuplicados(L2, X2),
    interseccion(X1, X2, Ys),
    length(X1, C),
    length(X2, C),
    length(Ys, C),
    mismaCantidadDeApariciones(Ys, L1, L2).


borrarUno(X, [X|Xs], Xs).
borrarUno(X, [Y|Ys], [Y|Zs]) :-
    X \= Y,
    borrarUno(X, Ys, Zs).

permutacion([], []).
permutacion(L1, [X|R]) :-
    pertenece(X, L1),
    borrarUno(X, L1, L1SinX),       
    permutacion(L1SinX, R).

/*reparto*/
reparto(L, 1, [L]).
reparto(L, N, [P | Restos]) :-
    N > 1,
    append(P, R, L),    
    N1 is N - 1,
    reparto(R, N1, Restos).   

/* desde*/

desde(X, X).
desde(X, Y):- 
    not(number(Y)),
    N is X+1,
    desde(N,Y).
    
desde(X, Y) :-
    number(Y),
    desde(N, Y), 
    X is N-1.

/* Desde */


/* Arbol binario*/
arbol(nil).
arbol(bin(Iq, V, Der)) :- arbol(Iq), arbol(Der).

vacio(arbol(nil)).

raiz(nil, nil).
raiz(bin(Iz, V, Der), V).

altura(nil, 0).
altura(bin(Iz, V, Der), Z) :-
    altura(Iz, I),
    altura(Der, D),
    Z is max(I, D) + 1.


cantidadDeNodos(nil, 0).
cantidadDeNodos(bin(Iz, _, Der), S):-
    cantidadDeNodos(Iz, I),
    cantidadDeNodos(Der, D),
    S is (I + D) + 1.


inorder(nil, []).
inorder(bin(Iz, V, Der), L) :-
    append(Zs,[V|M], L),
    inorder(Iz, Zs),
    inorder(Der, M).


arbolConInorder(L, Ab):- 
    inorder(Ab, L).
    

/*Parcial para repasar*/
unico(Rs, U):-append(Pre, [U|Suf], Rs), not(pertenece(U, Pre)), not(pertenece(U, Suf)).


hayRepetidos(Rs) :- member(U, Rs), not(unico(Rs, U)).

sinRepetidos(Rs) :- not(hayRepetidos(Rs)).


formulas(Xs, F):- desde(1, N), formulasDeN(Xs, F, N).
 
formulasDeN(Xs, F, 1):- member(F, Xs).
formulasDeN(Xs, neg(F), N):- N > 1, M is N-1, formulasDeN(Xs, F, M).
formulasDeN(Xs, imp(F1, F2), N):- N > 2, N1 is N-1,
    between(1, N1, NF1), NF2  is N1 - NF1,
    formulasDeN(Xs, F1, NF1), formulasDeN(Xs, F2, NF2).


/*coprimos*/
coprimos(X, Y) :-
    desde(1, Y),
    between(1, Y, X),
    1 is gcd(Y, X),
    Y \= X.

/*Cuadrado semi magico*/
listaSumaN(0, 0, []).
listaSumaN(M, N ,[X|Xs]):-
    M > 0,
    N >= 0,
    between(0, N, X),
    N1 is N-X,
    M1 is M-1,
    listaSumaN(M1, N1, Xs).

filasSumanN(_, _, []).
filasSumanN(N, M, [X|Xs]):-
    listaSumaN(N, M, X),
    filasSumanN(N, M ,Xs).

cuadradoSemiMagico(N, Xs):-
    length(Xs, N),
    desde(0, M),
    filasSumanN(N, M, Xs).
    

/*Cuadrado Magico*/
transponer([], []). 
transponer([[]|_], []) :- !.
transponer(M, [C|MT]) :- cabezasYColas(M,C,Resto), transponer(Resto, MT). 
 
cabezasYColas([],[],[]).
cabezasYColas([[C|Res]|YS], [C|ZS], [Res|TS]):-
	cabezasYColas(YS, ZS, TS).


cuadradoMagico(N, Xs):-
    length(Xs, N),
    desde(0, M),
    filasSumanN(N, M, Xs),
    transponer(Xs, Ys),
    filasSumanN(N, M, Ys).

/*Negacion por falla y cut */

frutal(frutilla).
frutal(banana).
frutal(manzana).
cremoso(banana).
cremoso(americana).
cremoso(frutilla).
cremoso(dulceDeLeche).


leGusta(X) :- frutal(X).
leG(G) :- cremoso(G).
cucurucho(X,Y) :- leGusta(X), !, leGusta(Y). /*El ! bloquea la eleccion de X, es decir, una vez que obtengo una unificacion valida en X, los resultados de una consulta sobre cucurucho solo daran respuestas con un mismo valor de X*/

/*
cucurucho(X,Y)

matchea con 1-legusta(X), 2-legusta(Y)
X <- Frutilla  Y <- Frutilla, X <- Banana, Y <- Frutilla. X <- Frutilla Y <- Banana.
1- machea (sin instanciar)X := X1(Sin intanciar), 1.1-frutal(X1), 1.2-cremoso(X1), 

Busca uno que matche, lo encuentra, y  luego se fija si puedo unificarlo con algun valor macheable en 1.2, entonces X:= Fritilla y X:= Banana, son posibles X
1.1-  --X1:= frutilla
1.1-  --X1:= banana
1.1-  X1:= manzana
1.2-  --X1:= banana
1.2-  X1:= americana
1.2-  --X1:= frutilla
1.2-  X1:= dulceDeleche

2- machea son Y := Y1, 2.1- frutal(Y1), 2.2- cremoso(Y1)
Y := Frutilla  Y := Banana
2.1-  --Y1:= banana
2.1-  --Y1:= frutilla
2.1-  Y1:= manzana
2.2-  --Y1:= banana
2.2-  Y1:= americana
2.2-  --Y1:= frutilla
2.2-  Y1:= dulceDeleche
*/

/* 
Asegurandome que Un Y sea solucion unica para un predicado P(?X)
P(Y), not((P(X), X \= Y)).
*/

/*
corteMásParejo()
*/
masParejo(L1, L2, D):-
    sum_list(L1, M),
    sum_list(L2, N),
    D is abs(M - N).
    
corte(L1, L2, Lz):-
    append(L1, L2, Lz).

corteMasParejo(Lz, L1, L2):-
    corte(L1, L2, Lz),
    masParejo(L1, L2, D),
    not(( 
        corte(L3, L4, Lz), 
        masParejo(L3, L4, D1), 
        D1 < D
    )).


/**/


esPrimo(N):-
    M is N-1,
    not((
            between(2,M,F),
            N mod F =:= 0
        )).

noDidi(X):-
    between(2, X, M),
    esPrimo(M),
    0 is X mod M,
    MM is M * M,
    not( X mod MM =:= 0).
    
esPoderoso(X):- not(noDidi(X)).

proximoNumPoderoso(X, Y):- Y is X+1, esPoderoso(Y).
proximoNumPoderoso(X, Y):- M is X+1, not(esPoderoso(M)), proximoNumPoderoso(M, Y).


esRotacion(L, R):-
    append(S,M,L),
    append(M,S,R).

% collatz(+N,-S) - Secuencia de Collatz desde N
collatz(N, N).
collatz(N, S) :- N > 1, N mod 2 =:= 0, N2 is N/2, collatz(N2, S).
collatz(N, S) :- N > 1, N mod 2 =:= 1, N2 is (3*N)+1, collatz(N2, S).

esMelodiaCompuesta(sec(_, _)).

submelodia(sec(X, Y), sec(X, Y)).
submelodia(sec(I, D), Res) :-
    esMelodiaCompuesta(I),
    esMelodiaCompuesta(D),
    submelodia(I, Res).
submelodia(sec(I, D), Res) :-
    esMelodiaCompuesta(I),
    esMelodiaCompuesta(D),
    submelodia(D, Res).
submelodia(sec(I, D), Res) :-
    not(esMelodiaCompuesta(I)),
    esMelodiaCompuesta(D),
    submelodia(D, Res).
submelodia(sec(I, D), Res) :-
    esMelodiaCompuesta(I),
    not(esMelodiaCompuesta(D)),
    submelodia(I, Res).
    