%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados básicos %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% proceso(+P)
proceso(computar).
proceso(escribir(_,_)).
proceso(leer(_)).
proceso(secuencia(P, Q)) :- proceso(P), proceso(Q).
proceso(paralelo(P, Q)) :- proceso(P), proceso(Q).

%% Ejercicio 2
%% buffersUsados(+P, -BS)
buffersUsados(computar, []).
buffersUsados(escribir(B, _), [B]).
buffersUsados(leer(B), [B]).
buffersUsados(secuencia(P, Q), XS) :-
  buffersUsados(P, XSP),
  buffersUsados(Q, XSQ),
  union(XSP, XSQ, XS).
buffersUsados(paralelo(P, Q), XS) :-
  buffersUsados(P, XSP),
  buffersUsados(Q, XSQ),
  union(XSP, XSQ, XS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Organización de procesos %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 3
%% intercalar(+XS, +YS, ?ZS)
intercalar([], [], []).
intercalar([X | XS], YS, [X | ZS]) :- intercalar(YS, XS, ZS).
intercalar(XS, [Y | YS], [Y | ZS]) :- intercalar(XS, YS, ZS).

%% Ejercicio 4
% procesoAtomico(+P)
procesoAtomico(computar).
procesoAtomico(leer(_)).
procesoAtomico(escribir(_, _)).

%% serializar(+P, ?XS)
serializar(computar, [computar]).
serializar(leer(B), [leer(B)]).
serializar(escribir(B, E), [escribir(B, E)]).
serializar(secuencia(P, Q), XS) :-
  serializar(P, XSP),
  serializar(Q, XSQ),
  append(XSP, XSQ, XS).
serializar(paralelo(P, Q), XS) :-
  serializar(P, XSP),
  serializar(Q, XSQ),
  intercalar(XSP, XSQ, XS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% contenidoBuffer(+B, +ProcesoOLista, ?Contenidos)
contenidoBuffer(B, P, C) :-
  proceso(P),
  serializar(P, XS),
  contenidoBuffer(B, XS, C).
contenidoBuffer(B, XS, C) :-
  reverse(XS, XS1),
  contenidoBufferListaInvertida(B, XS1, C).

contenidoBufferListaInvertida(_, [], []).
contenidoBufferListaInvertida(B, [computar | XS], C) :-
  contenidoBufferListaInvertida(B, XS, C).
contenidoBufferListaInvertida(B, [leer(B) | XS], C) :-
  contenidoBufferListaInvertida(B, XS, [_ | C]).
contenidoBufferListaInvertida(B, [escribir(B, E) | XS], C) :-
  contenidoBufferListaInvertida(B, XS, C1), append(C1, [E], C).

contenidoBufferListaInvertida(B1, [leer(B2) | XS], C)
  :- B1 =\= B2, contenidoBufferListaInvertida(B1, XS, C).
contenidoBufferListaInvertida(B1, [escribir(B2, _) | XS], C)
  :- B1 =\= B2, contenidoBufferListaInvertida(B1, XS, C).

/*
  contenidoBuffer(1,[escribir(1,pa),escribir(2,ma),escribir(1,hola),computar,escribir(1,mundo),leer(1)],C).
  contenidoBuffer(2,[escribir(1,pp),escribir(2,ala),escribir(1,ola),computar,escribir(1,mundo),leer(1)],C).
  contenidoBuffer(2,paralelo(escribir(2,sol),secuencia(escribir(1,agua),leer(1))),C).
  contenidoBuffer(1,paralelo(escribir(2,sol),secuencia(escribir(1,agua),leer(1))),XS).
  contenidoBuffer(1,paralelo(leer(1),escribir(1,agua)),XS).
*/

%% Ejercicio 6
%% puedeLeerBuffer(+B, +XS)
puedeLeerBuffer(B, XS) :- contenidoBufferListaInvertida(B, [leer(B) | XS], _).

%% contenidoLeido(+ProcesoOLista,?Contenidos)
contenidoLeido(P, C) :- proceso(P), serializar(P, XS), contenidoLeido(XS, C).
contenidoLeido(XS, C) :- reverse(XS, XS1), contenidoLeidoListaInvertida(XS1, C).

contenidoLeidoListaInvertida([], []).
contenidoLeidoListaInvertida([computar | XS], C) :- contenidoLeidoListaInvertida(XS, C).
contenidoLeidoListaInvertida([escribir(_, _) | XS], C) :- contenidoLeidoListaInvertida(XS, C).
contenidoLeidoListaInvertida([leer(B) | XS], C) :-
  puedeLeerBuffer(B, XS),
  contenidoBufferListaInvertida(B, XS, E),
  contenidoLeidoListaInvertida(XS, C1),
  append(C1, E, C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
% vacio(+L)
vacio([]).

%% noCompartenBuffers(+P, +Q)
noCompartenBuffers(P, Q) :- buffersUsados(P, BP), buffersUsados(Q, BQ), intersection(BP, BQ, B), vacio(B).

%% esSeguro(+P)
esSeguro(P) :- proceso(P), P \= paralelo(_, _), serializar(P, XS), esSerializacionSegura(XS).
esSeguro(paralelo(P,Q)) :-
  noCompartenBuffers(P, Q),
  serializar(paralelo(P,Q), XS),
  esSerializacionSegura(XS). 

% esSerializacionSegura(+XS)
esSerializacionSegura(XS) :- contenidoLeido(XS, _).

/*
  esSeguro(secuencia(leer(1),escribir(1,agua))).
  esSeguro(secuencia(escribir(1,agua),leer(1))).
  esSeguro(paralelo(escribir(1,sol),secuencia(escribir(1,agua),leer(1)))).
  esSeguro(paralelo(escribir(2,sol),secuencia(escribir(1,agua),leer(1)))).
*/

%% Ejercicio 8
%% Generamos todos los procesos atomicos posibles
% generarProcesosAtomicos(+BS, +CS, -P)
generarProcesosAtomicos(_, _, computar).
generarProcesosAtomicos(BS, _, leer(B)) :- member(B, BS).
generarProcesosAtomicos(BS, CS, escribir(B, E)) :- member(B, BS), member(E, CS).

%% Generamos todas las serializaciones de procesos posibles, segun longitud de serializacion
% serializacionesLongitud(+N, +BS, +CS, -XS)
serializacionesLongitud(0, _, _, []).
serializacionesLongitud(N, BS, CS, [P | T]) :-
  N > 0, N1 is N - 1,
  generarProcesosAtomicos(BS, CS, P),
  serializacionesLongitud(N1, BS, CS, T).

% Generar numeros desde (PRECONDICION: X <= Y)
desde(X, X).
desde(X, Y) :- N is X + 1, desde(N, Y).

% Para cada serializacion valida, vamos a testear que sea una ejecucion segura
%% ejecucionSegura(?XS,+BS,+CS)
ejecucionSegura(XS, BS, CS) :-
  var(XS),
  desde(0, N),
  serializacionesLongitud(N, BS, CS, XS),
  esSerializacionSegura(XS).
ejecucionSegura(XS, BS, CS) :-
  nonvar(XS),
  length(XS, N),
  serializacionesLongitud(N, BS, CS, XS),
  esSerializacionSegura(XS).

  %% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
  %% lo hace.


%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%
cantidadTestsBasicos(5). % Actualizar con la cantidad de tests que entreguen
testBasico(1) :- proceso(computar).
testBasico(2) :- proceso(secuencia(escribir(1,pepe),escribir(2,pipo))).
testBasico(3) :- proceso(paralelo(leer(1), computar)).
testBasico(4) :- buffersUsados(secuencia(escribir(1, hola), computar), [1]).
testBasico(5) :- buffersUsados(secuencia(escribir(1, hola), paralelo(leer(1), leer(2))), [1, 2]).

cantidadTestsProcesos(9).
testProcesos(1) :- intercalar([1,2,3], [4,5,6], [1,2,3,4,5,6]). 
testProcesos(2) :- intercalar([1,2,3], [4,5,6], [4,5,6,1,2,3]). 
testProcesos(3) :- intercalar([1,2,3], [4,5,6], [4,5,1,2,6,3]). 
testProcesos(4) :- serializar(computar, [computar]).
testProcesos(5) :- serializar(leer(B), [leer(B)]).
testProcesos(6) :- serializar(escribir(B, E), [escribir(B, E)]).
testProcesos(7) :- serializar(secuencia(computar,leer(2)),  [computar,leer(2)]).
testProcesos(8) :- serializar(paralelo(paralelo(leer(1),leer(2)),secuencia(leer(3),leer(4))), [leer(1),leer(3),leer(2),leer(4)]).
testProcesos(9) :- serializar(paralelo(paralelo(leer(1),leer(2)),secuencia(leer(3),leer(4))), [leer(3),leer(1),leer(4),leer(2)]).

cantidadTestsBuffers(6).
testBuffers(1) :- contenidoBuffer(1,[escribir(1,pa),escribir(2,ma),escribir(1,hola),computar,escribir(1,mundo),leer(1)], [hola, mundo]).
testBuffers(2) :- contenidoBuffer(2,[escribir(1,pp),escribir(2,ala),escribir(1,ola),computar,escribir(1,mundo),leer(1)], [ala]).
testBuffers(3) :- contenidoBuffer(2,paralelo(escribir(2,sol),secuencia(escribir(1,agua),leer(1))), [sol]).
testBuffers(4) :- contenidoBuffer(1,paralelo(escribir(2,sol),secuencia(escribir(1,agua),leer(1))), []).
testBuffers(5) :- contenidoBuffer(1,paralelo(leer(1),escribir(1,agua)), []).
testBuffers(6) :- contenidoBuffer(1,paralelo(secuencia(escribir(1,agua), leer(1)), secuencia(escribir(1, pan), leer(1))), []).

cantidadTestsSeguros(9).
testSeguros(1) :- esSeguro(computar).
testSeguros(2) :- not(esSeguro(secuencia(leer(1),escribir(1,agua)))).
testSeguros(3) :- esSeguro(secuencia(escribir(1,agua),leer(1))).
testSeguros(4) :- not(esSeguro(paralelo(escribir(1,sol),secuencia(escribir(1,agua),leer(1))))).
testSeguros(5) :- esSeguro(paralelo(escribir(2,sol),secuencia(escribir(1,agua),leer(1)))).
testSeguros(6) :- ejecucionSegura([computar], [1,2], [a,b]).
testSeguros(7) :- ejecucionSegura([escribir(2, b)], [1,2], [a,b]).
testSeguros(8) :- ejecucionSegura([escribir(2, b), leer(2), escribir(1, a)], [1,2], [a,b]).
testSeguros(9) :- ejecucionSegura([escribir(1, a), escribir(1, a), leer(1), leer(1)], [1,2], [a,b]).

tests(basico) :- cantidadTestsBasicos(M), forall(between(1,M,N), testBasico(N)).
tests(procesos) :- cantidadTestsProcesos(M), forall(between(1,M,N), testProcesos(N)).
tests(buffers) :- cantidadTestsBuffers(M), forall(between(1,M,N), testBuffers(N)).
tests(seguros) :- cantidadTestsSeguros(M), forall(between(1,M,N), testSeguros(N)).

tests(todos) :-
  tests(basico),
  tests(procesos),
  tests(buffers),
  tests(seguros).

tests :- tests(todos).
