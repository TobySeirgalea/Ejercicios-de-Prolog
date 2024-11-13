%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados básicos %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% proceso(+P)
proceso(computar).
proceso(escribir(_,_)).
proceso(leer(_)).
proceso(secuencia(_,_)).
proceso(paralelo(_,_)).

%% Ejercicio 2
%% buffersUsados(+P,-BS)
buffersUsados(escribir(Buffer, _), [Buffer]).
buffersUsados(leer(Buffer), [Buffer]).
buffersUsados(secuencia(Proceso1, Proceso2), L) :- buffersUsados(Proceso1, BuffersProceso1), buffersUsados(Proceso2, BuffersProceso2), append(BuffersProceso1, BuffersProceso2, L), sinRepetidos(L), ordenCreciente(L).
buffersUsados(paralelo(Proceso1, Proceso2), L)  :- buffersUsados(Proceso1, BuffersProceso1), buffersUsados(Proceso2, BuffersProceso2), append(BuffersProceso1, BuffersProceso2, L), sinRepetidos(L), ordenCreciente(L).

%%sinRepetidos(+Lista)
sinRepetidos([]).
sinRepetidos([X|XS]) :- not(member(X, XS)), sinRepetidos(XS).

%%ordenCreciente(+Lista)
ordenCreciente([]).
ordenCreciente([_]).
ordenCreciente([X, Y|YS]) :- X =< Y, ordenCreciente([Y|YS]). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Organización de procesos %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 3
%% intercalar(+XS,+YS,?ZS)
intercalar([], [], []).
intercalar(XS, [], XS) :- XS \= [].
intercalar([],YS, YS) :- YS \= [].
intercalar([X|XS], [Y|YS], [X|ZS]) :- intercalar(XS, [Y|YS], ZS).
intercalar([X|XS], [Y|YS], [Y|ZS]) :- intercalar([X|XS], YS, ZS).



%% Ejercicio 4
%% serializar(+P,?XS)
serializar(P, [P]) :- P \= secuencia(_,_), P\= paralelo(_,_).
serializar(secuencia(P, Q), L) :- serializar(P, L1), serializar(Q, L2), append(L1, L2, L). %, preservaOrden(L1, L2, L).
serializar(paralelo(P, Q), L) :- serializar(P, L1), serializar(Q, L2), append(L1, L2, L), intercalar(L1, L2, L).



procesosAtomicos(escribir).
procesosAtomicos(computar).
procesosAtomicos(leer(_)).
procesosAtomicos(escribir(_,_)).
%si es atomico lo quiero meter directo sin intercalar, i.e. hacer append como con secuencia
%

%Si es secuencia las debo meter a ambos en misma lista L1 para llamar a intercalar(L1, _, L)
%Si es paralelo también






Si son secuencias las apendeo
si son paralelos los intercalo
si son secuencia y paralelo intercalo solo los de paralelo 

21 ?- serializar(paralelo(paralelo(leer(1),leer(2)),secuencia(leer(3),leer(4))),XS).
      serializar((paralelo(leer(1),leer(2))), L1)    serializar((secuencia(leer(3),leer(4))), L2), append(L1, L2, L)
      serializar(leer(1), L11), serializar(leer(2), L12)
                [leer(1)] [leer(2)]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% contenidoBuffer(+B,+ProcesoOLista,?Contenidos)
contenidoBuffer(_,_,_).


%% Ejercicio 6
%% contenidoLeido(+ProcesoOLista,?Contenidos)
contenidoLeido(_,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% esSeguro(+P)

%% Ejercicio 8
%% ejecucionSegura( XS,+BS,+CS) - COMPLETAR LA INSTANCIACIÓN DE XS
ejecucionSegura(_,_,_).

  %% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
  %% lo hace.



%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

% Se espera que completen con las subsecciones de tests que crean necesarias, más allá de las puestas en estos ejemplos

cantidadTestsBasicos(2). % Actualizar con la cantidad de tests que entreguen
testBasico(1) :- proceso(computar).
testBasico(2) :- proceso(secuencia(escribir(1,pepe),escribir(2,pipo))).
testBasico(3) :- buffersUsados(escribir(1, hola), [1]).
% Agregar más tests

cantidadTestsProcesos(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsBuffers(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsSeguros(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests


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