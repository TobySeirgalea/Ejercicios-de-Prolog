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
buffersUsados(secuencia(Proceso1, Proceso2), L) :- buffersUsados(Proceso1, BuffersProceso1),
                                                   buffersUsados(Proceso2, BuffersProceso2), 
                                                   append(BuffersProceso1, BuffersProceso2, L), 
                                                   sinRepetidos(L),
                                                   ordenCreciente(L).
buffersUsados(paralelo(Proceso1, Proceso2), L)  :- buffersUsados(Proceso1, BuffersProceso1),
                                                   buffersUsados(Proceso2, BuffersProceso2),
                                                   append(BuffersProceso1, BuffersProceso2, L),
                                                   sinRepetidos(L),
                                                   ordenCreciente(L).

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
intercalar(XS, [], XS)             :- XS \= [].
intercalar([],YS, YS)              :- YS \= [].
intercalar([X|XS], [Y|YS], [X|ZS]) :- intercalar(XS, [Y|YS], ZS).
intercalar([X|XS], [Y|YS], [Y|ZS]) :- intercalar([X|XS], YS, ZS).

%% Ejercicio 4
%% serializar(+P,?XS)
serializar(computar, [computar]).
serializar(leer(B), [leer(B)]).
serializar(escribir(B, S), [escribir(B, S)]).
serializar(secuencia(P, Q), L) :- serializar(P, L1),
                                  serializar(Q, L2), 
                                  append(L1, L2, L).
serializar(paralelo(P, Q), L)  :- serializar(P, L1),
                                  serializar(Q, L2),
                                  intercalar(L1, L2, L).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% contenidoBuffer(+B,+ProcesoOLista,?Contenidos)
contenidoBuffer(B, [], []).
contenidoBuffer(B, Serializacion, Contenidos) :- Serializacion = [_|_], %Se podría utilizar is_list también
                                                 realizarLecturas(Serializacion, Contenidos),
                                                 noLeoNadaNoEscritoPreviamente(Contenidos).
contenidoBuffer(B, Proceso, Contenidos)       :- serializar(Proceso, Serializacion), 
                                                 realizarLecturas(Serializacion, Contenidos), 
                                                 noLeoNadaNoEscritoPreviamente(Contenidos).

%%noLeoNadaNoEscritoPreviamente(+Serialización)
noLeoNadaNoEscritoPreviamente([]).
noLeoNadaNoEscritoPreviamente(Serializacion) :- reverse(Serializacion, SerializacionEnOrdenLectura),
                                                lecturasLuegoDeEscrituras(SerializacionEnOrdenLectura).


%%lecturasLuegoDeEscrituras(+Serialización)
lecturasLuegoDeEscrituras([Proceso|Serializacion])      :- Proceso \= leer(Buffer),
                                                           lecturasLuegoDeEscrituras(Serializacion).
lecturasLuegoDeEscrituras([leer(Buffer)|Serializacion]) :- member(escribir(Buffer, _), Serializacion).

%%realizarLecturas(+Lista, -ListaConLecturasRealizadas)
realizarLecturas([], []).
realizarLecturas([escribir(Buffer, Contenido)|Serializacion], ListaConLecturasRealizadas) :- siPerteneceLoSaco(leer(Buffer), Serializacion, YaLeiBuffer),
                                                                                             realizarLecturas(YaLeiBuffer, ListaConLecturasRealizadas).
realizarLecturas([Proceso|Serializacion], [Proceso|ListaConLecturasRealizadas])           :- Proceso \= escribir(Buffer, Contenido),
                                                                                             realizarLecturas(Serializacion, ListaConLecturasRealizadas).

%%siPerteneceLoSaco(+P, +Lista, ?XS) : Si P pertenece a Lista instancia en XS la Lista-{P} sino falla.
siPerteneceLoSaco(Proceso, [Proceso|ListaProcesos], ListaProcesos).
siPerteneceLoSaco(Proceso, [Proceso1|ListaProcesos], [Proceso1|ListaProcesosFinal]) :- Proceso1 \= Proceso, 
                                                                                       siPerteneceLoSaco(Proceso, ListaProcesos, ListaProcesosFinal).


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