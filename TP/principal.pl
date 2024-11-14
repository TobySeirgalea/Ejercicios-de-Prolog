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
%% contenidoBuffer(+Buffer,+ProcesoOLista,?Contenidos)
contenidoBuffer(_, [], []).
contenidoBuffer(Buffer, Serializacion, ContenidosBuffer) :- Serializacion = [_|_], %Se podría utilizar is_list también
                                                            realizarLecturas(Buffer, Serializacion, Contenidos),
                                                            noLeoNadaNoEscritoPreviamente(Contenidos),
                                                            soloContenidoDel(Buffer, Contenidos, ContenidosBuffer).
contenidoBuffer(Buffer, Proceso, ContenidosBuffer)       :- serializar(Proceso, Serializacion), 
                                                            realizarLecturas(Buffer, Serializacion, Contenidos), 
                                                            noLeoNadaNoEscritoPreviamente(Contenidos),
                                                            soloContenidoDel(Buffer, Contenidos, ContenidosBuffer).

%%noLeoNadaNoEscritoPreviamente(+Serialización)
noLeoNadaNoEscritoPreviamente(Serializacion) :- reverse(Serializacion, SerializacionEnOrdenLectura),
                                                lecturasLuegoDeEscrituras(SerializacionEnOrdenLectura).



soloContenidoDel(Buffer, [], []).
soloContenidoDel(Buffer, [escribir(Buffer, Contenido)|XS], [Contenido|L]) :- soloContenidoDel(Buffer, XS, L). 
soloContenidoDel(Buffer, [X|XS], L)                                       :- X \= escribir(Buffer, _), 
                                                                             soloContenidoDel(Buffer, XS, L).

%%lecturasLuegoDeEscrituras(+Serialización)
lecturasLuegoDeEscrituras([]).
lecturasLuegoDeEscrituras([Proceso|Serializacion])      :- Proceso \= leer(_),
                                                           lecturasLuegoDeEscrituras(Serializacion).
lecturasLuegoDeEscrituras([leer(Buffer)|Serializacion]) :- memberchk(escribir(Buffer, _), Serializacion), siPerteneceLoSaco(escribir(Buffer, _), Serializacion, L), lecturasLuegoDeEscrituras(L).

%%realizarLecturas(+Buffer, +Lista, -ListaConLecturasRealizadas) : Realiza las lecturas del buffer, instancia en ListaConLecturasRealizadas el buffer resultante serializado
realizarLecturas(Buffer, [], [], []).
realizarLecturas(Buffer, [escribir(Buffer, Contenido)|XS], L) :- memberchk(leer(Buffer), XS), 
                                                                                             siPerteneceLoSaco(leer(Buffer), XS, R),
                                                                                             realizarLecturas(Buffer, R, L).
realizarLecturas(Buffer, [escribir(Buffer, Contenido)|XS], [escribir(Buffer, Contenido)|XS]) :- not(memberchk(leer(Buffer), XS)).
realizarLecturas(Buffer, [Proceso|XS], [Proceso|L])                                          :- Proceso \= escribir(Buffer,_),
                                                                                                                realizarLecturas(Buffer, XS, L).
 




%%siPerteneceLoSaco(+P, +Lista, ?XS) : Si P pertenece a Lista instancia en XS la Lista-{primera aparición de P} sino falla.
siPerteneceLoSaco(Proceso, [], []).
siPerteneceLoSaco(Proceso, [Proceso|ListaProcesos], ListaProcesos).
siPerteneceLoSaco(Proceso, [Proceso1|ListaProcesos], [Proceso1|ListaProcesosFinal]) :- Proceso1 \= Proceso, 
                                                                                       siPerteneceLoSaco(Proceso, ListaProcesos, ListaProcesosFinal).


%% Ejercicio 6
%% contenidoLeido(+ProcesoOLista,?Contenidos)
contenidoLeido(Serializacion, Contenido) :- Serializacion = [_|_], 
                                   noLeoNadaNoEscritoPreviamente(Serializacion),
                                   realizarLecturas(Serializacion, Contenido).
contenidoLeido(Procesos, Contenido) :- serializar(Procesos, Serializacion), 
                                   noLeoNadaNoEscritoPreviamente(Serializacion),
                                   realizarLecturas(Serializacion, Contenido).


                                   
realizarLecturas([], []).
realizarLecturas([escribir(Buffer, Contenido)|XS], [Contenido|L]) :- memberchk(leer(Buffer), XS), 
                                                                     siPerteneceLoSaco(leer(Buffer), XS, R),
                                                                     realizarLecturas(R, L).
realizarLecturas([escribir(Buffer, Contenido)|XS], L)             :- not(memberchk(leer(Buffer), XS)), realizarLecturas(XS, L).
realizarLecturas([Proceso|XS], L)                       :- Proceso \= escribir(_, _),
                                                                     realizarLecturas(XS, L).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% esSeguro(+P)
esSeguro([]).
esSeguro(Serializacion) :- Serializacion = [_|_],
                           noLeoNadaNoEscritoPreviamente(Serializacion).
esSeguro(Procesos) :-     procesosParalelosNoCompartenBuffer(Procesos), 
                          not((serializar(Procesos, Serializacion),
                          not(esSeguro(Serializacion)))).

procesosParalelosNoCompartenBuffer(escribir(_,_)).
procesosParalelosNoCompartenBuffer(computar).
procesosParalelosNoCompartenBuffer(leer(_)).
procesosParalelosNoCompartenBuffer(secuencia(P, Q)) :- procesosParalelosNoCompartenBuffer(P), procesosParalelosNoCompartenBuffer(Q). 
procesosParalelosNoCompartenBuffer(paralelo(P,Q)) :- serializar(P, L1), serializar(Q, L2), noCompartenBuffers(L1, L2).

noCompartenBuffers([], L).
noCompartenBuffers([escribir(Buffer, _)|PS], L2) :- not(memberchk(escribir(Buffer, _), L2)), noCompartenBuffers(PS, L2). 
noCompartenBuffers([leer(Buffer)|PS], L2) :- not(memberchk(leer(Buffer))), noCompartenBuffers(PS, L2). 

%% Ejercicio 8
%% ejecucionSegura( XS,+BS,+CS) - COMPLETAR LA INSTANCIACIÓN DE XS
ejecucionSegura(XS, ListaBuffers, Contenidos) :-  desde(0, N), between(0, N, N2), generarTodasLasEjecucionesDe(ListaBuffers, Contenidos, XS, N), length(XS, N2), esSeguro(XS).

%generar unidireccional y llamar con permutaciones de bs y cs

generarTodasLasEjecucionesDe(_,_,[],0).
generarTodasLasEjecucionesDe(BS, CS, [computar|XS] , N) :- N > 0, N2 is N - 1, generarTodasLasEjecucionesDe([B1|BS], [C1|CS], XS, N2).
generarTodasLasEjecucionesDe(BS, CS, [escribir(B, C)|XS] , N) :- N > 0, N2 is N - 1, member(BS, B), member(CS, C),generarTodasLasEjecucionesDe(BS, CS, XS, N2).
generarTodasLasEjecucionesDe(BS, CS, [leer(B)|XS] , N) :- N > 0, N2 is N - 1, member(BS, B),generarTodasLasEjecucionesDe(BS, CS, XS, N2).

permutaciones([], []).
permutaciones([X|XS], L) :- var(L), permutaciones(XS, L1), agregarEnTodasPartes(X, L1, L).
%permutacion(+L1, +L2) sería más eficiente si implementamos una función 'mismosElementos' / que si L1 y L2 tienen los mismos elementos y longitud, entonces una es permutación de la otra
permutaciones([X|XS], L) :- nonvar(L), length([X|XS], L1), length(L, L1), mismosElementos([X|XS], L).

desde(X,X).
desde(X, Y) :- X1 is X+1, desde(X1, Y).

%mismosElementos(+L1, +L2)
mismosElementos([], []).
mismosElementos(L1, L2) :- pertenece(X, L1), pertenece(X, L2). 

%agregarEnTodasPartes(+E, +L, -LS)
agregarEnTodasPartes(E, [], [E]).
agregarEnTodasPartes(E, [X|XS], [E,X|XS]).
agregarEnTodasPartes(E, [X|XS], [X|L]) :- agregarEnTodasPartes(E, XS, L).



  %% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
  %% lo hace.



%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%


% debería retornar false porque hay una lectura antes que una escritura
% contenidoBuffer(2,paralelo(secuencia(leer(2),escribir(2,sol)),secuencia(escribir(1,agua),leer(1))),XS).




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