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
                                                            realizarLecturas(Serializacion, _,BufferConLecturasRealizadas),
                                                            noLeoNadaNoEscritoPreviamente(BufferConLecturasRealizadas),
                                                            soloContenidoDel(Buffer, BufferConLecturasRealizadas, ContenidosBuffer).
contenidoBuffer(Buffer, Proceso, ContenidosBuffer)       :- serializar(Proceso, Serializacion), 
                                                            realizarLecturas(Serializacion, _, BufferConLecturasRealizadas), 
                                                            noLeoNadaNoEscritoPreviamente(BufferConLecturasRealizadas),
                                                            soloContenidoDel(Buffer, BufferConLecturasRealizadas, ContenidosBuffer).

%% noLeoNadaNoEscritoPreviamente(+Serialización)
noLeoNadaNoEscritoPreviamente(Serializacion) :- reverse(Serializacion, SerializacionEnOrdenLectura),
                                                lecturasLuegoDeEscrituras(SerializacionEnOrdenLectura).

%% soloContenidoDel(+Buffer, +ListaProcesos, -ListaContenidosBuffer) : Filtra y instancia en ListaContenidosBuffer la lista con sólo los contenidos del Buffer
soloContenidoDel(Buffer, [], []).
soloContenidoDel(Buffer, [escribir(Buffer, Contenido)|RestoProcesos], [Contenido|RestoContenidoBuffer]) :- soloContenidoDel(Buffer, RestoProcesos, RestoContenidoBuffer). 
soloContenidoDel(Buffer, [ProcesoActual|RestoProcesos], RestoContenidoBuffer)                           :- ProcesoActual \= escribir(Buffer, _), 
                                                                                                           soloContenidoDel(Buffer, RestoProcesos, RestoContenidoBuffer).

%% lecturasLuegoDeEscrituras(+Serialización)
lecturasLuegoDeEscrituras([]).
lecturasLuegoDeEscrituras([Proceso|Serializacion])      :- Proceso \= leer(_),
                                                           lecturasLuegoDeEscrituras(Serializacion).
lecturasLuegoDeEscrituras([leer(Buffer)|Serializacion]) :- memberchk(escribir(Buffer, _), Serializacion), 
                                                           siPerteneceLoSaco(escribir(Buffer, _), Serializacion, L), 
                                                           lecturasLuegoDeEscrituras(L).

 
%% siPerteneceLoSaco(+P, +Lista, ?XS) : Si P pertenece a Lista instancia en XS la Lista-{primera aparición de P} sino falla.
siPerteneceLoSaco(Proceso, [], []).
siPerteneceLoSaco(Proceso, [Proceso|ListaProcesos], ListaProcesos).
siPerteneceLoSaco(Proceso, [Proceso1|ListaProcesos], [Proceso1|ListaProcesosFinal]) :- Proceso1 \= Proceso, 
                                                                                       siPerteneceLoSaco(Proceso, ListaProcesos, ListaProcesosFinal).

%% Ejercicio 6
%% contenidoLeido(+ProcesoOLista,?Contenidos)
contenidoLeido(Serializacion, Contenido) :- Serializacion = [_|_], 
                                            noLeoNadaNoEscritoPreviamente(Serializacion),
                                            realizarLecturas(Serializacion, Contenido, _).
contenidoLeido(Procesos, Contenido)      :- serializar(Procesos, Serializacion), 
                                            noLeoNadaNoEscritoPreviamente(Serializacion),
                                            realizarLecturas(Serializacion, Contenido, _).

%% realizarLecturas(+ListaProcesos, -ListaContenidosLeidos, -ListaConLecturasRealizadas)
realizarLecturas([], [], []).
realizarLecturas([escribir(Buffer, Contenido)|RestoProcesos], [Contenido|ListaContenidosLeidos], ListaConLecturasRealizadas)                    :- memberchk(leer(Buffer), RestoProcesos), 
                                                                                                                                                   siPerteneceLoSaco(leer(Buffer), RestoProcesos, ProcesosTrasLectura),
                                                                                                                                                   realizarLecturas(ProcesosTrasLectura, ListaContenidosLeidos, ListaConLecturasRealizadas).
realizarLecturas([escribir(Buffer, Contenido)|RestoProcesos], ListaContenidosLeidos, [escribir(Buffer, Contenido)|ListaConLecturasRealizadas])  :- not(memberchk(leer(Buffer), RestoProcesos)), realizarLecturas(RestoProcesos, ListaContenidosLeidos, ListaConLecturasRealizadas).
realizarLecturas([Proceso|RestoProcesos], ListaContenidosLeidos, [Proceso|ListaConLecturasRealizadas])                                          :- Proceso \= escribir(_, _),
                                                                                                                                                   realizarLecturas(RestoProcesos, ListaContenidosLeidos, ListaConLecturasRealizadas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% esSeguro(+P)
esSeguro([]).
esSeguro(Serializacion) :- Serializacion = [_|_],
                           noLeoNadaNoEscritoPreviamente(Serializacion).
esSeguro(Procesos)      :- procesosParalelosNoCompartenBuffer(Procesos), 
                           not((serializar(Procesos, Serializacion),
                           not(esSeguro(Serializacion)))).

%% procesosParalelosNoCompartenBuffer(+Proceso)
procesosParalelosNoCompartenBuffer(escribir(_,_)).
procesosParalelosNoCompartenBuffer(computar).
procesosParalelosNoCompartenBuffer(leer(_)).
procesosParalelosNoCompartenBuffer(secuencia(P, Q)) :- procesosParalelosNoCompartenBuffer(P),
                                                       procesosParalelosNoCompartenBuffer(Q). 
procesosParalelosNoCompartenBuffer(paralelo(P,Q))   :- serializar(P, SerializacionDeP), 
                                                       serializar(Q, SerializacionDeQ), 
                                                       noCompartenBuffers(SerializacionDeP, SerializacionDeQ).

%% noComparteBuffers(+PrimerListaProcesos, +SegundaListaProcesos)
noCompartenBuffers([], SegundaListaProcesos).
noCompartenBuffers([escribir(Buffer, _)|RestoProcesos], SegundaListaProcesos) :- not(memberchk(escribir(Buffer, _), SegundaListaProcesos)), noCompartenBuffers(RestoProcesos, SegundaListaProcesos). 
noCompartenBuffers([leer(Buffer)|RestoProcesos], SegundaListaProcesos)        :- not(memberchk(leer(Buffer))), noCompartenBuffers(RestoProcesos, SegundaListaProcesos). 

%% Ejercicio 8
%% ejecucionSegura( ?XS,+BS,+CS) - COMPLETAR LA INSTANCIACIÓN DE XS
ejecucionSegura(Ejecucion, ListaBuffers, Contenidos) :- var(Ejecucion),
                                                        desde(0, N),
                                                        between(0, N, Longitud),
                                                        generarTodasLasEjecucionesDe(ListaBuffers, Contenidos, Ejecucion, N),
                                                        length(Ejecucion, Longitud), 
                                                        esSeguro(Ejecucion).
ejecucionSegura(Ejecucion, ListaBuffers, Contenidos) :- nonvar(Ejecucion),
                                                        length(Ejecucion, LongitudEjecucion),
                                                        generarTodasLasEjecucionesDe(ListaBuffers, Contenidos, Ejecución, LongitudEjecucion),
                                                        esSeguro(Ejecucion). 
%% generaTodasLasEjecucionesDe(+ListaBuffers, +ListaContenidos, ?Ejecucion, +N) En el caso de recibir Ejecución instanciado tiene éxito sii esta es una lista de N elementos, cuyos elementos o son leer(B) con B en ListaBuffers, escribir(B, C) con B en ListaBuffers y C en ListaContenidos o computar.
generarTodasLasEjecucionesDe(_, _, [], 0).
generarTodasLasEjecucionesDe(ListaBuffers, ListaContenidos, [escribir(Buffer, Contenido)|RestoProcesos], N) :- N >= 0, 
                                                                                                               member(Buffer, ListaBuffers),
                                                                                                               member(Contenido, ListaContenidos), 
                                                                                                               N2 is N-1,
                                                                                                               generarTodasLasEjecucionesDe(ListaBuffers, ListaContenidos, RestoProcesos, N2). 
generarTodasLasEjecucionesDe(ListaBuffers, ListaContenidos, [leer(Buffer)|RestoProcesos], N)                :- N >= 0,
                                                                                                               member(Buffer, ListaBuffers),
                                                                                                               N2 is N-1,
                                                                                                               generarTodasLasEjecucionesDe(ListaBuffers, ListaContenidos, RestoProcesos, N2). 
generarTodasLasEjecucionesDe(ListaBuffers, ListaContenidos, [computar|RestoProcesos], N)                    :- N >= 0,
                                                                                                               N2 is N-1,
                                                                                                               generarTodasLasEjecucionesDe(ListaBuffers, ListaContenidos, RestoProcesos, N2).

%% desde(+X, -Y)
desde(X,X).
desde(X, Y) :- X1 is X+1, desde(X1, Y).


  %% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
  %% lo hace.
  /*
  Si XS (Ejecución) no está instanciada entramos en la primer cláusula. Como Ejecución está instanciada var(Ejecucion) tiene éxito. Tanto el desde(0,N) como between(0,N,Longitud) no requieren que Ejecucion esté instanciada porque ni lo usan. generarTodasLasEjecucionesDe es reversible en Ejecución, así que puede recibirla instanciada
  length(Ejecucion, Longitud) tampoco tiene problemas si Ejecucion viene instanciada porque length es reversible en ambos argumentos, en este caso verificará que la longitud de Ejecucion sea Longitud. Luego entra Ejecucion instanciada en esSeguro(+P), por lo que tampoco hay problema.
  Si XS (Ejecucion) está instanciada var(Ejecucion) falla y nonvar(Ejecucion) tiene éxito, por lo que entra sólo en la segunda cláusula. length(Ejecucion, Longitud) no tiene problema porque es reversible en sus dos argumentos, en este caso también verifica que Ejecucion tenga como longitud a Longitud. generarTodasLasEjecucionDe es reversible en Ejecucion, en este caso verifica que todos los procesos de Ejecucion sean lecturas, escrituras o computar. Si son lecturas pide que sean de un Buffer de la ListaBuffers, si son escrituras también se fija que sus contenidos sean de ListaContenidos. Esto lo repite por cada elemento de la lista. En el caso de que no unifique con ninguna cláusula de estas es porque no todos sus procesos son sobre Buffers de ListaBuffers o no tienen contenidos de ListaContenidos y acaba fallando. Después de ver que se cumplan estas condiciones entra en esSeguro(+P) con Ejecucion ya instanciada por lo que no hay problema.
  */


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