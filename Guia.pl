%Ejercicio 1.
padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).
/* ¿Cuál el resultado de la consulta abuelo(X, manuel)? 
Instancia Y:= manuel.
padre(X,Z), padre(Z, manuel)
Lee la primer cláusula y unifica X:=juan, Z:=carlos. 
padre(carlos, manuel) da false, entonces hace backtracking a ultima instanciación
Lee la segunda cláusula y unifica X:=juan, Z:=luis.
padre(luis, manuel) da true -> retorna X:= juan.
Si pedimos más soluciones sigue. Hace backtracking a última instanciación
Lee la tercer cláusula y unifica X:=carlos, Z:=daniel.
padre(daniel, manuel) da false
Lee la cuarta cláusula y unifica X:=carlos, Z:=diego.
padre(diego, manuel) da false
Lee la quinta cláusula y unifica X:=luis, Z:=pablo.
padre(pablo, manuel) da false
Lee la sexta cláusula y unifica X:=luis, Z:=manuel.
padre(manuel, manuel) da false
Lee la séptima cláusula y unifica X:=luis, Z:=ramiro
padre(ramiro, manuel da false)
Hace backtracking a cláusula abuelo, como no hay otra debajo se terminó el árbol de búsqueda.
Las respuestas son:
X = juan;
false
*/

/*A partir del predicado binario padre, definir en Prolog los predicados binarios: hijo, hermano y
descendiente*/
hijo(X, Y)         :- padre(Y, X).
%Debo aclarar lo que quiero que sea distinto diciendo que no unifica \=
%hermano(X, Y)      :- padre(X, Z), padre(Y, Z). Retorna pablo es hermano de pablo
%hermano1(+X,+Y) No funciona si no viene ambos instanciados porque \= y = deben tener cosas instanciadas de ambos lados
hermano1(X, Y)      :- X \= Y, padre(Z, X), padre(Z, Y). 
%hermano2(?X, ?Y) ¿Por qué es reversible? padre solo está definido por hechos, entonces siempre se instancian sus dos argumentos. Con X e Y instanciados puedo usar \=
hermano2(X, Y)      :- padre(Z, X), padre(Z, Y), X \= Y.
descendiente(X, Y) :- padre(Y, X).
descendiente(X, Y) :- padre(Z, X), descendiente(Z, Y).

/*¿Qué consulta habría que hacer para encontrar a los nietos de juan?
RTA: Habría que llamar abuelo(juan, X)

¿Cómo se puede definir una consulta para cono cer a to dos los hermanos de pablo?
RTA: hermano2(pablo, X) o hermano2(X, pablo) 
*/
%Considerar el agregado del siguiente hecho y regla:
ancestro(X, X).
ancestro(X, Y) :- ancestro(Z, Y), padre(X, Z).
%y la base de cono cimiento del ítem anterior.
%vi i. Explicar la respuesta a la consulta ancestro(juan, X). ¿Qué sucede si se pide más de un resultado?
/* 
ancestro(juan, X) entra en la primer cláusula, unificando X:=juan.
Si se piden más respuestas sigue con la cláusula de abajo. 
llama a ancestro(Z, Y) que unifica con la primera entonces Z:=Y. 
    y sigue con padre(juan,Y) que retorna Y:=carlos.
    si se piden más respuestas hace backtracking a padre(juan,Y) que unifica con la 2da cláusula. y retorna Y:=luis
si se piden más respuestas hace backtracking a ancestro(Z,Y) que esta vez entra en la cláusula de abajo. quedando ancestro(V,Y), padre(Z,V)
    entra en la primera por lo que Y:=V, luego sigue con padre(Z,V) que unifica  

*/

%vi i i. Sugerir un solución al problema hallado en los puntos anteriores reescribiendo el programa de ancestro.
/*En esta solo busco descendientes de los hijos de X
En la otra buscaba desciendo infinitamente en arbol descendencia*/

ancestroV2(X, X).
ancestroV2(X, Y) :- padre(X, Z), ancestroV2(Z, Y).

%Ejercicio 2
vecino(X, Y, [X|[Y|Ls]]).
vecino(X, Y, [W|Ls]) :- vecino(X, Y, Ls).
natural(0).
natural(suc(X)) :- natural(X).
menorOIgual(X,X) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).

%juntar(?L1, ?L2, ?L3)
juntar([], YS, YS).
juntar([X|XS], YS, [X|Ls]) :- juntar(XS, YS, Ls).

%last(?L, ?U)
last([X],X).
last([X,Y|Ls], YS) :- last([Y|Ls], YS).

%reverse(+L, -L1)
reverse([],[]).
reverse([X|XS], YS) :- reverse(XS, YS1), append(YS1, [X], YS).

%prefijo(?P, +L)
prefijo(P, L) :- append(P,_,L).

%sufijo(?S, +L)
sufijo(S, L) :- append(_, S, L).

%sublista(?S, +L)
sublista(S, L) :- prefijo(P, L), sufijo(S, P).
%Otra versión
sublistaV2(R, L) :- sufijo(S, L), prefijo(R, S).

%pertenece(+X, +L)
pertenece(E, [X|Xs]) :- E = X.
pertenece(E, [X|Xs]) :- E \= X, pertenece(E, Xs).

%perteneceR(?X, +L)
perteneceReversible(E, [X|Xs]) :- E = X.
perteneceReversible(E, [X|Xs]) :- perteneceReversible(E, Xs).

%aplanar(+Xs, -Ys)
aplanar([], []).
aplanar(X, [X]) :- not(is_list(X)).
aplanar([X|Xs], L) :- aplanar(X, L1), aplanar(Xs, L2), append(L1,L2,L).

%intersección(+L1, +L2, -L3)
interseccion([], YS, []).
interseccion([X|XS], YS, [X|L]) :- member(X, YS), interseccion(XS, YS, L).
interseccion([X|XS], YS, L) :- not(member(X, YS)), interseccion(XS, YS, L).
%interseccion([1,2,3,4], [1,2,1,3,2,3,1,3,2,3], L). retorna muchas veces [1,2,3], pues member(1,[1,1,1,1,1]) retornar 5 veces true.
%En cambio si utilizo el pertenece(+X, +L), ese por su implementación apenas encuentra el primero retorna true y deja de buscar; por lo que ahora retorna una sola respuesta
interseccionV2([], YS, []).
interseccionV2([X|XS], YS, [X|L]) :- pertenece(X, YS), interseccionV2(XS, YS, L).
interseccionV2([X|XS], YS, L) :- not(pertenece(X, YS)), interseccionV2(XS, YS, L).

%partir(N, L, L1, L2)
partir(0, L, [], L).
partir(N, [L|LS], [L|L1], L2) :- N > 0, N2 is N-1, partir(N2, LS, L1, L2).
/*Analizamos su reversibilidad: 
    -N debe estar instanciado al llamarlo porque los operadores aritméticos solo funcionan entre expresiones aritméticas.
    -N debe estar instanciado porque se usa del lado derecho de un is, además de aplicarle otro operador aritmético
    -L debe estar instanciado porque es sobre él y N que hacemos la recursión.
    -L1 y L2 pueden venir instanciados como no, ya que son los resultados que retornaremos, o si ambos vienen instanciados verifica si cumplen. Si solo viene uno, entonces busca una instanciación del otro que satisfaga el predicado.
Luego partir(+N, +L, ?L1, ?L2) 
*/ 

%borrar(+ListaOriginal, +X, -ListaSinXs)
borrar([] ,_, []).
/*borrar([X|XS], E, L) :- X = E, borrar(XS, E, L). %USAR UNIFICACIÓN NATIVA
borrar([X|XS], E, [X|L]) :- X \= E, borrar(XS, E, L). %USAR UNIFICACIÓN NATIVA */
borrar([E|XS], E, L) :- borrar(XS, E, L). 
borrar([X|XS], E, [X|L]) :- X \= E, borrar(XS, E, L).



%sacarDuplicados(+L1, -L2) de nuevo, si usa member retorna repetidas soluciones, pero si uso pertenece no
sacarDuplicados([], []).
sacarDuplicados([X|XS], L) :- member(X, XS), borrar(XS, X, XS1), sacarDuplicados(XS1, L).
sacarDuplicados([X|XS], [X|L]) :- not(member(X, XS)), sacarDuplicados(XS, L). 

%permutacion(+L1, ?L2)
permutacion([], []).
permutacion([X|XS], L) :- var(L), permutacion(XS, L1), agregarEnTodasPartes(X, L1, L).
%permutacion(+L1, +L2) sería más eficiente si implementamos una función 'mismosElementos' / que si L1 y L2 tienen los mismos elementos y longitud, entonces una es permutación de la otra
permutacion([X|XS], L) :- nonvar(L), length([X|XS], L1), length(L, L1), mismosElementos([X|XS], L).

%mismosElementos(+L1, +L2)
mismosElementos([], []).
mismosElementos(L1, L2) :- pertenece(X, L1), pertenece(X, L2). 

%agregarEnTodasPartes(+E, +L, -LS)
agregarEnTodasPartes(E, [], [E]).
agregarEnTodasPartes(E, [X|XS], [E,X|XS]).
agregarEnTodasPartes(E, [X|XS], [X|L]) :- agregarEnTodasPartes(E, XS, L).

%reparto(+L, +N, -LListas)
reparto([], 0, []).
reparto([], N, [[]|LL]) :- N > 0, N2 is N-1, reparto([], N2, LL). %Completar con vacías
reparto([X|XS], 1, [[X|XS]]).
reparto([X|XS], N, [L1|LL]) :- N > 1, length([X|XS], Long), between(0, Long, N1), partir(N1, [X|XS], L1, L2), N2 is N-1, reparto(L2, N2, LL).

%repartoSinVacias(+L, -LListas)
repartoSinVacias([], 0, []).
repartoSinVacias([X|XS], 1, [[X|XS]]).
repartoSinVacias([X|XS], N, [L1|LL]) :- N > 1, length([X|XS], Long), between(1, Long, N1), partir(N1, [X|XS], L1, L2), N2 is N-1, repartoSinVacias(L2, N2, LL).

%partesQueSuman(+L, +S, -P)
partesQueSuman(L, S, P) :- partes(L, P), sumlist(P, S).

%partes(+L, -Partes)
partes([], []).
partes([X|XS], [X|L]) :- partes(XS, L).
partes([_|XS], L) :- partes(XS, L). 

desde(X, X).
desde(X, Y) :- N is X + 1, desde(N, Y).
% ¿Cómo deben instanciarse los parámetros para que el predicado funcione? (Es decir, para que no se cuelgue ni produzca un error). ¿Por qué?

%desde(+X, -Y) porque X se utiliza del lado derecho de un is, además de aplicarle un operador aritmético.
%Si Y viene instanciado y es igual a X retorna lo esperado porque entra de una en el caso base. Si no viene instanciado, va creciendo el X hasta que llegan a ser iguales y retorna el caso base. Ese es su funciomiento esperado. El problema surge cuando hace backtracking al pedir más soluciones y vuelve a entrar en la cláusula de abajo, haciendo recursión con desde(X+1,X) lo cual nunca entra en el caso base y sigue al infinito y más allá.

%Dar una nueva versión del predicado que funcione con la instanciación desdeReversible(+X,?Y), tal que si Y está instanciada, sea verdadero si Y es mayor o igual que X, y si no lo está genere todos los Y de X en adelante.
%desdeReversible(+X, ?Y)
desdeReversible(X, X).
desdeReversible(X, Y) :- nonvar(Y), X =< Y.
desdeReversible(X, Y) :- var(Y), N is X + 1, desde(N,Y).

%intercalar(?L1, ?L2, ?L3)
intercalar(L1, [], L1).
intercalar([], L2, L2).
intercalar([H1|T1], [H2|T2], [H1, H2|RT]) :- intercalar(T1, T2, RT).
%Con esta versión es posible obtener L3 a partir de L1 y L2, pero ¿al revés?
%Si L3 viene instanciada pero L1 y L2 no entonces las instancia con listas que tengan como primer elemento el correspondiente de L3 y lo mismo recursivamente. El tema es cuando se termina L3, para eso habría que agregar caso base intercalar([], [], []) !!No necesario porque cualquiera de los dos que tiene servirían

vacio(nil).

raiz(bin(I, V, D), V).

altura(nil, 0).
altura(bin(I, V, D), H) :- altura(I, HI), altura(D, HD), H is 1 + max(HI, HD). 

cantidadNodos(nil, 0).
cantidadNodos(bin(I, V, D), K) :- cantidadNodos(I, KI), cantidadNodos(D, KD), K is KI+KD+1. 

%inorder(+AB, -Lista)
inorder(nil, []).
inorder(bin(I, V, D), L) :- inorder(I, LI), inorder(D, LD), append(LI, [V|LD], L).

%preorder(+AB, -Lista)
preorder(nil, []).
preorder(bin(I, V, D), [V|L]) :- preorder(I, LI), preorder(D, LD), append(LI, LD, L).

%postorder(+AB, -Lista)
postorder(nil, []).
postorder(bin(I, V, D), L) :- postorder(I, LI), postorder(D, LD), append(LI, LD, LL), append(LL, [V], L).

%arbolConInorder(+Lista, -AB)
arbolConInorder([], nil).
arbolConInorder([V], bin(nil, V, nil)).
arbolConInorder([X,Y|XS], bin(I, V, D)) :- obtenerRaizYSublistas([X,Y|XS], LI, LD, V), arbolConInorder(LI, I), arbolConInorder(LD, D).  

%obtenerRaiz(+Lista, -MitadIzq, -MitadDer, -R)
obtenerRaizYSublistas(Lista, MitadIzq, MitadDer, Raiz) :- length(Lista, CantNodos), Mitad is div(CantNodos, 2), partir(Mitad, Lista, MitadIzq, L2), separarCabezaYCola(L2, Raiz, MitadDer). 

%separarCabezaYCola(+L, -X, -XS)
separarCabezaYCola([X|XS], X, XS).

%aBB(+T)
aBB(nil).
aBB(bin(nil, V, nil)).
aBB(bin(nil, V, Der)) :- Der \= nil, raiz(Der, D), V < D, aBB(Der).
aBB(bin(Izq, V, nil)) :- Izq \= nil, raiz(Izq, I), V > I, aBB(Izq).
aBB(bin(Izq, V, Der)) :- Izq \= nil, Der \= nil, raiz(Izq, I), V > I, raiz(Der, D), V < D, aBB(Izq), aBB(Der).

%aBBInsertar(+X, +T1, -T2)
aBBInsertar(X, nil, bin(nil, X, nil)).
aBBInsertar(V, bin(I, V, D), bin(I, V, D)). %Clásula que permite que sea ?T2 en vez de -T2
aBBInsertar(X, bin(I, V, D), bin(I2, V, D)) :- V > X, aBBInsertar(X, I, I2).
aBBInsertar(X, bin(I, V, D), bin(I, V, D2)) :- V =< X, aBBInsertar(X, D, D2).
%Si le agregamos un hecho que sea aBBInsertar(V, bin(I,V,D),bin(I,V,D)) y le damos un T2 instanciado nos retorna true si ese elemento X pertenece

%Generate & Test

%coprimos(-X, -Y)
coprimos(X, Y) :- desde(1, S), paresQueSuman(S, X, Y), 1 is gcd(X, Y).

%paresQueSuman(+S, -X, -Y) desde 1 en adelante. Si quiero que generen los ceros también sacar S2 y usar S directo, además de arrancar el between en 0 y no en 1.
paresQueSuman(S, X, Y) :- S2 is S-1, between(1, S2, X), Y is S-X.

%paresQueSumanAntisimetrico(+S, -X, -Y)
paresQueSumanAntisimetrico(S, X, Y) :- S2 is S-1, between(1, S2, X), Y is S-X, Y > X.

%coprimosSinRepetidos(-X, -Y)
coprimosSinRepetidos(X, Y) :- desde(1, S), paresQueSumanAntisimetrico(S, X, Y), 1 is gcd(X, Y).

%cuadradoSemiMagico(+N, -XS) debo generar lista con n listas de n elementos que sumen S
cuadradoSemiMagico(N, XS) :- desde(0, S), listasDeListasDeNElementosQueSuman(N, N, S, XS).

%listasDeListasDeNElementosQueSuman(+D, +N, +S, -L).
listasDeListasDeNElementosQueSuman(0, _ , _, []).
listasDeListasDeNElementosQueSuman(D, N, S, [M|MS]) :- D > 0, listasDeNElementosQueSuman(N, S, M), D2 is D - 1, listasDeListasDeNElementosQueSuman(D2, N, S, MS).

%listasDeNElementosQueSuman(+N, +S, -L)
listasDeNElementosQueSuman(0, 0, []). %Clave que el caso base sea este. Así sólo consideramos válidas las que sumen S y tengan N elementos.
listasDeNElementosQueSuman(N, S, [Primero|L]) :- N > 0, between(0, S, Primero), N2 is N - 1, S2 is S - Primero, listasDeNElementosQueSuman(N2, S2, L).

%cuadradoMagico(+N, -XS) es un cuadradoSemiMagico donde todas sus columnas suman S también
cuadradoMagico(N, CuadradoMagico) :- desde(0,S), listasDeListasDeNElementosQueSuman(N, N, S, CuadradoMagico), sumaColumnas(CuadradoMagico, S).

%sumaColumnas(+LL, ?S)
sumaColumnas([], _).
sumaColumnas([[]], _).
sumaColumnas([[],[]], _).
sumaColumnas([XS], S) :- todosElementosSonIgualesAS(XS, S).
sumaColumnas([XS, YS| YSS], S) :- separarCabezaYCola(XS, H1, T1), separarCabezaYCola(YS, H2, T2), S is H1 + H2, sumaColumnas([T1,T2], S), sumaColumnas(YSS, S).

%todosElementosSonIgualesAS(+L, ?S)
todosElementosSonIgualesAS([], S).
todosElementosSonIgualesAS([S|XS], S) :- todosElementosSonIgualesAS(XS, S).

%esTriangulo(+T)
esTriangulo(tri(A, B, C)) :- ladosMayoresQueCero(A, B, C), cadaLadoMenorQueSumaOtrosDos(A, B, C), cadaLadoMayorQueDiferenciaOtrosDos(A, B, C).

%ladosMayoresQueCero(+A, +B, +C)
ladosMayoresQueCero(A, B, C) :- C > 0, A > 0, B > 0.

%cadaLadoMenorQueSumaOtrosDos(+A, +B, +C)
cadaLadoMenorQueSumaOtrosDos(A, B, C) :- A < B + C, B < A + C, C < A + B.

%cadaLadoMayorQueDiferenciaOtrosDos(+A, +B, +C)
cadaLadoMayorQueDiferenciaOtrosDos(A, B, C) :- A > B - C, B > A - C, C > B - A.

%perimetro(?T, ?P)
perimetro(tri(A, B, C), P) :- ground(tri(A, B, C)), esTriangulo(tri(A, B, C)), P is A + B + C. %Sirve si +T, ?P con P sin instanciar o instanciado porque is hace unificación
perimetro(tri(A, B, C), P) :- not(ground(tri(A, B, C))), nonvar(P), triplasQueSuman(P, A, B, C), esTriangulo(tri(A, B, C)). %Sirve si -T, +P
perimetro(tri(A, B, C), P) :- not(ground(tri(A, B, C))), var(P), desde(3, P), triplasQueSuman(P, A, B, C), esTriangulo(tri(A, B, C)). %Sirve si -T, -P

%triplasQueSuman(+S, ?A, ?B, ?C)
triplasQueSuman(S, A, B, C) :- not(ground([A, B, C])), between(1, S, A), S1 is S-A, between(1, S1, B), C is S - A - B, C > 0.
triplasQueSuman(S, A, B, C) :- ground([A, B, C]), S is A + B + C.

%triangulo(-T)
triangulo(tri(A, B, C)) :- desde(3, S), triplasQueSuman(S, A, B, C), esTriangulo(tri(A, B, C)).

%Negación por falla:

%corteMasParejo(+L, -L1, -L2)
corteMasParejo(L, L1, L2) :- esUnCorteDeDiferencia(L, L1, L2, D1), not((esUnCorteDeDiferencia(L, L3, L4, D2), D2 < D1)).

%esUnCorteDeDiferencia(+L, -L1, -L2, -Diferencia)
esUnCorteDeDiferencia(L, L1, L2, Diferencia) :- append(L1, L2, L), sumlist(L1, S1), sumlist(L2, S2), abs(S1 - S2, Diferencia).

%minimoNaturalTalQue(+P(X), -X) PROBLEMA: naturalesQueSatisfacen es infinito, por lo que si lo ponemos dentro de un not va a buscar y probar con las infinitas posibles instanciaciones, en este caso de S2.
%minimoNaturalTalQue(P, N) :- naturalesQueSatisfacen(P, S1), not((naturalesQueSatisfacen(P, S2), S2 < S1)).
naturalesQueSatisfacen(P, N) :- desde(1, N), call(P, N).
mayorQue10(N) :- N > 10.

%Una vez encuentres el primero que cumple no vuelvas a seguir buscando más
minimoNaturalTalQue(P, N) :- desde(1, N), call(P, N), !.

%Ejercicio importante:
%Versión final correcta.
%proximoNumPoderoso(+X, -Y) sin el ! (cut) no termina, ya que cuando hace backtracking vuelve al generador infinito y sigue retornando todos los numeros poderosos en lugar de sólo el próximo 
proximoNumPoderoso(X, Y) :- Inicio is X+1, desde(Inicio, Y), esNumeroPoderosoV2(Y), !. 

%esNumeroPoderoso(+N) primero lo factorizamos (es importante evitar considerar al 1 y a N como factores), luego verificamos que todos ellos cumplan la propiedad
esNumeroPoderosoV2(N) :- factorizar(N, ListaFactores), verificarPoderoso(N, ListaFactores).

%verificarPoderoso(+N, +L) por cada factor primo vemos si su cuadrado también divide a N. Tiene éxito sii todos los factores primos lo cumplen
verificarPoderoso(_, []).
verificarPoderoso(N, [Factor|ListaFactores]) :- FactorCuadrado is Factor*Factor, 0 is mod(N, FactorCuadrado), verificarPoderoso(N, ListaFactores).

%factorizar(+N, -L)
factorizar(N, L) :- factoresMenoresQue(N, N, L1), soloPrimos(L1, L).

%soloPrimos(+L, -L1)
soloPrimos([],[]).
soloPrimos([X|XS], [X|L]) :- esPrimo(X), soloPrimos(XS, L).
soloPrimos([X|XS], L) :- not(esPrimo(X)), soloPrimos(XS, L).

%factoresMenoresQue(+N, +I, -L)
factoresMenoresQue(N, 1, []).
factoresMenoresQue(N, I, [I|L]) :- I > 0, 0 is mod(N, I), I2 is I-1, factoresMenoresQue(N, I2, L). 
factoresMenoresQue(N, I, L) :- I > 0, not(0 is mod(N, I)), I2 is I-1, factoresMenoresQue(N, I2, L). 


%Intentos fallidos y lo que enseñan:


%La lógica estaba mal planteada: Si pongo todo en una cláusula como conjunciones, una vez encuentre uno que cumpla va a dar true y seguir buscando otro que cumpla hasta agotar espacio de búsqueda. Si se quiere obtener todos los X tal que cumplen esa propiedad está bien, pero acá queríamos verificar que todos los elementos de la listaFactores cumplan la propiedad. Entonces no sirve encararlo así.
%Otra forma de encararlo: Si generás una lista con todos los X para los que querés que esa propiedad se cumpla en simultáneo, y por cada elemento de esa lista verificas la propiedad y haces recursión sobre la cola de la lista, entonces estarás verificando que esa propiedad se cumpla para todas las instanciaciones que satisfagan el predicado y no usás un forall
esNumeroPoderoso(N) :- factorizar(N, ListaFactores), not((member(Factor, ListaFactores), FactorCuadrado is Factor*Factor, not(member(FactorCuadrado, ListaFactores)))).

%Este otro intento fue con esta lógica: Si quiero dar true solo si todos cumplen, entonces por cada instanciación me fijo si cumple. De nuevo esto genera errores porque retorna true por cada FactorPrimo que cumpla en vez de true solo si todos lo hacen
numeroPoderoso(N) :- factores(N, FactoresPrimos), PrimoAlCuadrado is FactoresPrimos*FactoresPrimos, factores(N, PrimoAlCuadrado).

%Esto funciona bien, pero el tema es que yo no quería recibir instanciaciones sino tenerlas todas en una lista para verificar la propiedad
factores(N, Candidato) :- divisores(N, Candidato), esPrimo(Candidato).

%Si usamos between vamos a dar una solución por cada vez que se haga backtracking y se llegue a algo true. Pero necesitaba todo en una lista y no puedo usar findall 
divisores(N, M) :- between(1, N, M), 0 is mod(N, M).

esPrimo(X) :- not((Ultimo is X-1 , between(2, Ultimo, Candidato), 0 is mod(X, Candidato))).

natural(cero).
natural(suc(X)) :- natural(X).

%conjuntoDeNaturales(+Conjunto)
conjuntoDeNaturales(Conjunto) :- perteneceAlConj(Elemento, Conjunto), natural(Elemento).
%Necesita que Conjunto sea instanciado porque el perteneceAlConj(?E, +C) lo necesita para poder instanciarnos en E un elemento del conjunto y nosotro verificar si es natural (para hacerlo necesitamos el E instanciado).

%Indicar el error en la siguiente definición alternativa, justificando por qué no funciona correctamente:
conjuntoDeNaturalesMalo(X) :- not( (not(natural(E)), pertenece(E,X)) ).
%Este es el error que estaba cometiendo yo antes. not(natural) instancia en E un natural y luego se fija si ese pertenece al conjunto X. A ese predicado le aplica not, por lo que si este falla (i.e. cada vez que encuentra un natural que pertenezca al conjunto retorna false, y true con el primer natural que pertenezca al conjunto) 
%Primer error: natural de una variable instancia en ella todos los naturales, que son infinitos. Cuando llega al pertenece se está fijando si ese natural que instanció pertenece a X. En vez de fijarnos por cada uno de los infinitos naturales, si este pertenece a X conviene fijarse si cada elemento de X es natural.
%Segundo error: Cuando encuentre un natural que pertenece a X, el término dentro del not externo da true, por lo que el externo falla, y sigue buscando con el siguiente natural. Pero cuando encuentra un natural que no pertenece a X el interno falla y entonces el externo tiene éxito. Finalmente conjuntoDeNaturalesMalo tiene éxito sin no todos los naturales pertenecen a X. 

%Ejercicios integradores:

/*Queremos generar todos los árboles posibles: ? arbol(A).
A = nil ;
A = bin(nil, _G2388, nil) ;
A = bin(nil, _G2391, bin(nil, _G2398, nil)) ;
A = bin(bin(nil, _G2398, nil), _G2391, nil) ;
...
*/
%Este solo avanza hacia la izquierda (->) ya que tenemos un generador infinito a derecha de otro
arbol(nil).
arbol(bin(I, _, D)) :- arbol(I), arbol(D). 
%Necesitamos al igual que con paresQueSuman un parámetro sobre el que 

arbol2(nil).
arbol2(bin(I, _, D)) :- desde(0, CantNodos), between(0, CantNodos, NodosSubarbolIzq), NodosSubarbolDer is CantNodos - NodosSubarbolIzq, arbolesDeNNodos(NodosSubarbolIzq, I), arbolesDeNNodos(NodosSubarbolDer, D).

arbolesDeNNodos(0, nil).
arbolesDeNNodos(1, bin(nil, _, nil)).
arbolesDeNNodos(N, bin(I,_,nil)) :- N > 1, N2 is N - 1, arbolesDeNNodos(N2, I). 
arbolesDeNNodos(N, bin(nil,_,D)) :- N > 1, N2 is N - 1, arbolesDeNNodos(N2, D).

% nodosEn(?A, +L)
nodosEn(nil, L).
nodosEn(bin(I, V, D), L) :- member(V, L), nodosEn(I, L), nodosEn(D, L).

%sinRepEn(-A, +L)
sinRepEn(A, L) :- length(L, N), todosLosArbolesDeNnodos(A, N), nodosEn(A, L), sinElementosRepetidos(A).

%sinElementosRepetidos(+A)
sinElementosRepetidos(nil).
sinElementosRepetidos(bin(I, R, D)) :- noPerteneceAlArbol(R, I), noPerteneceAlArbol(R, D), sinElementosRepetidos(I), sinElementosRepetidos(D).

%noPerteneceAlArbol(+Elemento, +A)
noPerteneceAlArbol(Elemento, nil).
noPerteneceAlArbol(Elemento, bin(I, R, D)) :- Elemento \= R, noPerteneceAlArbol(Elemento, I), noPerteneceAlArbol(Elemento, D).

%todosLosArbolesDeNnodos(-A, +N) notar que debo restarle uno a N porque por el constructor bin(I, _, D) ya estoy agregando uno entonces solo necesito los de los subárboles
todosLosArbolesDeNnodos(nil, 0).
todosLosArbolesDeNnodos(bin(I , _, D), N) :- N > 0, NodosSubarboles is N - 1, between(0, NodosSubarboles, CantidadI), CantidadD is NodosSubarboles - CantidadI, arbolesDeNNodos(CantidadI, I), arbolesDeNNodos(CantidadD, D).

