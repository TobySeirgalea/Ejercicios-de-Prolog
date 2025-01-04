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
%palindromo(+L, ?L1) %TIP no querés la mitad sino toda la palabra y después al revés 123321
palindromo([],[]). 
palindromo(XS, P) :- reverse(XS, L), append(XS, L, P). 

%Consulta: Por qué sin la primera cláusula no funciona. No debería bastar con la segunda para el caso de un elemento?.
iesimo([X], 0, X).
iesimo([X|_], 0, X). 
iesimo([X|XS], I, N) :- I >= 1, N2 is I-1, iesimo(XS, N2, N).
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
paresQueSuman(0,0,0).
paresQueSuman(S, X, Y) :- S2 is S-1, between(1, S2, X), Y is S-X.

paresQueSumanDesdeCero(S, X, Y) :- between(0, S, X), Y is S-X.



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
%Sin usar cut
minimoNaturalTalQue(P, X) :- call(P,X), X >= 0, not((call(P,Y), Y < X)).
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

esPrimo(X) :- X > 1, not((Ultimo is X-1 , between(2, Ultimo, Candidato), 0 is mod(X, Candidato))).

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

parPositivo(X,Y) :- mayor(X, 0), mayor(Y, 0).
natural(0).
natural(succ(N)) :- natural(N).
mayor(succ(X),0) :- natural(X).
mayor(succ(X),succ(Y)) :- mayor(X,Y).

natural(0).
natural(suc(X)) :- natural(X).
mayor(suc(X),X).
mayor(suc(X),Y) :- mayor(X,Y).
parDeNat(X,Y) :- natural(X), natural(Y).

%sublistaMasLargaDePrimos(+L, ?S)
sublistaMasLargaDePrimos(Lista, Sublista) :- sublistaDePrimos(Lista, Sublista), not((sublistaDePrimos(Lista, OtraSublista), deLongitudMayor(OtraSublista, Sublista))).

%deLongirudMayor(+XS, +YS)
deLongitudMayor(XS, YS) :- length(XS, LongXS), length(YS, LongYS), LongXS > LongYS.

%sublistaDePrimos(+L, ?S)
sublistaDePrimos(Lista, Sublista) :- sublista(Sublista, Lista), todosPrimos(Sublista).

%todosPrimos(+XS)
todosPrimos([]).
todosPrimos([X|XS]) :- esPrimo(X) , todosPrimos(XS).

%listasQueSuman(+S, -L) tambien se banca +L
listasQueSuman(0, []).
listasQueSuman(S, [X|XS]) :- S > 0, between(1, S, X), S2 is S - X, listasQueSuman(S2, XS).

% generarTodasLasListasDeNaturasles(-L)
generarTodasLasListasDeNaturasles(Lista) :- desde(1, S), listasQueSuman(S, Lista).

% esCapicua(+L)
esCapicua(L) :- reverse(L,L).

% generarCapicuas(-Lista)
generarCapicuas(Lista) :- generarTodasLasListasDeNaturasles(Lista), esCapicua(Lista).

% palabra(+A, +N, -P) que genere todas las palabras del alfabeto A de N letras
palabra(Alfabeto, 0, []).                            %Clave no olvidar el Longitud > 0 sino se cuelga
palabra(Alfabeto, Longitud, [Letra|RestoPalabra]) :- Longitud > 0, member(Letra, Alfabeto), LongitudCola is Longitud - 1, palabra(Alfabeto, LongitudCola, RestoPalabra).

% frase(+Alfabeto, -Frase) ASI NO TERMINA NUNCA
% frase(Alfabeto, [PrimerFrase|Frase]) :- desde(1, N), palabra(Alfabeto, N, PrimerFrase), frase(Alfabeto, Frase).
% frasesDeLongitud(+Alfabeto, +Longitud, -Frase)
frasesDeLongitudYkLetras(Alfabeto, 0, _, []).
frasesDeLongitudYkLetras(Alfabeto, CantPalabras, K,[PrimerPalabra|RestoFrase]) :- CantPalabras > 0, between(1, K, Long),
                                                                                  palabra(Alfabeto, Long, PrimerPalabra),
                                                                                  Cant2 is CantPalabras - 1,
                                                                                  frasesDeLongitudYkLetras(Alfabeto, Cant2, K, RestoFrase).                                                                        
%frase(+Alfabeto, -Frase)                                                                        
frase(Alfabeto, Frase) :- desde(1, S), paresQueSuman(S, CantPalabras, LongPalabras), frasesDeLongitudYkLetras(Alfabeto, CantPalabras, LongPalabras, Frase).

% simbolo(?S)
simbolo(S) :- member(S, Alfabeto).

% clausura(-L)
clausura(L) :- desde(0, Longitud), palabra([a,b], Longitud, L).

% ochoReinas(?XS)
ochoReinas(Tablero) :- var(Tablero), length(Tablero, 8), llenarTableros(Tablero), listaSinRepetidos(Tablero), nadieEnMismaDiagonal(Tablero, 1).
ochoReinas(XS) :- nonvar(XS), listaSinRepetidos(XS), nadieEnMismaDiagonal(XS, 1).

% llenarTableros(-Tablero)
llenarTableros([]).
llenarTableros([Celda|Tablero]) :- member(Celda, [1,2,3,4,5,6,7,8]), llenarTableros(Tablero).

% listaSinRepetidos(+XS)
listaSinRepetidos([]).
listaSinRepetidos([X|XS]) :- not(memberchk(X, XS)), listaSinRepetidos(XS).

nadieEnMismaDiagonal([X|XS], Index) :- 
    between(-8, 8, Incremento), 
    FilaDiagonal is X + Incremento,
    ColumnaDiagonal is Index + Incremento,
    FilaDiagonal > 0, ColumnaDiagonal > 0,
    not(nth1(ColumnaDiagonal, XS, FilaDiagonal)), 
    Siguiente is Index + 1, 
    nadieEnMismaDiagonal(XS, Siguiente).
%Las listas empiezan en index 1 e.g.  nth1(1,[1],X) instancia X = 1.
%nth1(?Index, ?List, ?Elem) Is true when Elem is the Index’th element of List. Counting starts at 1.

% masRepetido(+L, -X)
masRepetido(L, X) :- member(X, L), cantidadRepeticiones(X, L, C1), not((member(Y, L), Y \= X, cantidadRepeticiones(Y, L, C2), C2 > C1)).

% cantidadRepeticiones(+E, +L, -C)
cantidadRepeticiones(_, [], 0).
cantidadRepeticiones(E, [X|XS], C) :- E\= X, cantidadRepeticiones(E, XS, C).
cantidadRepeticiones(E, [E|XS], C) :- cantidadRepeticiones(E, XS, C2), C is C2 + 1.


% partesQueSuman(+L, +S, -P)
partesQueSuman(Lista, Suma, Partes) :- partes(Lista, Partes), listasQueSuman(Partes, Suma).

% montaña(+L, -L1, -C, -L2)
montana([], [], _, []).
montana([X,Y|YS], [X|L1], C, L2) :- X < Y, montana([Y|YS], L1, C, L2).
montana([X,Y|YS], L1, X, [Y|L2]) :- X > Y, montana(YS, L1, C, L2).

% todasLasListas(+A, -L)
todasLasListasDelAlfabeto(A, L) :- desde(1, N), palabra(A, N, L).


% caminoRoseTree(+R, -C)
caminoRoseTree((N, []), [N]).
caminoRoseTree((N, Hijos), [N|CaminoHijos]) :- member((Hijo, ListaHijos), Hijos), caminoRoseTree((Hijo, ListaHijos), CaminoHijos). 


% caminoDeMayorValor(+R, -C)
caminoDeMayorValor(RoseTree, Camino) :- caminoRoseTree(RoseTree, Camino),
                                        sumlist(Camino, Suma1),
                                        not((caminoRoseTree(RoseTree, OtroCamino),
                                            OtroCamino \= Camino,
                                            sumlist(OtroCamino, Suma2),
                                            Suma2 > Suma1)).
% sumlist(+List, -Sum) True when Sum is the list of all numbers in List.
% length(?List, ?Length) 
% nth1(?Index, ?List, ?Elem) Is true when Elem is the Index’th element of List. Counting starts at 1.
% member(?Elem, ?List)
% append(?List1, ?List2, ?List1AndList2) List1AndList2 is the concatenation of List1 and List2
% last(?List, ?Last)
% between(+Low, +High, ?Value)
% union(+Set1, +Set2, -Set3)
% intersection(+Set1, +Set2, -Set3)
% subset(+SubSet, +Set)
% subtract(+Set, +Delete, -Result)
% select(?Elem, ?List1, ?List2)
% reverse(?List1, ?List2)

/* Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.

    Example:
    ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
    X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
*/

/*Idea: Recorro la lista, tomo un elemento, llamo a predicado que me recolecta todos los repetidos consecutivos y me retorna lista sin estos, hago recursión sobre esta última*/

pack([], []).
pack([X|XS], [Consecutivos|DemasConsecutivos]) :- agregarConsecutivosIguales([X|XS], X, Consecutivos, Resto), pack(Resto, DemasConsecutivos).


% agregarConsecutivosIguales(+XS, +UltimoAgregado, -Res, Resto)
agregarConsecutivosIguales([], _, [], []).
% Caso último agregado es igual al actual
agregarConsecutivosIguales([X|XS], X, [X|Res], Resto ) :- agregarConsecutivosIguales(XS, X, Res, Resto).
% Caso último agregado es distinto al actual
agregarConsecutivosIguales([X|XS], Y, [], [X|XS]) :- X \= Y.

sinRepetidosConsecutivos([X], [X]).
% si son distintos agregalo a lista y hace recursion con el siguiente y resto de la lista
sinRepetidosConsecutivos([Y, X|XS], [Y|Res]) :- X \= Y, sinRepetidosConsecutivos([X|XS], Res).
% si son iguales hace recursion con el resto y no agregues este repetido
sinRepetidosConsecutivos([X,X|XS], Res) :- sinRepetidosConsecutivos([X|XS], Res).

/*    Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as terms [N,E] where N is the number of duplicates of the element E.

    Example:
    ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
    X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]
*/
encode(XS, Res) :- pack(XS, Res1), contarApariciones(Res1, Res).

contarApariciones([], []).
contarApariciones([[Elemento|YS]|XS], [[Apariciones, Elemento]|Resto]) :- length([Elemento|YS], Apariciones), contarApariciones(XS, Resto).

/* Decode a run-length encoded list.
    Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.*/
decodificar([], []).
decodificar([[Apariciones, Elemento]|XS], [Rep|Rec]) :- repetir(Apariciones, Elemento, Rep), decodificar(XS, Rec).

repetir(0, _, []).
repetir(Cantidad, Elemento, [Elemento|Res]) :- Cantidad \= 0, Cantidad2 is Cantidad-1, repetir(Cantidad2, Elemento, Res).
/*Duplicate the elements of a list a given number of times.
    Example:
    ?- dupli([a,b,c],3,X).
    X = [a,a,a,b,b,b,c,c,c]*/
dupli([], _, []).
dupli([X|XS], N, Res) :- repetir(N, X, Rep), dupli(XS, N, Rep1), append(Rep, Rep1, Res).
/*Drop every N'th element from a list.
    Example:
    ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
    X = [a,b,d,e,g,h,k]*/
/*Uso auxiliar que recorra N lugares y al N-esimo lo saque y también de resto lista sobre que hará recursión drop*/
drop([], _, []).
drop([X|XS], N, Res) :- sacarNesimo([X|XS], N, PrimerosSinNesimo, SiguientesAlNesimo), drop(SiguientesAlNesimo, N, ResultadoRecursivo), append(PrimerosSinNesimo, ResultadoRecursivo, Res).

sacarNesimo([], _, [], []).
sacarNesimo([X|XS], 0, [], XS).
sacarNesimo([X|XS], N, [X|R], RS) :- N > 0, N2 is N-1, sacarNesimo(XS, N2, R, RS).

/* Split a list into two parts; the length of the first part is given.
    Do not use any predefined predicates.

    Example:
    ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
    L1 = [a,b,c]
    L2 = [d,e,f,g,h,i,k]*/
split(XS, N, L1, L2) :- append(L1, L2, XS), length(L1, N).
/*Extract a slice from a list.
    Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.

    Example:
    ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
    X = [c,d,e,f,g]*/
slice([], _, _, []).
slice([X|XS], Init, Finish, Res) :- Init > 0, Finish >= Init, Init2 is Init-1, Finish2 is Finish - 1, slice(XS, Init2, Finish2, Res).
slice([X|XS], 1, Finish, [X|Res]) :- Finish > 0, Finish2 is Finish - 1, slice(XS, 1, Finish2, Res).
slice([X|XS], 1, 0, []).

/*(**) Rotate a list N places to the left.
    Examples:
    ?- rotate([a,b,c,d,e,f,g,h],3,X).
    X = [d,e,f,g,h,a,b,c]

    ?- rotate([a,b,c,d,e,f,g,h],-2,X).
    X = [g,h,a,b,c,d,e,f]

    Hint: Use the predefined predicates length/2 and append/3, as well as the result of problem P17.*/
rotate([], _, []).
rotate([X|XS], N, L) :- N >= 0, split([X|XS], N, L1, L2), append(L2, L1, L).
rotate([X|XS], N, L) :- N < 0, length([X|XS], Longitud), N2 is Longitud + N, split([X|XS], N2, L1, L2), append(L2, L1, L).

/**) Remove the K'th element from a list.
    Example:
    ?- remove_at(X,[a,b,c,d],2,R).
    X = b
    R = [a,c,d]*/
remove_at(X, [X|XS], 1, XS).
remove_at(X, [Y|YS], N, [Y|Resto]) :- N > 0, N2 is N - 1, remove_at(X, YS, N2, Resto).
/*(*) Insert an element at a given position into a list.
    Example:
    ?- insert_at(alfa,[a,b,c,d],2,L).
    L = [a,alfa,b,c,d]*/
insert_at(X, [Y|YS], N, [Y|L]) :- N > 1, N2 is N-1, insert_at(X, YS, N2, L).
insert_at(X, [Y|YS], 1, [X, Y|YS]).
/*(*) Create a list containing all integers within a given range.
    Example:
    ?- range(4,9,L).
    L = [4,5,6,7,8,9]*/
range(Init, End, [Init|Res]) :- Init =< End, Init2 is Init + 1, range(Init2, End, Res).
range(Init, End, []) :- Init > End.

/*Construct completely balanced binary trees
    In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.

    Write a predicate cbal_tree/2 to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.
    Example:
    ?- cbal_tree(4,T).
    T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
    T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
    etc......No*/

paresConDiferenciaMaximaDe(X, Y, Z, LimiteSuperior) :- paresQueSumanDesdeCero(LimiteSuperior, X, Y), Z >= abs(X-Y).

cbal_tree(0, nil).
cbal_tree(N, t(x, Izq, Der)) :- N > 0, N2 is N-1, paresConDiferenciaMaximaDe(X, Y, 1, N2), cbal_tree(X, Izq), cbal_tree(Y, Der). 


