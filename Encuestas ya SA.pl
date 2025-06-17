% -----------------------------------------PRODUCTOS-------------------------------------

producto(ps5).
producto(telefono_a_disco).
producto(iphone_13).
producto(radio).
producto(destornillador).

% -----------------------------------------PERSONAS-------------------------------------

% personas(ID, edad, genero)
persona(1,20-30,masculino).
persona(2,20-30,masculino).
persona(3,50-60,masculino).
persona(4,20-30,masculino).
persona(5,50-60,masculino).
persona(6,20-30,femenino).
persona(7,30-40,femenino).
persona(8,40-50,femenino).
persona(9,20-30,femenino).
persona(10,30-40,femenino).
persona(11,40-50,femenino).
persona(12,50-60,masculino).
persona(13,10-20,femenino).
persona(14,10-20,femenino).
persona(15,30-40,masculino).
persona(16,50-60,femenino).
persona(17,40-50,masculino).
persona(18,30-40,femenino).
persona(19,20-30,masculino).
persona(20,10-20,femenino).
persona(21,50-60,masculino).

% -----------------------------------------ENCUESTAS-------------------------------------

% encuesta(IDPersona, Producto, Aceptacion, Motivo, Precio)

% p1 masculino
encuesta(1,ps5,si,graficos,1000000).
encuesta(1,telefono_a_disco,no,anticuado,0).
encuesta(1,iphone_13,si,fluidez,700000).
encuesta(1,radio,no,inutil,3000).
encuesta(1,destornillador,si,util,5000).

% p2 masculino
encuesta(2,ps5,si,entretenimiento,800000).
encuesta(2,telefono_a_disco,no,incomodo,0).
encuesta(2,iphone_13,si,camara,600000).
encuesta(2,radio,si,costumbre,2000).
encuesta(2,destornillador,si,util,6000).

% p3 masculino
encuesta(3,ps5,no,complejo,0).
encuesta(3,telefono_a_disco,si,costumbre,1000).
encuesta(3,iphone_13,no,caro,0).
encuesta(3,radio,si,facil_uso,3000).
encuesta(3,destornillador,si,util,5000).

% p4 masculino
encuesta(4,ps5,no,caro,0).
encuesta(4,telefono_a_disco,no,lento,0).
encuesta(4,iphone_13,si,camara,720000).
encuesta(4,radio,no,anticuado,1000).
encuesta(4,destornillador,si,durabilidad,6000).

% p5 masculino
encuesta(5,ps5,no,complejo,0).
encuesta(5,telefono_a_disco,si,conocido,2000).
encuesta(5,iphone_13,no,innecesario,0).
encuesta(5,radio,si,costumbre,2000).
encuesta(5,destornillador,si,util,4500).

% p6 femenino
encuesta(6,ps5,si,diversion,1000000).
encuesta(6,telefono_a_disco,no,viejo,0).
encuesta(6,iphone_13,si,estilo,700000).
encuesta(6,radio,no,anticuado,0).
encuesta(6,destornillador,si,practico,7000).

% p7 femenino
encuesta(7,ps5,si,juegos,850000).
encuesta(7,telefono_a_disco,no,desactualizado,0).
encuesta(7,iphone_13,si,fluidez,680000).
encuesta(7,radio,si,nostalgia,2000).
encuesta(7,destornillador,si,util,5000).

% p8 femenino
encuesta(8,ps5,no,complicado,0).
encuesta(8,telefono_a_disco,si,facil_uso,1500).
encuesta(8,iphone_13,no,caro,0).
encuesta(8,radio,si,costumbre,2500).
encuesta(8,destornillador,si,util,4000).

% p9  femenino
encuesta(9,ps5,si,graficos,950000).
encuesta(9,telefono_a_disco,no,viejo,0).
encuesta(9,iphone_13,si,estilo,750000).
encuesta(9,radio,no,aburrido,0).
encuesta(9,destornillador,si,practico,6000).

% p10 femenino
encuesta(10,ps5,si,entretenimiento,870000).
encuesta(10,telefono_a_disco,no,obsoleto,0).
encuesta(10,iphone_13,si,camara,690000).
encuesta(10,radio,si,informacion,1500).
encuesta(10,destornillador,si,util,5500).

% p11 femenino
encuesta(11,ps5,no,complicado,0).
encuesta(11,telefono_a_disco,no,obsoleto,0).
encuesta(11,iphone_13,si,camara,790000).
encuesta(11,radio,no,aburrido,0).
encuesta(11,destornillador,si,util,3500).

% p12 masculino
encuesta(12,ps5,no,complicado,0).
encuesta(12,telefono_a_disco,si,facil_uso,2000).
encuesta(12,iphone_13,no,caro,0).
encuesta(12,radio,si,informacion,0).
encuesta(12,destornillador,si,util,3700).

% p13 femenino
encuesta(13,ps5,no,complicado,0).
encuesta(13,telefono_a_disco,no,viejo,0).
encuesta(13,iphone_13,si,camara,840000).
encuesta(13,radio,no,aburrido,0).
encuesta(13,destornillador,no,aburrido,0).

% p14 femenino
encuesta(14,ps5,si,graficos,700000).
encuesta(14,telefono_a_disco,si,facil_uso,2000).
encuesta(14,iphone_13,si,camara,800000).
encuesta(14,radio,no,aburrido,0).
encuesta(14,destornillador,no,aburrido,0).

/*
% p15 masculino (30-40)
encuesta(15,ps5,si,entretenimiento,870000).
encuesta(15,telefono_a_disco,no,obsoleto,0).
encuesta(15,iphone_13,si,camara,750000).
encuesta(15,radio,no,anticuado,0).
encuesta(15,destornillador,si,util,5500).

% p16 femenino (50-60)
encuesta(16,ps5,no,complejo,0).
encuesta(16,telefono_a_disco,si,nostalgia,1000).
encuesta(16,iphone_13,no,caro,0).
encuesta(16,radio,si,costumbre,1500).
encuesta(16,destornillador,si,util,4000).

% p17 masculino (40-50)
encuesta(17,ps5,si,juegos,900000).
encuesta(17,telefono_a_disco,no,incomodo,0).
encuesta(17,iphone_13,si,fluidez,680000).
encuesta(17,radio,si,informacion,1200).
encuesta(17,destornillador,si,util,5000).

% p18 femenino (30-40)
encuesta(18,ps5,si,graficos,950000).
encuesta(18,telefono_a_disco,no,viejo,0).
encuesta(18,iphone_13,si,estilo,700000).
encuesta(18,radio,no,anticuado,0).
encuesta(18,destornillador,si,practico,6000).

% p19 masculino (20-30)
encuesta(19,ps5,si,juegos,880000).
encuesta(19,telefono_a_disco,no,lento,0).
encuesta(19,iphone_13,si,camara,740000).
encuesta(19,radio,no,anticuado,0).
encuesta(19,destornillador,si,util,5800).

% p20 femenino (10-20)
encuesta(20,ps5,si,graficos,720000).
encuesta(20,telefono_a_disco,no,viejo,0).
encuesta(20,iphone_13,si,fluidez,780000).
encuesta(20,radio,no,aburrido,0).
encuesta(20,destornillador,no,innecesario,0).


% p21 masculino (50-60)
encuesta(21,ps5,no,complejo,0).
encuesta(21,telefono_a_disco,si,costumbre,1000).
encuesta(21,iphone_13,no,caro,0).
encuesta(21,radio,si,costumbre,1500).
encuesta(21,destornillador,si,util,4300).
*/
% -----------------------------------------CONSULTAS--------------------------------------

% ----------------------------------PUNTO 1--------------------------------
% Producto con mas aceptacion.

                %consultas
                %cuantas veces un producto fue aceptado
cuenta_cantidad_de_aceptacion(Producto, Cantidad):-
    findall(Producto, encuesta(_,Producto,si,_,_), Lista),
     length(Lista,Cantidad).

                %trae todos los productos
todos_los_productos(Productos):-
    findall(P,producto(P), Productos).

                %retorna la aceptacion de cada producto
cantidades_de_aceptacion([],[]).
cantidades_de_aceptacion([P|Ps], [(P,Cant)|Resto]):-  
    cuenta_cantidad_de_aceptacion(P,Cant), 
    cantidades_de_aceptacion(Ps,Resto).   

                %devuelve y compara el producto mas aceptado
max_aceptacion([(P, C)], P, C).
max_aceptacion([(P1, C1)|Resto], MaxP, MaxC) :-
    max_aceptacion(Resto, P2, C2),
    (C1 >= C2 -> MaxP = P1, MaxC = C1 ; MaxP = P2, MaxC = C2).

                %producto mas aceptado
producto_mas_aceptado_funcion(Producto):-
todos_los_productos(ListaProductos),
    cantidades_de_aceptacion(ListaProductos, Pares),
    max_aceptacion(Pares,Producto,_).

                %imprime producto mas aceptado         
producto_mas_aceptado:-
     producto_mas_aceptado_funcion(P),
     write("el producto mas aceptado es: "), write(P), nl.

% CONSULTAS
% producto_mas_aceptado.

% ----------------------------------PUNTO 2--------------------------------
% Producto con menos aceptacion.


                %devuelve y compara el producto menos aceptado
min_aceptacion([(P, C)], P, C).
min_aceptacion([(P1,C1) | Resto], MinP, MinC):-
    min_aceptacion(Resto, P2, C2),
    (C1 =< C2 -> MinP = P1, MinC = C1 ; MinP = P2, MinC = C2).


                %producto menos aceptado
producto_menos_aceptado_funcion(Producto):-
    todos_los_productos(ListaProductos),
    cantidades_de_aceptacion(ListaProductos,Pares),
    min_aceptacion(Pares,Producto,_).

                %imprimir producto menos aceptado        
producto_menos_aceptado:-
     producto_menos_aceptado_funcion(P),
     write("el producto menos aceptado es: "), write(P), nl.


% CONSULTAS
% producto_menos_aceptado.

% ----------------------------------PUNTO 3--------------------------------
% Distintos tipos de listados (por ejemplo: listado de productos, encuestas por producto, etc.)

% --------------PUNTO 3.1-------------
%Listado de productos

listar_productos :-
    producto(P),
    writeln(P),
    fail.

% CONSULTAS
% listar_productos.

% --------------PUNTO 3.2-------------
%Encuestas por producto

encuestas_por_producto(Producto) :-
    producto(Producto),
    writeln('---'),
    format('Encuestas para ~w:~n', [Producto]),
    encuesta(ID, Producto, Acepta, Motivo, Precio),
    format('Persona ~w - Acepta: ~w - Motivo: ~w - Precio: ~w~n', [ID, Acepta, Motivo, Precio]),
    fail.

% CONSULTAS
% encuestas_por_producto(_).
% encuestas_por_producto(ps5).
% encuestas_por_producto(iphone_13).

% --------------PUNTO 3.3-------------
% Encuestas realizadas por una persona

encuestas_de_persona(IDPersona) :-
    persona(IDPersona, Edad, Genero),
    format('Encuestas de Persona ~w (Edad: ~w, Género: ~w):~n', [IDPersona, Edad, Genero]),
    encuesta(IDPersona, Producto, Acepta, Motivo, Precio),
    format('Producto: ~w - Acepta: ~w - Motivo: ~w - Precio: $~w~n', [Producto, Acepta, Motivo, Precio]),
    fail.

% CONSULTAS
% encuestas_de_persona(_).
% encuestas_de_persona(3).
% encuestas_de_persona(6).

% --------------PUNTO 3.4-------------
%Listado general de todas las encuestas

listar_todas_las_encuestas :-
    writeln('Listado completo de encuestas:'),
    encuesta(ID, Producto, Acepta, Motivo, Precio),
    format('Persona: ~w - Producto: ~w - Acepta: ~w - Motivo: ~w - Precio: $~w~n',
           [ID, Producto, Acepta, Motivo, Precio]),
    fail.
listar_todas_las_encuestas.

% --------------PUNTO 3.5-------------
%Listado de personas encuestadas

%Mostrar todas las personas que participaron en las encuestas
listar_personas :-
    writeln('Listado de personas encuestadas:'),
    persona(ID, Edad, Genero),
    format('ID: ~w - Edad: ~w - Género: ~w~n', [ID, Edad, Genero]),
    fail.

% CONSULTAS
% listar_personas.

% ----------------------------------PUNTO 4--------------------------------
%Cual es el rango de edad y genero que mas acepta cada producto?

                   %criterios de aceptacion para rango de edad, genero y aceptacion
criterios_de_aceptacion_positivo_genero(Producto, Rango ,Genero):-
    encuesta(ID,Producto,si,_,_),
    persona(ID,Rango,Genero).

                    %cuenta de criterios de aceptacion para rango de edad, genero y aceptacion
cuenta_aceptacion_positivos_genero(Producto, Rango, Genero, Cant):-
    findall(1,criterios_de_aceptacion_positivo_genero(Producto, Rango, Genero), Lista),
    length(Lista,Cant).

                            %busqueda principal aceptacion
encontrar_mejor_grupo_aceptacion_genero(Producto, MejorRango, MejorGenero, MaxCant):-
    findall((Rango, Genero), criterios_de_aceptacion_positivo_genero(Producto, Rango, Genero), Lista),
    setof((R, G), member((R, G), Lista), CombinacionesUnicas),
    findall((Cant, R, G),
        (member((R, G), CombinacionesUnicas), cuenta_aceptacion_positivos_genero(Producto, R, G, Cant)),
        Conteos),
    max_member((MaxCant, MejorRango, MejorGenero), Conteos).


                            %imprimir productos mas aceptados por genero, edad y aceptacion  
mostrar_mejores_grupos_aceptacion_genero_edad:-
    todos_los_productos(Productos),
    forall(member(P, Productos),
        (
            encontrar_mejor_grupo_aceptacion_genero(P, Rango, Genero, Cant),
            format("Producto: ~w - Grupo con mas aceptaciones: Edad ~w, Genero ~w, Cantidad: ~w~n", [P, Rango, Genero, Cant])
        )).

%CONSULTAS
% mostrar_mejores_grupos_aceptacion_genero_edad.

% ----------------------------------PUNTO 5--------------------------------
%Cual es el rango de edad y genero que menos acepta cada producto?


                    %criterios de no aceptacion para rango de edad, genero y aceptacion
criterios_de_aceptacion_negativo_genero(Producto, Rango ,Genero):-
    encuesta(ID,Producto,no,_,_),
    persona(ID,Rango,Genero).

                    %cuenta de criterios de no aceptacion para rango de edad, genero y aceptacion
cuenta_aceptacion_negativos_genero(Producto, Rango, Genero, Cant):-
    findall(1,criterios_de_aceptacion_negativo_genero(Producto, Rango, Genero), Lista),
    length(Lista,Cant).


                           %busqueda principal negativa
encontrar_mejor_grupo_no_aceptacion_genero(Producto, MejorRango, MejorGenero, MaxCant):-
    findall((Rango, Genero), criterios_de_aceptacion_negativo_genero(Producto, Rango, Genero), Lista),
    setof((R, G), member((R, G), Lista), CombinacionesUnicas),
    findall((Cant, R, G),
        (member((R, G), CombinacionesUnicas), cuenta_aceptacion_negativos_genero(Producto, R, G, Cant)),
        Conteos),
    max_member((MaxCant, MejorRango, MejorGenero), Conteos).

                            %imprimir productos menos aceptados por genero, edad y aceptacion   
mostrar_mejores_grupos_no_aceptacion_genero_edad:-
    todos_los_productos(Productos),
    forall(member(P, Productos),
        (
            encontrar_mejor_grupo_no_aceptacion_genero(P, Rango, Genero, Cant),
            format("Producto: ~w - Grupo con menos aceptaciones: Edad ~w, Genero ~w, Cantidad: ~w~n", [P, Rango, Genero, Cant])
        )).

%CONSULTAS
% mostrar_mejores_grupos_no_aceptacion_genero_edad.

% ----------------------------------PUNTO 6--------------------------------
%Cantidad de encuestados.

                  %cantidad de encuestados
cantidad_personas_encuestadas(Cant):-
    findall(ID, encuesta(ID, _, _, _, _), Lista),
    sort(Lista, SinRepetidos),
    length(SinRepetidos, Cant).

                    %mostrar cantidad de encuestados       
personas_encuestadas:-
    cantidad_personas_encuestadas(Cant),
    write("cantidad de personas encuestadas:"), write(Cant),nl.

% CONSULTAS
% personas_encuestadas.

% ----------------------------------PUNTO 7--------------------------------
%Cantidad de encuestas de aceptacion, en general.

                    %cantidad de encuestas con aceptacion general

cantidad_de_encuestas_con_aceptacion(Cantidad):-
    findall(encuesta,encuesta(_,_,si,_,_), Lista),
    length(Lista,Cantidad).

                    %imprimir encuestas de aceptacion           

encuestas_aceptacion_general:-
       cantidad_de_encuestas_con_aceptacion(Cantidad),
       write("cantidad general de encuestas con aceptacion: "),write(Cantidad), nl.

% CONSULTAS
% encuestas_aceptacion_general.

% ----------------------------------PUNTO 8--------------------------------
% Cantidad de encuestas de no aceptacion, en general.

                    %cantidad de encuestas con aceptacion negativa general
cantidad_de_encuestas_con_aceptacion_negativa(Cantidad):-
    findall(encuesta,encuesta(_,_,no,_,_), Lista),
    length(Lista,Cantidad).


                    %imprimir encuestas de aceptacion negativas           IMPORTANTE

encuestas_sin_aceptacion_general:-
       cantidad_de_encuestas_con_aceptacion_negativa(Cantidad),
       write("cantidad general de encuestas con aceptacion negativa: "),write(Cantidad), nl.

% CONSULTAS
% encuestas_sin_aceptacion_general

% ----------------------------------PUNTO 9--------------------------------
% Razon principal de aceptacion (la mas mencionada) de cada producto.

% Obtener la razon de aceptacion mas mencionada para un producto dado
razon_principal_aceptacion(Producto, MotivoPrincipal) :-
    findall(Motivo, encuesta(_, Producto, si, Motivo, _), Motivos),
    motivo_mas_frecuente(Motivos, MotivoPrincipal).

% Contar ocurrencias de cada motivo y devolver el mas frecuente
motivo_mas_frecuente(Lista, Motivo) :-
    sort(Lista, Unicos),
    contar_motivos(Lista, Unicos, Pares),
    maximo_motivo(Pares, Motivo).

% Contar cuantas veces aparece cada motivo
contar_motivos(_, [], []).
contar_motivos(Lista, [H|T], [(H, C)|Resto]) :-
    incluir(H, Lista, C),
    contar_motivos(Lista, T, Resto).

% Contar cuantas veces un elemento aparece en una lista
incluir(_, [], 0).
incluir(X, [X|T], N) :- incluir(X, T, N1), N is N1 + 1.
incluir(X, [Y|T], N) :- X \= Y, incluir(X, T, N).

% Obtener el motivo con mayor cantidad
maximo_motivo([(Motivo, Cant)|T], MaxMotivo) :-
    maximo_motivo_aux(T, (Motivo, Cant), MaxMotivo).

maximo_motivo_aux([], (Motivo, _), Motivo).
maximo_motivo_aux([(M,C)|T], (_, CMax), Resultado) :-
    C > CMax,
    maximo_motivo_aux(T, (M, C), Resultado).
maximo_motivo_aux([(_,C)|T], (MMax, CMax), Resultado) :-
    C =< CMax,
    maximo_motivo_aux(T, (MMax, CMax), Resultado).

% CONSULTAS
% razon_principal_aceptacion(ps5, Motivo).
% producto(P), razon_principal_aceptacion(P, Motivo), format('Producto: ~w - Motivo principal de aceptacion: ~w~n', [P, Motivo]), fail.

% ----------------------------------PUNTO 10--------------------------------
% Razon principal de no aceptacion (la mas mencionada) de cada producto.

% Obtener la razon de no aceptacion mas mencionada para un producto dado
razon_principal_no_aceptacion(Producto, MotivoPrincipal) :-
    findall(Motivo, encuesta(_, Producto, no, Motivo, _), Motivos),
    motivo_mas_frecuente(Motivos, MotivoPrincipal).

% CONSULTAS
% razon_principal_no_aceptacion(ps5, Motivo).
% producto(P), razon_principal_no_aceptacion(P, Motivo), format('Producto: ~w - Motivo principal de no aceptacion: ~w~n', [P, Motivo]), fail.

% ----------------------------------PUNTO 11--------------------------------
% Cuanto estaran dispuestos a pagar los encuestados que aceptan cada producto?

% Calcular el promedio de precios para un producto aceptado
promedio_precio_aceptacion(Producto, Promedio) :-
    findall(Precio, encuesta(_, Producto, si, _, Precio), Precios),
    calcular_promedio(Precios, Promedio).

% Sumar y promediar
calcular_promedio(Lista, Promedio) :-
    sum_list(Lista, Suma),
    length(Lista, Cant),
    Cant > 0,
    Promedio is Suma / Cant.

% CONSULTAS
% promedio_precio_aceptacion(ps5, Promedio).
% producto(P), promedio_precio_aceptacion(P, Prom), format('Producto: ~w - Promedio de precio aceptado: $~2f~n', [P, Prom]), fail.

% ----------------------------------PUNTO 12--------------------------------
% Realizar todas las consultas extras que usted interprete que sea necesaria para cumplimentar el trabajo

% --------------PUNTO 12.1-------------
producto_aceptado_por_todos(P) :-
    producto(P),
    forall(persona(ID, _, _), encuesta(ID, P, si, _, _)).

%CONSULTAS
% producto_aceptado_por_todos(P).

% --------------PUNTO 12.2-------------
producto_con_mas_de_n_aceptaciones(P, N) :-
    producto(P),  % Aseguramos que P sea un producto definido
    findall(ID, encuesta(ID, P, si, _, _), Lista),
    sort(Lista, Unicos),
    length(Unicos, Cant),
    Cant > N.

imprimir_productos_con_mas_de_n_aceptaciones(N) :-
    producto_con_mas_de_n_aceptaciones(P, N),
    findall(ID, encuesta(ID, P, si, _, _), Lista),
    sort(Lista, Unicos),
    length(Unicos, Cant),
    write('Producto: '), write(P),
    write(' - Cantidad de aceptaciones: '), write(Cant), nl,
    fail.


%CONSULTAS
%imprimir_productos_con_mas_de_n_aceptaciones(5).

% --------------PUNTO 12.3-------------
productos_aceptados_por_mayoria(Lista) :-
    findall(P, (
        producto(P),
        findall(ID, encuesta(ID, P, si, _, _), Aceptan),
        length(Aceptan, Cant),
        findall(IDP, persona(IDP, _, _), Personas),
        length(Personas, Total),
        Cant > Total // 2
    ), Lista).

%CONSULTAS
%productos_aceptados_por_mayoria(Lista). 
