% productos 
producto(ps5).
producto(telefono_a_disco).  
producto(iphone_13).
producto(radio).
producto(destornillador).

% personas(ID, edad, genero)
persona(1,20,masculino).
persona(2,35,masculino).   
persona(3,60,masculino).
persona(4,25,masculino).
persona(5,70,masculino).
persona(6,25,femenino).
persona(7,40,femenino).
persona(8,57,femenino).
persona(9,17,femenino).
persona(10,34,femenino).      

% encuesta(IDPersona, Producto, Aceptacion, Motivo, Precio)

% p1 (20 años, masculino)
encuesta(1,ps5,si,graficos,1000000).
encuesta(1,telefono_a_disco,no,anticuado,0).
encuesta(1,iphone_13,si,fluidez,700000).
encuesta(1,radio,no,inutil,3000).
encuesta(1,destornillador,si,util,5000).

% p2 (35 años, masculino)
encuesta(2,ps5,si,entretenimiento,800000).
encuesta(2,telefono_a_disco,no,incomodo,0).
encuesta(2,iphone_13,si,camara,600000).
encuesta(2,radio,si,costumbre,2000).
encuesta(2,destornillador,si,util,6000).

% p3 (60 años, masculino)
encuesta(3,ps5,no,complejo,0).
encuesta(3,telefono_a_disco,si,costumbre,1000).
encuesta(3,iphone_13,no,caro,0).
encuesta(3,radio,si,facil_uso,3000).
encuesta(3,destornillador,si,util,5000).

% p4 (25 años, masculino)
encuesta(4,ps5,si,juegos,950000).
encuesta(4,telefono_a_disco,no,lento,0).
encuesta(4,iphone_13,si,camara,720000).
encuesta(4,radio,no,anticuado,1000).
encuesta(4,destornillador,si,durabilidad,6000).

% p5 (70 años, masculino)
encuesta(5,ps5,no,complejo,0).
encuesta(5,telefono_a_disco,si,conocido,2000).
encuesta(5,iphone_13,no,innecesario,0).
encuesta(5,radio,si,costumbre,2000).
encuesta(5,destornillador,si,util,4500).

% p6 (25 años, femenino)
encuesta(6,ps5,si,diversion,1000000).
encuesta(6,telefono_a_disco,no,viejo,0).
encuesta(6,iphone_13,si,estilo,700000).
encuesta(6,radio,no,anticuado,0).
encuesta(6,destornillador,si,practico,7000).

% p7 (40 años, femenino)
encuesta(7,ps5,si,juegos,850000).
encuesta(7,telefono_a_disco,no,desactualizado,0).
encuesta(7,iphone_13,si,fluidez,680000).
encuesta(7,radio,si,nostalgia,2000).
encuesta(7,destornillador,si,util,5000).

% p8 (57 años, femenino)
encuesta(8,ps5,no,complicado,0).
encuesta(8,telefono_a_disco,si,facil_uso,1500).
encuesta(8,iphone_13,no,caro,0).
encuesta(8,radio,si,costumbre,2500).
encuesta(8,destornillador,si,util,4000).

% p9 (17 años, femenino)
encuesta(9,ps5,si,graficos,950000).
encuesta(9,telefono_a_disco,no,viejo,0).
encuesta(9,iphone_13,si,estilo,750000).
encuesta(9,radio,no,aburrido,0).
encuesta(9,destornillador,si,practico,6000).

% p10 (34 años, femenino)
encuesta(10,ps5,si,entretenimiento,870000).
encuesta(10,telefono_a_disco,no,obsoleto,0).
encuesta(10,iphone_13,si,camara,690000).
encuesta(10,radio,si,informacion,1500).
encuesta(10,destornillador,si,util,5500).

                %consultas
                %cuantas veces un producto fue aceptado    
cuenta_cantidad_de_aceptacion(Producto, Cantidad):-
    findall(Producto, encuesta(_,Producto,si,_,_), Lista),
     length(Lista,Cantidad).

                %trae todos los productos
todos_los_productos(Productos):-
    findall(P,producto(P), Productos).

                %retorna la aceptacion de cada producto
cantidades_de_aceptacion([],[]). %si no hay nada en la lista termina aca
cantidades_de_aceptacion([P|Ps], [(P,Cant)|Resto]):-  %p es el producto, ps la lista de productos p y cant es la lista con sus resultados, resto es la lista ya hecha
    cuenta_cantidad_de_aceptacion(P,Cant), %devuelve el valor_del primer valor
    cantidades_de_aceptacion(Ps,Resto).   %llama de froma recursiva

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

                %imprime producto mas aceptado         IMPORTANTE
producto_mas_aceptado:-
     producto_mas_aceptado(P),
     write("el producto mas aceptado es: "), write(P), nl.   


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

                %imprimir producto menos aceptado         IMPORTANTE
producto_menos_aceptado:-
     producto_menos_aceptado_funcion(P),
     write("el producto menos aceptado es: "), write(P), nl.      


                  %cantidad de encuestados
cantidad_personas_encuestadas(Cant):-
    findall(ID, encuesta(ID, _, _, _, _), Lista), 
    sort(Lista, SinRepetidos), 
    length(SinRepetidos, Cant).

                    %mostrar cantidad de encuestados        IMPORTANTE
personas_encuestadas:-
    cantidad_personas_encuestadas(Cant),
    write("cantidad de personas encuestadas:"), write(Cant),nl.


                    %cantidad de encuestas con aceptacion general

cantidad_de_encuestas_con_aceptacion(Cantidad):-
    findall(encuesta,encuesta(_,_,si,_,_), Lista),
    length(Lista,Cantidad).                    

                    %imprimir encuestas de aceptacion           IMPORTANTE

encuestas_aceptacion_general:-
       cantidad_de_encuestas_con_aceptacion(Cantidad),
       write("cantidad general de encuestas con aceptacion: "),write(Cantidad), nl.        

                    %cantidad de encuestas con aceptacion negativa general
cantidad_de_encuestas_con_aceptacion_negativa(Cantidad):-
    findall(encuesta,encuesta(_,_,no,_,_), Lista),
    length(Lista,Cantidad).        


                    %imprimir encuestas de aceptacion negativas           IMPORTANTE

encuestas_sin_aceptacion_general:-
       cantidad_de_encuestas_con_aceptacion_negativa(Cantidad),
       write("cantidad general de encuestas con aceptacion negativa: "),write(Cantidad), nl.                  



%-----------------RAMA--------------------------
% 3)Distintos tipos de listados (por ejemplo: listado de productos, encuestas por producto, etc.)

%Listado de productos
listar_productos :-
    producto(P),
    writeln(P),
    fail.
listar_productos.

%Encuestas por producto
encuestas_por_producto(Producto) :-
    producto(Producto),
    writeln('---'),
    format('Encuestas para ~w:~n', [Producto]),
    encuesta(ID, Producto, Acepta, Motivo, Precio),
    format('Persona ~w - Acepta: ~w - Motivo: ~w - Precio: ~w~n', [ID, Acepta, Motivo, Precio]),
    fail.
encuestas_por_producto(_).

% Encuestas realizadas por una persona
encuestas_de_persona(IDPersona) :-
    persona(IDPersona, Edad, Genero),
    format('Encuestas de Persona ~w (Edad: ~w, Género: ~w):~n', [IDPersona, Edad, Genero]),
    encuesta(IDPersona, Producto, Acepta, Motivo, Precio),
    format('Producto: ~w - Acepta: ~w - Motivo: ~w - Precio: $~w~n', [Producto, Acepta, Motivo, Precio]),
    fail.
encuestas_de_persona(_).

%Listado general de todas las encuestas
listar_todas_las_encuestas :-
    writeln('Listado completo de encuestas:'),
    encuesta(ID, Producto, Acepta, Motivo, Precio),
    format('Persona: ~w - Producto: ~w - Acepta: ~w - Motivo: ~w - Precio: $~w~n',
           [ID, Producto, Acepta, Motivo, Precio]),
    fail.
listar_todas_las_encuestas.

%Listado de personas encuestadas
%Mostrar todas las personas que participaron en las encuestas
listar_personas :-
    writeln('Listado de personas encuestadas:'),
    persona(ID, Edad, Genero),
    format('ID: ~w - Edad: ~w - Género: ~w~n', [ID, Edad, Genero]),
    fail.
listar_personas.

%------------------------------ PUNTO 9 -------------------------------------------------
% Razón principal de aceptación (la más mencionada) de cada producto.

% Obtener la razón de aceptación más mencionada para un producto dado
razon_principal_aceptacion(Producto, MotivoPrincipal) :-
    findall(Motivo, encuesta(_, Producto, si, Motivo, _), Motivos),
    motivo_mas_frecuente(Motivos, MotivoPrincipal).

% Contar ocurrencias de cada motivo y devolver el más frecuente
motivo_mas_frecuente(Lista, Motivo) :-
    sort(Lista, Unicos),
    contar_motivos(Lista, Unicos, Pares),
    maximo_motivo(Pares, Motivo).

% Contar cuántas veces aparece cada motivo
contar_motivos(_, [], []).
contar_motivos(Lista, [H|T], [(H, C)|Resto]) :-
    incluir(H, Lista, C),
    contar_motivos(Lista, T, Resto).

% Contar cuántas veces un elemento aparece en una lista
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
% producto(P), razon_principal_aceptacion(P, Motivo), format('Producto: ~w - Motivo principal de aceptación: ~w~n', [P, Motivo]), fail.

%------------------------------ PUNTO 10 -------------------------------------------------
% Razón principal de no aceptación (la más mencionada) de cada producto.

% Obtener la razón de no aceptación más mencionada para un producto dado
razon_principal_no_aceptacion(Producto, MotivoPrincipal) :-
    findall(Motivo, encuesta(_, Producto, no, Motivo, _), Motivos),
    motivo_mas_frecuente(Motivos, MotivoPrincipal).
	
% CONSULTAS
% razon_principal_no_aceptacion(ps5, Motivo).
% producto(P), razon_principal_no_aceptacion(P, Motivo), format('Producto: ~w - Motivo principal de no aceptación: ~w~n', [P, Motivo]), fail.

%------------------------------ PUNTO 11 -------------------------------------------------
% ¿Cuánto estarían dispuestos a pagar los encuestados que aceptan cada producto?

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

