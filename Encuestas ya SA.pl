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

                %devuelve el producto con mas aceptacion
max_aceptacion([(P, C)], P, C).                           
max_aceptacion([(P1, C1)|Resto], MaxP, MaxC) :-
    max_aceptacion(Resto, P2, C2),
    (C1 >= C2 -> MaxP = P1, MaxC = C1 ; MaxP = P2, MaxC = C2).      

                %producto mas aceptado
producto_mas_aceptado_funcion(Producto):-
todos_los_productos(ListaProductos),
    cantidades_de_aceptacion(ListaProductos, Pares),
    max_aceptacion(Pares,Producto,_).   

                %imprimir producto mas aceptado
producto_mas_aceptado:-
     producto_mas_aceptado(P),
     write("el producto mas aceptado es: "), write(P), nl.            