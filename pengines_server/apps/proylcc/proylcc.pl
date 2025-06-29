:- module(proylcc,
    [
        randomBlock/2,
        shoot/5
    ]).

:- use_module(library(random)).
:- use_module(library(clpfd)).
/*
randomBlock(Grid, Block) :-
    max_in_grid(Grid, Max),
    range_for_max(Max, Rango),
    ( Rango \= [] -> random_member(Block, Rango)
    ; Block = 2
    ).
*/

:- dynamic bloques_retirados/1.
bloques_retirados([]).

actualizar_bloques_retirados(NuevosRetirados) :-
    retractall(bloques_retirados(_)),
    assertz(bloques_retirados(NuevosRetirados)).

randomBlock(_Grid, Block) :-
    max_historico(Max),
    range_for_max(Max, RangoOriginal),
    bloques_retirados(Retirados),
    subtract(RangoOriginal, Retirados, RangoFiltrado),
    ( RangoFiltrado \= [] -> random_member(Block, RangoFiltrado)
    ; Block = 2
    ).
:- dynamic max_historico/1.
max_historico(64).  % Valor inicial

actualizar_max_historico(NuevoMax) :-
    max_historico(Anterior),
    ( NuevoMax > Anterior ->
        retractall(max_historico(_)),
        assertz(max_historico(NuevoMax))
    ; true ).

max_in_grid(Grid, Max) :-
    include(number, Grid, Numeros),
    ( Numeros == [] -> Max = 0 ; max_list(Numeros, Max) ).

range_for_max(Max, Rango) :-
    ( Max =< 8    -> Rango = [2,4,8]
    ; Max =< 16   -> Rango = [2,4,8,16]
    ; Max =< 32   -> Rango = [2,4,8,16,32]
    ; Max =< 64   -> Rango = [2,4,8,16,32,64]
    ; Max =< 512  -> Rango = [2,4,8,16,32,64,128,256,512]
    ; Max =< 1024 -> Rango = [4,8,16,32,64,128,256,512,1024]
    ; Max =< 2048 -> Rango = [8,16,32,64,128,256,512,1024,2048]
    ; Max =< 8192 -> Rango = [16,32,64,128,256,512,1024,2048,4096,8192]
    ; Max =< 16384-> Rango = [32,64,128,256,512,1024,2048,4096,8192,16384]
    ; Rango = [2,4,8]
    ).

shoot(Block, Col, Grid, NumCols, Effects) :-
    encontrar_posicion_vacia(Grid, Col, NumCols, Pos),
    poner_en_posicion(Grid, Pos, Block, GridConBloqueDisparado),
    resolver_combinaciones(GridConBloqueDisparado, NumCols, Pos, Effects).

encontrar_posicion_vacia(Grid, Col, NumCols, Pos) :-
    length(Grid, Len),
    Filas is Len // NumCols,
    encontrar_posicion_vacia_en_columna(Grid, Col, NumCols, 1, Filas, Pos).

encontrar_posicion_vacia_en_columna(Grid, Col, NumCols, FilaActual, MaxFila, Pos) :-
    FilaActual =< MaxFila,
    Index is (FilaActual - 1) * NumCols + (Col - 1),
    nth0(Index, Grid, Cell),
    ( var(Cell) ; Cell == - ; Cell == 0 ), !,
    Pos = Index.

encontrar_posicion_vacia_en_columna(Grid, Col, NumCols, FilaActual, MaxFila, Pos) :-
    FilaActual < MaxFila,
    NextFila is FilaActual + 1,
    encontrar_posicion_vacia_en_columna(Grid, Col, NumCols, NextFila, MaxFila, Pos).

poner_en_posicion(Grid, Pos, Block, Grid1) :-
    same_length(Grid, Grid1),
    poner_en_posicion_aux(Grid, Pos, Block, 0, Grid1).

poner_en_posicion_aux([], _, _, _, []).
poner_en_posicion_aux([_|T], Pos, Block, Pos, [Block|T]).
poner_en_posicion_aux([H|T], Pos, Block, Index, [H|Resto]) :-
    Index \= Pos,
    Next is Index + 1,
    poner_en_posicion_aux(T, Pos, Block, Next, Resto).

/*
resolver_combinaciones(GridInicial, NumCols, PosDisparo, Effects) :-
    buscar_y_combinar(GridInicial, NumCols, PosDisparo, GridPostCombinacionInicial, CombinacionesIniciales),
    resolver_combinaciones_recursivo(GridPostCombinacionInicial, NumCols, CombinacionesIniciales, Effects).

resolver_combinaciones(GridInicial, NumCols, PosDisparo, EffectsFinal) :-
    max_in_grid(GridInicial, MaxAntes),
    buscar_y_combinar(GridInicial, NumCols, PosDisparo, GridPostCombinacionInicial, CombinacionesIniciales),
    resolver_combinaciones_recursivo(GridPostCombinacionInicial, NumCols, CombinacionesIniciales, EffectsParciales),
    last(EffectsParciales, effect(GridFinal, _)),
    max_in_grid(GridFinal, MaxDespues),
    (   bloques_retirados(MaxAntes, MaxDespues, Retirados)
    ->  eliminar_bloques_retirados(GridFinal, Retirados, GridLimpio),
        append(EffectsParciales, [effect(GridLimpio, [retiredBlocks(Retirados)])], EffectsFinal)
    ;   EffectsFinal = EffectsParciales
    ).
*/

% Declarar un predicado para obtener el bloque máximo anterior (puede guardarse en estado)
% y compararlo con el nuevo máximo para detectar bloques retirados.

% Suponiendo que guardas el máximo anterior en algún lugar o lo pasas como argumento,
% o para test simple, calculamos el máximo antes y después:

resolver_combinaciones(GridInicial, NumCols, PosDisparo, Effects) :-
    max_in_grid(GridInicial, MaxAnt),
    buscar_y_combinar(GridInicial, NumCols, PosDisparo, GridPostCombinacionInicial, CombinacionesIniciales),
    resolver_combinaciones_recursivo(GridPostCombinacionInicial, NumCols, CombinacionesIniciales, EffectsSinRetirados),
    max_in_grid(GridPostCombinacionInicial, MaxNuevo),
    actualizar_max_historico(MaxNuevo),
    bloques_retirados(MaxAnt, MaxNuevo, Retirados),
    bloques_retirados(MaxAnt, MaxNuevo, Retirados),
   %retirados
 ( Retirados = [] ->
    Effects = EffectsSinRetirados
;
    last(EffectsSinRetirados, effect(GridFinalAnterior, _)),
    eliminar_bloques_retirados(GridFinalAnterior, Retirados, GridSinRetirados),
    aplicar_gravedad(GridSinRetirados, NumCols, GridLimpio),
    actualizar_bloques_retirados(Retirados),
    append(EffectsSinRetirados, [effect(GridLimpio, [retiredBlocks(Retirados)])], Effects)
).


resolver_combinaciones_recursivo(GridActual, NumCols, AccCombinaciones, Effects) :-
    aplicar_gravedad(GridActual, NumCols, GridConGravedad),
    buscar_todas_las_combinaciones(GridConGravedad, NumCols, GridPostNuevasCombinaciones, NuevasCombinaciones),
    (   GridActual =@= GridPostNuevasCombinaciones, NuevasCombinaciones = [] ->
        Effects = [effect(GridPostNuevasCombinaciones, AccCombinaciones)]
    ;
        append(AccCombinaciones, NuevasCombinaciones, TodasLasCombinaciones),
        resolver_combinaciones_recursivo(GridPostNuevasCombinaciones, NumCols, TodasLasCombinaciones, Effects)
    ).

encontrar_grupo_conectado(Grid, NumCols, PosInicio, ValorBuscado, Grupo, VisitadosFinal) :-
    encontrar_grupo_conectado_recursivo(Grid, NumCols, [PosInicio], ValorBuscado, [PosInicio], Grupo, VisitadosFinal).

encontrar_grupo_conectado_recursivo(_Grid, _NumCols, [], _ValorBuscado, Visitados, Visitados, Visitados).

encontrar_grupo_conectado_recursivo(Grid, NumCols, [PosActual|Cola], ValorBuscado, VisitadosActual, GrupoFinal, VisitadosFinal) :-
    posiciones_adyacentes(Grid, PosActual, NumCols, Adyacentes),
    findall(P_adyacente, (
        member(P_adyacente, Adyacentes),
        nth0(P_adyacente, Grid, Val_adyacente),
        number(Val_adyacente),
        Val_adyacente =:= ValorBuscado,
        \+ member(P_adyacente, VisitadosActual)
    ), NuevosAdyacentes),
    
    append(VisitadosActual, NuevosAdyacentes, ProximosVisitados),
    append(Cola, NuevosAdyacentes, ProximaCola),
    
    encontrar_grupo_conectado_recursivo(Grid, NumCols, ProximaCola, ValorBuscado, ProximosVisitados, GrupoFinal, VisitadosFinal).

% Predicado para calcular el nuevo valor al multiplicar
calcular_nuevo_valor_multiplicado(ValorBase, LongitudGrupo, NuevoValor) :-
    Multiplicador is 2 ^ (LongitudGrupo - 1),
    NuevoValor is ValorBase * Multiplicador.

buscar_y_combinar(GridEntrada, NumCols, PosInicio, GridSalida, Combinaciones) :-
    nth0(PosInicio, GridEntrada, ValorInicio),
    number(ValorInicio),
    encontrar_grupo_conectado(GridEntrada, NumCols, PosInicio, ValorInicio, GrupoCombinable, _),
    length(GrupoCombinable, Len),
    (   Len >= 2 ->
        % Usar el nuevo predicado para calcular el valor
        calcular_nuevo_valor_multiplicado(ValorInicio, Len, NuevoValor),
        eliminar_bloques_combinados(GridEntrada, GrupoCombinable, GridSinCombinados),
        poner_en_posicion(GridSinCombinados, PosInicio, NuevoValor, GridSalida),
        Combinaciones = [combination(GrupoCombinable, PosInicio, NuevoValor)]
    ;
        GridSalida = GridEntrada,
        Combinaciones = []
    ).

es_adyacente_y_mismo_valor(Grid, NumCols, PosBase, PosAdyacente, Valor) :-
    posiciones_adyacentes(Grid, PosBase, NumCols, PosicionesAdyacentes),
    member(PosAdyacente, PosicionesAdyacentes),
    nth0(PosAdyacente, Grid, ValorAdyacente),
    number(ValorAdyacente),
    ValorAdyacente = Valor.

posiciones_adyacentes(Grid, Pos, NumCols, Adyacentes) :-
    Fila is Pos // NumCols,
    Columna is Pos mod NumCols,
    length(Grid, LenGrid),
    MaxGridIndex is LenGrid - 1,

    findall(P, (
        (
            Fila > 0,
            P is (Fila - 1) * NumCols + Columna,
            P >= 0
        );
        (
            P is (Fila + 1) * NumCols + Columna,
            P =< MaxGridIndex
        );
        (
            Columna > 0,
            P is Fila * NumCols + (Columna - 1),
            P >= 0
        );
        (
            P is Fila * NumCols + (Columna + 1),
            P =< MaxGridIndex,
            P // NumCols =:= Fila
        )
    ), Adyacentes).

buscar_todas_las_combinaciones(GridEntrada, NumCols, GridSalida, Combinaciones) :-
    length(GridEntrada, Len),
    MaxIndex is Len - 1,
    findall(
        CombinacionEffect,
        (   between(0, MaxIndex, Pos),
            nth0(Pos, GridEntrada, Valor),
            number(Valor),
            Valor \= 0,
            encontrar_grupo_conectado(GridEntrada, NumCols, Pos, Valor, GrupoCombinable, _),
            length(GrupoCombinable, LenGrupo),
            LenGrupo >= 2,
            min_list(GrupoCombinable, MinPos),
            Pos = MinPos,
            % Usar el nuevo predicado para calcular el valor
            calcular_nuevo_valor_multiplicado(Valor, LenGrupo, NuevoValor),
            CombinacionEffect = combination(GrupoCombinable, MinPos, NuevoValor)
        ),
        TodasLasCombinaciones
    ),
    aplicar_multiples_combinaciones(GridEntrada, NumCols, TodasLasCombinaciones, GridSalida, Combinaciones).

aplicar_multiples_combinaciones(GridEntrada, _NumCols, [], GridEntrada, []).
aplicar_multiples_combinaciones(GridEntrada, NumCols, [combination(Grupo, PosRes, NuevoVal)|RestoCombinaciones], GridSalida, [combination(Grupo, PosRes, NuevoVal)|RestoEffects]) :-
    eliminar_bloques_combinados(GridEntrada, Grupo, GridSinCombinados),
    poner_en_posicion(GridSinCombinados, PosRes, NuevoVal, GridConUnaCombinacion),
    aplicar_multiples_combinaciones(GridConUnaCombinacion, NumCols, RestoCombinaciones, GridSalida, RestoEffects).


eliminar_bloques_combinados(GridEntrada, PosicionesAEliminar, GridSalida) :-
    length(GridEntrada, Len),
    length(GridSalida, Len),
    eliminar_bloques_combinados_aux(GridEntrada, PosicionesAEliminar, 0, GridSalida).

eliminar_bloques_combinados_aux([], _, _, []).
eliminar_bloques_combinados_aux([_|T], PosicionesAEliminar, Index, ['-'|Resto]) :-
    member(Index, PosicionesAEliminar), !,
    NextIndex is Index + 1,
    eliminar_bloques_combinados_aux(T, PosicionesAEliminar, NextIndex, Resto).
eliminar_bloques_combinados_aux([H|T], PosicionesAEliminar, Index, [H|Resto]) :-
    NextIndex is Index + 1,
    eliminar_bloques_combinados_aux(T, PosicionesAEliminar, NextIndex, Resto).

eliminar_bloques_retirados([], _, []).
eliminar_bloques_retirados([H|T], Retirados, ['-'|Resto]) :-
    member(H, Retirados), !,
    eliminar_bloques_retirados(T, Retirados, Resto).
eliminar_bloques_retirados([H|T], Retirados, [H|Resto]) :-
    eliminar_bloques_retirados(T, Retirados, Resto).



aplicar_gravedad(GridEntrada, NumCols, GridSalida) :-
    length(GridEntrada, Len),
    _NumFilas is Len // NumCols,
    list_to_rows(GridEntrada, NumCols, RowsEntrada),
    transpose_matrix(RowsEntrada, ColsEntrada),
    maplist(aplicar_gravedad_columna, ColsEntrada, ColsSalida),
    transpose_matrix(ColsSalida, RowsSalida),
    flatten(RowsSalida, GridSalida).





list_to_rows([], _, []).
list_to_rows(List, ChunkSize, [Head|Tail]) :-
    length(Head, ChunkSize),
    append(Head, Rest, List),
    list_to_rows(Rest, ChunkSize, Tail).

transpose_matrix([], []).
transpose_matrix([[]|_], []).
transpose_matrix(Matrix, [Row|Rows]) :-
    get_first_elements(Matrix, Row, RestMatrix),
    transpose_matrix(RestMatrix, Rows).

get_first_elements([], [], []).
get_first_elements([[H|T]|Rest], [H|Hs], [T|Ts]) :-
    get_first_elements(Rest, Hs, Ts).

aplicar_gravedad_columna(ColumnaEntrada, ColumnaSalida) :-
    include(=('-'), ColumnaEntrada, Vacios),
    exclude(=('-'), ColumnaEntrada, Bloques),
    append(Bloques, Vacios, ColumnaSalida).


normalizar_grid([], []).
normalizar_grid([H|T], ['-'|R]) :- var(H), !,
    normalizar_grid(T, R).
normalizar_grid([H|T], [H|R]) :-
    normalizar_grid(T, R).
/*
bloques_retirados(MaxAnterior, MaxNuevo, Retirados) :-
    range_for_max(MaxAnterior, RangoAnterior),
    range_for_max(MaxNuevo, RangoNuevo),
    subtract(RangoAnterior, RangoNuevo, Retirados),
    Retirados \= [].
*/
% bloques_retirados(MaxAnt, MaxNuevo, ListaRetirados)
bloques_retirados(MaxAnt, MaxNuevo, Retirados) :-
    range_for_max(MaxAnt, RangoAnt),
    range_for_max(MaxNuevo, RangoNuevo),
    findall(Bloque, (member(Bloque, RangoAnt), \+ member(Bloque, RangoNuevo)), Retirados).