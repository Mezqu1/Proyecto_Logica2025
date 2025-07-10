:- module(proylcc,
    [
        randomBlock/2,
        shoot/5,
        get_hint/6   % <--- Mantener esta línea para exportar el hint
    ]).

:- use_module(library(random)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

randomBlock(Grid, Block) :-
    max_in_grid(Grid, Max),
    range_for_max(Max, Rango),
    ( Rango \= [] -> random_member(Block, Rango)
    ; Block = 2
    ).

% Estas funciones de gravedad deben estar solo UNA VEZ y preferiblemente aquí arriba
aplicar_gravedad(GridEntrada, NumCols, GridSalida) :-
    length(GridEntrada, Len),
    _NumFilas is Len // NumCols,
    list_to_rows(GridEntrada, NumCols, RowsEntrada),
    transpose_matrix(RowsEntrada, ColsEntrada),
    maplist(aplicar_gravedad_columna, ColsEntrada, ColsSalida),
    transpose_matrix(ColsSalida, RowsSalida),
    flatten(RowsSalida, GridSalida).

aplicar_gravedad_columna(ColumnaEntrada, ColumnaSalida) :-
    include(=('-'), ColumnaEntrada, Vacios),
    exclude(=('-'), ColumnaEntrada, Bloques),
    append(Bloques, Vacios, ColumnaSalida).

max_in_grid(Grid, Max) :-
    include(number, Grid, Numeros),
    ( Numeros == [] -> Max = 0 ; max_list(Numeros, Max) ).

range_for_max(Max, Rango) :-
    % >>> RESOLVIENDO CONFLICTO EN range_for_max/2: Manteniendo tu versión (HEAD)
    ( Max =< 8    -> Rango = [2,4]
    ; Max =< 16   -> Rango = [2,4,8]
    ; Max =< 32   -> Rango = [2,4,8,16]
    ; Max =< 64   -> Rango = [2,4,8,16,32]
    ; Max =< 512  -> Rango = [2,4,8,16,32,64]
    ; Max =< 1024 -> Rango = [4,8,16,32,64,128]
    ; Max =< 2048 -> Rango = [8,16,32,64,128,256]
    ; Max =< 8192 -> Rango = [16,32,64,128,256,512]
    ; Max =< 16384-> Rango = [32,64,128,256,512,1024]
    ; Rango = [32,64,128,256,512,1024]
    ).

% Predicados para la funcionalidad de 'bloques_a_retirar' (solo en tu rama)
bloques_a_retirar_acumulados(Max, BloquesRetirados) :-
    ( Max >= 16000 -> BloquesRetirados = [16, 8, 4, 2]
    ; Max >= 4096  -> BloquesRetirados = [8, 4, 2]
    ; Max >= 2048  -> BloquesRetirados = [4, 2]
    ; Max >= 1024  -> BloquesRetirados = [2]
    ; BloquesRetirados = []
    ).

shoot(Block, Col, Grid, NumCols, Effects) :-
    %normalizar_grid(GridEntrada, Grid), % <--- Mantener línea comentada si la quieres
    encontrar_posicion_vacia(Grid, Col, NumCols, PosDisparo),
    poner_en_posicion(Grid, PosDisparo, Block, GridConBloqueDisparado),
    EffectDisparo = effect(GridConBloqueDisparado, [disparo(PosDisparo, Block)]),
    resolver_pasos_juego(GridConBloqueDisparado, NumCols, PosDisparo, [EffectDisparo], Effects).

encontrar_posicion_vacia(Grid, Col, NumCols, Pos) :-
    length(Grid, Len),
    Filas is Len // NumCols,
    encontrar_posicion_vacia_en_columna(Grid, Col, NumCols, 1, Filas, Pos).

encontrar_posicion_vacia_en_columna(Grid, Col, NumCols, FilaActual, MaxFila, Pos) :-
    FilaActual =< MaxFila,
    Index is (FilaActual - 1) * NumCols + (Col - 1),
    nth0(Index, Grid, Cell),
    ( var(Cell) ; Cell == '-' ; Cell == 0 ), !,
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

resolver_pasos_juego(GridActual, NumCols, PosDisparo, AccEffects, FinalEffects) :-
    aplicar_gravedad(GridActual, NumCols, GridPostGravedad),
    % >>> RESOLVIENDO CONFLICTO EN resolver_pasos_juego/5
    (   GridActual =@= GridPostGravedad ->
        buscar_todas_las_combinaciones(GridPostGravedad, NumCols, PosDisparo, GridPostCombinaciones, NuevasCombinaciones),

        ( NuevasCombinaciones = [] ->
            % Lógica de tu rama: Limpiar bloques retirados y aplicar gravedad luego de la limpieza
            max_in_grid(GridPostCombinaciones, Max),
            bloques_a_retirar_acumulados(Max, BloquesRetirados), % Usar tu nuevo predicado
            eliminar_bloques_retirados(GridPostCombinaciones, BloquesRetirados, GridLimpia), % Usar tu nuevo predicado
            EffectLimpieza = effect(GridLimpia, [limpieza_bloques_retirados(BloquesRetirados)]),

            aplicar_gravedad(GridLimpia, NumCols, GridFinal),
            EffectGravedadPostLimpieza = effect(GridFinal, [gravedad]),

            append(AccEffects, [EffectLimpieza, EffectGravedadPostLimpieza], FinalEffects)
        ;
            EffectCombinacion = effect(GridPostCombinaciones, NuevasCombinaciones),
            append(AccEffects, [EffectCombinacion], NextAccEffects),
            resolver_pasos_juego(GridPostCombinaciones, NumCols, PosDisparo, NextAccEffects, FinalEffects)
        )
    ;
        EffectGravedad = effect(GridPostGravedad, [gravedad]),
        append(AccEffects, [EffectGravedad], AccEffectsConGravedad),
        buscar_todas_las_combinaciones(GridPostGravedad, NumCols, PosDisparo, GridPostCombinaciones, NuevasCombinaciones),

        % Lógica de tu rama: Limpiar bloques retirados después de gravedad y combinaciones
        ( NuevasCombinaciones = [] ->
            max_in_grid(GridPostCombinaciones, Max),
            bloques_a_retirar_acumulados(Max, BloquesRetirados),
            eliminar_bloques_retirados(GridPostCombinaciones, BloquesRetirados, GridLimpia),
            EffectLimpieza = effect(GridLimpia, [limpieza_bloques_retirados(BloquesRetirados)]),

            aplicar_gravedad(GridLimpia, NumCols, GridFinal),
            EffectGravedadPostLimpieza = effect(GridFinal, [gravedad]),

            append(AccEffectsConGravedad, [EffectLimpieza, EffectGravedadPostLimpieza], FinalEffects)
        ;
            EffectCombinacion = effect(GridPostCombinaciones, NuevasCombinaciones),
            append(AccEffectsConGravedad, [EffectCombinacion], NextAccEffects),
            resolver_pasos_juego(GridPostCombinaciones, NumCols, PosDisparo, NextAccEffects, FinalEffects)
        )
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

calcular_nuevo_valor_multiplicado(ValorBase, LongitudGrupo, NuevoValor) :-
    Multiplicador is 2 ^ (LongitudGrupo - 1),
    NuevoValor is ValorBase * Multiplicador.

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
            Columna < NumCols - 1,
            P is Fila * NumCols + (Columna + 1),
            P =< MaxGridIndex,
            P // NumCols =:= Fila
        )
    ), Adyacentes).

buscar_todas_las_combinaciones(GridEntrada, NumCols, PosDisparo, GridSalida, Combinaciones) :-
    length(GridEntrada, Len),
    MaxIndex is Len - 1,
    findall(
        combination(GrupoCombinable, PosResultado, NuevoValor, LenGrupo),
        (   between(0, MaxIndex, Pos),
            nth0(Pos, GridEntrada, Valor),
            number(Valor),
            Valor \= 0,
            encontrar_grupo_conectado(GridEntrada, NumCols, Pos, Valor, GrupoCombinable, _),
            length(GrupoCombinable, LenGrupo),
            LenGrupo >= 2,
            min_list(GrupoCombinable, Pos),
            calcular_nuevo_valor_multiplicado(Valor, LenGrupo, NuevoValor),
            (   member(PosDisparo, GrupoCombinable) ->
                FilaDisparo is PosDisparo // NumCols,
                (   forall(member(P_grupo, GrupoCombinable), (P_grupo // NumCols) =:= FilaDisparo) ->
                    PosResultado = PosDisparo
                ;
                    min_list(GrupoCombinable, PosResultado)
                )
            ;
                min_list(GrupoCombinable, PosResultado)
            )
        ),
        TodasLasCombinacionesSinDuplicados
    ),
    aplicar_multiples_combinaciones(GridEntrada, NumCols, TodasLasCombinacionesSinDuplicados, GridSalida, Combinaciones).
    
aplicar_multiples_combinaciones(GridEntrada, _NumCols, [], GridEntrada, []).
aplicar_multiples_combinaciones(GridEntrada, NumCols, [combination(Grupo, PosRes, NuevoVal, LenGrupo)|RestoCombinaciones], GridSalida, [combination(Grupo, PosRes, NuevoVal, LenGrupo)|RestoEffects]) :-
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

normalizar_grid([], []).
normalizar_grid([H|T], ['-'|R]) :- var(H), !,
    normalizar_grid(T, R).
normalizar_grid([H|T], [H|R]) :-
    normalizar_grid(T, R).

% Predicados para la funcionalidad de limpieza de bloques retirados (solo en tu rama)
memberchk_in(List, Elem) :- memberchk(Elem, List).

bloques_retirados_en_grilla(Grid, _RangoActual, Max, BloquesRetirados) :-
    bloques_a_retirar_acumulados(Max, BloquesRetiradosCandidatos),
    include(number, Grid, NumerosEnGrilla),
    sort(NumerosEnGrilla, BloquesEnGrillaUnicos),
    include({BloquesEnGrillaUnicos}/[X]>>memberchk(X, BloquesEnGrillaUnicos), BloquesRetiradosCandidatos, BloquesRetirados).

protege_max_y_rango(Rango, Max, Elem) :-
    memberchk(Elem, Rango);
    Elem =:= Max.
eliminar_bloques_retirados(Grid, BloquesRetirados, GridLimpia) :-
    maplist(reemplazar_si_retirado(BloquesRetirados), Grid, GridLimpia).

reemplazar_si_retirado(BloquesRetirados, Valor, NuevoValor) :-
    ( number(Valor), memberchk(Valor, BloquesRetirados) -> NuevoValor = '-'
    ; NuevoValor = Valor
    ).

rango_valido(Max, Rango) :-
    range_for_max(Max, Rango).

% Predicados para la funcionalidad de hints (solo en la rama de tu compañero)
get_hint(Block, Grid, NumCols, Columna, HintEffects, FinalGrid) :-
    between(1, NumCols, Columna), % Itera sobre cada columna
    (   encontrar_posicion_vacia(Grid, Columna, NumCols, PosDisparo) ->
        poner_en_posicion(Grid, PosDisparo, Block, GridConBloqueDisparado),
        resolver_pasos_juego_hint(GridConBloqueDisparado, NumCols, PosDisparo, [], HintEffects),
        (   HintEffects = [] -> FinalGrid = GridConBloqueDisparado
        ;   last(HintEffects, effect(FinalGrid, _))
        )
    ;   
        HintEffects = [effect(Grid, [columna_llena])], 
        FinalGrid = Grid
    ).

resolver_pasos_juego_hint(GridActual, NumCols, PosDisparo, AccEffects, FinalEffects) :-
    aplicar_gravedad(GridActual, NumCols, GridPostGravedad),
    (   GridActual =@= GridPostGravedad ->
        buscar_todas_las_combinaciones_hint(GridPostGravedad, NumCols, PosDisparo, GridPostCombinaciones, NuevasCombinaciones),
        (   NuevasCombinaciones = [] ->
            FinalEffects = AccEffects
        ;
            EffectCombinacion = effect(GridPostCombinaciones, NuevasCombinaciones),
            append(AccEffects, [EffectCombinacion], NextAccEffects),
            resolver_pasos_juego_hint(GridPostCombinaciones, NumCols, PosDisparo, NextAccEffects, FinalEffects)
        )
    ;
        EffectGravedad = effect(GridPostGravedad, [gravedad]),
        append(AccEffects, [EffectGravedad], AccEffectsConGravedad),
        buscar_todas_las_combinaciones_hint(GridPostGravedad, NumCols, PosDisparo, GridPostCombinaciones, NuevasCombinaciones),
        (   NuevasCombinaciones = [] ->
            FinalEffects = AccEffectsConGravedad
        ;
            EffectCombinacion = effect(GridPostCombinaciones, NuevasCombinaciones),
            append(AccEffectsConGravedad, [EffectCombinacion], NextAccEffects),
            resolver_pasos_juego_hint(GridPostCombinaciones, NumCols, PosDisparo, NextAccEffects, FinalEffects)
        )
    ).

buscar_todas_las_combinaciones_hint(GridEntrada, NumCols, PosDisparo, GridSalida, Combinaciones) :-
    length(GridEntrada, Len),
    MaxIndex is Len - 1,
    findall(
        combination(GrupoCombinable, PosResultado, NuevoValor, LenGrupo),
        (   between(0, MaxIndex, Pos),
            nth0(Pos, GridEntrada, Valor),
            number(Valor),
            Valor \= 0,
            encontrar_grupo_conectado(GridEntrada, NumCols, Pos, Valor, GrupoCombinable, _),
            length(GrupoCombinable, LenGrupo),
            LenGrupo >= 2,
            min_list(GrupoCombinable, Pos),
            calcular_nuevo_valor_multiplicado(Valor, LenGrupo, NuevoValor),
            (   member(PosDisparo, GrupoCombinable) ->
                FilaDisparo is PosDisparo // NumCols,
                (   forall(member(P_grupo, GrupoCombinable), (P_grupo // NumCols) =:= FilaDisparo) ->
                    PosResultado = PosDisparo
                ;
                    min_list(GrupoCombinable, PosResultado)
                )
            ;
                min_list(GrupoCombinable, PosResultado)
            )
        ),
        TodasLasCombinacionesSinDuplicados
    ),
    aplicar_multiples_combinaciones(GridEntrada, NumCols, TodasLasCombinacionesSinDuplicados, GridSalida, Combinaciones).