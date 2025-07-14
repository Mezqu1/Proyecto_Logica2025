:- module(proylcc,
    [
        randomBlock/2,
        shoot/5,
        get_hint/6
    ]).

:- use_module(library(random)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

% --- Generación de Bloques ---
randomBlock(Grid, Block) :-
    max_in_grid(Grid, Max),
    range_for_max(Max, RangoBase), % Obtiene el rango inicial basado en el maximo
    bloques_a_retirar_acumulados(Max, BloquesARetirar), % Obtiene los bloques que deben ser retirados
    % Filtra el RangoBase para excluir los BloquesARetirar
    % Usa 'exclude' para quitar los elementos de RangoBase que están en BloquesARetirar
    exclude(memberchk_in(BloquesARetirar), RangoBase, RangoFinal),
    ( RangoFinal \= [] -> random_member(Block, RangoFinal)
    ; Block = 2
    ).

% --- Gravedad ---
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

% --- Máximo y Rango de Bloques ---
max_in_grid(Grid, Max) :-
    include(number, Grid, Numeros),
    ( Numeros == [] -> Max = 0 ; max_list(Numeros, Max) ).

range_for_max(Max, Rango) :-
    ( Max =< 8     -> Rango = [2,4]
    ; Max =< 16    -> Rango = [2,4,8]
    ; Max =< 32    -> Rango = [2,4,8,16]
    ; Max =< 64    -> Rango = [2,4,8,16,32]
    ; Max =< 512   -> Rango = [2,4,8,16,32,64]
    ; Max =< 1024  -> Rango = [4,8,16,32,64,128]
    ; Max =< 2048  -> Rango = [8,16,32,64,128,256]
    ; Max =< 8192  -> Rango = [16,32,64,128,256,512]
    ; Max =< 16384 -> Rango = [32,64,128,256,512,1024]
    ; Rango = [32,64,128,256,512,1024]
    ).

% Predicados para la funcionalidad de 'bloques_a_retirar'
bloques_a_retirar_acumulados(Max, BloquesRetirados) :-
    ( Max >= 16000 -> BloquesRetirados = [16,8,4,2]
    ; Max >= 4096  -> BloquesRetirados = [8,4,2]
    ; Max >= 2048  -> BloquesRetirados = [4,2]
    ; Max >= 1024  -> BloquesRetirados = [2]
    ; BloquesRetirados = []
    ).

% --- Lógica de Disparo (Shoot) ---
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

% --- Resolución de Pasos del Juego (Principal) ---
resolver_pasos_juego(GridActual, NumCols, PosDisparo, AccEffects, FinalEffects) :-
    aplicar_gravedad(GridActual, NumCols, GridPostGravedad),
    ( GridActual =@= GridPostGravedad -> % Si no hubo gravedad
        buscar_todas_las_combinaciones(GridPostGravedad, NumCols, PosDisparo, GridPostCombinaciones, NuevasCombinaciones),
        ( NuevasCombinaciones = [] -> % Si no hay combinaciones
            max_in_grid(GridPostCombinaciones, Max),
            bloques_a_retirar_acumulados(Max, BloquesCandidatosARetirar), % Candidatos según el Max
            % 1. Obtener los números actualmente en la grilla
            include(number, GridPostCombinaciones, NumerosEnGrillaActual),
            % 2. Identificar qué bloques *de los candidatos* realmente están en la grilla para ser retirados
            intersection(NumerosEnGrillaActual, BloquesCandidatosARetirar, BloquesRealmenteRetirados),
            ( BloquesRealmenteRetirados \= [] -> % <-- SOLO AÑADIR EL EFECTO SI REALMENTE SE RETIRÓ ALGO
                eliminar_bloques_retirados(GridPostCombinaciones, BloquesCandidatosARetirar, GridLimpia),
                EffectLimpieza = effect(GridLimpia, [limpieza_bloques_retirados(BloquesRealmenteRetirados)]), % Informa lo que se quitó
                aplicar_gravedad(GridLimpia, NumCols, GridFinal),
                EffectGravedadPostLimpieza = effect(GridFinal, [gravedad]),
                append(AccEffects, [EffectLimpieza, EffectGravedadPostLimpieza], FinalEffects)
            ; % Si no se retiró nada real, simplemente avanza sin el efecto de limpieza
                aplicar_gravedad(GridPostCombinaciones, NumCols, GridFinal),
                EffectGravedadPostLimpieza = effect(GridFinal, [gravedad]),
                append(AccEffects, [EffectGravedadPostLimpieza], FinalEffects)
            )
        ; % Si hay combinaciones
            EffectCombinacion = effect(GridPostCombinaciones, NuevasCombinaciones),
            append(AccEffects, [EffectCombinacion], NextAccEffects),
            resolver_pasos_juego(GridPostCombinaciones, NumCols, PosDisparo, NextAccEffects, FinalEffects)
        )
    ; % Esta es la rama para cuando sí hay gravedad
        EffectGravedad = effect(GridPostGravedad, [gravedad]),
        append(AccEffects, [EffectGravedad], AccEffectsConGravedad),
        buscar_todas_las_combinaciones(GridPostGravedad, NumCols, PosDisparo, GridPostCombinaciones, NuevasCombinaciones),
        ( NuevasCombinaciones = [] -> % Si no hay combinaciones después de gravedad
            max_in_grid(GridPostCombinaciones, Max),
            bloques_a_retirar_acumulados(Max, BloquesCandidatosARetirar),
            include(number, GridPostCombinaciones, NumerosEnGrillaActual),
            intersection(NumerosEnGrillaActual, BloquesCandidatosARetirar, BloquesRealmenteRetirados),
            ( BloquesRealmenteRetirados \= [] ->
                eliminar_bloques_retirados(GridPostCombinaciones, BloquesCandidatosARetirar, GridLimpia),
                EffectLimpieza = effect(GridLimpia, [limpieza_bloques_retirados(BloquesRealmenteRetirados)]),
                aplicar_gravedad(GridLimpia, NumCols, GridFinal),
                EffectGravedadPostLimpieza = effect(GridFinal, [gravedad]),
                append(AccEffectsConGravedad, [EffectLimpieza, EffectGravedadPostLimpieza], FinalEffects)
            ;
                aplicar_gravedad(GridPostCombinaciones, NumCols, GridFinal),
                EffectGravedadPostLimpieza = effect(GridFinal, [gravedad]),
                append(AccEffectsConGravedad, [EffectGravedadPostLimpieza], FinalEffects)
            )
        ; % Si hay combinaciones después de gravedad
            EffectCombinacion = effect(GridPostCombinaciones, NuevasCombinaciones),
            append(AccEffectsConGravedad, [EffectCombinacion], NextAccEffects),
            resolver_pasos_juego(GridPostCombinaciones, NumCols, PosDisparo, NextAccEffects, FinalEffects)
        )
    ).

% --- Manejo de Grupos Conectados y Combinaciones ---
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
        ( % Arriba
            Fila > 0,
            P is (Fila - 1) * NumCols + Columna,
            P >= 0
        );
        ( % Abajo
            P is (Fila + 1) * NumCols + Columna,
            P =< MaxGridIndex
        );
        ( % Izquierda
            Columna > 0,
            P is Fila * NumCols + (Columna - 1),
            P >= 0
        );
        ( % Derecha
            Columna < NumCols - 1,
            P is Fila * NumCols + (Columna + 1),
            P =< MaxGridIndex,
            P // NumCols =:= Fila % Asegura que no salte de fila
        )
    ), Adyacentes).

buscar_todas_las_combinaciones(GridEntrada, NumCols, PosDisparo, GridSalida, Combinaciones) :-
    length(GridEntrada, Len),
    MaxIndex is Len - 1,
    findall(
        combination(GrupoCombinable, PosResultado, NuevoValor, LenGrupo),
        (   between(0, MaxIndex, Pos),
            nth0(Pos, GridEntrada, Valor),
            number(Valor),
            Valor \= 0,
            encontrar_grupo_conectado(GridEntrada, NumCols, Pos, Valor, GrupoCombinable, _),
            length(GrupoCombinable, LenGrupo),
            LenGrupo >= 2,
            min_list(GrupoCombinable, Pos), % Asegura que cada grupo se procese una sola vez
            calcular_nuevo_valor_multiplicado(Valor, LenGrupo, NuevoValor),
            (   member(PosDisparo, GrupoCombinable) -> % Si el disparo es parte del grupo
                FilaDisparo is PosDisparo // NumCols,
                (   forall(member(P_grupo, GrupoCombinable), (P_grupo // NumCols) =:= FilaDisparo) -> % Si todo el grupo está en la misma fila del disparo
                    PosResultado = PosDisparo
                ; % Si no, usa la posición mínima
                    min_list(GrupoCombinable, PosResultado)
                )
            ; % Si el disparo no es parte del grupo, usa la posición mínima
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

% --- Utilidades de Grilla (Conversión y Transposición) ---
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

% --- Predicados para la funcionalidad de limpieza de bloques retirados ---
memberchk_in(List, Elem) :- memberchk(Elem, List).

bloques_retirados_en_grilla(Grid, _RangoActual, Max, BloquesRetirados) :-
    bloques_a_retirar_acumulados(Max, BloquesRetiradosCandidatos),
    include(number, Grid, NumerosEnGrilla),
    sort(NumerosEnGrilla, BloquesEnGrillaUnicos),
    include({BloquesEnGrillaUnicos}/[X]>>memberchk(X, BloquesEnGrillaUnicos), BloquesRetiradosCandidatos, BloquesRetirados).

% Este predicado 'protege_max_y_rango/3' parece no estar siendo usado en el resto del código.
% Si no se usa, considera eliminarlo.
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

% --- Predicados para la funcionalidad de hints ---
get_hint(Block, Grid, NumCols, Columna, HintEffects, FinalGrid) :-
    between(1, NumCols, Columna), % Itera sobre cada columna
    (   encontrar_posicion_vacia(Grid, Columna, NumCols, PosDisparo) ->
        poner_en_posicion(Grid, PosDisparo, Block, GridConBloqueDisparado),
        resolver_pasos_juego_hint(GridConBloqueDisparado, NumCols, PosDisparo, [], HintEffects),
        (   HintEffects = [] -> FinalGrid = GridConBloqueDisparado
        ;   last(HintEffects, effect(FinalGrid, _))
        )
    ;   % Si la columna está llena
        HintEffects = [effect(Grid, [columna_llena])],
        FinalGrid = Grid
    ).

resolver_pasos_juego_hint(GridActual, NumCols, PosDisparo, AccEffects, FinalEffects) :-
    aplicar_gravedad(GridActual, NumCols, GridPostGravedad),
    ( GridActual =@= GridPostGravedad -> % Si no hubo gravedad
        buscar_todas_las_combinaciones_hint(GridPostGravedad, NumCols, PosDisparo, GridPostCombinaciones, NuevasCombinaciones),
        (   NuevasCombinaciones = [] -> % Si no hay combinaciones
            FinalEffects = AccEffects
        ;   % Si hay combinaciones
            EffectCombinacion = effect(GridPostCombinaciones, NuevasCombinaciones),
            append(AccEffects, [EffectCombinacion], NextAccEffects),
            resolver_pasos_juego_hint(GridPostCombinaciones, NumCols, PosDisparo, NextAccEffects, FinalEffects)
        )
    ;   % Esta es la rama para cuando sí hay gravedad
        EffectGravedad = effect(GridPostGravedad, [gravedad]),
        append(AccEffects, [EffectGravedad], AccEffectsConGravedad),
        buscar_todas_las_combinaciones_hint(GridPostGravedad, NumCols, PosDisparo, GridPostCombinaciones, NuevasCombinaciones),
        (   NuevasCombinaciones = [] -> % Si no hay combinaciones después de gravedad
            FinalEffects = AccEffectsConGravedad
        ;   % Si hay combinaciones después de gravedad
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
        (   between(0, MaxIndex, Pos),
            nth0(Pos, GridEntrada, Valor),
            number(Valor),
            Valor \= 0,
            encontrar_grupo_conectado(GridEntrada, NumCols, Pos, Valor, GrupoCombinable, _),
            length(GrupoCombinable, LenGrupo),
            LenGrupo >= 2,
            min_list(GrupoCombinable, Pos),
            calcular_nuevo_valor_multiplicado(Valor, LenGrupo, NuevoValor),
            (   member(PosDisparo, GrupoCombinable) ->
                FilaDisparo is PosDisparo // NumCols,
                (   forall(member(P_grupo, GrupoCombinable), (P_grupo // NumCols) =:= FilaDisparo) ->
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