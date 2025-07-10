import { useEffect, useState, useRef } from 'react';
import PengineClient, { PrologTerm } from './PengineClient';
import Board from './Board';
import Block from './Block';
import { delay } from './util';

export type Grid = (number | "-")[];

// --- INTERFACES DE PROLOG ---
export interface EffectTerm extends PrologTerm {
    functor: "effect";
    args: [Grid, EffectInfoTerm[]];
}

// Exportadas para ser usadas en otros componentes (como Board para los hints)
export interface DisparoTerm extends PrologTerm { functor: "disparo"; args: [number, number]; }
export interface GravedadTerm extends PrologTerm { functor: "gravedad"; args: []; }
export interface CombinationTerm extends PrologTerm { functor: "combination"; args: [number[], number, number, number]; } // NuevoValor es args[2], TamanioGrupo es args[3]
export interface NewBlockTerm extends PrologTerm { functor: "newBlock"; args: [number]; }
export interface ColumnaLlenaTerm extends PrologTerm { functor: "columna_llena"; args: []; } // ¡NUEVA INTERFAZ EXPORTADA!
export interface LimpiezaBloquesRetiradosTerm extends PrologTerm { functor: "limpieza_bloques_retirados"; args: [number[]]; } // Interfaz para tu nueva mecánica

// Tipo de unión que incluye todos los posibles términos en effectInfo
export type EffectInfoTerm = DisparoTerm | GravedadTerm | CombinationTerm | NewBlockTerm | ColumnaLlenaTerm | LimpiezaBloquesRetiradosTerm | PrologTerm;

// Interfaz para los datos de hint (de tu compañero)
export interface HintData {
    grid: Grid;
    effects: EffectInfoTerm[];
    summary?: string; // Nuevo: para almacenar el string del hint (ej. "COMBO x2 (+50 pts)")
}
// -------------------------------------------------------------------------------------

// Función auxiliar para mostrar mensajes temporales (de tu rama)
function showTemporaryMessage(
    setter: React.Dispatch<React.SetStateAction<string | null>>,
    message: string,
    duration: number
) {
    setter(message);
    setTimeout(() => setter(null), duration);
}

function Game() {
    const [pengine, setPengine] = useState<any>(null);
    const [grid, setGrid] = useState<Grid | null>(null);
    const [numOfColumns, setNumOfColumns] = useState<number | null>(null);
    const [score, setScore] = useState<number>(0);
    const [shootBlock, setShootBlock] = useState<number | null>(null);
    const [waiting, setWaiting] = useState<boolean>(false);
    const [comboMessage, setComboMessage] = useState<string | null>(null);

    // --- ESTADOS DE TU RAMA PARA MENSAJES ADICIONALES ---
    const [removedMessage, setRemovedMessage] = useState<string | null>(null);
    const [maxBlockMessage, setMaxBlockMessage] = useState<string | null>(null);
    const [newBlockRangeMessage, setNewBlockRangeMessage] = useState<string | null>(null);
    // ---------------------------------------------------

    // --- ESTADOS DE LA RAMA DE TU COMPAÑERO PARA BOOSTERS ---
    const [nextShootBlock, setNextShootBlock] = useState<number | null>(null); // Bloque para el booster
    const [isBoosterActive, setIsBoosterActive] = useState<boolean>(false);
    const [boosterTimeRemaining, setBoosterTimeRemaining] = useState<number>(0);
    const [showHints, setShowHints] = useState<boolean>(false);
    const [hintsData, setHintsData] = useState<{ [col: number]: HintData | null } | null>(null);
    // --------------------------------------------------------

    // Nuevo estado para la cola de bloques futuros (de tu compañero)
    const [futureBlocks, setFutureBlocks] = useState<number[]>([]);

    const COMBO_DISPLAY_DURATION = 1500; // Duración del cartel de combo en ms
    const DEFAULT_EFFECT_DELAY = 500; // Retraso entre efectos generales
    const REMOVED_DISPLAY_DURATION = 5000; // Duración del aviso de eliminación (de tu rama)
    const MAX_BLOCK_DISPLAY_DURATION = 3000; // 3 segundos (de tu rama)
    const NEW_BLOCK_RANGE_DISPLAY_DURATION = 4000; // o el tiempo que quieras (de tu rama)
    const BOOSTER_DURATION_SECONDS = 5; // Duración del booster en segundos (de tu compañero)
    const FUTURE_BLOCKS_COUNT = 3; // Cuántos bloques futuros pre-generar (actual + booster + 1 de reserva) (de tu compañero)


    // Ref para almacenar el ID del intervalo del booster, para poder limpiarlo (de tu compañero)
    const boosterTimerRef = useRef<NodeJS.Timeout | null>(null);

    // Efectos de inicialización y limpieza (de la rama de tu compañero)
    useEffect(() => {
        connectToPenginesServer();
    }, []);

    useEffect(() => {
        if (pengine) {
            initGame();
        }
    }, [pengine]);

    // Efecto para limpiar el temporizador del booster cuando el componente se desmonta (de tu compañero)
    useEffect(() => {
        return () => {
            if (boosterTimerRef.current) {
                clearInterval(boosterTimerRef.current);
            }
        };
    }, []); // Solo se ejecuta al montar y desmontar

    async function connectToPenginesServer() {
        setPengine(await PengineClient.create());
    }

    /**
     * Fetches a specified number of random blocks from Prolog.
     * @param currentGrid The current game grid.
     * @param count The number of blocks to fetch.
     * @returns A promise that resolves to an array of random block values.
     */
    async function fetchRandomBlocks(currentGrid: Grid, count: number): Promise<number[]> {
        const blocks: number[] = [];
        const gridS = JSON.stringify(currentGrid).replace(/"/g, '');
        for (let i = 0; i < count; i++) {
            const queryS = `randomBlock(${gridS}, Block)`;
            const response = await pengine.query(queryS);
            if (response && response['Block'] !== undefined) {
                blocks.push(response['Block']);
            } else {
                console.error("Error fetching random block.");
                // Si hay un error, añade un bloque por defecto para evitar que la app se rompa
                blocks.push(2);
            }
        }
        return blocks;
    }

    async function initGame() {
        // Solo pedimos la grilla inicial a Prolog
        const queryS = 'init(Grid, NumOfColumns)';
        const response = await pengine!.query(queryS);
        const initialGrid = response['Grid'];
        const numOfCols = response['NumOfColumns'];

        setGrid(initialGrid);
        setNumOfColumns(numOfCols);

        // Generamos los primeros bloques futuros
        const initialFutureBlocks = await fetchRandomBlocks(initialGrid, FUTURE_BLOCKS_COUNT);
        setFutureBlocks(initialFutureBlocks);
        setShootBlock(initialFutureBlocks[0]); // El primer bloque para disparar
        if (initialFutureBlocks.length > 1) {
            setNextShootBlock(initialFutureBlocks[1]); // El segundo bloque para el booster
        }
    }

    async function handleLaneClick(lane: number) {
        // Pre-condiciones para asegurar que el juego esté listo
        if (waiting || !grid || shootBlock === null || numOfColumns === null) {
            return;
        }

        // --- OCULTAR HINTS AL DISPARAR (de tu compañero) ---
        setShowHints(false);
        setHintsData(null);
        // ---------------------------------------------------

        // Ocultar avisos temporales si existían (adaptado de tu rama)
        setRemovedMessage(null);
        setMaxBlockMessage(null);
        setNewBlockRangeMessage(null);

        const gridS = JSON.stringify(grid).replace(/"/g, '');
        // Llamamos a shoot/5, que solo devuelve los efectos y la grilla resultante
        // Se asegura de que la consulta pida la RGrid para generar el próximo randomBlock
        const queryS = `shoot(${shootBlock}, ${lane}, ${gridS}, ${numOfColumns}, Effects), last(Effects, effect(RGrid,_))`;
        setWaiting(true);
        const response = await pengine.query(queryS);

        if (response) {
            // Animamos los efectos del juego
            await animateEffectsRecursive(response['Effects']); // Usar await aquí para asegurar que las animaciones terminan

            // Obtener la grilla final después de todos los efectos para generar el siguiente bloque
            const finalGridAfterEffects = response['RGrid'];

            // Actualizar la cola de bloques futuros (de tu compañero)
            const newFutureBlocks = futureBlocks.slice(1); // Quitar el bloque que acabamos de disparar (shootBlock)

            // Generar UN nuevo bloque aleatorio basado en la grilla final y añadirlo al final de la cola
            const newRandomBlock = await fetchRandomBlocks(finalGridAfterEffects, 1);
            newFutureBlocks.push(newRandomBlock[0]);

            setFutureBlocks(newFutureBlocks); // Actualizar el estado con la nueva cola
            setShootBlock(newFutureBlocks[0]); // El nuevo bloque actual es el primer elemento de la cola

            // El bloque del booster es el segundo elemento de la cola, si existe
            if (newFutureBlocks.length > 1) {
                setNextShootBlock(newFutureBlocks[1]);
            } else {
                setNextShootBlock(null); // Si no hay un segundo bloque (ej. cola muy corta), el booster no muestra nada
            }
        } else {
            setWaiting(false); // Si no hay respuesta, terminar el estado de espera
        }
    }

    // Función auxiliar para obtener el rango de bloques (de tu rama)
    async function getRangeForMax(max: number): Promise<number[]> {
        const query = `range_for_max(${max}, Rango)`;
        const response = await pengine.query(query);
        return response["Rango"];
    }


    /**
     * Activates the 'Next Block' booster.
     * Displays the next block to be shot for a limited time.
     * (de tu compañero)
     */
    async function activateBooster() {
        if (!pengine || !grid) {
            console.warn("Pengine or Grid not ready to activate booster.");
            return;
        }

        setIsBoosterActive(true);
        setBoosterTimeRemaining(BOOSTER_DURATION_SECONDS);

        // Limpiar cualquier temporizador existente para evitar múltiples intervalos
        if (boosterTimerRef.current) {
            clearInterval(boosterTimerRef.current);
        }

        // Iniciar el temporizador de cuenta regresiva
        boosterTimerRef.current = setInterval(() => {
            setBoosterTimeRemaining(prev => {
                if (prev <= 1) {
                    // Cuando el tiempo llega a 0, desactivar el booster
                    clearInterval(boosterTimerRef.current!);
                    setIsBoosterActive(false);
                    // Opcional: setNextShootBlock(null); si quieres que el bloque del booster desaparezca al terminar el efecto.
                    return 0;
                }
                return prev - 1;
            });
        }, 1000); // Actualizar cada segundo
    }

    /**
     * Activates the 'Hint' booster.
     * Fetches and displays a hint for each column's outcome.
     * (de tu compañero)
     */
    async function activateHintBooster() {
        if (waiting || !pengine || !grid || shootBlock === null || numOfColumns === null) {
            console.warn("No se puede activar el hint. Estado del juego no listo.");
            return;
        }

        setShowHints(prev => !prev); // Alternar visibilidad

        if (!showHints) { // Si vamos a mostrar los hints (pasó de false a true), hay que fetcharlos
            setWaiting(true); // Opcional: poner en estado de espera mientras se calculan los hints
            const currentHints: { [col: number]: HintData | null } = {};
            const gridS = JSON.stringify(grid).replace(/"/g, '');

            try {
                // Consulta para cada columna para obtener sus hints
                // Asegúrate de que tu proylcc.pl exporte get_hint/6 y lo implemente como se sugirió
                const queryHints = `findall(hint(Columna, Effects, FinalGrid), get_hint(${shootBlock}, ${gridS}, ${numOfColumns}, Columna, Effects, FinalGrid), HintsList)`;
                const response = await pengine.query(queryHints);

                if (response && response['HintsList']) {
                    response['HintsList'].forEach((hintTerm: PrologTerm) => {
                        const col = hintTerm.args[0] as number;
                        const effects = hintTerm.args[1] as EffectInfoTerm[];
                        const finalGridForHint = hintTerm.args[2] as Grid;
                        currentHints[col] = { grid: finalGridForHint, effects: effects };
                    });
                    setHintsData(currentHints);
                } else {
                    console.error("Error fetching hints or no hints returned.");
                    setHintsData(null);
                }
            } catch (error) {
                console.error("Error activating hint booster:", error);
                setHintsData(null);
            } finally {
                setWaiting(false); // Liberar el estado de espera
            }
        } else {
            setHintsData(null); // Si se desactiva (pasó de true a false), limpiar los datos del hint
        }
    }


    /**
     * Animates the sequence of game effects (shot, gravity, combinations).
     * Updates the grid and score based on each effect.
     * @param effects An array of effect terms from Prolog.
     */
    async function animateEffectsRecursive(effects: EffectTerm[]) {
        // Si no hay más efectos, termina la animación y habilita el disparo.
        if (effects.length === 0) {
            setWaiting(false);
            setComboMessage(null);
            return;
        }

        const currentEffect = effects[0];
        const [effectGrid, effectInfo] = currentEffect.args;
        // console.log("EFFECT INFO:", effectInfo); // Para depuración

        // --- Lógica para el score y el combo message para el efecto actual ---
        let scoreUpdateForThisStep = 0;
        let comboCountForThisStep = 0;
        let anyCombinationOccurred = false;

        // Iterar sobre todos los elementos de effectInfo para calcular score y combos.
        for (const item of effectInfo) {
            if (item.functor === 'combination') {
                const nuevoValorCombinacion = (item as CombinationTerm).args[2];
                const tamanioGrupo = (item as CombinationTerm).args[3];

                // Sumar directamente el valor del nuevo bloque generado
                scoreUpdateForThisStep += nuevoValorCombinacion;
                comboCountForThisStep++;
                anyCombinationOccurred = true;
            }
        }

        // --- Lógica para los mensajes de tu rama (bloques retirados, nuevo máximo, nuevo rango) ---
        for (const item of effectInfo) {
            if (
                item?.functor === 'limpieza_bloques_retirados' &&
                Array.isArray(item.args?.[0]) &&
                item.args[0].length > 0
            ) {
                const bloques = item.args[0];
                const textoBloques = bloques.join(', ');
                showTemporaryMessage(setRemovedMessage, `Se eliminaron los bloques retirados: ${textoBloques}`, REMOVED_DISPLAY_DURATION);
                await delay(300); // Un pequeño delay para que el mensaje sea visible antes de seguir
            }
        }

        const currentGridNumbers = grid!.filter(x => typeof x === "number") as number[];
        const previousMax = currentGridNumbers.length > 0 ? Math.max(...currentGridNumbers) : 0;
        
        const effectGridNumbers = effectGrid.filter(x => typeof x === "number") as number[];
        const currentMax = effectGridNumbers.length > 0 ? Math.max(...effectGridNumbers) : 0;

        if (currentMax > previousMax) {
            showTemporaryMessage(
                setMaxBlockMessage,
                `¡Nuevo máximo alcanzado: ${currentMax}!`,
                MAX_BLOCK_DISPLAY_DURATION
            );

            // Obtener y comparar rangos
            const prevRange = await getRangeForMax(previousMax);
            const newRange = await getRangeForMax(currentMax);
            const nuevosBloques = newRange.filter(b => !prevRange.includes(b));

            if (nuevosBloques.length > 0) {
                showTemporaryMessage(
                    setNewBlockRangeMessage,
                    `¡Nuevo rango desbloqueado! Ahora puedes disparar: ${newRange.join(', ')}`,
                    NEW_BLOCK_RANGE_DISPLAY_DURATION
                );
            }
            await delay(1000); // Un delay para que los mensajes de max/rango sean visibles
        }
        // ----------------------------------------------------------------------------------

        // Actualizar la grilla y el score
        setGrid(effectGrid);
        if (scoreUpdateForThisStep > 0) {
            setScore(prevScore => prevScore + scoreUpdateForThisStep);
        }

        // LÓGICA DE COMBO
        if (anyCombinationOccurred) {
            // Opción 1: Combo general si hubo más de una combinación en este effect
            if (comboCountForThisStep > 1) {
                showTemporaryMessage(setComboMessage, `COMBO x${comboCountForThisStep}`, COMBO_DISPLAY_DURATION);
            } else if (comboCountForThisStep === 1) {
                // Si solo hubo una combinación, pero es parte de un grupo grande (ej. 4 bloques),
                const firstCombination = effectInfo.find(item => item.functor === 'combination') as CombinationTerm;
                if (firstCombination && firstCombination.args[3] >= 3) {
                    showTemporaryMessage(setComboMessage, `COMBO x${firstCombination.args[3] - 1}`, COMBO_DISPLAY_DURATION);
                } else {
                    setComboMessage(null); // No hay combo "grande" ni múltiple
                }
            } else {
                setComboMessage(null); // No hubo ninguna combinación
            }
        } else {
            setComboMessage(null); // Si no hubo ninguna combinación en este efecto, asegúrate de que no haya mensaje de combo
        }

        await delay(DEFAULT_EFFECT_DELAY);

        const restEffects = effects.slice(1);
        animateEffectsRecursive(restEffects);
    }


    if (grid === null) {
        return null;
    }

    return (
        <div className="game">
            <div className="header">
                <div className="score">{score}</div>
            </div>

            {/* Mensajes de tu rama */}
            {comboMessage && (
                <div className="combo-message">
                    {comboMessage}
                </div>
            )}
            {removedMessage && (
                <div className="removed-message">
                    {removedMessage}
                </div>
            )}
            {maxBlockMessage && (
                <div className="max-block-message">{maxBlockMessage}</div>
            )}
            {newBlockRangeMessage && (
                <div className="new-block-range-message">{newBlockRangeMessage}</div>
            )}
            {/* Fin Mensajes de tu rama */}

            <Board
                grid={grid}
                numOfColumns={numOfColumns!}
                onLaneClick={handleLaneClick}
                // --- PROPS PARA BOOSTER HINT (de tu compañero) ---
                showHints={showHints}
                hintsData={hintsData}
                // ------------------------------------------------
            />

            <div className='footer'>
                {/* Botón del Booster 'Bloque Siguiente' y su información (de tu compañero) */}
                <div className="booster-controls">
                    <button
                        onClick={activateBooster}
                        className="booster-button"
                        disabled={!pengine || !grid || isBoosterActive} // Deshabilitar si ya está activo
                    >
                        Booster Bloque Siguiente
                    </button>
                    {isBoosterActive && (
                        <div className="booster-info">
                            Tiempo restante: {boosterTimeRemaining}s
                        </div>
                    )}
                </div>

                {/* --- BOTÓN Y ESTADO DEL BOOSTER HINT (de tu compañero) --- */}
                <div className="booster-controls"> {/* Puedes reutilizar la clase o crear una nueva */}
                    <button
                        onClick={activateHintBooster}
                        className="booster-button"
                        disabled={waiting || !pengine || !grid || shootBlock === null || numOfColumns === null}
                    >
                        Booster Hint
                    </button>
                    {showHints && <div className="booster-info">Pistas activas</div>}
                </div>
                {/* ------------------------------------------------------ */}

                <div className='blockShoot'>
                    {/* Mostrar el bloque actual */}
                    <Block value={shootBlock!} position={[0, 0]} />
                </div>
                {/* Mostrar el siguiente bloque del booster si está activo y hay un bloque siguiente (de tu compañero) */}
                {isBoosterActive && nextShootBlock !== null && (
                    <div className='blockShoot next-block-display'>
                        <Block value={nextShootBlock} position={[0, 0]} />
                    </div>
                )}
            </div>
        </div>
    );
}

export default Game;