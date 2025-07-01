import { useEffect, useState, useRef } from 'react';
import PengineClient, { PrologTerm } from './PengineClient';
import Board from './Board';
import Block from './Block';
import { delay } from './util';

export type Grid = (number | "-")[];

// --- INTERFACES DE PROLOG ---
export interface EffectTerm extends PrologTerm { // Exportado
    functor: "effect";
    args: [Grid, EffectInfoTerm[]];
}

export interface CombinationTerm extends PrologTerm { // Exportado
    functor: "combination";
    args: [number[], number, number, number]; // NuevoValor es args[2], TamanioGrupo es args[3]
}

export interface NewBlockTerm extends PrologTerm { functor: "newBlock"; args: [number]; } // Exportado
export interface ColumnaLlenaTerm extends PrologTerm { functor: "columna_llena"; args: []; } // ¡NUEVA INTERFAZ EXPORTADA!
export interface DisparoTerm extends PrologTerm { functor: "disparo"; args: [number, number]; } // Exportado
export interface GravedadTerm extends PrologTerm { functor: "gravedad"; args: []; } // Exportado
export type EffectInfoTerm = DisparoTerm | GravedadTerm | CombinationTerm | NewBlockTerm | ColumnaLlenaTerm | PrologTerm; // Actualizado
// -------------------------------------------------------------------------------------

// Tipo de unión que incluye todos los posibles términos en effectInfo
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
  const COMBO_DISPLAY_DURATION = 1500; // Duración del cartel de combo en ms
  const DEFAULT_EFFECT_DELAY = 500; // Retraso entre efectos generales
  const [removedMessage, setRemovedMessage] = useState<string | null>(null);
  const REMOVED_DISPLAY_DURATION = 5000; // Duración del aviso de eliminación
  const [maxBlockMessage, setMaxBlockMessage] = useState<string | null>(null);
  const MAX_BLOCK_DISPLAY_DURATION = 3000; // 3 segundos
  const [newBlockRangeMessage, setNewBlockRangeMessage] = useState<string | null>(null);
  const NEW_BLOCK_RANGE_DISPLAY_DURATION = 4000; // o el tiempo que quieras
  const [nextShootBlock, setNextShootBlock] = useState<number | null>(null); // Bloque para el booster
  const [isBoosterActive, setIsBoosterActive] = useState<boolean>(false);
  const [boosterTimeRemaining, setBoosterTimeRemaining] = useState<number>(0);

// -------------------------------------------------------------------------------------

    // --- NUEVOS ESTADOS PARA EL BOOSTER HINT ---
    const [showHints, setShowHints] = useState<boolean>(false);
    const [hintsData, setHintsData] = useState<{ [col: number]: { grid: Grid, effects: EffectInfoTerm[] } | null } | null>(null);
    // -------------------------------------------

    // Nuevo estado para la cola de bloques futuros
    const [futureBlocks, setFutureBlocks] = useState<number[]>([]);
    const BOOSTER_DURATION_SECONDS = 5; // Duración del booster en segundos
    const FUTURE_BLOCKS_COUNT = 3; // Cuántos bloques futuros pre-generar (actual + booster + 1 de reserva)

    // Ref para almacenar el ID del intervalo del booster, para poder limpiarlo
    const boosterTimerRef = useRef<NodeJS.Timeout | null>(null);

  async function handleLaneClick(lane: number) {
   if (waiting || !grid || shootBlock === null || numOfColumns === null) {
            return;
        }

    // 💡 Ocultar aviso de bloques retirados si existía
    //setRemovedMessage(null);
// --- OCULTAR HINTS AL DISPARAR ---
        setShowHints(false);
        setHintsData(null);
        // ---------------------------------
    const gridS = JSON.stringify(grid).replace(/"/g, '');
    const queryS = `shoot(${shootBlock}, ${lane}, ${gridS}, ${numOfColumns}, Effects), last(Effects, effect(RGrid,_))`;
    setWaiting(true);
    const response = await pengine.query(queryS);    
    if (response) {      
      
      animateEffectsRecursive(response['Effects']); 
      // Obtener la grilla final después de todos los efectos para generar el siguiente bloque
      const finalGridAfterEffects = response['RGrid'];

      // Actualizar la cola de bloques futuros
      const newFutureBlocks = futureBlocks.slice(1); // Quitar el bloque que acabamos de disparar (shootBlock)
      // Generar UN nuevo bloque aleatorio basado en la grilla final y añadirlo al final de la cola
      const newRandomBlock = await fetchRandomBlocks(finalGridAfterEffects, 1);
      newFutureBlocks.push(newRandomBlock[0]);
      setFutureBlocks(newFutureBlocks); // Actualizar el estado con la nueva cola
      setShootBlock(newFutureBlocks[0]); // El nuevo bloque actual es el primer elemento de la cola
      if (newFutureBlocks.length > 1) {
        setNextShootBlock(newFutureBlocks[1]);
      } else {
                setNextShootBlock(null); // Si no hay un segundo bloque (ej. cola muy corta), el booster no muestra nada
            }

        } else {
            setWaiting(false); // Si no hay respuesta, terminar el estado de espera
        }
  }
  async function getRangeForMax(max: number): Promise<number[]> {
  const query = `range_for_max(${max}, Rango)`;
  const response = await pengine.query(query);
  return response["Rango"];
}

  
useEffect(() => {
        connectToPenginesServer();
    }, []);

    useEffect(() => {
        if (pengine) {
            initGame();
        }
    }, [pengine]);

    // Efecto para limpiar el temporizador del booster cuando el componente se desmonta
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

    

    /**
     * Activates the 'Next Block' booster.
     * Displays the next block to be shot for a limited time.
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
     */
    async function activateHintBooster() {
        if (waiting || !pengine || !grid || shootBlock === null || numOfColumns === null) {
            console.warn("No se puede activar el hint. Estado del juego no listo.");
            return;
        }

        setShowHints(prev => !prev); // Alternar visibilidad

        if (!showHints) { // Si vamos a mostrar los hints (pasó de false a true), hay que fetcharlos
            setWaiting(true); // Opcional: poner en estado de espera mientras se calculan los hints
            const currentHints: { [col: number]: { grid: Grid, effects: EffectInfoTerm[] } | null } = {};
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
    console.log("EFFECT INFO:", effectInfo);
   
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
   
   // Aviso de bloques eliminados
    let removedBlocksCount = 0;
    let removedMessageToShow: string | null = null;

for (const item of effectInfo) {
  if (
    item?.functor === 'limpieza_bloques_retirados' &&
    Array.isArray(item.args?.[0]) &&
    item.args[0].length > 0
  ) {
    const bloques = item.args[0];
    const textoBloques = bloques.join(', ');
    showTemporaryMessage(setRemovedMessage, `Se eliminaron los bloques retirados: ${textoBloques}`, REMOVED_DISPLAY_DURATION);
    await delay(1000); // para que el usuario lo vea antes de continuar
  }
}
const previousMax = Math.max(...(grid!.filter(x => typeof x === "number") as number[]));
const currentMax = Math.max(...(effectGrid.filter(x => typeof x === "number") as number[]));

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
}

// Actualizar la grilla y el score
    setGrid(effectGrid); 
    if (scoreUpdateForThisStep > 0) {
      setScore(prevScore => prevScore + scoreUpdateForThisStep);
    }
    // *** LÓGICA DE COMBO REVISADA ***
    if (anyCombinationOccurred) {
        // Opción 1: Combo general si hubo más de una combinación en este effect
        if (comboCountForThisStep > 1) {
            setComboMessage(`COMBO x${comboCountForThisStep}`);
            
            setTimeout(() => setComboMessage(null), COMBO_DISPLAY_DURATION);
        } else if (comboCountForThisStep === 1) {
            // Si solo hubo una combinación, pero es parte de un grupo grande (ej. 4 bloques),
          
            const firstCombination = effectInfo.find(item => item.functor === 'combination') as CombinationTerm;
            if (firstCombination && firstCombination.args[3] >= 3) {
                   setComboMessage(`COMBO x${firstCombination.args[3] - 1}`);
                   setTimeout(() => setComboMessage(null), COMBO_DISPLAY_DURATION);
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
    // console.log("Co☺mboMessage:", comboMessage); // Solo para depuración
    return (
        <div className="game">
            <div className="header">
                <div className="score">{score}</div>
            </div>

            {/* Cartel de Combo */}
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
            <Board
                grid={grid}
                numOfColumns={numOfColumns!}
                onLaneClick={handleLaneClick}
                // --- PROPS PARA BOOSTER HINT ---
                showHints={showHints}
                hintsData={hintsData}
                // --------------------------------
            />

            <div className='footer'>
                {/* Botón del Booster 'Bloque Siguiente' y su información */}
                <div className="booster-controls">
                    <button
                        onClick={activateBooster}
                        className="booster-button"
                        disabled={!pengine || !grid}
                    >
                        Booster bloque siguiente
                    </button>
                    {isBoosterActive && (
                        <div className="booster-info">
                            Tiempo restante: {boosterTimeRemaining}s
                        </div>
                    )}
                </div>

                {/* --- BOTÓN Y ESTADO DEL BOOSTER HINT --- */}
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
                {/* -------------------------------------- */}

                <div className='blockShoot'>
                    <Block value={shootBlock!} position={[0, 0]} />
                </div>
                {/* Mostrar el siguiente bloque del booster si está activo y hay un bloque siguiente */}
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