import { useEffect, useState, useRef } from 'react';
import PengineClient, { PrologTerm } from './PengineClient';
import Board from './Board';
import Block from './Block';
import { delay } from './util';

export type Grid = (number | "-")[];

// --- INTERFACES DE PROLOG ---
interface EffectTerm extends PrologTerm {
  functor: "effect";
  args: [Grid, EffectInfoTerm[]];
}

interface DisparoTerm extends PrologTerm { functor: "disparo"; args: [number, number]; }
interface GravedadTerm extends PrologTerm { functor: "gravedad"; args: []; }

interface CombinationTerm extends PrologTerm {
  functor: "combination";
  args: [number[], number, number, number]; // NuevoValor es args[2], TamanioGrupo es args[3]
}

interface NewBlockTerm extends PrologTerm { functor: "newBlock"; args: [number]; }

type EffectInfoTerm = DisparoTerm | GravedadTerm | CombinationTerm | NewBlockTerm | PrologTerm;
// -------------------------------------------------------------------------------------

function Game() {
  const [pengine, setPengine] = useState<any>(null);
  const [grid, setGrid] = useState<Grid | null>(null);
  const [numOfColumns, setNumOfColumns] = useState<number | null>(null);
  const [score, setScore] = useState<number>(0);
  const [shootBlock, setShootBlock] = useState<number | null>(null);
  const [nextShootBlock, setNextShootBlock] = useState<number | null>(null); // Bloque para el booster
  const [isBoosterActive, setIsBoosterActive] = useState<boolean>(false);
  const [boosterTimeRemaining, setBoosterTimeRemaining] = useState<number>(0);
  const [waiting, setWaiting] = useState<boolean>(false);
  const [comboMessage, setComboMessage] = useState<string | null>(null);
  
  // Nuevo estado para la cola de bloques futuros
  const [futureBlocks, setFutureBlocks] = useState<number[]>([]); 

  const COMBO_DISPLAY_DURATION = 1500; // Duración del cartel de combo en ms
  const DEFAULT_EFFECT_DELAY = 500; // Retraso entre efectos generales
  const BOOSTER_DURATION_SECONDS = 5; // Duración del booster en segundos
  const FUTURE_BLOCKS_COUNT = 3; // Cuántos bloques futuros pre-generar (actual + booster + 1 de reserva)

  // Ref para almacenar el ID del intervalo del booster, para poder limpiarlo
  const boosterTimerRef = useRef<NodeJS.Timeout | null>(null);

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

  async function handleLaneClick(lane: number) {
    // Pre-condiciones para asegurar que el juego esté listo
    if (waiting || !grid || shootBlock === null || numOfColumns === null) {
      return;
    }

    const gridS = JSON.stringify(grid).replace(/"/g, '');
    // Llamamos a shoot/5, que solo devuelve los efectos y la grilla resultante
    const queryS = `shoot(${shootBlock}, ${lane}, ${gridS}, ${numOfColumns}, Effects), last(Effects, effect(RGrid,_))`;
    setWaiting(true);
    const response = await pengine.query(queryS); 
      
    if (response) {       
      // Animamos los efectos del juego
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
    * Animates the sequence of game effects (shot, gravity, combinations).
    * Updates the grid and score based on each effect.
    * @param effects An array of effect terms from Prolog.
    */
  async function animateEffectsRecursive(effects: EffectTerm[]) {
    // Si no hay más efectos, termina la animación y habilita el disparo.
    if (effects.length === 0) {
      setWaiting(false);
      setComboMessage(null); // Asegura que el mensaje de combo se oculte al finalizar completamente
      return;
    }

    const currentEffect = effects[0];    
    const [effectGrid, effectInfo] = currentEffect.args;

    // --- Lógica para el score y el combo message para el efecto actual ---
    let scoreUpdateForThisStep = 0;
    let comboCountForThisStep = 0;
    let anyCombinationOccurred = false; // Bandera para saber si hubo al menos 1 combinación

    // Iterar sobre todos los elementos de effectInfo para calcular score y combos.
    for (const item of effectInfo) {
      if (item.functor === 'combination') {
        const nuevoValorCombinacion = (item as CombinationTerm).args[2];
        const tamanioGrupo = (item as CombinationTerm).args[3];
        
        // Sumar directamente el valor del nuevo bloque generado
        scoreUpdateForThisStep += nuevoValorCombinacion;
        comboCountForThisStep++; // Contar esta combinación
        anyCombinationOccurred = true; // Al menos una combinación ocurrió
      } 
      // Puedes agregar aquí la lógica para 'newBlock' si quieres que sume puntos
      // else if (item.functor === 'newBlock') {
      //   scoreUpdateForThisStep += (item as NewBlockTerm).args[0];
      // }
    }

    // Actualizar la grilla y el score
    setGrid(effectGrid); 
    if (scoreUpdateForThisStep > 0) {
      setScore(prevScore => prevScore + scoreUpdateForThisStep);
    }

    // *** LÓGICA DE COMBO REVISADA ***
    // Muestra el mensaje de combo basado en si hubo combinaciones y el tamaño del grupo
    if (anyCombinationOccurred) {
        // Opción 1: Combo general si hubo más de una combinación en este effect
        if (comboCountForThisStep > 1) {
            setComboMessage(`COMBO x${comboCountForThisStep}`);
            // El mensaje se ocultará después de COMBO_DISPLAY_DURATION
            setTimeout(() => setComboMessage(null), COMBO_DISPLAY_DURATION);
        } else if (comboCountForThisStep === 1) {
            // Si solo hubo una combinación, pero es parte de un grupo grande (ej. 4 bloques),
            // podemos usar el tamanioGrupo del primer combo encontrado (o el único)
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
    // --- Fin Lógica de Combo Revisada ---

    // Esperar antes de animar el siguiente efecto en la secuencia
    await delay(DEFAULT_EFFECT_DELAY); 

    // Llamada recursiva para el resto de los efectos
    const restEffects = effects.slice(1);
    animateEffectsRecursive(restEffects);
  }

  if (grid === null) {
    return null;
  }
  // console.log("ComboMessage:", comboMessage); // Solo para depuración

  return (
    <div className="game">
      <div className="header">
        <div className="score">Score: {score}</div>
        {/* Botón del Booster y su información */}
        <button
          onClick={activateBooster}
          className="booster-button"
          // Puedes deshabilitar el botón si no hay pengine o grid
          disabled={!pengine || !grid} 
        >
          Activar Booster
        </button>
        {isBoosterActive && (
          <div className="booster-info">
            Tiempo restante: {boosterTimeRemaining}s
          </div>
        )}
      </div>

      {/* Cartel de Combo */}
      {comboMessage && (
        <div className="combo-message">
          {comboMessage}
        </div>
      )}

      <Board
        grid={grid}
        numOfColumns={numOfColumns!}
        onLaneClick={handleLaneClick}
      />

      <div className='footer'>
        <div className='blockShoot'>
          {/* Aquí se muestra el bloque actual a disparar */}
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
