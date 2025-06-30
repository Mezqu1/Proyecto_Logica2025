import { useEffect, useState } from 'react';
import PengineClient, { PrologTerm } from './PengineClient';
import Board from './Board';
import Block from './Block';
import { delay } from './util';

export type Grid = (number | "-")[];

// --- INTERFACES DE PROLOG (Asegúrate de que estas estén completas como antes) ---
interface EffectTerm extends PrologTerm {
  functor: "effect";
  args: [Grid, EffectInfoTerm[]];
}

interface DisparoTerm extends PrologTerm { functor: "disparo"; args: [number, number]; }
interface GravedadTerm extends PrologTerm { functor: "gravedad"; args: []; }

// 'combination' tiene 4 argumentos: [GrupoCombinable, PosRes, NuevoValor, TamanioGrupo]
interface CombinationTerm extends PrologTerm {
  functor: "combination";
  args: [number[], number, number, number]; // NuevoValor es args[2], TamanioGrupo es args[3]
}

interface NewBlockTerm extends PrologTerm { functor: "newBlock"; args: [number]; }

// Tipo de unión que incluye todos los posibles términos en effectInfo
type EffectInfoTerm = DisparoTerm | GravedadTerm | CombinationTerm | NewBlockTerm | PrologTerm;
// -------------------------------------------------------------------------------------

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

  useEffect(() => {
    connectToPenginesServer();
  }, []);

  useEffect(() => {
    if (pengine) {
      initGame();
    }
  }, [pengine]);

  async function connectToPenginesServer() {
    setPengine(await PengineClient.create());
  }

  async function initGame() {
    const queryS = 'init(Grid, NumOfColumns), randomBlock(Grid, Block)';
    const response = await pengine!.query(queryS);
    setGrid(response['Grid']);
    setShootBlock(response['Block']);
    setNumOfColumns(response['NumOfColumns']);
  }

  async function handleLaneClick(lane: number) {
    if (waiting) {
      return;
    }
    const gridS = JSON.stringify(grid).replace(/"/g, '');
    const queryS = `shoot(${shootBlock}, ${lane}, ${gridS}, ${numOfColumns}, Effects), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block)`;
    setWaiting(true);
    const response = await pengine.query(queryS);    
    if (response) {      
      // Aquí llamamos a la función que anima la secuencia de efectos
      animateEffectsRecursive(response['Effects']); // Cambié el nombre a 'animateEffectsRecursive' para evitar confusiones
      setShootBlock(response['Block']);
    } else {
      setWaiting(false);
    }
  }
  
  /**
   * Esta función es la versión recursiva de la animación de efectos,
   * combinando tu estructura original con la lógica de score y combos.
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
        // Tu lógica previa de "COMBO xN" para grupos de 3 o más era:
        // if (tamanioGrupo >= 3) { setComboMessage(`COMBO x${tamanioGrupo - 1}`); }
        // Si tienes varias combinaciones en effectInfo, aquí podrías decidir:
        // 1. Mostrar un combo por CADA combinación individual (esto sería más complejo, requiriendo múltiples mensajes o un solo mensaje muy rápido)
        // 2. Mostrar un solo mensaje general si hubo más de una combinación en este 'effect' (como estaba antes)
        // 3. Mostrar un mensaje para cada combinación si su `tamanioGrupo` es >= 3.

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
  console.log("ComboMessage:", comboMessage); // Solo para depuración

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

    <Board
      grid={grid}
      numOfColumns={numOfColumns!}
      onLaneClick={handleLaneClick}
    />

    <div className='footer'>
      <div className='blockShoot'>
        <Block value={shootBlock!} position={[0, 0]} />
      </div>
    </div>
  </div>
);

}

export default Game;