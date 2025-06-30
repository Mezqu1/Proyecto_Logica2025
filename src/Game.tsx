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
interface CombinationTerm extends PrologTerm {functor: "combination"; args: [number[], number, number, number]; }

interface NewBlockTerm extends PrologTerm { functor: "newBlock"; args: [number]; }

// Tipo de unión que incluye todos los posibles términos en effectInfo
type EffectInfoTerm = DisparoTerm | GravedadTerm | CombinationTerm | NewBlockTerm | PrologTerm;

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

    // 💡 Ocultar aviso de bloques retirados si existía
    //setRemovedMessage(null);

    const gridS = JSON.stringify(grid).replace(/"/g, '');
    const queryS = `shoot(${shootBlock}, ${lane}, ${gridS}, ${numOfColumns}, Effects), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block)`;
    setWaiting(true);
    const response = await pengine.query(queryS);    
    if (response) {      
      
      animateEffectsRecursive(response['Effects']); 
      setShootBlock(response['Block']);
    } else {
      setWaiting(false);
    }
  }
  
  
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

    /*/ --- Aviso de bloques eliminados correctamente filtrado ---
    for (const item of effectInfo) {
    if (
      item &&
      typeof item === 'object' &&
      'functor' in item &&
      item.functor === 'limpieza_bloques_retirados' &&
      Array.isArray(item.args) &&
      Array.isArray(item.args[0]) &&
      item.args[0].length > 0 // ✅ Solo si hay bloques retirados
    ) {
      const bloques = item.args[0];
      const textoBloques = bloques.join(', ');
      setRemovedMessage(`Se eliminaron los bloques retirados: ${textoBloques}`);
      setTimeout(() => setRemovedMessage(null), REMOVED_DISPLAY_DURATION);
    }
    }
    // Mostrar mensaje si hubo eliminación
    if (removedBlocksCount > 0) {
      setRemovedMessage(`Se eliminaron ${removedBlocksCount} bloque${removedBlocksCount > 1 ? 's' : ''}`);
      setTimeout(() => setRemovedMessage(null), REMOVED_DISPLAY_DURATION);
      await delay(DEFAULT_EFFECT_DELAY); 
    }
*/
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
}


    // Actualizar la grilla y el score
    setGrid(effectGrid); 
    if (scoreUpdateForThisStep > 0) {
      setScore(prevScore => prevScore + scoreUpdateForThisStep);
    }
   /*
    for (const item of effectInfo) {
  if (item && item.functor === 'gravedad') {
    if (removedMessageToShow) {
      setRemovedMessage(removedMessageToShow);
      setTimeout(() => setRemovedMessage(null), REMOVED_DISPLAY_DURATION);
    }
  }
}
*/

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