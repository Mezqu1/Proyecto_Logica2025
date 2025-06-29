import { useEffect, useState } from 'react';
import PengineClient, { PrologTerm } from './PengineClient';
import Board from './Board';
import Block from './Block';
import { delay } from './util';

export type Grid = (number | "-")[];
interface EffectTerm extends PrologTerm {
  functor: "effect";
  args: [Grid, EffectInfoTerm[]];
}

type EffectInfoTerm = NewBlockTerm | PrologTerm;
interface NewBlockTerm extends PrologTerm {
  functor: "newBlock";
  args: [number];
}

function Game() {

  // State
  const [pengine, setPengine] = useState<any>(null);
  const [grid, setGrid] = useState<Grid | null>(null);
  const [numOfColumns, setNumOfColumns] = useState<number | null>(null);
  const [score, setScore] = useState<number>(0);
  const [shootBlock, setShootBlock] = useState<number | null>(null);
  const [waiting, setWaiting] = useState<boolean>(false);
  const [comboMessage, setComboMessage] = useState<string | null>(null);
  const [retiredBlocksMessage, setRetiredBlocksMessage] = useState<string | null>(null);


  useEffect(() => {
    // This is executed just once, after the first render.
    connectToPenginesServer();
  }, []);

  useEffect(() => {
    if (pengine) {
      // This is executed after pengine was set.
      initGame();
    }
  }, [pengine]);

  async function connectToPenginesServer() {
    setPengine(await PengineClient.create()); // Await until the server is initialized
  }

  async function initGame() {
    const queryS = 'init(Grid, NumOfColumns), randomBlock(Grid, Block)';
    const response = await pengine!.query(queryS);
    setGrid(response['Grid']);
    setShootBlock(response['Block']);
    setNumOfColumns(response['NumOfColumns']);
  }

  /**
   * Called when the player clicks on a lane.
   */
  async function handleLaneClick(lane: number) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    /*
    Build Prolog query, which will be something like:
    shoot(2, 2, [4,2,8,64,32,2,-,-,4,16,-,-,-,-,2,-,-,-,-,16,-,-,-,-,2,-,-,-,-,-,-,-,-,-,-], 5, Effects), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block).
    */
    const gridS = JSON.stringify(grid).replace(/"/g, '');
    const queryS = `shoot(${shootBlock}, ${lane}, ${gridS}, ${numOfColumns}, Effects), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block)`;
    setWaiting(true);
    const response = await pengine.query(queryS);    
    if (response) {      
      animateEffect(response['Effects']);
      setShootBlock(response['Block']);
    } else {
      setWaiting(false);
    }
  }
  
  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals, and considers the other effect information.
   * @param effects The list of effects to be animated.
   
  async function animateEffect(effects: EffectTerm[]) {
    const effect = effects[0];    
    const [effectGrid, effectInfo] = effect.args;
    setGrid(effectGrid);
    effectInfo.forEach((effectInfoItem) => {
      const { functor, args } = effectInfoItem;
      switch (functor) {
        case 'newBlock':
          setScore(score => score + args[0]);
          break;
        default:
          break;
      }
    });
    const restRGrids = effects.slice(1);
    if (restRGrids.length === 0) {
      setWaiting(false);
      return;
    }
    await delay(1000);
    animateEffect(restRGrids);
  }
    

  async function animateEffect(effects: EffectTerm[]) {
  console.log("Efectos recibidos:", effects);

  // Calcular el total de combinaciones en todos los efectos:
  const totalCombos = effects.reduce((acc, effect) => {
    const combos = effect.args[1]; // effectInfoTerm[]
    return acc + combos.length;
  }, 0);

  if (totalCombos > 1) {
    setComboMessage(`Combo x ${totalCombos}`);
    setTimeout(() => setComboMessage(null), 1500);
  } else {
    // Opcional: si solo hay 1 combo, podés mostrar o no el mensaje
    setComboMessage(null);
  }

  const effect = effects[0];    
  const [effectGrid, effectInfo] = effect.args;
  setGrid(effectGrid);

  effectInfo.forEach((effectInfoItem) => {
    const { functor, args } = effectInfoItem;
    if (functor === 'newBlock') {
      setScore(score => score + args[0]);
    }
  });

  const restRGrids = effects.slice(1);
  if (restRGrids.length === 0) {
    setWaiting(false);
    return;
  }

  await delay(1000);
  animateEffect(restRGrids);
}
*/
async function animateEffect(effects: EffectTerm[]) {
  console.log("Efectos recibidos:", effects);

  if (effects.length > 0) {
    // Solo la primera efecto (generalmente las combinaciones)
    const firstEffect = effects[0];
    const effectInfo = firstEffect.args[1];
    // Filtramos solo combinaciones
    const combosCount = effectInfo.filter(info => info.functor === "combination").length;

    if (combosCount > 1) {
      setComboMessage(`Combo x ${combosCount}`);
      setTimeout(() => setComboMessage(null), 1500);
    } else {
      setComboMessage(null);
    }
  }

  // Luego el resto del código sigue igual...
  for (const effect of effects) {
    const [effectGrid, effectInfo] = effect.args;
    setGrid(effectGrid);

    const retiredEffect = effectInfo.find(info => info.functor === "retiredBlocks");
    if (retiredEffect) {
      const blocksRetirados = retiredEffect.args[0];
      setRetiredBlocksMessage(`Bloques retirados: ${blocksRetirados.join(", ")}`);
      setTimeout(() => setRetiredBlocksMessage(null), 3000);
    }

    effectInfo.forEach(({functor, args}) => {
      if (functor === 'newBlock') {
        setScore(score => score + args[0]);
      }
    });

    await delay(1000);
  }

  setWaiting(false);
}




  if (grid === null) {
    return null;
  }
  console.log("ComboMessage:", comboMessage);
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
    {/* Cartel de Bloques Retirados */}
    {retiredBlocksMessage && (
      <div className="retired-blocks-message">
        {retiredBlocksMessage}
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