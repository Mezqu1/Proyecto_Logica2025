import Block, { Position } from './Block';
import { Grid, EffectInfoTerm, CombinationTerm, EffectTerm, DisparoTerm } from './Game'; 
interface BoardProps {
    grid: Grid;
    numOfColumns: number;
    onLaneClick: (lane: number) => void;
    showHints: boolean; // Nueva prop
    hintsData: { [col: number]: { grid: Grid, effects: EffectInfoTerm[] } | null } | null;
}

function Board({ grid, numOfColumns, onLaneClick, showHints, hintsData }: BoardProps) {
    const numOfRows = grid.length / numOfColumns;
    // Función auxiliar para obtener el resumen del hint
    const getHintSummary = (col: number) => {
        if (!showHints || !hintsData) return null;
        const hint = hintsData[col];
        if (!hint) return null; // No hay hint para esta columna (ej. columna llena)

        const effectInfos = hint.effects.flatMap(e => (e as EffectTerm).args[1] || []);
        let summaryText = "";
        let totalValue = 0;
        let comboCount = 0;

        // Comprobar si la columna está llena (usando el nuevo functor)
        if (effectInfos.some(info => info.functor === 'columna_llena')) {
            return "Columna llena";
        }

       effectInfos.forEach(info => {
            if (info.functor === 'combination') {
                const nuevoValorCombinacion = (info as CombinationTerm).args[2];
                const tamanioGrupo = (info as CombinationTerm).args[3];
                totalValue += nuevoValorCombinacion; // Suma los valores generados
                comboCount++; // Incrementa el conteo de combinaciones
            }
        });

          if (comboCount > 0) {
                const firstCombination = effectInfos.find(item => item.functor === 'combination') as CombinationTerm;
                if (firstCombination) {
                    const tamanioGrupo = firstCombination.args[3];
                    summaryText = `COMBO x${tamanioGrupo -1} (+${totalValue} pts)`;
                }
            }
        return summaryText;
    };
    return (
        <div className="board">
            <div className="blocks" style={{ gridTemplateColumns: `repeat(${numOfColumns}, 70px)`, gridTemplateRows: `repeat(${numOfRows}, 70px)` }}>
                {Array.from({ length: numOfColumns }).map((_, i) => {
                    const hintText = getHintSummary(i + 1); // Obtener el resumen para esta columna
                    return (
                        <div
                            className='lane'
                            style={{ gridColumn: i + 1, gridRow: `1 / span ${numOfRows}` }}
                            onClick={() => onLaneClick(i + 1)}
                            key={i}
                        >
                            {showHints && hintText && (
                                <div className="hint-overlay">
                                    {hintText}
                                </div>
                            )}
                        </div>
                    );
                })}
                {grid.map((num, i) => {
                    if (num === "-") {
                        return null;
                    }
                    const pos: Position = [Math.floor(i / numOfColumns), i % numOfColumns];
                    return (
                        <Block
                            value={num}
                            position={pos}
                            key={i}
                        />
                    );
                })}
            </div>
        </div>
    );
}

export default Board;