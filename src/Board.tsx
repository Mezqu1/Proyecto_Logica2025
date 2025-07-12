import Block, { Position } from './Block';
import { Grid } from './Game'; // Ya no se necesita EffectInfoTerm, CombinationTerm, EffectTerm, DisparoTerm aquí para los hints

interface BoardProps {
    grid: Grid;
    numOfColumns: number;
    onLaneClick: (lane: number) => void;
    // showHints y hintsData props eliminadas
}

function Board({ grid, numOfColumns, onLaneClick }: BoardProps) {
    const numOfRows = grid.length / numOfColumns;
    
    // La función getHintSummary ha sido eliminada por completo.

    return (
        <div className="board">
            <div className="blocks" style={{ gridTemplateColumns: `repeat(${numOfColumns}, 70px)`, gridTemplateRows: `repeat(${numOfRows}, 70px)` }}>
                {Array.from({ length: numOfColumns }).map((_, i) => {
                    return (
                        <div
                            className='lane'
                            style={{ gridColumn: i + 1, gridRow: `1 / span ${numOfRows}` }}
                            onClick={() => onLaneClick(i + 1)}
                            key={i}
                        >
                            {/* La lógica de renderizado del hint-overlay ha sido eliminada por completo */}
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