export function numberToColor(num: number) {
    switch (num) {
        case 2: return "#0083bb";
        case 4: return "#fc7f40";
        case 8: return "#e6538a";
        case 16: return "#058555";
        case 32: return "#b82bfa";
        case 64: return "#a24e78";
        case 128: return "#b9c508";
        case 256: return "#2196F3";
        case 512: return "#795548";
        case 1024: return "#FFC107";
        case 2048: return "#E91E63";
        case 4096: return "#ff0000ff"
        default: return "black";
    }
}

export const delay = (ms: number) => new Promise(res => setTimeout(res, ms));
