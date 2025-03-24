export declare class RGBA {
    readonly asBytes: Uint8ClampedArray;
    constructor(red: number, green: number, blue: number, alpha?: number);
    constructor(bytes: Uint8Array | Uint8ClampedArray);
    get red(): number;
    get green(): number;
    get blue(): number;
    get alpha(): number;
    get hexCode(): string;
    toString(): string;
    fade(alpha: number): RGBA;
    xor(c: RGBA): RGBA;
    add(c: RGBA): RGBA;
    equals(c: RGBA): boolean;
    blend(target: RGBA, targetBias?: number): RGBA;
    static fromHex(code: string): RGBA;
    static black: RGBA;
    static blue: RGBA;
    static green: RGBA;
    static cyan: RGBA;
    static red: RGBA;
    static magenta: RGBA;
    static yellow: RGBA;
    static white: RGBA;
}
