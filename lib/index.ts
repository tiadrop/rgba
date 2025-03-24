export class RGBA {
    readonly asBytes: Uint8ClampedArray;

    constructor(red: number, green: number, blue: number, alpha?: number)
    constructor(bytes: Uint8Array | Uint8ClampedArray)
    constructor(redOrValues: number | Uint8Array | Uint8ClampedArray, green: number = 0, blue: number = 0, alpha: number = 255) {
        if (typeof redOrValues == "number") {
            this.asBytes = new Uint8ClampedArray([redOrValues, green, blue, alpha]);
        } else {
            if (redOrValues.length !== 4) throw new Error("Invalid colour data; typed arrays must contain 4 values");
            this.asBytes = redOrValues instanceof Uint8ClampedArray
                ? redOrValues
                : new Uint8ClampedArray(redOrValues);
        }
    }

    get red(){ return this.asBytes[0] }
    get green(){ return this.asBytes[1] }
    get blue(){ return this.asBytes[2] }
    get alpha(){ return this.asBytes[3] }

    get hexCode(): string {
        return "#" + [
            ...(
                this.asBytes[3] == 255
                    ? this.asBytes
                    : this.asBytes.slice(0, 3)
            )
        ].map(n => n.toString(16).padStart(2, "0")).join("");
    }

    toString() {
        return this.hexCode;
    }

    fade(alpha: number) {
        return new RGBA(this.red, this.green, this.blue, this.alpha * alpha);
    }

    xor(c: RGBA) {
        return this.blend(new RGBA(
            this.red ^ c.red,
            this.green ^ c.green,
            this.blue ^ c.blue,
            this.alpha
        ), c.alpha / 255);
    }

    add(c: RGBA) {
        const a = c.alpha / 255;
        return new RGBA(
            this.red + c.red * a,
            this.green + c.green * a,
            this.blue + c.blue * a,
            this.alpha
        )
    }

    equals(c: RGBA) {
        return this.red == c.red && this.green == c.green && this.blue == c.blue && this.alpha == c.alpha;
    }

    blend(target: RGBA, targetBias: number = .5) {
        if (targetBias == 0) return this;
        if (targetBias == 1) return target;
        return new RGBA(
            blendNumbers(this.red, target.red, targetBias),
            blendNumbers(this.green, target.green, targetBias),
            blendNumbers(this.blue, target.blue, targetBias),
            blendNumbers(this.alpha, target.alpha, targetBias),
        )
    }

    static fromHex(code: string) {
        if (code[0] == "#") code = code.substring(1);
        if (
            code.length < 1
            || code.length > 8
            || code.length == 5
            || code.length == 7
            || /[^a-zA-Z\d]/.test(code)
        ) throw new Error(`Invalid hex colour code '${code}`);

        if (code.length < 3) code = code + code + code; // 1 => 111, 12 => 121212
        if (code.length <= 4) { // 123 => 112233, 1234 => 11223344
            code = code.split("").map(c => c + c).join("");
        }
        const red = parseInt(code[0] + code[1], 16);
        const green = parseInt(code[2] + code[3], 16);
        const blue = parseInt(code[4] + code[5], 16);
        const alpha = code.length == 6 ? 255 : parseInt(code[6] + code[7], 16);
        return new RGBA(red, green, blue, alpha);
    }

    static black = new RGBA(0, 0, 0);
    static blue = new RGBA(0, 0, 255);
    static green = new RGBA(0, 255, 0);
    static cyan = new RGBA(0, 255, 255);
    static red = new RGBA(255, 0, 0);
    static magenta = new RGBA(255, 0, 255);
    static yellow = new RGBA(255, 255, 0);
    static white = new RGBA(255, 255, 255);
}

const blendNumbers = (from: number, to: number, progress: number) => {
    return from + (to - from) * progress;
};
