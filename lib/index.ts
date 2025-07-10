export class RGBA {
    readonly data: Uint8ClampedArray;

    /**
     * Create a RGBA instance from individual channel values
     * @param red Value of Red channel (0..255)
     * @param green Value of Green channel (0..255)
     * @param blue Value of Blue channel (0..255)
     * @param alpha Value of Alpha channel (0..255, default 255)
     */
    constructor(red: number, green: number, blue: number, alpha?: number)
    /**
     * Create a RGBA instance from a byte array of length 4
     * @param bytes
     */
    constructor(bytes: Uint8Array | Uint8ClampedArray)
    constructor(redOrValues: number | Uint8Array | Uint8ClampedArray, green: number = 0, blue: number = 0, alpha: number = 255) {
        if (typeof redOrValues == "number") {
            this.data = new Uint8ClampedArray([redOrValues, green, blue, alpha]);
        } else {
            if (redOrValues.length !== 4) throw new Error("Invalid colour data; typed arrays must contain 4 values");
            this.data = redOrValues instanceof Uint8ClampedArray
                ? redOrValues
                : new Uint8ClampedArray(redOrValues);
        }
    }

    /**
     * Read the Red channel (0..255)
     */
    get redValue(){ return this.data[0] }
    /**
     * Read the Green channel (0..255)
     */
    get greenValue(){ return this.data[1] }
    /**
     * Read the Blue channel (0..255)
     */
    get blueValue(){ return this.data[2] }
    /**
     * Read the Alpha channel (0..255)
     */
    get alphaValue(){ return this.data[3] }

    /**
     * Gets the hex code in the format #rrggbb (or #rrggbbaa where appropriate)
     */
    get hexCode(): string {
        return "#" + [
            ...(
                this.data[3] == 255
                    ? this.data.slice(0, 3)
                    : this.data
            )
        ].map(n => n.toString(16).padStart(2, "0")).join("");
    }

    toString() {
        return this.hexCode;
    }

    /**
     * Returns a new RGBA by multiplying alpha with a given value
     * @param alpha Alpha multiplier
     * @returns Faded RGBA colour
     */
    fade(alpha: number) {
        return new RGBA(this.redValue, this.greenValue, this.blueValue, this.alphaValue * alpha);
    }

    /**
     * Returns a new RGBA by applying binary xor to RGB channels. The original (called object's) alpha will be retained and the result will be blended by secondary colour's alpha
     * @param c 
     * @returns Result of XOR'd colour channels
     */
    xor(c: RGBA) {
        return this.blend(new RGBA(
            this.redValue ^ c.redValue,
            this.greenValue ^ c.greenValue,
            this.blueValue ^ c.blueValue,
            this.alphaValue
        ), c.alphaValue / 255);
    }

    /**
     * Returns a new RGBA by adding RGB channels. The original (called object's) alpha will be retained and the result will be blended by secondary colour's alpha
     * @param c 
     * @returns Result of adding colour channels
     */
    add(c: RGBA) {
        const a = c.alphaValue / 255;
        return new RGBA(
            this.redValue + c.redValue * a,
            this.greenValue + c.greenValue * a,
            this.blueValue + c.blueValue * a,
            this.alphaValue
        )
    }

    /**
     * Compares two RGBA instances and returns `true` if the represented colours are identical
     * @param c 
     * @returns Equality of two RGBA colours
     */
    equals(c: RGBA) {
        return this.redValue == c.redValue && this.greenValue == c.greenValue && this.blueValue == c.blueValue && this.alphaValue == c.alphaValue;
    }

    /**
     * Returns a new RGBA by blending an instance with another
     * @param target 
     * @param targetBias Specifies how close to the target colour the result will be (0..1, default .5)
     * @returns Blended RGBA colour
     */
    blend(target: RGBA, targetBias: number = .5) {
        if (targetBias == 0) return this;
        if (targetBias == 1) return target;
        return new RGBA(
            blendNumbers(this.redValue, target.redValue, targetBias),
            blendNumbers(this.greenValue, target.greenValue, targetBias),
            blendNumbers(this.blueValue, target.blueValue, targetBias),
            blendNumbers(this.alphaValue, target.alphaValue, targetBias),
        )
    }

    /**
     * Creates an RGBA instance from a hex code, optionally prefixed with '#'
     * 
     * Channels may be specified as single or double digit hexadecimal, and one, three or four channels may be specified; eg
     * * `#f00` and `#ff0000` produce red (255, 0, 0)
     * * `#f005` and `#ff000055` produce a translucent red (255, 0, 0, 85)
     * * `#f` and `#ff` produce white (255, 255, 255)
     * @param code 
     * @returns RGBA from hex code
     */
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
