"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.RGBA = void 0;
class RGBA {
    constructor(redOrValues, green = 0, blue = 0, alpha = 255) {
        if (typeof redOrValues == "number") {
            this.asBytes = new Uint8ClampedArray([redOrValues, green, blue, alpha]);
        }
        else {
            if (redOrValues.length !== 4)
                throw new Error("Invalid colour data; typed arrays must contain 4 values");
            this.asBytes = redOrValues instanceof Uint8ClampedArray
                ? redOrValues
                : new Uint8ClampedArray(redOrValues);
        }
    }
    get red() { return this.asBytes[0]; }
    get green() { return this.asBytes[1]; }
    get blue() { return this.asBytes[2]; }
    get alpha() { return this.asBytes[3]; }
    get hexCode() {
        return "#" + [
            ...(this.asBytes[3] == 255
                ? this.asBytes
                : this.asBytes.slice(0, 3))
        ].map(n => n.toString(16).padStart(2, "0")).join("");
    }
    toString() {
        return this.hexCode;
    }
    fade(alpha) {
        return new RGBA(this.red, this.green, this.blue, this.alpha * alpha);
    }
    xor(c) {
        return this.blend(new RGBA(this.red ^ c.red, this.green ^ c.green, this.blue ^ c.blue, this.alpha), c.alpha / 255);
    }
    add(c) {
        const a = c.alpha / 255;
        return new RGBA(this.red + c.red * a, this.green + c.green * a, this.blue + c.blue * a, this.alpha);
    }
    equals(c) {
        return this.red == c.red && this.green == c.green && this.blue == c.blue && this.alpha == c.alpha;
    }
    blend(target, targetBias = .5) {
        if (targetBias == 0)
            return this;
        if (targetBias == 1)
            return target;
        return new RGBA(blendNumbers(this.red, target.red, targetBias), blendNumbers(this.green, target.green, targetBias), blendNumbers(this.blue, target.blue, targetBias), blendNumbers(this.alpha, target.alpha, targetBias));
    }
    static fromHex(code) {
        if (code[0] == "#")
            code = code.substring(1);
        if (code.length < 1
            || code.length > 8
            || code.length == 5
            || code.length == 7
            || /[^a-zA-Z\d]/.test(code))
            throw new Error(`Invalid hex colour code '${code}`);
        if (code.length < 3)
            code = code + code + code;
        if (code.length <= 4) {
            code = code.split("").map(c => c + c).join("");
        }
        const red = parseInt(code[0] + code[1], 16);
        const green = parseInt(code[2] + code[3], 16);
        const blue = parseInt(code[4] + code[5], 16);
        const alpha = code.length == 6 ? 255 : parseInt(code[6] + code[7], 16);
        return new RGBA(red, green, blue, alpha);
    }
}
exports.RGBA = RGBA;
RGBA.black = new RGBA(0, 0, 0);
RGBA.blue = new RGBA(0, 0, 255);
RGBA.green = new RGBA(0, 255, 0);
RGBA.cyan = new RGBA(0, 255, 255);
RGBA.red = new RGBA(255, 0, 0);
RGBA.magenta = new RGBA(255, 0, 255);
RGBA.yellow = new RGBA(255, 255, 0);
RGBA.white = new RGBA(255, 255, 255);
const blendNumbers = (from, to, progress) => {
    return from + (to - from) * progress;
};
