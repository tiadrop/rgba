export class RGBA {
	readonly asBytes: Uint8ClampedArray;

	/**
	 * Create a RGBA instance from individual channel values
	 * @param red Value of Red channel (0..255)
	 * @param green Value of Green channel (0..255)
	 * @param blue Value of Blue channel (0..255)
	 * @param alpha Value of Alpha channel (0..255, default 255)
	 */
	constructor(red: number, green: number, blue: number, alpha?: number);
	/**
	 * Create a RGBA instance from a byte array of length 4
	 * @param bytes
	 */
	constructor(bytes: Uint8Array | Uint8ClampedArray);
	constructor(
		redOrValues: number | Uint8Array | Uint8ClampedArray,
		green: number = 0,
		blue: number = 0,
		alpha: number = 255
	) {
		if (typeof redOrValues == "number") {
			this.asBytes = new Uint8ClampedArray([redOrValues, green, blue, alpha]);
		} else {
			if (redOrValues.length !== 4)
				throw new Error(
					"Invalid colour data; typed arrays must contain 4 values"
				);
			this.asBytes =
				redOrValues instanceof Uint8ClampedArray
					? redOrValues
					: new Uint8ClampedArray(redOrValues);
		}
	}

	/**
	 * Read the Red channel (0..255)
	 */
	get redValue() {
		return this.asBytes[0];
	}
	/**
	 * Read the Green channel (0..255)
	 */
	get greenValue() {
		return this.asBytes[1];
	}
	/**
	 * Read the Blue channel (0..255)
	 */
	get blueValue() {
		return this.asBytes[2];
	}
	/**
	 * Read the Alpha channel (0..255)
	 */
	get alphaValue() {
		return this.asBytes[3];
	}

	/**
	 * Gets the hex code in the format #rrggbb (or #rrggbbaa where appropriate)
	 */
	get hexCode(): string {
		return (
			"#" +
			[...(this.asBytes[3] == 255 ? this.asBytes.slice(0, 3) : this.asBytes)]
				.map((n) => n.toString(16).padStart(2, "0"))
				.join("")
		);
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
		return new RGBA(
			this.redValue,
			this.greenValue,
			this.blueValue,
			this.alphaValue * alpha
		);
	}

	/**
	 * Returns a new RGBA by applying binary xor to RGB channels. The original (called object's) alpha will be retained and the result will be blended by secondary colour's alpha
	 * @param c
	 * @returns Result of XOR'd colour channels
	 */
	xor(c: RGBA) {
		return this.blend(
			new RGBA(
				this.redValue ^ c.redValue,
				this.greenValue ^ c.greenValue,
				this.blueValue ^ c.blueValue,
				this.alphaValue
			),
			c.alphaValue / 255
		);
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
		);
	}

	apply(c: RGBA) {
		const a = c.alphaValue / 255;
		return new RGBA(
			blendNumbers(this.redValue, c.redValue, a),
			blendNumbers(this.greenValue, c.greenValue, a),
			blendNumbers(this.blueValue, c.blueValue, a),
			blendNumbers(this.alphaValue, 255, a)
		);
	}

	/**
	 * The colour's luminance, using the luma709 formula
	 */
	get luma709() {
		const [r, g, b] = [
			this.redValue / 255,
			this.greenValue / 255,
			this.blueValue / 255
		];
		return 0.2126 * r + 0.7152 * g + 0.0722 * b;
	}

	/**
	 * The colour's lightness, using the bi-hexcone formula
	 */
	get lightness() {
		const max = Math.max(this.redValue, this.greenValue, this.blueValue) / 255;
		const min = Math.min(this.redValue, this.greenValue, this.blueValue) / 255;
		return (max + min) / 2;
	}

	/**
	 * The colour's hue angle, in radians
	 */
	get hue() {
		const [r, g, b] = [
			this.redValue / 255,
			this.greenValue / 255,
			this.blueValue / 255
		];
		let angle = 0;
		if (r > g && r > b) angle = (g - b) / (r - Math.min(g, b));
		if (g > r && g > b) angle = 2 + (b - r) / (g - Math.min(r, b));
		if (b > r && b > g) angle = 4 + (r - g) / (b - Math.min(r, g));
		while (angle < 0) angle += 6;
		return angle * (Math.PI / 3);
	}

	/**
	 * The colour's saturation value
	 */
	get saturation() {
		const [r, g, b] = [
			this.redValue / 255,
			this.greenValue / 255,
			this.blueValue / 255
		];
		const max = Math.max(r, g, b);
		const min = Math.min(r, g, b);
		const chroma = max - min;
		if (chroma == 0) return 0;
		return chroma / (1 - Math.abs(this.lightness * 2 - 1));
	}

	contrast(n: number) {
		return new RGBA(this.asBytes.map(channel => channel * n));
	}

	saturate(amount: number) {
		const mean = (this.redValue + this.greenValue + this.blueValue) / 3;
		const greyscale = new RGBA(mean, mean, mean, this.alphaValue);
		return greyscale.blend(this, amount);
	}

	shiftHue(angleDelta: Angle): RGBA
	shiftHue(angleDeltaRadians: number): RGBA
	shiftHue(delta: Angle | number): RGBA {
		const deltaRadians = typeof delta == "number"
			? delta
			: (angleAsDegrees(delta) * (Math.PI / 180));
		const hue = this.hue + deltaRadians;
		return RGBA.fromHSL(
			this.hue + deltaRadians,
			this.saturation,
			this.lightness,
			this.alphaValue
		)
	}	

	/**
	 * Compares two RGBA instances and returns `true` if the represented colours are identical
	 * @param c
	 * @returns Equality of two RGBA colours
	 */
	equals(c: RGBA) {
		return (
			this.redValue == c.redValue &&
			this.greenValue == c.greenValue &&
			this.blueValue == c.blueValue &&
			this.alphaValue == c.alphaValue
		);
	}

	/**
	 * Returns a new RGBA by inverting R, G and B channels, leaving A unchanged
	 * @returns Inverted colour
	 */
	invert() {
		return new RGBA(
			255 - this.redValue,
			255 - this.greenValue,
			255 - this.blueValue,
			this.alphaValue
		);
	}

	/**
	 * Returns a new RGBA by blending an instance with another
	 * @param target RGBA instance or `parse()`-compatible string
	 * @param targetBias Specifies how close to the target colour the result will be (0..1, default .5)
	 * @returns Blended RGBA colour
	 */
	blend(target: RGBA, targetBias?: number): RGBA
	blend(target: string, targetBias?: number): RGBA
	blend(target: RGBA | string, targetBias: number = 0.5): RGBA {
		if (typeof target == "string") target = RGBA.parse(target);
		if (targetBias == 0) return this;
		if (targetBias == 1) return target;
		return new RGBA(
			blendNumbers(this.redValue, target.redValue, targetBias),
			blendNumbers(this.greenValue, target.greenValue, targetBias),
			blendNumbers(this.blueValue, target.blueValue, targetBias),
			blendNumbers(this.alphaValue, target.alphaValue, targetBias)
		);
	}

	/**
	 * Creates an RGBA instance from a hex code, optionally prefixed with '#'
	 *
	 * Channels may be specified as single or double digit hexadecimal, and one, three or four channels may be specified; eg
	 * * `#f00` and `#ff0000` produce red (255, 0, 0)
	 * * `#f005` and `#ff000055` produce a translucent red (255, 0, 0, 85)
	 * * `#a` expands to `#aaaaaa` and `#ab` expands to `#ababab`
	 * @param code
	 * @returns RGBA from hex code
	 * @deprecated use RGBA.parse(code with # prefix)
	 */
	static fromHex(code: string) {
		// remove when private vv (exposed path removes #)
		if (code[0] == "#") code = code.substring(1);
		if (
			code.length < 1 ||
			code.length > 8 ||
			code.length == 5 ||
			code.length == 7 ||
			/[^a-zA-Z\d]/.test(code)
		)
			throw new Error(`Invalid hex colour code '${code}'`);

		if (code.length < 3) code = code + code + code; // 1 => 111, 12 => 121212
		if (code.length <= 4) {
			// 123 => 112233, 1234 => 11223344
			code = code
				.split("")
				.map((c) => c + c)
				.join("");
		}
		const red = parseInt(code[0] + code[1], 16);
		const green = parseInt(code[2] + code[3], 16);
		const blue = parseInt(code[4] + code[5], 16);
		const alpha = code.length == 6 ? 255 : parseInt(code[6] + code[7], 16);
		return new RGBA(red, green, blue, alpha);
	}

	/**
	 * Creates a colour representation using a CSS colour string
	 *
	 * Supports `#<hex>`, `rgb(r, g, b)`, `rgba(r, g, b, a)` and `hsl(h, s%, l%)`
	 * 
	 * Hex values may be #rgb, #rrggbb, #rgba, #rrggbbaa, #w or #ww
	 * @param cssValue
	 * @returns
	 */
	static parse(cssValue: string) {
		// #<hex>
		if (cssValue[0] == "#") return RGBA.fromHex(cssValue.substring(1));
		// css func?
		const funcMatch = cssValue.match(cssFuncRegex);
		if (!funcMatch) throw new Error("Unrecognised CSS colour string");
		// func name match
		const funcName = funcMatch[1] as keyof typeof cssFuncs;
		const args = parseArgStr(funcMatch[2]);
		if (!(funcName in cssFuncs)) throw new Error("Unknown CSS colour function");
		const func = cssFuncs[funcName];

		// find a signature with matching arg count
		const overload = func.find(f => {
			const numParams = Array.isArray(f.params) ? f.params.length : f.params;
			return numParams === args.length;
		});
		if (!overload) throw new Error("Invalid CSS colour function argument count");

		// validate args
		for (let i = 0; i < args.length; i++) {
			const arg = args[i];
			const validator = Array.isArray(overload.params)
				? overload.params[i]
				: validateNumberOrPercent;
			const result = validator(arg);
			if (!result.valid) {
				throw new Error("Invalid CSS colour argument: " + result.reason);
			}
		}
		try {
			return overload.parse(args);
		} catch (e: any) {
			const message = (typeof e == "object" && "message" in e) ? e.message : `${e}`;
			throw new Error("Failed to parse CSS colour: " + message);
		}
	}

	static fromHSL(angle: Angle, saturation: number, lightness: number, alpha?: number): RGBA;
	static fromHSL(radians: number, saturation: number, lightness: number, alpha?: number): RGBA;
	static fromHSL(
		hue: number | Angle,
		saturation: number,
		lightness: number,
		alpha: number = 1
	): RGBA {
		const hueDeg = (typeof hue == "number"
			? ((hue * 180 / Math.PI) % 360 + 360)
			: angleAsDegrees(hue)) % 360;

		if (saturation === 0) {
			const v = lightness * 255;
			return new RGBA(v, v, v, alpha * 255);
		}

		const chroma = (1 - Math.abs(2 * lightness - 1)) * saturation;
		const hPrime = hueDeg / 60;
		const x = chroma * (1 - Math.abs((hPrime % 2) - 1));
		const m = lightness - chroma / 2;

		let r1 = 0, g1 = 0, b1 = 0;
		if (hPrime < 1) [r1, g1, b1] = [chroma, x, 0];
		else if (hPrime < 2) [r1, g1, b1] = [x, chroma, 0];
		else if (hPrime < 3) [r1, g1, b1] = [0, chroma, x];
		else if (hPrime < 4) [r1, g1, b1] = [0, x, chroma];
		else if (hPrime < 5) [r1, g1, b1] = [x, 0, chroma];
		else[r1, g1, b1] = [chroma, 0, x];

		const r = (r1 + m) * 255;
		const g = (g1 + m) * 255;
		const b = (b1 + m) * 255;
		const a = alpha * 255;

		return new RGBA(r, g, b, a);
	}


	// may slow-deprecate in future if exported colour consts prove useful
	static black = new RGBA(0, 0, 0);
	static blue = new RGBA(0, 0, 255);
	static green = new RGBA(0, 255, 0);
	static cyan = new RGBA(0, 255, 255);
	static red = new RGBA(255, 0, 0);
	static magenta = new RGBA(255, 0, 255);
	static yellow = new RGBA(255, 255, 0);
	static white = new RGBA(255, 255, 255);
}

const cssFuncRegex = /^\s*(\w+)\(([^)]+)\)\s*$/;

function parseArgStr(s: string) {
	if (s.includes(",")) return s.split(",").map((s) => s.trim());
	const [beforeSlash, afterSlash] = s.split("/", 2).map(s => s.trim());
	if (afterSlash) return [
		...beforeSlash.split(/\s+/),
		afterSlash
	];
	return beforeSlash.split(/\s+/);
}

type CssFunc = {
	params: (((value: string) => ArgumentValidationResult)[]) | number;
	parse: (args: string[]) => RGBA;
}[];

const validateNumberOrPercent = (v: string) => {
	if (v[v.length - 1] == "%") {
		return isNaN(v.replace(/%$/, "") as any)
			? validationResult(false, "not a number")
			: validationResult(true);
	}
	return isNaN(v as any)
		? validationResult(false, "not a number")
		: validationResult(true);
};

const validateAngle = (v: string) => {
	return /^\d+(deg|rad|turn)?$/.test(v) ? validationResult(true) : validationResult(false, "not a valid angle");
}

type ArgumentValidationResult =
	{
		valid: true;
	}
	| {
		valid: false;
		reason: string;
	};

function validationResult(valid: true): ArgumentValidationResult;
function validationResult(
	valid: false,
	reason: string
): ArgumentValidationResult;
function validationResult(
	valid: boolean,
	reason: string = ""
): ArgumentValidationResult {
	return { valid, reason };
}

function percentToByte(v: string) {
	return percentToValue(v, 255);
}

function percentToValue(v: string, percentMultiplier: number = 1) {
	if (v[v.length - 1] == "%") {
		const num = Number(v.replace(/%$/, ""));
		return (num / 100) * percentMultiplier;
	}
	return Number(v);
}

const cssFuncs = {
	rgb: [
		{
			params: 3,
			parse: ([r, g, b]) =>
				new RGBA(percentToByte(r), percentToByte(g), percentToByte(b)),
		},
		{ // css rgb() accepts 4th alpha arg
			params: 4,
			parse: ([r, g, b, a]) =>
				new RGBA(
					percentToByte(r), // x% -> (x/100)*255, x -> x
					percentToByte(g),
					percentToByte(b),
					percentToValue(a) * 255 // x% -> (x/100)*255, x -> x*255
				),
		},

	],
	rgba: [
		{
			params: 4,
			parse: ([r, g, b, a]) =>
				new RGBA(
					percentToByte(r), // x% -> (x/100)*255, x -> x
					percentToByte(g),
					percentToByte(b),
					percentToValue(a) * 255 // x% -> (x/100)*255, x -> x*255
				),
		},
	],
	hsl: [
		{
			params: [
				validateAngle,
				validateNumberOrPercent,
				validateNumberOrPercent,
			],
			parse: ([h, s, l]) => RGBA.fromHSL(
				stringToAngle(h),
				percentToValue(s),
				percentToValue(l)
			)
		},
		{
			params: [
				validateAngle,
				validateNumberOrPercent,
				validateNumberOrPercent,
				validateNumberOrPercent,
			],
			parse: ([h, s, l, a]) => RGBA.fromHSL(
				stringToAngle(h),
				percentToValue(s),
				percentToValue(l),
				percentToValue(a)
			)
		},
	],

	hsla: [
		{
			params: [
				validateAngle,
				validateNumberOrPercent,
				validateNumberOrPercent,
				validateNumberOrPercent,
			],
			parse: ([h, s, l, a]) => RGBA.fromHSL(
				stringToAngle(h),
				percentToValue(s),
				percentToValue(l),
				percentToValue(a)
			)
		},
	]
} satisfies Record<string, CssFunc>;

const blendNumbers = (from: number, to: number, progress: number) => {
	return from + (to - from) * progress;
};

type ColourHelper = {
	[K in `x${string}`]: RGBA;
} & {
	/**
	 * Dynamic property: colours can be specified with `C.x<hexcode>`
	 * @example
	 * ```ts
	 * const theme = {
	 *   background: C.x222226,
	 *   text: C.xe8e8e8,
	 * }
	 * ```
	 */
	x___: RGBA;
	/**
	 * Creates a colour, represented as RGBA
	 * @param red Red (0..255)
	 * @param green Green (0..255)
	 * @param blue Blue (0..255)
	 * @returns
	 */
	rgb: (red: number, green: number, blue: number) => RGBA;
	/**
	 * Creates a colour, represented as RGBA
	 * @param red Red (0..255)
	 * @param green Green (0..255)
	 * @param blue Blue (0..255)
	 * @param alpha Alpha (normalised 0..1, per CSS rgba function)
	 * @returns
	 */
	rgba: (red: number, green: number, blue: number, alpha: number) => RGBA;
};

/**
 * ### Experimental; use at risk
 * A shortcut helper for specifying RGBA colour representations using CSS-like syntax
 */
export const C = new Proxy(
	{
		rgb: (r: number, g: number, b: number) => new RGBA(r, g, b),
		rgba: (r: number, g: number, b: number, a: number) => new RGBA(r, g, b, a),
	},
	{
		get: (base, prop) => {
			if (typeof prop != "string") return undefined;
			if (prop[0] == "x") return RGBA.parse("#" + prop.substring(1));
			if (prop in base) return base[prop as keyof typeof base];
			throw new Error("Invalid colour");
		},
	}
) as ColourHelper;

type Angle = {
	asDegrees: number;
} | {
	asRadians: number;
} | {
	asTurns: number;
};

function angleAsDegrees(angle: Angle) {
	if ("asDegrees" in angle) return angle.asDegrees;
	if ("asTurns" in angle) return angle.asTurns * 360;
	return angle.asRadians * (180 / Math.PI);
}

function stringToAngle(s: string): Angle {
	if (/^\d+$/.test(s)) return {asDegrees: Number(s)};
	const match = s.match(/^(\d+)(deg|rad|turn)/);
	if (match) {
		switch (match[2]) {
			case "deg": return {asDegrees: Number(match[1])};
			case "rad": return {asRadians: Number(match[1])};
			case "turn": return {asTurns: Number(match[1])};
		}
	}
	throw new Error("Unknown unit: " + s.replace(/^\d+/, ''));
}

export const egaPalette = [
	C.x000, C.x00a, C.x0a0, C.x0aa, C.xa00, C.xa0a, C.xaa0, C.xaaa, C.x555, C.x55f, C.x5f5, C.x5ff, C.xf55, C.xf5f, C.xff5, C.xfff,	
];

export const aliceblue = C.xf0f8ff;
export const antiquewhite = C.xfaebd7;
export const aqua = C.x00ffff;
export const aquamarine = C.x7fffd4;
export const azure = C.xf0ffff;
export const beige = C.xf5f5dc;
export const bisque = C.xffe4c4;
export const black = C.x000000;
export const blanchedalmond = C.xffebcd;
export const blue = C.x0000ff;
export const blueviolet = C.x8a2be2;
export const brown = C.xa52a2a;
export const burlywood = C.xdeb887;
export const cadetblue = C.x5f9ea0;
export const chartreuse = C.x7fff00;
export const chocolate = C.xd2691e;
export const coral = C.xff7f50;
export const cornflowerblue = C.x6495ed;
export const cornsilk = C.xfff8dc;
export const crimson = C.xdc143c;
export const cyan = C.x00ffff;
export const darkblue = C.x00008b;
export const darkcyan = C.x008b8b;
export const darkgoldenrod = C.xb8860b;
export const darkgray = C.xa9a9a9;
export const darkgreen = C.x006400;
export const darkgrey = C.xa9a9a9;
export const darkkhaki = C.xbdb76b;
export const darkmagenta = C.x8b008b;
export const darkolivegreen = C.x556b2f;
export const darkorange = C.xff8c00;
export const darkorchid = C.x9932cc;
export const darkred = C.x8b0000;
export const darksalmon = C.xe9967a;
export const darkseagreen = C.x8fbc8f;
export const darkslateblue = C.x483d8b;
export const darkslategray = C.x2f4f4f;
export const darkslategrey = C.x2f4f4f;
export const darkturquoise = C.x00ced1;
export const darkviolet = C.x9400d3;
export const deeppink = C.xff1493;
export const deepskyblue = C.x00bfff;
export const dimgray = C.x696969;
export const dimgrey = C.x696969;
export const dodgerblue = C.x1e90ff;
export const firebrick = C.xb22222;
export const floralwhite = C.xfffaf0;
export const forestgreen = C.x228b22;
export const fuchsia = C.xff00ff;
export const gainsboro = C.xdcdcdc;
export const ghostwhite = C.xf8f8ff;
export const goldenrod = C.xdaa520;
export const gold = C.xffd700;
export const gray = C.x808080;
export const green = C.x008000;
export const greenyellow = C.xadff2f;
export const grey = C.x808080;
export const honeydew = C.xf0fff0;
export const hotpink = C.xff69b4;
export const indianred = C.xcd5c5c;
export const indigo = C.x4b0082;
export const ivory = C.xfffff0;
export const khaki = C.xf0e68c;
export const lavenderblush = C.xfff0f5;
export const lavender = C.xe6e6fa;
export const lawngreen = C.x7cfc00;
export const lemonchiffon = C.xfffacd;
export const lightblue = C.xadd8e6;
export const lightcoral = C.xf08080;
export const lightcyan = C.xe0ffff;
export const lightgoldenrodyellow = C.xfafad2;
export const lightgray = C.xd3d3d3;
export const lightgreen = C.x90ee90;
export const lightgrey = C.xd3d3d3;
export const lightpink = C.xffb6c1;
export const lightsalmon = C.xffa07a;
export const lightseagreen = C.x20b2aa;
export const lightskyblue = C.x87cefa;
export const lightslategray = C.x778899;
export const lightslategrey = C.x778899;
export const lightsteelblue = C.xb0c4de;
export const lightyellow = C.xffffe0;
export const lime = C.x00ff00;
export const limegreen = C.x32cd32;
export const linen = C.xfaf0e6;
export const magenta = C.xff00ff;
export const maroon = C.x800000;
export const mediumaquamarine = C.x66cdaa;
export const mediumblue = C.x0000cd;
export const mediumorchid = C.xba55d3;
export const mediumpurple = C.x9370db;
export const mediumseagreen = C.x3cb371;
export const mediumslateblue = C.x7b68ee;
export const mediumspringgreen = C.x00fa9a;
export const mediumturquoise = C.x48d1cc;
export const mediumvioletred = C.xc71585;
export const midnightblue = C.x191970;
export const mintcream = C.xf5fffa;
export const mistyrose = C.xffe4e1;
export const moccasin = C.xffe4b5;
export const navajowhite = C.xffdead;
export const navy = C.x000080;
export const oldlace = C.xfdf5e6;
export const olive = C.x808000;
export const olivedrab = C.x6b8e23;
export const orange = C.xffa500;
export const orangered = C.xff4500;
export const orchid = C.xda70d6;
export const palegoldenrod = C.xeee8aa;
export const palegreen = C.x98fb98;
export const paleturquoise = C.xafeeee;
export const palevioletred = C.xdb7093;
export const papayawhip = C.xffefd5;
export const peachpuff = C.xffdab9;
export const peru = C.xcd853f;
export const pink = C.xffc0cb;
export const plum = C.xdda0dd;
export const powderblue = C.xb0e0e6;
export const purple = C.x800080;
export const rebeccapurple = C.x663399;
export const red = C.xff0000;
export const rosybrown = C.xbc8f8f;
export const royalblue = C.x4169e1;
export const saddlebrown = C.x8b4513;
export const salmon = C.xfa8072;
export const sandybrown = C.xf4a460;
export const seagreen = C.x2e8b57;
export const seashell = C.xfff5ee;
export const sienna = C.xa0522d;
export const silver = C.xc0c0c0;
export const skyblue = C.x87ceeb;
export const slateblue = C.x6a5acd;
export const slategray = C.x708090;
export const slategrey = C.x708090;
export const snow = C.xfffafa;
export const springgreen = C.x00ff7f;
export const steelblue = C.x4682b4;
export const tan = C.xd2b48c;
export const teal = C.x008080;
export const thistle = C.xd8bfd8;
export const tomato = C.xff6347;
export const turquoise = C.x40e0d0;
export const violet = C.xee82ee;
export const wheat = C.xf5deb3;
export const white = C.xffffff;
export const whitesmoke = C.xf5f5f5;
export const yellow = C.xffff00;
export const yellowgreen = C.x9acd32;
