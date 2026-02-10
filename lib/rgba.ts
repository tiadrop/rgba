const hexLookup = new Array(256);
for (let i = 0; i < 256; i++) {
	hexLookup[i] = i.toString(16).padStart(2, "0");
}

export class RGBA {
	/**
	 * The object's primary channel storage
	 * **Warning** - mutating this will corrupt the colour
	 */
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
		alpha: number = 255,
	) {
		if (typeof redOrValues == "number") {
			this.asBytes = new Uint8ClampedArray([redOrValues, green, blue, alpha]);
		} else {
			if (redOrValues.length !== 4)
				throw new Error(
					"Invalid colour data; typed arrays must contain 4 values",
				);
			this.asBytes =
				redOrValues instanceof Uint8ClampedArray ? redOrValues : (
					new Uint8ClampedArray(redOrValues)
				);
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
	 * Gets the colour's hex code in the format #rrggbb[aa]
	 */
	get hexCode(): string {
		const channels = this.asBytes;
		return channels[3] == 255
			? ("#" + hexLookup[channels[0]] + hexLookup[channels[1]] + hexLookup[channels[2]])
			: ("#" + hexLookup[channels[0]] + hexLookup[channels[1]] + hexLookup[channels[2]] + hexLookup[channels[3]])
	}

	toString() {
		return this.hexCode;
	}
	toJSON() {
		return this.hexCode;
	}

	/**
	 * Returns a new RGBA by multiplying alpha with a given value
	 * @param alpha Alpha multiplier
	 * @returns Faded RGBA colour
	 */
	fade(alpha: number) {
		const bytes = this.asBytes;
		return new RGBA(
			bytes[0],
			bytes[1],
			bytes[2],
			bytes[3] * alpha,
		);
	}

	/**
	 * The colour's luminance, using the luma709 formula
	 */
	get luma709() {
		const {red, green, blue} = this.asFloats;
		return 0.2126 * red + 0.7152 * green + 0.0722 * blue;
	}

	/**
	 * The colour's lightness, using the bi-hexcone formula
	 */
	get lightness() {
		const [red, green, blue] = this.asBytes;
		const max = Math.max(red, green, blue) / 255;
		const min = Math.min(red, green, blue) / 255;
		return (max + min) / 2;
	}

	/**
	 * The colour's hue angle, in radians
	 */
	get hue() {
		const {red, green, blue} = this.asFloats;
		
		const max = Math.max(red, green, blue);
		const min = Math.min(red, green, blue);
		const delta = max - min;
		
		if (delta === 0) return 0;
		
		let h = 0;
		if (max === red) {
			h = ((green - blue) / delta) % 6;
		} else if (max === green) {
			h = (blue - red) / delta + 2;
		} else { // max === b
			h = (red - green) / delta + 4;
		}
		
		h *= Math.PI / 3; // to radians
		if (h < 0) h += TAU;
		return h;
	}

	/**
	 * The colour's saturation value
	 */
	get saturation() {
		const {red, green, blue} = this.asFloats;
		const max = Math.max(red, green, blue);
		const min = Math.min(red, green, blue);
		const chroma = max - min;
		if (chroma == 0) return 0;
		return chroma / (1 - Math.abs(this.lightness * 2 - 1));
	}

	applyContrast(n: number) {
		return new RGBA(
			this.redValue * n,
			this.greenValue * n,
			this.blueValue * n,
			this.alphaValue
		);
	}

	saturate(amount: number) {
		const h = this.hue;
		const s = this.saturation;
		const l = this.lightness;

		const newSat = Math.max(0, Math.min(1, s * amount));
		return RGBA.fromHSL(h, newSat, l, this.alphaValue / 255);
	}

	shiftHue(angleDelta: Angle): RGBA;
	shiftHue(angleDeltaRadians: number): RGBA;
	shiftHue(delta: Angle | number): RGBA {
		const deltaRadians = angleAsRadians(delta);
		return RGBA.fromHSL(
			this.hue + deltaRadians,
			this.saturation,
			this.lightness,
			this.alphaValue,
		);
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

	private _inverted: RGBA | null = null;
	get inverted(): RGBA {
		const bytes = this.asBytes;
		if (!this._inverted) {
			this._inverted = new RGBA(
				255 - bytes[0],
				255 - bytes[1],
				255 - bytes[2],
				bytes[3],
			);
		}
		return this._inverted;
	}

	replace(channels: {
		red?: number;
		green?: number;
		blue?: number;
		alpha?: number;
	}) {
		const [r, g, b, a] = this.asBytes;
		return new RGBA(
			channels.red ?? r,
			channels.green ?? g,
			channels.blue ?? b,
			channels.alpha ?? a,
		);
	}

	/**
	 * Returns a new RGBA by blending an instance with another
	 * @param target RGBA instance or `parse()`-compatible string
	 * @param targetBias Specifies how close to the target colour the result will be (0..1, default .5)
	 * @returns Blended RGBA colour
	 */
	blend(target: RGBA, targetBias?: number): RGBA;
	blend(target: string, targetBias?: number): RGBA;
	blend(target: RGBA | string, targetBias: number = 0.5): RGBA {
		if (typeof target == "string") target = RGBA.parse(target);
		if (targetBias == 0) return this;
		if (targetBias == 1) return target;
		const [sr, sg, sb, sa] = this.asBytes;
		const [tr, tg, tb, ta] = target.asBytes;
		return new RGBA(
			blendNumbers(sr, tr, targetBias),
			blendNumbers(sg, tg, targetBias),
			blendNumbers(sb, tb, targetBias),
			blendNumbers(sa, ta, targetBias),
		);
	}

	private _floats: null | {
		readonly red: number;
		readonly green: number;
		readonly blue: number;
		readonly alpha: number;
	} = null;
	get asFloats() {
		if (!this._floats) {
			const [r, g, b, a] = this.asBytes;
			this._floats = {
				red: r / 255,
				green: g / 255,
				blue: b / 255,
				alpha: a / 255,
			};
		}
		return this._floats;
	}

	private _linear: {
		readonly red: number;
		readonly green: number;
		readonly blue: number;
	} | null = null;
	get linearRGB() {
		if (!this._linear) {
			const { red, green, blue } = this.asFloats;
			this._linear = {
				red:
					red <= 0.04045 ? red / 12.92 : Math.pow((red + 0.055) / 1.055, 2.4),
				green:
					green <= 0.04045 ?
						green / 12.92
						: Math.pow((green + 0.055) / 1.055, 2.4),
				blue:
					blue <= 0.04045 ?
						blue / 12.92
						: Math.pow((blue + 0.055) / 1.055, 2.4),
			};
		}
		return this._linear;
	}

	private _vec3: {
		readonly x: number;
		readonly y: number;
		readonly z: number;
	} | null = null;
	get asVec3() {
		if (!this._vec3) {
			const {red, green, blue} = this.linearRGB;
			const x =
				red * 0.4124564 +
				green * 0.3575761 +
				blue * 0.1804375;
			const y =
				red * 0.2126729 +
				green * 0.7151522 +
				blue * 0.072175;
			const z =
				red * 0.0193339 +
				green * 0.119192 +
				blue * 0.9503041;
			this._vec3 = { x, y, z };
		}
		return this._vec3;
	}

	private _lab: {
		readonly l: number;
		readonly a: number;
		readonly b: number;
	} | null = null;
	get asLAB() {
		if (!this._lab) {
			const { x, y, z } = this.asVec3;

			const refX = 0.95047;
			const refY = 1.0;
			const refZ = 1.08883;

			const fx = x / refX;
			const fy = y / refY;
			const fz = z / refZ;

			const epsilon = 0.008856; // 216/24389
			const kappa = 903.3;

			const f = (t: number) =>
				t > epsilon ? Math.cbrt(t) : (kappa * t + 16) / 116;

			const l = fy > epsilon ? 116 * f(fy) - 16 : kappa * fy;
			const a = 500 * (f(fx) - f(fy));
			const b = 200 * (f(fy) - f(fz));

			this._lab = { l, a, b };
		}
		return this._lab;
	}

	private _cmyk: { cyan: number, magenta: number, yellow: number, black: number } | null = null;
	get asCMYK() {
		if (!this._cmyk) {
			const { red, green, blue } = this.asFloats;

			const k = 1 - Math.max(red, green, blue);

			let c = 0, m = 0, y = 0;
			if (k < 1) {
				c = (1 - red - k) / (1 - k);
				m = (1 - green - k) / (1 - k);
				y = (1 - blue - k) / (1 - k);
			}

			this._cmyk = {
				cyan: c,
				magenta: m,
				yellow: y,
				black: k
			};
		}
		return this._cmyk;
	}

	apply(
		c: RGBA,
		mode:
			| "draw"
			| "multiply"
			| "screen"
			| "overlay"
			| "difference"
			| "xor"
			| "add"
			| "exclusion" = "draw",
	) {
		switch (mode) {
			case "draw":
				const selfBytes = this.asBytes;
				const brushBytes = c.asBytes;
				const a = brushBytes[3] / 255;
				return new RGBA(
					blendNumbers(selfBytes[0], brushBytes[0], a),
					blendNumbers(selfBytes[1], brushBytes[1], a),
					blendNumbers(selfBytes[2], brushBytes[2], a),
					blendNumbers(selfBytes[3], 255, a),
				);
			case "multiply":
				return transformChannelsFloat(this, c, (a, b) => a * b);
			case "screen":
				return transformChannelsFloat(this, c, (a, b) => 1 - (1 - a) * (1 - b));
			case "overlay":
				return transformChannelsFloat(this, c, (a, b) =>
					a < 0.5 ? 2 * a * b : 1 - 2 * (1 - a) * (1 - b),
				);
			case "difference":
				return transformChannelsByte(this, c, (a, b) => Math.abs(a - b));
			case "exclusion":
				return transformChannelsFloat(this, c, (a, b) => a + b - 2 * a * b);
			case "add":
				return transformChannelsByte(this, c, (a, b) => a + b);
			case "xor":
				return transformChannelsByte(this, c, (a, b) => a ^ b);
			default:
				throw new Error(`Unsupported blend mode '${mode}'`);
		}
	}

	/**
	 * Perform XOR operation
	 * @param other
	 * @returns XOR'd colour
	 * @deprecated use `rgba.apply(other, "xor")`
	 */
	xor(other: RGBA) {
		return this.apply(other, "xor");
	}

	/**
	 * Perform Add operation
	 * @param other
	 * @returns Added colour
	 * @deprecated use `rgba.apply(other, "add")`
	 */
	add(other: RGBA) {
		return this.apply(other, "add");
	}

	distance(target: RGBA, metric: "euclidean" | "cie76" = "euclidean") {
		switch (metric) {
			case "euclidean": {
				const dr = this.redValue - target.redValue;
				const dg = this.greenValue - target.greenValue;
				const db = this.blueValue - target.blueValue;
				const da = this.alphaValue - target.alphaValue;
				return Math.sqrt(dr * dr + dg * dg + db * db + da * da);
			}
			case "cie76": {
				const lab1 = this.asLAB;
				const lab2 = target.asLAB;
				const dl = lab1.l - lab2.l;
				const da = lab1.a - lab2.a;
				const db = lab1.b - lab2.b;
				return Math.sqrt(dl * dl + da * da + db * db);
			}
			default:
				throw new Error(`Unsupported metric: ${metric}`);
		}
	}

	/**
	 * Find the perceptually nearest colour in a palette
	 * @param palette Array of RGBA colours to search
	 * @param metric Distance metric (default: 'euclidean')
	 * @returns The nearest colour from the palette
	 */
	nearest(palette: RGBA[], metric: "euclidean" | "cie76" = "euclidean"): RGBA {
		if (palette.length === 0) {
			throw new RangeError("Cannot find nearest colour in empty palette");
		}

		return palette.reduce(
			(nearest, current) => {
				const distance = this.distance(current, metric);
				return distance < nearest.distance ?
					{ colour: current, distance }
					: nearest;
			},
			{ colour: palette[0], distance: this.distance(palette[0], metric) },
		).colour;
	}

	/**
	 * Creates an RGBA instance from a hex code, optionally prefixed with '#'
	 *
	 * Channels may be specified as single or double digit hexadecimal, and one, three or four channels may be specified; eg
	 * * `#f00` and `#ff0000` produce red (255, 0, 0)
	 * * `#f005` and `#ff000055` produce a translucent red (255, 0, 0, 85)
	 * * `#c` expands to `#cccccc` and `#cd` expands to `#cdcdcd`
	 * @param code
	 * @returns RGBA from hex code
	 * @deprecated use RGBA.parse(code with # prefix)
	 */
	static fromHex(code: string) {
		if (code[0] == "#") code = code.substring(1);
		return fromHex(code);
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
		if (cssValue[0] == "#") return fromHex(cssValue.substring(1));
		// css func?
		const funcMatch = cssValue.match(cssFuncRegex);
		if (!funcMatch) throw new SyntaxError("Unrecognised CSS colour string");
		// func name match
		const funcName = funcMatch[1] as keyof typeof cssFuncs;
		const args = parseArgStr(funcMatch[2]);
		if (!(funcName in cssFuncs)) throw new Error("Unknown CSS colour function");
		const func = cssFuncs[funcName];

		// find a signature with matching arg count
		const overload = func.find((f) => {
			const numParams = Array.isArray(f.params) ? f.params.length : f.params;
			return numParams === args.length;
		});
		if (!overload)
			throw new Error("Invalid CSS colour function argument count");

		// validate args
		for (let i = 0; i < args.length; i++) {
			const arg = args[i];
			const validator =
				Array.isArray(overload.params) ?
					overload.params[i]
					: validateNumberOrPercent;
			const result = validator(arg);
			if (!result.valid) {
				throw new Error("Invalid CSS colour argument: " + result.reason);
			}
		}
		try {
			return overload.parse(args);
		} catch (e: any) {
			const message =
				typeof e == "object" && "message" in e ? e.message : `${e}`;
			throw new Error("Failed to parse CSS colour: " + message);
		}
	}

	/**
	 * Define a colour using hue, saturation and lightness values
	 * @param hue Hue angle as an Angle object or radians
	 * @param saturation 
	 * @param lightness 
	 * @param alpha Normalised (0..1) alpha value
	 */
	static fromHSL(
		hue: Angle,
		saturation: number,
		lightness: number,
		alpha?: number,
	): RGBA;
	static fromHSL(
		hueRadians: number,
		saturation: number,
		lightness: number,
		alpha?: number,
	): RGBA;
	static fromHSL(
		hue: number | Angle,
		saturation: number,
		lightness: number,
		alpha: number = 1,
	): RGBA {
		const hueRadians = angleAsRadians(hue);
		const hueDeg = (hueRadians * (180 / Math.PI)) % 360;

		if (saturation === 0) {
			const v = lightness * 255;
			return new RGBA(v, v, v, alpha * 255);
		}

		const chroma = (1 - Math.abs(2 * lightness - 1)) * saturation;
		const hPrime = hueDeg / 60;
		const x = chroma * (1 - Math.abs((hPrime % 2) - 1));
		const m = lightness - chroma / 2;

		let r1 = 0,
			g1 = 0,
			b1 = 0;
		if (hPrime < 1) [r1, g1, b1] = [chroma, x, 0];
		else if (hPrime < 2) [r1, g1, b1] = [x, chroma, 0];
		else if (hPrime < 3) [r1, g1, b1] = [0, chroma, x];
		else if (hPrime < 4) [r1, g1, b1] = [0, x, chroma];
		else if (hPrime < 5) [r1, g1, b1] = [x, 0, chroma];
		else [r1, g1, b1] = [chroma, 0, x];

		const r = (r1 + m) * 255;
		const g = (g1 + m) * 255;
		const b = (b1 + m) * 255;
		const a = alpha * 255;

		return new RGBA(r, g, b, a);
	}

	/**
	 * Define a colour using normalised cyan, magenta, yellow and black values
	 * @param cyan 
	 * @param magenta 
	 * @param yellow 
	 * @param black 
	 * @returns 
	 */
	static fromCMYK(
		cyan: number,
		magenta: number,
		yellow: number,
		black: number
	): RGBA {
		const r = (1 - Math.min(1, cyan * (1 - black) + black)) * 255;
		const g = (1 - Math.min(1, magenta * (1 - black) + black)) * 255;
		const b = (1 - Math.min(1, yellow * (1 - black) + black)) * 255;

		return new RGBA(r, g, b);
	}

	static average(colours: RGBA[]) {
		const channels = colours
			.reduce(
				([sr, sg, sb, sa], c) => {
					const [r, g, b, a] = c.asBytes;
					return [
						r + sr,
						g + sg,
						b + sb,
						a + sa,
					];
				},
				[0, 0, 0, 0],
			)
			.map((n) => n / colours.length) as [number, number, number, number];
		return new RGBA(...channels);
	}
}

const TAU = Math.PI * 2;
const cssFuncRegex = /^\s*(\w+)\(([^)]+)\)\s*$/;

function parseArgStr(s: string) {
	if (s.includes(",")) return s.split(",").map((s) => s.trim());
	const [beforeSlash, afterSlash] = s.split("/", 2).map((s) => s.trim());
	if (afterSlash) return [...beforeSlash.split(/\s+/), afterSlash];
	return beforeSlash.split(/\s+/);
}



type CssFunc = {
	params: ((value: string) => ArgumentValidationResult)[] | number;
	parse: (args: string[]) => RGBA;
}[];

const validateNumberOrPercent = (v: string) => {
	if (v[v.length - 1] == "%") {
		return isNaN(v.replace(/%$/, "") as any) ?
			validationResult(false, "not a number")
			: validationResult(true);
	}
	return isNaN(v as any) ?
		validationResult(false, "not a number")
		: validationResult(true);
};

const validateAngle = (v: string) => {
	return /^\d+(deg|rad|turn)?$/.test(v) ?
		validationResult(true)
		: validationResult(false, "not a valid angle");
};

type ArgumentValidationResult =
	| {
		valid: true;
	}
	| {
		valid: false;
		reason: string;
	};

function validationResult(valid: true): ArgumentValidationResult;
function validationResult(
	valid: false,
	reason: string,
): ArgumentValidationResult;
function validationResult(
	valid: boolean,
	reason: string = "",
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
		{
			// css rgb() accepts 4th alpha arg
			params: 4,
			parse: ([r, g, b, a]) =>
				new RGBA(
					percentToByte(r), // x% -> (x/100)*255, x -> x
					percentToByte(g),
					percentToByte(b),
					percentToValue(a) * 255, // x% -> (x/100)*255, x -> x*255
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
					percentToValue(a) * 255, // x% -> (x/100)*255, x -> x*255
				),
		},
	],
	hsl: [
		{
			params: [validateAngle, validateNumberOrPercent, validateNumberOrPercent],
			parse: ([h, s, l]) =>
				RGBA.fromHSL(stringToAngle(h), percentToValue(s), percentToValue(l)),
		},
		{
			params: [
				validateAngle,
				validateNumberOrPercent,
				validateNumberOrPercent,
				validateNumberOrPercent,
			],
			parse: ([h, s, l, a]) =>
				RGBA.fromHSL(
					stringToAngle(h),
					percentToValue(s),
					percentToValue(l),
					percentToValue(a),
				),
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
			parse: ([h, s, l, a]) =>
				RGBA.fromHSL(
					stringToAngle(h),
					percentToValue(s),
					percentToValue(l),
					percentToValue(a),
				),
		},
	],
} satisfies Record<string, CssFunc>;

const blendNumbers = (from: number, to: number, progress: number) => {
	return from + (to - from) * progress;
};

function fromHex(code: string) {
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


type Angle =
	| {
		asDegrees: number;
	}
	| {
		asRadians: number;
	}
	| {
		asTurns: number;
	};


function transformChannelsFloat(
	source: RGBA,
	target: RGBA,
	fn: (self: number, target: number) => number,
) {
	const floatSource = source.asFloats;
	const floatTarget = target.asFloats;
	return source.blend(
		new RGBA(
			fn(floatSource.red, floatTarget.red) * 255,
			fn(floatSource.green, floatTarget.green) * 255,
			fn(floatSource.blue, floatTarget.blue) * 255,
			source.alphaValue,
		),
		target.alphaValue / 255,
	);
}

function transformChannelsByte(
	source: RGBA,
	target: RGBA,
	fn: (self: number, target: number) => number,
) {
	const sourceBytes = source.asBytes;
	const targetBytes = target.asBytes;
	return source.blend(
		new RGBA(
			fn(sourceBytes[0], targetBytes[0]),
			fn(sourceBytes[1], targetBytes[1]),
			fn(sourceBytes[2], targetBytes[2]),
			sourceBytes[3],
		),
		targetBytes[3] / 255,
	);
}

function angleAsRadians(angle: Angle | number) {
	if (typeof angle == "number") return angle;
	if ("asRadians" in angle) return angle.asRadians;
	if ("asDegrees" in angle) return angle.asDegrees * (Math.PI / 180);
	if ("asTurns" in angle) return angle.asTurns * TAU;
	throw new Error("Invalid angle");
}

function stringToAngle(s: string): Angle {
	if (/^\d+$/.test(s)) return { asDegrees: Number(s) };
	const match = s.match(/^(\d+)(deg|rad|turn)/);
	if (match) {
		switch (match[2]) {
			case "deg":
				return { asDegrees: Number(match[1]) };
			case "rad":
				return { asRadians: Number(match[1]) };
			case "turn":
				return { asTurns: Number(match[1]) };
		}
	}
	throw new Error("Unknown unit: " + s.replace(/^\d+/, ""));
}
