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
			this.alphaValue * alpha,
		);
	}

	/**
	 * The colour's luminance, using the luma709 formula
	 */
	get luma709() {
		const [r, g, b] = [
			this.redValue / 255,
			this.greenValue / 255,
			this.blueValue / 255,
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
			this.blueValue / 255,
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
			this.blueValue / 255,
		];
		const max = Math.max(r, g, b);
		const min = Math.min(r, g, b);
		const chroma = max - min;
		if (chroma == 0) return 0;
		return chroma / (1 - Math.abs(this.lightness * 2 - 1));
	}

	applyContrast(n: number) {
		return new RGBA(this.asBytes.map((channel) => channel * n));
	}

	saturate(amount: number) {
		const mean = (this.redValue + this.greenValue + this.blueValue) / 3;
		const greyscale = new RGBA(mean, mean, mean, this.alphaValue);
		return greyscale.blend(this, amount);
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
		if (!this._inverted) {
			this._inverted = new RGBA(
				255 - this.redValue,
				255 - this.greenValue,
				255 - this.blueValue,
				this.alphaValue,
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
		return new RGBA(
			channels.red ?? this.redValue,
			channels.green ?? this.greenValue,
			channels.blue ?? this.blueValue,
			channels.alpha ?? this.alphaValue,
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
		return new RGBA(
			blendNumbers(this.redValue, target.redValue, targetBias),
			blendNumbers(this.greenValue, target.greenValue, targetBias),
			blendNumbers(this.blueValue, target.blueValue, targetBias),
			blendNumbers(this.alphaValue, target.alphaValue, targetBias),
		);
	}

	private _floats: null | {
		red: number;
		green: number;
		blue: number;
		alpha: number;
	} = null;
	get asFloats() {
		if (!this._floats)
			this._floats = {
				red: this.redValue / 255,
				green: this.greenValue / 255,
				blue: this.blueValue / 255,
				alpha: this.alphaValue / 255,
			};
		return this._floats;
	}

	private _linear: { red: number; green: number; blue: number } | null = null;
	get linearRGB() {
		if (!this._linear) {
			const { red, green, blue } = this.asFloats;
			this._linear = {
				red:
					red <= 0.04045 ? red / 12.92 : Math.pow((red + 0.055) / 1.055, 2.4),
				green:
					green <= 0.04045 ?
						green / 12.92
					:	Math.pow((green + 0.055) / 1.055, 2.4),
				blue:
					blue <= 0.04045 ?
						blue / 12.92
					:	Math.pow((blue + 0.055) / 1.055, 2.4),
			};
		}
		return this._linear;
	}

	private _vec3: { x: number; y: number; z: number } | null = null;
	get asVec3() {
		if (!this._vec3) {
			const linear = this.linearRGB;
			const x =
				linear.red * 0.4124564 +
				linear.green * 0.3575761 +
				linear.blue * 0.1804375;
			const y =
				linear.red * 0.2126729 +
				linear.green * 0.7151522 +
				linear.blue * 0.072175;
			const z =
				linear.red * 0.0193339 +
				linear.green * 0.119192 +
				linear.blue * 0.9503041;
			this._vec3 = { x, y, z };
		}
		return this._vec3;
	}

	private _lab: { l: number; a: number; b: number } | null = null;
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

	private transformChannelsFloat(
		target: RGBA,
		fn: (self: number, target: number) => number,
	) {
		const floatSelf = this.asFloats;
		const floatTarget = target.asFloats;
		return this.blend(
			new RGBA(
				fn(floatSelf.red, floatTarget.red) * 255,
				fn(floatSelf.green, floatTarget.green) * 255,
				fn(floatSelf.blue, floatTarget.blue) * 255,
				this.alphaValue,
			),
			target.alphaValue / 255,
		);
	}

	private transformChannelsByte(
		target: RGBA,
		fn: (self: number, target: number) => number,
	) {
		return this.blend(
			new RGBA(
				fn(this.redValue, target.redValue),
				fn(this.greenValue, target.greenValue),
				fn(this.blueValue, target.blueValue),
				this.alphaValue,
			),
			target.alphaValue / 255,
		);
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
				const a = c.alphaValue / 255;
				return new RGBA(
					blendNumbers(this.redValue, c.redValue, a),
					blendNumbers(this.greenValue, c.greenValue, a),
					blendNumbers(this.blueValue, c.blueValue, a),
					blendNumbers(this.alphaValue, 255, a),
				);
			case "multiply":
				return this.transformChannelsFloat(c, (a, b) => a * b);
			case "screen":
				return this.transformChannelsFloat(c, (a, b) => 1 - (1 - a) * (1 - b));
			case "overlay":
				return this.transformChannelsFloat(c, (a, b) =>
					a < 0.5 ? 2 * a * b : 1 - 2 * (1 - a) * (1 - b),
				);
			case "difference":
				return this.transformChannelsByte(c, (a, b) => Math.abs(a - b));
			case "exclusion":
				return this.transformChannelsFloat(c, (a, b) => a + b - 2 * a * b);
			case "add":
				return this.transformChannelsByte(c, (a, b) => a + b);
			case "xor":
				return this.transformChannelsByte(c, (a, b) => a ^ b);
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
	 * Perform ADd operation
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
			throw new Error("Cannot find nearest color in empty palette");
		}

		return palette.reduce(
			(nearest, current) => {
				const distance = this.distance(current, metric);
				return distance < nearest.distance ?
						{ color: current, distance }
					:	nearest;
			},
			{ color: palette[0], distance: this.distance(palette[0], metric) },
		).color;
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
		if (!funcMatch) throw new Error("Unrecognised CSS colour string");
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
				:	validateNumberOrPercent;
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

	static fromHSL(
		angle: Angle,
		saturation: number,
		lightness: number,
		alpha?: number,
	): RGBA;
	static fromHSL(
		radians: number,
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

	static average(colours: RGBA[]) {
		const channels = colours
			.reduce(
				([sr, sg, sb, sa], c) => [
					c.redValue + sr,
					c.greenValue + sg,
					c.blueValue + sb,
					c.alphaValue + sa,
				],
				[0, 0, 0, 0],
			)
			.map((n) => n / colours.length) as [number, number, number, number];
		return new RGBA(...channels);
	}

	/**
	 * @deprecated - use exported colours, eg `import { black } from "@xtia/rgba"
	 */
	static black = new RGBA(0, 0, 0);
	/**
	 * @deprecated - use exported colours, eg `import { blue } from "@xtia/rgba"
	 */
	static blue = new RGBA(0, 0, 255);
	/**
	 * @deprecated - use exported colours, eg `import { green } from "@xtia/rgba"
	 */
	static green = new RGBA(0, 255, 0);
	/**
	 * @deprecated - use exported colours, eg `import { cyan } from "@xtia/rgba"
	 */
	static cyan = new RGBA(0, 255, 255);
	/**
	 * @deprecated - use exported colours, eg `import { red } from "@xtia/rgba"
	 */
	static red = new RGBA(255, 0, 0);
	/**
	 * @deprecated - use exported colours, eg `import { magenta } from "@xtia/rgba"
	 */
	static magenta = new RGBA(255, 0, 255);
	/**
	 * @deprecated - use exported colours, eg `import { yellow } from "@xtia/rgba"
	 */
	static yellow = new RGBA(255, 255, 0);
	/**
	 * @deprecated - use exported colours, eg `import { white } from "@xtia/rgba"
	 */
	static white = new RGBA(255, 255, 255);
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
			:	validationResult(true);
	}
	return isNaN(v as any) ?
			validationResult(false, "not a number")
		:	validationResult(true);
};

const validateAngle = (v: string) => {
	return /^\d+(deg|rad|turn)?$/.test(v) ?
			validationResult(true)
		:	validationResult(false, "not a valid angle");
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

type ColourHelper = ((s: string) => RGBA) &
	((red: number, green: number, blue: number, alpha?: number) => RGBA) & {
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

/**
 * A shortcut helper for specifying RGBA colour representations using CSS-like syntax
 * @example
 * ```js
 * let colour = C.xff0080;
 * ```
 */
export const C = new Proxy(
	(s: string | number, green?: number, blue?: number, alpha?: number) =>
		green === undefined ?
			RGBA.parse(s as string)
		:	new RGBA(s as number, green, blue!, alpha ?? 255),
	{
		apply: (parse, _, args) => parse(args[0]),
		get: (parse, prop) => {
			if (typeof prop != "string") return undefined;
			if (prop[0] == "x") return parse("#" + prop.substring(1));
			throw new Error("Invalid colour");
		},
	},
) as ColourHelper;

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

/**
 * Create an optimised color palette from a set of colors
 * @param colors Array of source colors
 * @param maxColors Maximum number of colors in palette
 * @returns Optimized palette
 */
export function createPalette(
	colors: RGBA[],
	maxColors: number,
): RGBA[] {
	if (maxColors <= 0) throw new Error("maxColors must be positive");
	if (colors.length === 0) return [];
	if (maxColors >= colors.length) return [...new Set(colors)];
	return medianCutQuantize(colors, maxColors);
}

function medianCutQuantize(colors: RGBA[], maxColors: number): RGBA[] {
	const buckets = [colors];

	while (buckets.length < maxColors && buckets.some((b) => b.length > 1)) {
		// find widest bucket
		const bucketIndex = buckets.reduce((maxIdx, bucket, idx) => {
			if (bucket.length <= 1) return maxIdx;
			const range = getColorRange(bucket);
			const maxRange = getColorRange(buckets[maxIdx]);
			return range > maxRange ? idx : maxIdx;
		}, 0);

		const bucket = buckets[bucketIndex];
		if (bucket.length <= 1) break;

		// sort by widest channel
		const ranges = getChannelRanges(bucket);
		const maxChannel =
			ranges.red > ranges.green && ranges.red > ranges.blue ? "red"
			: ranges.green > ranges.blue ? "green"
			: "blue";

		bucket.sort((a, b) => {
			const aVal =
				maxChannel === "red" ? a.redValue
				: maxChannel === "green" ? a.greenValue
				: a.blueValue;
			const bVal =
				maxChannel === "red" ? b.redValue
				: maxChannel === "green" ? b.greenValue
				: b.blueValue;
			return aVal - bVal;
		});

		const mid = Math.floor(bucket.length / 2);
		buckets.splice(bucketIndex, 1, bucket.slice(0, mid), bucket.slice(mid));
	}

	// Average each bucket
	return buckets.map((bucket) =>
		bucket.length === 0 ? new RGBA(0, 0, 0)
		: bucket.length === 1 ? bucket[0]
		: RGBA.average(bucket),
	);
}

function getColorRange(colors: RGBA[]): number {
	const ranges = getChannelRanges(colors);
	return Math.max(ranges.red, ranges.green, ranges.blue);
}

function getChannelRanges(colors: RGBA[]): {
	red: number;
	green: number;
	blue: number;
} {
	let minR = 255,
		maxR = 0,
		minG = 255,
		maxG = 0,
		minB = 255,
		maxB = 0;

	for (const color of colors) {
		minR = Math.min(minR, color.redValue);
		maxR = Math.max(maxR, color.redValue);
		minG = Math.min(minG, color.greenValue);
		maxG = Math.max(maxG, color.greenValue);
		minB = Math.min(minB, color.blueValue);
		maxB = Math.max(maxB, color.blueValue);
	}

	return {
		red: maxR - minR,
		green: maxG - minG,
		blue: maxB - minB,
	};
}

export const egaPalette = [C.x000, C.x00a, C.x0a0, C.x0aa, C.xa00, C.xa0a, C.xaa0, C.xaaa, C.x555, C.x55f, C.x5f5, C.x5ff, C.xf55, C.xf5f, C.xff5, C.xfff] as const;

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
