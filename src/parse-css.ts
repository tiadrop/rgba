import { parseHex } from "./parse-hex.js";
import { Angle, RGBA } from "./rgba.js";

const cssFuncRegex = /^\s*(\w+)\(([^)]+)\)\s*$/;

export function parseRGBA(cssValue: string) {
	// #<hex>
	if (cssValue[0] == "#") return parseHex(cssValue.substring(1));
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


function percentToByte(v: string) {
	return percentToValue(v, 255);
}

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
	return /^(\d*\.)?\d+(deg|rad|turn)?$/.test(v) ?
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