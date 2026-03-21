import { RGBA } from "./rgba.js";

export function parseHex(code: string) {
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
