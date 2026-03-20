import { C } from "./helper";

export const egaPalette = [C.x0, C.x00a, C.x0a0, C.x0aa, C.xa00, C.xa0a, C.xa50, C.xa, C.x5, C.x55f, C.x5f5, C.x5ff, C.xf55, C.xf5f, C.xff5, C.xf] as const;
export const c64Palette = [C.x0, C.xf, C.x800, C.xafe, C.xc4c, C.x0c5, C.x00a, C.xee7, C.xd85, C.x640, C.xf77, C.x3, C.x7, C.xaf6, C.x08f, C.xb] as const;

/**
 * Petrichor palette
 * By Russel Henderson https://github.com/russell-henderson
 */
export const petrichor = {
	ink: C.x1a1814,
	storm: C.x2e3d4f,
	slate: C.x6b7f8e,
	rain: C.x8fafc2,
	moss: C.x4a5e45,
	clay: C.x9c7c5e,
	ochre: C.xc4893a,
	petal: C.xd4a5a0,
	dust: C.xc8bfad
};

export const sorbet = [
    C.xada9da,
	C.xa9daa9,
	C.xa9cfda,
	C.xdaa9a9,
	C.xabdaa9,
];
