import { RGBA } from ".";


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

/**
 * A shortcut helper for specifying RGBA colour representations using CSS-like syntax
 * @example
 * ```js
 * let colour = C.xff0080;
 * ```
 */
export const C = /*#__PURE__*/ new Proxy(
    (s: string | number, green?: number, blue?: number, alpha?: number) =>
        green === undefined ?
            RGBA.parse(s as string)
            : new RGBA(s as number, green, blue!, alpha ?? 255),
    {
        apply: (parse, _, args) => parse.apply(_, args as any),
        get: (parse, prop) => {
            if (typeof prop != "string") return undefined;
            if (prop[0] == "x") return parse("#" + prop.substring(1));
            throw new Error("Invalid colour");
        },
    },
) as ColourHelper;
