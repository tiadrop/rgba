import { RGBA } from "./rgba";

function getColourRange(colours: RGBA[]): number {
	const ranges = getChannelRanges(colours);
	return Math.max(ranges.red, ranges.green, ranges.blue);
}

function getChannelRanges(colours: RGBA[]): {
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

	for (const colour of colours) {
		minR = Math.min(minR, colour.redValue);
		maxR = Math.max(maxR, colour.redValue);
		minG = Math.min(minG, colour.greenValue);
		maxG = Math.max(maxG, colour.greenValue);
		minB = Math.min(minB, colour.blueValue);
		maxB = Math.max(maxB, colour.blueValue);
	}

	return {
		red: maxR - minR,
		green: maxG - minG,
		blue: maxB - minB,
	};
}

/**
 * Create an optimised colour palette from a set of colours
 * @param colours Array of source colours
 * @param maxColours Maximum number of colours in palette
 * @returns Optimised palette
 */
export function createPalette(
	colours: RGBA[],
	maxColours: number,
): RGBA[] {
	if (maxColours <= 0) throw new Error("maxColours must be positive");
	if (colours.length === 0) return [];
	if (maxColours >= colours.length) return [...new Set(colours)];
	return medianCutQuantize(colours, maxColours);
}

function medianCutQuantize(colours: RGBA[], maxColours: number): RGBA[] {
	const buckets = [colours];

	while (buckets.length < maxColours && buckets.some((b) => b.length > 1)) {
		// find widest bucket
		const bucketIndex = buckets.reduce((maxIdx, bucket, idx) => {
			if (bucket.length <= 1) return maxIdx;
			const range = getColourRange(bucket);
			const maxRange = getColourRange(buckets[maxIdx]);
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

	// average each bucket
	return buckets.map((bucket) =>
		bucket.length === 0 ? new RGBA(0, 0, 0)
			: bucket.length === 1 ? bucket[0]
				: RGBA.average(bucket),
	);
}

