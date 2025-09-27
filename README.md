# `RGBA`

A class to represent colours, designed for fast read/writing canvas ImageData

```ts
import { RGBA } from "@xtia/rgba";

// specifying & mixing colours
const green = RGBA.parse("#0f0");
const black = new RGBA(0, 0, 0);
const darkRed = RGBA.hsl(0, 1, .25);
const darkGreen = green.blend(black, .5);
const darkBlue = RGBA.parse("#000080");
const pink = RGBA.parse("rgb(255, 130, 200)");
const darkPurple = darkRed.add(darkBlue);

// transparency:
const translucentRed = new RGBA(255, 0, 0, 128);
// or
const translucentGreen = green.fade(.5);

// use as hex code
document.body.style.backgroundColor = darkRed.hexCode;

// read from canvas
const colour = new RGBA(imageData.data.slice(idx, idx + 4));

// write to canvas
imageData.data.set(colour.data, idx);

// import named colours
import { orangered, rebeccapurple } from "@xtia/rgba";

// use directly with Jel
import { $ } from "@xtia/jel";

const el = $.div({
	style: {
		color: darkBlue
		border: `3px solid ${darkBlue.saturate(.5)}`
	},
});
el.style.background = orangered.blend(darkRed, .25);

// and Timeline
import { animate } from "@xtia/timeline";

animate(1000)
	.tween(black, rebeccapurple)
	.listen(c => el.style.background = c);
```
