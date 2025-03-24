# `RGBA`

A class to represent colours, designed for fast read/writing canvas ImageData

```ts
import { RGBA } from "@xtia/rgba";

const darkRed = new RGBA(128, 0, 0);
// or
const darkGreen = RGBA.green.blend(RGBA.black, .5);
// or
const darkBlue = RGBA.fromHex("#000088"); // or #008

const translucentRed = new RGBA(255, 0, 0, 128);
// or
const translucentGreen = RGBA.green.fade(.5);

// use as hex code
document.body.style.backgroundColor = darkRed.hexCode;

// read from canvas
const colour = new RGBA(imageData.data.slice(idx, idx + 4));

// write to canvas
imageData.data.set(colour.asBytes, idx);
```
