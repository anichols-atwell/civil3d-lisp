# VOL

Basin Volume Calculator (Selection Based).

## Description
`VOL` is a flexible volume calculator for simple basins. Unlike `BASINAREAS-CSV` which processes an entire layer based on IDs, `VOL` works on a **selection set**. You select the contours you want to calculate, and it generates a volume table and clipboard report.

## Features
- **Selection Based**: Allows manual selection of specific contours (window/crossing).
- **Volume Method**: Average End Area calculation suitable for civil grading.
- **CAD Table**: Inserts a "BASIN VOL. TABLE" directly into the drawing.
- **Clipboard Export**: Copies tab-delimited data for Excel.
- **Open Contour Handling**: Can include open contours for reference (though they contribute 0 area/volume).

## Usage
1. Type `VOL` in the command line.
2. Select the **Contour Polylines** that make up your basin.
3. The tool sorts them by elevation automatically.
4. Click to place the **Summary Table** in the drawing.
5. The data is also copied to your clipboard.

## Difference from BASINAREAS-CSV
- **VOL**: Best for single quick calcs, manual selection, places a CAD table.
- **BASINAREAS-CSV**: Best for batch processing many basins at once, requires layers and text IDs, exports to CSV.
