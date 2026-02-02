# BASINBUILDER

Automated Basin Grading and Volume Table Generator.

## Description
`BASINBUILDER` automates the 3D grading of detention basins and ponds. Starting from a single base contour, it generates concentric offset contours at a specified slope and vertical interval, creating the 3D shape of the basin. It also automatically calculates the volume and inserts a summary table into the drawing.

## Features
- **Automated Grading**: Generates 3D contours based on a starting polyline, side slope (H:V), and depth settings.
- **Smart Elevations**: Automatically detects if you are offsetting inward (down/pond) or outward (up/berm) and assigns elevation values accordingly.
- **Volume Calculation**: Instantly calculates Incremental and Total Volume (CF) for the generated shape.
- **CAD Table**: Inserts a formatted "BASIN VOL. TABLE" directly into Model Space.
- **Clipboard Export**: Copies the volume data to the clipboard for easy pasting into Excel.

## Usage
1. Type `BASINBUILDER` in the command line.
2. Select the **Base Polyline** (must be closed and have an elevation).
3. Enter **Side Slope** (H value). E.g., `4` for 4:1 slope.
4. Enter **Depth Increment** (Vertical step per contour). E.g., `1.0`.
5. Enter **Total Depth** (Total height to grade). E.g., `5.0`.
6. Pick a point on the **Side** to offset (Inside for a hole, Outside for a hill/berm).
7. Pick a point to place the **Volume Table**.

The tool will draw the new contours, assign their elevations, and generate the table.

## Requirements
- Civil 3D or AutoCAD with LISP support.
- Base object must be a closed LWPOLYLINE with a non-zero elevation.
