# VOFFSET

Vertical Offset tool for applying elevation changes to offset lines.

## Description
`VOFFSET` mimics the standard AutoCAD `OFFSET` command but applies a vertical difference (elevation delta) to the connected entities. This is particularly useful for grading workflows where you need to step offsets up or down by a specific elevation difference (e.g., curb heights, wall steps).

## Features
- **Vertical Delta**: Prompts for an elevation change (`+` for up, `-` for down) applied to the new object.
- **Pre-selection Support**: Works with currently selected objects (noun-verb).
- **Batch Processing**: Can process multiple pre-selected objects sequentially.
- **Standard Workflow**: Mimics the familiar offset command interaction (select object, select side).
- **Live Feedback**: Reports the new elevation in the command line.

## Usage
1. Type `VOFFSET` in the command line.
2. Enter the **Horizontal Offset Distance**.
3. Enter the **Vertical Change** (Delta Elevation).
   - Example: `0.5` adds 0.5 to the elevation.
   - Example: `-0.1` subtracts 0.1 from the elevation.
4. Select the object to offset (or use pre-selected objects).
5. Pick the side to offset.
6. The new object is created at the calculated elevation.

## Requirements
- Civil 3D or AutoCAD with LISP support.
- Objects must have an `Elevation` property (e.g., Polylines, Lines, Feature Lines).
