# LAYERLEN

Layer Length Summation Tool.

## Description
`LAYERLEN` quickly calculates the total length of all line and polyline entities on a specific layer. It functions as a quick takeoff tool for linear items like curb, striping, or pipes.

## Features
- **Total Length Calculation**: Sums up `LINE`, `LWPOLYLINE`, and `POLYLINE` entities.
- **Excel Export**: Optionally exports the results directly to a new Excel spreadsheet.
- **Quick Selection**: Allows you to pick an object to define the layer, or type the layer name.

## Usage
1. Type `LAYERLEN`.
2. Enter the **Layer Name** or press Enter to pick an object.
3. The command reports the Total Length in the command line.
4. You are prompted: `Create Excel file? [Yes/No]`.
5. If Yes, it opens Excel and populates a new sheet with the Layer Name, Entity Count, and Total Length.

## Requirements
- Excel (for the export feature).
