# TC (Time of Concentration Data Extractor)

Extracts polyline data from a Civil 3D surface for Time of Concentration calculations.

## Description
`TC` is a helper tool for hydrology workflows. It extracts the length and start/end surface elevations of a selected flow path (polyline) and copies the data to the clipboard in a format ready for direct pasting into Excel.

## Features
- **Surface Elevation Lookups**: Automatically finds surface elevations at the start and end points of the polyline.
- **Unit Conversion**: Automatically converts length to Feet based on the drawing's `INSUNITS`.
- **Clipboard Integration**: Copies `Length(ft) <TAB> StartElev <TAB> EndElev` to the clipboard.
- **Excel Friendly**: Formatted as a single row, tab-delimited, perfect for pasting into calculation spreadsheets.

## Usage
1. Type `TC` in the command line.
2. Select the **Civil 3D Surface** to sample elevations from.
3. Select the **Polyline** representing the flow path.
4. The tool processes the data and reports it in the command line.
5. **Paste** (Ctrl+V) into your Excel spreadsheet.

## Requirements
- Civil 3D (to access Surface objects).
