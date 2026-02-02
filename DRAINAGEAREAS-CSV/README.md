# DRAINAGEAREAS-CSV

Drainage Area Exporter.

## Description
This tool batches the process of calculating drainage areas. It takes a layer of drainage area polygons and matches them with text labels found inside (or near) them, then exports a CSV report with area calculations in both Square Feet and Acres.

## Features
- **Spatial Matching**: automatically pairs drainage area polygons with their text labels (e.g., "A-1", "A-2") using point-in-polygon tests.
- **Smart Centroid Support**: For open polylines (which shouldn't happen for areas, but if they exist), it attempts to find the closest text label.
- **Dual Units**: Reports area in both Square Feet and Acres.
- **CSV Export**: Creates a clean table ready for hydrology reports.

## Usage
1. Type `DRAINAGEAREAS-CSV`.
2. Select a **Drainage Polyline** to define the layer to process.
3. Select a **Text Label** to define the layer containing IDs.
4. Specify the **Output CSV file**.
5. The command processes the entire layer and saves the file.
