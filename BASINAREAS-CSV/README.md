# BASINAREAS-CSV

Retention Basin Volume Calculator and CSV Exporter.

## Description
This tool automates the process of calculating retention basin volumes from contour polylines. It analyzes closed polylines on a specific layer, identifies their basin ID based on text labels, and calculates incremental and cumulative volumes using the Average End Area method. The results are exported to a CSV file.

## Features
- **Automated Geometry Analysis**: Identifies if text labels are inside the contour polylines to assign Basin IDs.
- **Volume Calculation**: Uses the Average End Area method (standard for civil engineering) to calculate volumes between elevation steps.
- **CSV Export**: Generates a detailed report including Handle, Layer, Basin ID, Elevation, Area, Depth, Incremental Volume, and Cumulative Volume (in both CF and AF).
- **Sorting**: Automatically sorts data by Basin ID and Elevation for correct volume math.
- **Error Handling**: Identifies open contours vs closed contours.

## Usage
1. Type `BASINAREAS-CSV` in the command line.
2. Select a **Polyline Contour** on your target basin layer (e.g., `C-POND-CNTR`). The tool will use this entity's layer to find all other contours.
3. Select a **Text Label** on your basin label layer (e.g., `C-POND-TEXT`). The tool will use this layer to identify which basin each contour belongs to.
4. Specify the **CSV Filename** and location to save the report.
5. The tool processes all polylines on the contour layer, performs calculations, and saves the CSV.

## Method
**Average End Area Formula**:
`Volume = ((Area1 + Area2) / 2) * Depth`

Where:
- `Area1` = Area of lower contour
- `Area2` = Area of upper contour
- `Depth` = Difference in elevation between contours
