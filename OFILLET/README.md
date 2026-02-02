# OFILLET

Smart filleting tool with automatic radius incrementing for offset contours.

## Description
`OFILLET` (Offset Fillet) streamlines the process of filleting concentric or offset contours (like parking lot islands or curbs). Instead of manually restarting the command to change the radius for each parallel line, `OFILLET` automatically adjusts the radius by a set increment when it detects you are working on a new contour.

## Features
- **Auto-Increment Radius**: Automatically adds (or subtracts) a value to the fillet radius when you select a new object.
- **Continuous Workflow**: Stays active, allowing you to fillet multiple sets of lines without restarting.
- **Workflow Optimization**: Designed specifically to save time on civil site grading and drafting tasks.

## Usage
1. Type `OFILLET` in the command line.
2. Enter the **Initial Radius**.
3. Enter the **Radius Offset/Increment**:
   - Postive value (`+`): Increases radius for outer offsets.
   - Negative value (`-`): Decreases radius for inner offsets.
4. Select the first line segment.
5. Select the second line segment (standard fillet behavior).
6. Continue to the next pair of lines.
   - If you select a *new* object (not the one you just filleted), the tool assumes it's the next offset line and updates the radius automatically.

## Example
If you have curbs offset by 0.5 units:
1. Start with Radius `2.0`.
2. Set Increment to `0.5`.
3. Fillet the first curb (Radius 2.0).
4. Move to the next curb (Radius becomes 2.5 automatically).
5. Move to the next (Radius becomes 3.0), and so on.
