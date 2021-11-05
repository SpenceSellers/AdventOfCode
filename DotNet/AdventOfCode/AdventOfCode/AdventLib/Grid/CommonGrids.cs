namespace AdventOfCode.AdventLib.Grid
{
    public class CommonGrids
    {
        /// <summary>
        /// A Grid where each cell contains its own coordinates
        /// </summary>
        public static IGrid<GridPoint> CoordinateGrid = new GeneratedGrid<GridPoint>(point => point);
    }
}