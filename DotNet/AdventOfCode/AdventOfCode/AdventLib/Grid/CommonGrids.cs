namespace AdventOfCode.AdventLib.Grid
{
    public class CommonGrids
    {
        public static IGrid<GridPoint> CoordinateGrid = new GeneratedGrid<GridPoint>(point => point);
    }
}