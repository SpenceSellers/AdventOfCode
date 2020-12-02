namespace AdventOfCode.AdventLib.Grid
{
    public interface IMutableGrid<in T>
    {
        public void Set(GridPoint point, T value);
    }
}