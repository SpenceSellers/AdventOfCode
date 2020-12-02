namespace AdventOfCode.AdventLib.Grid
{
    public interface IMutableGrid<T>: IGrid<T>
    {
        public void Set(GridPoint point, T value);
    }
}