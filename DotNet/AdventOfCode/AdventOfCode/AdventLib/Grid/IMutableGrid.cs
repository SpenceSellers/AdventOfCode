namespace AdventOfCode.AdventLib.Grid
{
    public interface IMutableGrid<T>: IGrid<T>
    {
        public void Set(GridPoint point, T value);

        public bool TrySet(GridPoint point, T value)
        {
            try
            {
                Set(point, value);
                return true;
            }
            catch (NonexistentCellException e)
            {
                return false;
            }
        }
    }
}