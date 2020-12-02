namespace AdventOfCode.AdventLib.Grid
{
    
    public interface IGrid<out T>
    {
        public T Get(GridPoint point);
    }
}