namespace AdventOfCode.AdventLib.Grid
{
    public interface IDefinedSizeGrid<out T> : IGrid<T>
    {
        public int Width { get; }
        public int Height { get; }
        
        public GridRegion ZeroOriginRegion => new GridRegion(GridPoint.Origin, Width, Height);
    }
}