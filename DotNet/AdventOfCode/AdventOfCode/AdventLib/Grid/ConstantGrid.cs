namespace AdventOfCode.AdventLib.Grid;

public class ConstantGrid<T> : IGrid<T>
{
    private readonly T _value;

    public ConstantGrid(T value)
    {
        _value = value;
    }

    public T Get(GridPoint point)
    {
        return _value;
    }
}