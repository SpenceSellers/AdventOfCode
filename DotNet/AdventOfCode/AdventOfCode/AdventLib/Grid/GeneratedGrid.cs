using System;

namespace AdventOfCode.AdventLib.Grid
{
    public class GeneratedGrid<T> : IGrid<T>
    {
        public static GeneratedGrid<T> Constant(T value)
        {
            return new GeneratedGrid<T>(_ => value);
        }

        private readonly Func<GridPoint, T> _generatorFunc;

        public GeneratedGrid(Func<GridPoint, T> generatorFunc)
        {
            _generatorFunc = generatorFunc;
        }
        
        public T Get(GridPoint point) => _generatorFunc(point);
    }
}