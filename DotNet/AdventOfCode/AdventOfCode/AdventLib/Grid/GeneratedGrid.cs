using System;

namespace AdventOfCode.AdventLib.Grid
{
    public class GeneratedGrid<T> : IGrid<T>
    {
        private readonly Func<GridPoint, T> _generatorFunc;

        public GeneratedGrid(Func<GridPoint, T> generatorFunc)
        {
            _generatorFunc = generatorFunc;
        }
        
        public T Get(GridPoint point) => _generatorFunc(point);
    }
}