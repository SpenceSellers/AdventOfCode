using System.Collections;
using System.Linq;
using System.Text;

namespace AdventOfCode.AdventLib.Grid
{
    public interface IDefinedSizeGrid<T> : IGrid<T>
    {
        public int Width { get; }
        public int Height { get; }
        

    }
}