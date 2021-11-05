using System.Collections.Generic;

namespace AdventOfCode.AdventLib.BreadthFirstSearch
{
    public record BfsNode<T>
    {
        public T State { get; set; }
    }
}