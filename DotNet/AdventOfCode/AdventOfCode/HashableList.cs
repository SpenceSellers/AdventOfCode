using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode
{
    public class HashableList<T> : IReadOnlyList<T>
    {
        private T[] Items { get; }
        
        public HashableList(IEnumerable<T> items)
        {
            Items = items.ToArray();
        }

        public IEnumerator<T> GetEnumerator()
        {
            return Items.GetEnumerator() as IEnumerator<T>;
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public int Count { get; }

        public T this[int index] => Items[index];

        public override int GetHashCode()
        {
            unchecked
            {
                const int seed = 487;
                const int modifier = 31;
                return Items.Aggregate(seed, (current, item) =>
                    (current*modifier) + item.GetHashCode());
            }       
        }

        public override bool Equals(object? obj)
        {
            // TODO
            throw new NotImplementedException();
        }
    }
}