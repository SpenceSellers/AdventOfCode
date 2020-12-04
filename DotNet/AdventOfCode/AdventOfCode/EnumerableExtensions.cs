using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace AdventOfCode
{
    public static class EnumerableExtensions
    {
        /// <summary>
        /// Breaks a sequence into chunks of length n. The last item may be a partial chunk.
        /// </summary>
        public static IEnumerable<IList<T>> Chunks<T>(this IEnumerable<T> items, int n)
        {
            var chunk = new List<T>();
            foreach (var item in items)
            {
                if (chunk.Count >= n)
                {
                    yield return chunk;
                    chunk = new List<T>();
                }
                
                chunk.Add(item);
            }

            yield return chunk;
        }

        public static IEnumerable<IList<T>> CompleteChunks<T>(this IEnumerable<T> items, int n)
        {
            return Chunks(items, n).Where(chunk => chunk.Count == n);
        }

        public static IEnumerable<T> Rotate<T>(this IList<T> items, int n)
        {
            n *= -1; // Rotate's contract is the opposite of the below code.
            n %= items.Count; // Allow wrap-around.
            if (n < 0)
            {
                n = items.Count + n;
            }

            var head = items.Take(n);
            return items.Skip(n).Concat(head);
        }
        
        
        /// <summary>
        /// Returns all possible k-pairings, but not all possible _orderings_, of a sequence.
        /// </summary>
        public static IEnumerable<IEnumerable<T>> Combinations<T>(this IEnumerable<T> elements, int k)
        {
            if (k == 0)
            {
                return Enumerable.Repeat(Enumerable.Empty<T>(), 1);
            }

            var enumerable = elements.ToList();
            return enumerable.SelectMany((e, i) => enumerable.Skip(i + 1).Combinations(k - 1).Select(c => Enumerable.Repeat(e, 1).Concat(c)));
        }

        /// <summary>
        /// Multiplies the numbers. Yep.
        /// </summary>
        public static int Product(this IEnumerable<int> items)
        {
            return items.Aggregate(1, (current, i) => current * i);
        }
        
        public static BigInteger Product(this IEnumerable<BigInteger> items)
        {
            return items.Aggregate(BigInteger.One, (current, i) => current * i);
        }
    }
}