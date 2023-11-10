using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Runtime.CompilerServices;
using AdventOfCode.AdventLib.DataStructures;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.AdventLib
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

        /// <summary>
        /// Breaks a sequence into chunks of length n. Any values (if any) in an incomplete chunk at the end will be thrown away.
        /// </summary>
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


        // Thanks, stack overflow
        public static IEnumerable<IEnumerable<T>>
            Permutations<T>(this IList<T> list, int length = -1)
        {
            if (length == -1)
            {
                length = list.Count;
            }
            if (length == 1) return list.Select(t => new T[] { t });

            return Permutations(list, length - 1)
                .SelectMany(t => list.Where(e => !t.Contains(e)),
                    (t1, t2) => t1.Concat(new T[] { t2 }));
        }

        /// <summary>
        /// Returns all contiguous sub-sequences of a given size
        /// </summary>
        public static IEnumerable<IList<T>> SequencesOfSize<T>(this IEnumerable<T> elements, int size)
        {
            var list = elements.ToList();
            if (size <= 0)
            {
                throw new ArgumentException("Sequence size cannot be zero or smaller", nameof(size));
            }

            if (size > list.Count)
            {
                throw new ArgumentException("Cannot get sequences larger than list", nameof(size));
            }

            for (var start = 0; start + size - 1 < list.Count; start++)
            {
                yield return list.GetRange(start, size);
            }
        }

        /// <summary>
        /// Multiplies the numbers. Yep.
        /// </summary>
        public static int Product(this IEnumerable<int> items)
        {
            return items.Aggregate(1, (current, i) => current * i);
        }

        public static long Product(this IEnumerable<long> items)
        {
            return items.Aggregate(1L, (current, i) => current * i);
        }

        public static BigInteger Product(this IEnumerable<BigInteger> items)
        {
            return items.Aggregate(BigInteger.One, (current, i) => current * i);
        }

        public static IEnumerable<IList<T>> SplitList<T>(this IEnumerable<T> items, Func<T, bool> splitter)
        {
            var group = new List<T>();
            foreach (var item in items)
            {
                if (splitter(item))
                {
                    yield return group;
                    group = new List<T>();
                }
                else
                {
                    group.Add(item);
                }
            }

            yield return group;
        }

        public static IEnumerable<IList<T>> SplitList<T>(this IEnumerable<T> items, T splitOn)
        {
            return items.SplitList(i => i.Equals(splitOn));
        }

        public static void Deconstruct<T>(this IEnumerable<T> items, out T a, out T b)
        {
            var list = items.Take(2).ToList();
            a = list[0];
            b = list[1];
        }

        public static IEnumerable<T> Trace<T>(this IEnumerable<T> items, string display = "traced")
        {
            foreach (var item in items)
            {
                Console.Out.WriteLine($"{display}: {item.ToString()}");
                yield return item;
            }
        }

        public static List<T> Realize<T>(this IEnumerable<T> items) =>
            items.ToList();

        public static List<List<T>> Realize<T>(this IEnumerable<IEnumerable<T>> items) =>
            items.Select(i => i.ToList()).ToList();

        public static List<List<List<T>>> Realize<T>(this IEnumerable<IEnumerable<IEnumerable<T>>> items) =>
            items.Select(i => i.Select(j => j.ToList()).ToList()).ToList();

        /// <summary>
        /// Convert rows into columns
        /// </summary>
        public static IEnumerable<List<T>> Transpose<T>(IEnumerable<IList<T>> items)
        {
            var list = items.ToList();
            var length = list.First().Count;
            if (list.Any(row => row.Count != length))
            {
                throw new ArgumentException("List does not contain the same number of elements on each row");
            }
            for (var i = 0; i < length; i++)
            {
                var column = list.Select(x => x[i]).ToList();
                yield return column;
            }
        }

        public static SolidGrid<T> ToGrid<T>(this IEnumerable<IEnumerable<T>> items)
        {
            return new SolidGrid<T>(items);
        }

        public static Dictionary<T, long> Histogram<T>(this IEnumerable<T> items)
        {
            var dict = new Dictionary<T, long>();
            foreach (var item in items)
            {
                dict.UpdateWithDefault(item, 0, i => i + 1);
            }

            return dict;
        }

        public static IEnumerable<(int index, T value)> WithIndex<T>(this IEnumerable<T> items)
        {
            return items.Select((x, i) => (i, x));
        }

        public static int FirstIndex<T>(this IEnumerable<T> items, Func<T, bool> predicate)
        {
            return items.WithIndex().First(x => predicate(x.value)).index;
        }

        public static int FirstIndexOrDefault<T>(this IEnumerable<T> items, Func<T, bool> predicate, int def)
        {
            var result = items.WithIndex().FirstOrDefault(x => predicate(x.value), (-1, default));
            if (result.index == -1)
            {
                return def;
            }

            return result.index;
        }

        public static DoubleEndedList<T> ToDoubleEndedList<T>(this IEnumerable<T> items)
        {
            var l = new DoubleEndedList<T>();
            l.AddRangeBack(items);
            return l;
        }
    }
}