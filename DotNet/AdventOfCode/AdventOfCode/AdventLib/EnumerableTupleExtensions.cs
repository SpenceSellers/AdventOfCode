using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;

namespace AdventOfCode
{
    public static class EnumerableTupleExtensions
    {
        public static (T, T) Two<T>(this IEnumerable<T> items)
        {
            var iter = items.Take(2).ToList();
            return (iter[0], iter[1]);
        }

        public static Dictionary<TA, TB> ToDictionary<TA, TB>(this IEnumerable<(TA, TB)> items)
        {
            return items.ToDictionary(x => x.Item1, x => x.Item2);
        }

        /// <summary>
        /// Select, but we keep the original items around too
        /// </summary>
        public static IEnumerable<(TOriginal, TA)> SelectAlongside<TOriginal, TA>(
            this IEnumerable<TOriginal> items,
            Func<TOriginal, TA> func)
        {
            return items.Select(x => (x, func(x)));
        }

        public static IEnumerable<(TOriginal, TA, TB)> SelectAlongside<TOriginal, TA, TB>(
            this IEnumerable<TOriginal> items,
            Func<TOriginal, TA> funcA,
            Func<TOriginal, TB> funcB)
        {
            return items.Select(x => (x, funcA(x), funcB(x)));
        }

        public static IEnumerable<(TNew, TB)> Select1<TOld, TNew, TB>(
            this IEnumerable<(TOld, TB)> items,
            Func<TOld, TNew> func)
        {
            return items.Select(x => (func(x.Item1), x.Item2));
        }

        public static IEnumerable<TOld> Select1<TOld, TB>(
            this IEnumerable<(TOld, TB)> items)
        {
            return items.Select(x => x.Item1);
        }

        public static IEnumerable<TOld> Select2<TA, TOld>(
            this IEnumerable<(TA, TOld)> items)
        {
            return items.Select(x => x.Item2);
        }

        public static IEnumerable<(TA, TNew)> Select2<TOld, TNew, TA>(
            this IEnumerable<(TA, TOld)> items,
            Func<TOld, TNew> func)
        {
            return items.Select(x => (x.Item1, func(x.Item2)));
        }

        public static IEnumerable<(TA, TB)> Where1<TA, TB>(this IEnumerable<(TA, TB)> items, Func<TA, bool> predicate)
        {
            return items.Where(x => predicate(x.Item1));
        }

        public static IEnumerable<(TA, TB, TC)> Where1<TA, TB, TC>(this IEnumerable<(TA, TB, TC)> items, Func<TA, bool> predicate)
        {
            return items.Where(x => predicate(x.Item1));
        }

        public static IEnumerable<(TA, TB)> Where2<TA, TB>(this IEnumerable<(TA, TB)> items, Func<TB, bool> predicate)
        {
            return items.Where(x => predicate(x.Item2));
        }

        public static IEnumerable<(TA, TB, TC)> Where2<TA, TB, TC>(this IEnumerable<(TA, TB, TC)> items, Func<TB, bool> predicate)
        {
            return items.Where(x => predicate(x.Item2));
        }

        public static IEnumerable<(TA, TB, TC)> Where3<TA, TB, TC>(this IEnumerable<(TA, TB, TC)> items, Func<TC, bool> predicate)
        {
            return items.Where(x => predicate(x.Item3));
        }
    }
}