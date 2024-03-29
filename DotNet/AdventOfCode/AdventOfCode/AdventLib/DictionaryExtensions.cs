using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;

namespace AdventOfCode.AdventLib
{
    public static class DictionaryExtensions
    {
        public static void Increment<T>(this IDictionary<T, int> dict, T key, int count = 1)
        {
            dict.UpdateWithDefault(key, 0, i => i + count);
        }

        public static void Increment<T>(this IDictionary<T, long> dict, T key, long count = 1)
        {
            dict.UpdateWithDefault(key, 0, i => i + count);
        }

        public static void Increment<T>(this IDictionary<T, BigInteger> dict, T key, BigInteger count)
        {
            dict.UpdateWithDefault(key, 0, i => i + count);
        }

        /// <summary>
        /// The updater func will still be run on initialDefault if the key doesn't exist.
        /// </summary>
        public static void UpdateWithDefault<K, V>(this IDictionary<K, V> dict, K key, V initialDefault,
            Func<V, V> updater)
        {
            var existed = dict.TryGetValue(key, out var before);
            if (!existed)
            {
                before = initialDefault;
            }

            dict[key] = updater(before);
        }

        public static V GetDefault<K, V>(this IDictionary<K, V> dict, K key, V def)
        {
            var existed = dict.TryGetValue(key, out var value);
            return existed ? value : def;
        }

        /// <summary>
        /// Treat a dictionary as a quick-and-dirty cache.
        /// </summary>
        public static V Caching<K, V>(this IDictionary<K, V> dict, K key, Func<K, V> calculateValue)
        {
            if (dict.ContainsKey(key)) return dict[key];

            var result = calculateValue(key);
            dict.Add(key, result);
            return result;
        }

        public static string Dump<K, V>(this IDictionary<K, V> dict)
        {
            var sb = new StringBuilder();
            foreach (var k in dict.Keys.OrderBy(x => x))
            {
                sb.AppendLine($"{k}: {dict[k]}");
            }

            return sb.ToString();
        }
    }
}