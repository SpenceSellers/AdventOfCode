using System;
using System.Collections.Generic;

namespace AdventOfCode.AdventLib
{
    public static class DictionaryExtensions
    {
        public static void Increment<T>(this IDictionary<T, int> dict, T key, int count = 1)
        {
            if (dict.ContainsKey(key))
            {
                dict[key] += count;
            }
            else
            {
                dict[key] = count;
            }
        }

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
    }
}