using System.Collections.Generic;

namespace AdventOfCode
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
    }
}