using System.Collections.Generic;

namespace AdventOfCode.AdventLib;

public static class ListExtensions
{
    public static T IndexOrDefault<T>(this IList<T> list, int index, T def = default)
    {
        return list.Count > index && index >= 0 ? list[index] : def;
    }
}