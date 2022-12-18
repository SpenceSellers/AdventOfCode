using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Text.Json.Nodes;
using System.Text.Json.Serialization;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2022;

public class Advent202213 : Problem
{
    public override object PartOne(string[] input)
    {
        var pairs = input.SplitList(x => x == "")
            .Select(chunk => chunk.Select(line => JsonSerializer.Deserialize<List<JsonElement>>(line)
                .Select(TranslateToSanity)
                .ToList()).ToList())
            .ToList();

        return pairs.Select((pair, index) =>
        {
            if (Compare(pair[0], pair[1]) == Ordering.Smaller)
            {
                return index + 1;
            }
            else
            {
                return 0;
            }
        }).Sum();

    }

    private object TranslateToSanity(JsonElement elem)
    {
        return elem.ValueKind switch
        {
            JsonValueKind.Number => elem.GetInt32(),
            JsonValueKind.Array => elem.EnumerateArray().Select(TranslateToSanity).ToList()
        };
    }

    private Ordering Compare(object left, object right)
    {
        if (left is int li && right is int ri)
        {
            if (li == ri)
            {
                return Ordering.Equal;
            }

            if (li < ri)
            {
                return Ordering.Smaller;
            }

            return Ordering.Bigger;
        }

        if (left is List<object> ll && right is List<object> rl)
        {
            var i = 0;
            while (true)
            {
                if (i >= ll.Count && i >= rl.Count)
                {
                    // Same length, no difference
                    return Ordering.Equal;
                }

                if (i >= ll.Count && i < rl.Count)
                {
                    // Left list ran out first
                    return Ordering.Smaller;
                }

                if (i < ll.Count && i >= rl.Count)
                {
                    // Right list ran out first
                    return Ordering.Bigger;
                }

                var comparison = Compare(ll[i], rl[i]);
                if (comparison != Ordering.Equal)
                {
                    return comparison;
                }

                i++;
            }
        }

        if (left is int && right is List<object>)
        {
            return Compare(new List<object> { left }, right);
        }

        if (left is List<object> && right is int)
        {
            return Compare(left, new List<object> { right });
        }

        return Ordering.Equal;
    }

    private enum Ordering
    {
        Bigger,
        Equal,
        Smaller
    }

    public override object PartTwo(string[] input)
    {
        throw new System.NotImplementedException();
    }
}