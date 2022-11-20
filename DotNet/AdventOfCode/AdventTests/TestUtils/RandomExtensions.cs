using System;

namespace AdventTests.TestUtils;

public static class RandomExtensions
{
    public static bool NextBool(this Random r, double ratio = 0.5)
    {
        return r.NextDouble() <= ratio;
    }
}