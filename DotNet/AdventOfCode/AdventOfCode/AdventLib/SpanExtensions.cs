using System;
using System.Collections.Generic;

namespace AdventOfCode.AdventLib;

public static class SpanExtensions
{
    // I benchmarked this as about twice as fast as Chunks
    public static IEnumerable<Memory<T>> MemoryChunks<T>(Memory<T> array, int size)
    {
        if (array.Length % size != 0)
        {
            throw new ArgumentException("Input memory length must be a multiple of size");
        }
        var numChunks = array.Length / size;
        for (int i = 0; i < numChunks; i++)
        {
            yield return array.Slice(i * size, size);
        }
    }
}