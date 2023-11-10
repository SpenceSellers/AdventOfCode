using System;
using System.Collections.Generic;

namespace AdventOfCode.AdventLib;

public static class SpanExtensions
{
    // I benchmarked this as about twice as fast as Chunks
    public static IEnumerable<Memory<T>> MemoryChunks<T>(this Memory<T> array, int size)
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

    public static IEnumerable<ReadOnlyMemory<T>> MemorySplit<T>(this ReadOnlyMemory<T> memory, T splitOn)
    where T: IEquatable<T>
    {
        var i = 0;
        var currentRegionStart = 0;
        while (i < memory.Length)
        {
            if (memory.Span[i].Equals(splitOn))
            {
                yield return memory[currentRegionStart..i];
                currentRegionStart = i + 1;
            }

            i++;
        }

        if (currentRegionStart != memory.Length - 1)
        {
            yield return memory[currentRegionStart..];
        }
    }
}