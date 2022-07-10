using System;
using System.Text.Json;

namespace AdventOfCode.AdventLib;

public static class ObjectExtensions
{
    public static void JsonTrace(this object obj, string tag = "Trace")
    {
        Console.Out.WriteLine($"{tag}: {JsonSerializer.Serialize(obj)}");
    }
}