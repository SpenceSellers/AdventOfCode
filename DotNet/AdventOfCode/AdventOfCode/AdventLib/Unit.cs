namespace AdventOfCode.AdventLib;

/// <summary>
/// The unit type.
/// There's only one value it can be, and it's this one.
/// </summary>
public record struct Unit
{
    public static readonly Unit Value = new();
};