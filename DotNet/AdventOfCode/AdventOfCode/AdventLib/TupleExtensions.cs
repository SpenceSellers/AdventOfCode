using System;

namespace AdventOfCode.AdventLib;

public static class TupleExtensions
{
    public static (TResult, TResult) SelectT<T, TResult>(this (T, T) tuple, Func<T, TResult> f)
    {
        return (f(tuple.Item1), f(tuple.Item2));
    }

    public static (TResult, TResult, TResult) SelectT<T, TResult>(this (T, T, T) tuple, Func<T, TResult> f)
    {
        return (f(tuple.Item1), f(tuple.Item2), f(tuple.Item3));
    }
}