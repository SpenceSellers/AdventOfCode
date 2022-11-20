using System;
using System.Collections;
using System.Collections.Generic;

namespace AdventOfCode.AdventLib;

public class DoubleEndedQueue<T> : IEnumerable<T>
{
    private T[] _buffer;
    private int _start = 0; // first index that actually contains an item
    private int _end = 0; // Index of first empty item

    public DoubleEndedQueue(int initialCapacity = 1)
    {
        _buffer = new T[initialCapacity];
    }

    public int Count
    {
        get
        {
            if (_start <= _end)
            {
                return _end - _start;
            }
            else
            {
                return (_buffer.Length - _start) + _end;
            }
        }
    }

    // -1 because a completely fully array is indistinguishable from an empty one given our current indexing strategy.
    // This mean does that one array slot is wasted no matter what...
    private int RemainingCapacity => _buffer.Length - Count - 1;

    public T PopBack()
    {
        _end = Forwards(_end);
        var val = _buffer[_end];
        _buffer[_end] = default;
        return val;
    }

    public T PopFront()
    {
        var val = _buffer[_start];
        _buffer[_start] = default;
        _start = Backwards(_start);
        return val;
    }

    public void PushFront(T value)
    {
        if (RemainingCapacity == 0)
        {
            Resize();
        }
        _start = Forwards(_start);
        _buffer[_start] = value;
    }

    public void PushBack(T value)
    {
        if (RemainingCapacity == 0)
        {
            Resize();
        }
        _buffer[_end] = value;
        _end = Backwards(_end);
    }

    public IEnumerator<T> GetEnumerator()
    {
        var i = _start;
        while (i != _end)
        {
            yield return _buffer[i];
            i = Backwards(i);
        }
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }

    /// <summary>
    /// A step towards the "back" / end of the buffer
    /// </summary>
    private int Backwards(int x)
    {
        return (x + 1 + _buffer.Length) % _buffer.Length;
    }

    /// <summary>
    /// A step towards the "front" / start of the buffer
    /// </summary>
    private int Forwards(int x )
    {
        return (x - 1 + _buffer.Length) % _buffer.Length;
    }

    private void Resize()
    {
        var nextSize =_buffer.Length * 2;
        var newBuffer = new T[nextSize];
        var i = 0;
        foreach (var val in this)
        {
            newBuffer[i] = val;
            i++;
        }

        _buffer = newBuffer;
        _start = 0;
        _end = i;
    }
}