using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.AdventLib;

/// <summary>
/// A list growable in both directions, implemented by a copy-on-resize circular buffer.
/// </summary>
public class DoubleEndedList<T> : IList<T>
{
    private T[] _buffer;
    private int _start = 0;
    private int _end = 0;

    public DoubleEndedList(int initialCapacity = 1)
    {
        if (initialCapacity <= 0)
        {
            throw new InvalidOperationException($"Invalid initialCapacity of {initialCapacity}");
        }
        _buffer = new T[initialCapacity];
    }

    /// <summary>
    /// Alias for PushBack
    /// </summary>
    public void Add(T item)
    {
        PushBack(item);
    }

    public void Clear()
    {
        for (int i = 0; i < Count; i++)
        {
            this[i] = default;
        }

        _start = 0;
        _end = 0;
    }

    public bool Contains(T item)
    {
        return this.Any(i => i.Equals(item));
    }

    public void CopyTo(T[] array, int arrayIndex)
    {
        var i = arrayIndex;
        foreach (var item in this)
        {
            array[i] = item;
            i++;
        }
    }

    public bool Remove(T item)
    {
        var index = IndexOf(item);
        if (index >= 0)
        {
            RemoveAt(index);
            return true;
        }

        return false;
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

    public bool IsReadOnly => false;

    public int IndexOf(T item)
    {
        var i = 0;
        foreach (var v in this)
        {
            if (v.Equals(item))
            {
                return i;
            }
            i++;
        }

        return -1;
    }

    public void Insert(int index, T item)
    {
        // OPTIMIZE: We move everything backwards towards the back of the list, which will only be optimal 50% of the time.
        // It'd be nice to move items to the shorter side.
        var i = Count;

        // Move the end by 1 now, to avoid getting out of range errors
        _end = Backwards(_end);
        while (i > index)
        {
            // shift everything by 1
            this[i] = this[i - 1];
            i--;
        }

        // And finally put the new item in its place;
        this[index] = item;
    }

    public void RemoveAt(int index)
    {
        // OPTIMIZE: We move everything forwards from the back of the list, which will only be optimal 50% of the time.
        // It'd be nice to move items from the shorter side.
        var lastIndex = Count - 1;
        while (index < lastIndex)
        {
            // shift everything back 1
            this[index] = this[index + 1];
            index++;
        }
        // Get rid of the last item, which is now duplicated
        PopBack();
    }

    public T this[int index]
    {
        get
        {
            if (index < 0 || index >= Count)
            {
                throw new IndexOutOfRangeException($"Index: {index}, Count: {Count} when getting value");
            }
            var i = Backwards(_start, index);
            return _buffer[i];
        }
        set
        {
            if (index < 0 || index >= Count)
            {
                throw new IndexOutOfRangeException($"Index: {index}, Count: {Count} when setting value");
            }
            var i = Backwards(_start, index);
            _buffer[i] = value;
        }
    }

    // -1 because a completely full array is indistinguishable from an empty one given our current indexing strategy.
    // This mean does that one array slot is wasted no matter what...
    private int RemainingCapacity => _buffer.Length - Count - 1;

    public T PopBack()
    {
        if (Count == 0)
        {
            throw new InvalidOperationException("DoubleEndedList is empty, cannot pop");
        }
        _end = Forwards(_end);
        var val = _buffer[_end];
        _buffer[_end] = default;
        return val;
    }

    public T PopFront()
    {
        if (Count == 0)
        {
            throw new InvalidOperationException("DoubleEndedList is empty, cannot pop");
        }
        var val = _buffer[_start];
        _buffer[_start] = default;
        _start = Backwards(_start);
        return val;
    }

    public void PushFront(T value)
    {
        Reserve(1);
        _start = Forwards(_start);
        _buffer[_start] = value;
    }

    public void PushBack(T value)
    {
        Reserve(1);
        _buffer[_end] = value;
        _end = Backwards(_end);
    }

    public void AddRangeBack(IEnumerable<T> range)
    {
        if (range.TryGetNonEnumeratedCount(out var count))
        {
            Reserve(count);
        }

        foreach (var v in range)
        {
            PushBack(v);
        }
    }

    /// <summary>
    /// Inserting A, B, C onto D, E, F yields A B C D E F
    /// </summary>
    public void AddRangeFront(IEnumerable<T> range)
    {
        if (range.TryGetNonEnumeratedCount(out var count))
        {
            Reserve(count);
        }

        foreach (var v in range.Reverse())
        {
            PushFront(v);
        }
    }

    /// <summary>
    /// Inserting A, B, C onto D, E, F yields C B A D E F, equivalent to successively pushing the items onto the front.
    /// </summary>
    public void AddRangeFrontMirrored(IEnumerable<T> range)
    {
        if (range.TryGetNonEnumeratedCount(out var count))
        {
            Reserve(count);
        }

        foreach (var v in range)
        {
            PushFront(v);
        }
    }
    
    /// <summary>
    /// Ensure that we can fit at least N additional elements without resizing
    /// </summary>
    public void Reserve(int additional)
    {
        if (additional <= RemainingCapacity) return;

        var nextSize = NextPowerOfTwo(_buffer.Length + additional);
        ResizeTo(nextSize);
    }

    // This doesn't do any bounds checking so be sure you know what you're doing.
    private void ResizeTo(int newSize)
    {
        var newBuffer = new T[newSize];
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

    private static int NextPowerOfTwo(int n)
    {
        var power = 1;
        while(power < n)
            power*=2;
        return power;
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
    private int Backwards(int x, int distance = 1)
    {
        return (x + distance + _buffer.Length) % _buffer.Length;
    }

    /// <summary>
    /// A step towards the "front" / start of the buffer
    /// </summary>
    private int Forwards(int x, int distance = 1)
    {
        return (x - distance + _buffer.Length) % _buffer.Length;
    }
}