using System;
using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.DataStructures;
using AdventTests.TestUtils;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests.DataStructures;

public class DoubleEndedListTests
{
    [Test]
    public void PushPopFrontSingle()
    {
        var q = new DoubleEndedList<int>();
        q.PushFront(100);
        q.PopFront().Should().Be(100);
    }

    [Test]
    public void PushPopBackSingle()
    {
        var q = new DoubleEndedList<int>();
        q.PushBack(100);
        q.PopBack().Should().Be(100);
    }

    [Test]
    public void PushPopSwitchSingle()
    {
        var q = new DoubleEndedList<int>();
        q.PushBack(100);
        q.PopFront().Should().Be(100);
    }

    [Test]
    public void PushPopSwitchMultiple()
    {
        var q = new DoubleEndedList<int>();
        q.PushBack(100);
        q.PushBack(200);
        q.PopFront().Should().Be(100);
        q.PopFront().Should().Be(200);
    }

    [Test]
    public void PushPopBackMultiple()
    {
        var q = new DoubleEndedList<int>();
        q.PushBack(100);
        q.PushBack(200);
        q.PopBack().Should().Be(200);
        q.PopBack().Should().Be(100);
    }

    [Test]
    public void PushPopFrontMultiple()
    {
        var q = new DoubleEndedList<int>();
        q.PushFront(100);
        q.PushFront(200);
        q.PopFront().Should().Be(200);
        q.PopFront().Should().Be(100);
    }

    [Test]
    public void TestLength()
    {
        var q = new DoubleEndedList<int>();
        q.Count.Should().Be(0);
        q.PushBack(1);
        q.Count.Should().Be(1);
        q.PushFront(1);
        q.Count.Should().Be(2);
    }

    [Test]
    public void Large()
    {
        var q = new DoubleEndedList<int>();
        for (int i = 0; i < 500; i++)
        {
            q.PushBack(i);
        }
        for (int i = 0; i < 500; i++)
        {
            q.PopFront().Should().Be(i);
        }
    }

    [Test]
    public void Fuzz()
    {
        var q = new DoubleEndedList<int>();
        var rng = new Random();
        for (int i = 0; i < 200; i++)
        {
            if (rng.NextBool())
            {
                q.PushFront(10);
            }
            else
            {
                q.PushBack(10);
            }

            if (rng.NextBool())
            {
                q.PopFront();
            }
            else
            {
                q.PopBack();
            }
        }
    }

    [Test]
    public void Iterates()
    {
        var q = new DoubleEndedList<int>();
        q.PushFront(2);
        q.PushFront(1);
        q.PushBack(3);
        q.PushBack(4);
        q.ToList().Should().BeEquivalentTo(new[] { 1, 2, 3, 4 }, cfg => cfg.WithStrictOrdering());
    }

    [Test]
    public void IndexGet()
    {
        var q = new DoubleEndedList<int>();
        q.PushFront(3);
        q.PushFront(2);
        q.PushFront(1);
        q.PushBack(4);
        q.PushBack(5);
        q.PushBack(6);
        q[0].Should().Be(1);
        q[1].Should().Be(2);
        q[2].Should().Be(3);
        q[3].Should().Be(4);
        q[4].Should().Be(5);
        q[5].Should().Be(6);
    }

    [Test]
    public void IndexSet()
    {
        var q = new DoubleEndedList<int>();
        q.PushFront(3);
        q.PushFront(2);
        q.PushFront(1);
        q.PushBack(4);
        q.PushBack(5);
        q.PushBack(6);

        q[0] = 000;
        q[1] = 100;
        q[2] = 200;
        q[3] = 300;
        q[4] = 400;
        q[5] = 500;

        q.ToList().Should().BeEquivalentTo(new[] { 0, 100, 200, 300, 400, 500 }, c => c.WithStrictOrdering());
    }

    [Test]
    public void IndexGetOutOfBounds()
    {
        var q = new DoubleEndedList<int>();
        q.PushFront(3);
        q.PushFront(2);
        q.PushFront(1);
        q.PushBack(4);
        q.PushBack(5);
        q.PushBack(6);

        var six = () => q[6];
        var seven = () => q[7];
        var negative = () => q[-1];
        six.Should().Throw<IndexOutOfRangeException>();
        seven.Should().Throw<IndexOutOfRangeException>();
        negative.Should().Throw<IndexOutOfRangeException>();
    }

    [Test]
    public void IndexSetOutOfBounds()
    {
        var q = new DoubleEndedList<int>();
        q.PushFront(3);
        q.PushFront(2);
        q.PushFront(1);
        q.PushBack(4);
        q.PushBack(5);
        q.PushBack(6);

        var six = () => q[6] = 10;
        var seven = () => q[7] = 10;
        var negative = () => q[-1] = 10;
        six.Should().Throw<IndexOutOfRangeException>();
        seven.Should().Throw<IndexOutOfRangeException>();
        negative.Should().Throw<IndexOutOfRangeException>();
    }

    [Test]
    public void Remove()
    {
        var q = new DoubleEndedList<int>();
        q.PushFront(3);
        q.PushFront(2);
        q.PushFront(1);
        q.PushBack(4);
        q.PushBack(5);
        q.PushBack(6);

        q.Remove(2);
        q.Remove(5);

        q.ToList().Should().BeEquivalentTo(new[] { 1, 3, 4, 6 }, c => c.WithStrictOrdering());
        q.Count.Should().Be(4);
    }

    [Test]
    public void RemoveFirstAndLast()
    {
        var q = new DoubleEndedList<int>();
        q.PushFront(3);
        q.PushFront(2);
        q.PushFront(1);
        q.PushBack(4);
        q.PushBack(5);
        q.PushBack(6);

        q.Remove(1);
        q.Remove(6);

        q.ToList().Should().BeEquivalentTo(new[] { 2, 3, 4, 5 }, c => c.WithStrictOrdering());
        q.Count.Should().Be(4);
    }

    [Test]
    public void Insert()
    {
        var q = new DoubleEndedList<int>();
        q.PushFront(3);
        q.PushFront(2);
        q.PushFront(1);
        q.PushBack(4);
        q.PushBack(5);

        q.Insert(1, 100);

        q.ToList().Should().BeEquivalentTo(new[] { 1, 100, 2, 3, 4, 5 }, c => c.WithStrictOrdering());
    }

    [Test]
    public void ListToDeList()
    {
        var l = new List<int> { 1, 2, 3, 4 };
        l.ToDoubleEndedList().ToList().Should().BeEquivalentTo(l, c => c.WithStrictOrdering());
    }
}