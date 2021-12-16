using System;
using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Grid;
using Priority_Queue;

namespace AdventOfCode.Days._2021
{
    public class Advent202115 : Problem
    {
        public override object PartOne(string[] input)
        {
            var grid = input.ToGrid().Map(c => int.Parse(c.ToString()));
            var end = new GridPoint(grid.Width - 1, grid.Height - 1);

            foreach (var i in AStar(grid, GridPoint.Origin, end))
            {
                Console.Out.WriteLine(i);
                break;
            }

            return 0;
        }

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }

        record SearchNode
        {
            public GridPoint Point;
            public int Cost;
            public HashSet<GridPoint> PreviousPoints;
        }

        private IEnumerable<int> AStar(IDefinedSizeGrid<int> grid, GridPoint start, GridPoint destination)
        {
            //var queue = new List<SearchNode> { new() { Point = start, Cost = 0} };
            //var searched = new HashSet<GridPoint>();

            var queue = new SimplePriorityQueue<SearchNode, int>();
            queue.Enqueue(new SearchNode { Point = start, Cost = 0, PreviousPoints = new HashSet<GridPoint> {new GridPoint(0, 0)}}, 0);

            var i = 0;

            while (queue.Any())
            {
                i++;
                // Console.Out.WriteLine($"Queue is {queue.Count}");
                // if (queue.Count > 5_000)
                // {
                //     queue.
                // }

                var next = queue.Dequeue();

                // Console.Out.WriteLine($"Trying {next}");
                if (i % 1_000 == 0)
                {
                // if (next.PreviousPoints.Any())
                // {
                Console.Out.WriteLine($"{i}, Queue: {queue.Count}, Node: {next}");
                Console.Out.WriteLine(next.PreviousPoints.ToGrid().AsDefinedSizeNotPreservingCoordinates().Dump());
                }
                // }

                if (next.Point == destination)
                {
                    Console.Out.WriteLine(next.PreviousPoints.ToGrid().AsDefinedSizeNotPreservingCoordinates().Dump());
                    yield return next.Cost;
                }

                foreach (var point in next.Point
                    .Adjacent4
                    .Where(p => grid.Region().ContainsPoint(p)))
                {
                    if (next.PreviousPoints.Contains(point)) continue;
                    var previousPoints = next.PreviousPoints.ToHashSet();
                    previousPoints.Add(point);
                    var searchNode = new SearchNode
                        { Cost = next.Cost + grid.Get(point), Point = point, PreviousPoints = previousPoints };
                    var heuristicScore = searchNode.Cost + Heuristic(grid, point, destination);
                    // if (heuristicScore > next.Cost)
                    // {
                    //     continue;
                    // }
                    queue.Enqueue(searchNode, heuristicScore);
                }

            }

            Console.Out.WriteLine("We're literally giving up");
        }

        private int Heuristic(IGrid<int> grid, GridPoint current, GridPoint destination)
        {
            return Math.Abs((destination - current).ManhattanDistanceFromOrigin());
        }
    }
}