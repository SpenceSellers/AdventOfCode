using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib.BreadthFirstSearch;

namespace AdventOfCode.Days._2021
{
    public class Advent202112 : Problem
    {
        public override object PartOne(string[] input)
        {
            var edges = ParseDirectedEdges(input);

            var bfsConfig = new BfsSearchConfig<List<string>>
            {
                InitialState = new List<string> {"start"},
                IsSuccessState = path => path.Last() == "end",
                NextStates = (path) =>
                {
                    var last = path.Last();
                    var possibleWaysToGo = edges
                        .Where1(cave => cave == last)
                        .Select2()
                        .Where(cave => IsLargeCave(cave) || !path.Contains(cave));

                    return possibleWaysToGo.Select(next => path.Append(next).ToList());
                }
            };

            var bfs = new BreadthFirstSearch<List<string>>(bfsConfig);
            var paths = bfs.Bfs().ToList();
            return paths.Count;
        }

        public override object PartTwo(string[] input)
        {
            var edges = ParseDirectedEdges(input);

            var bfsConfig = new BfsSearchConfig<List<string>>
            {
                InitialState = new List<string> { "start" },
                IsSuccessState = path => path.Last() == "end",
                NextStates = (path) =>
                {
                    var last = path.Last();
                    var smallCavesVisited = path
                        .Where(cave => cave is not "start" or "end")
                        .Where(cave => !IsLargeCave(cave))
                        .ToList();
                    var haveVisitedSmallCaveTwice = smallCavesVisited.Count != smallCavesVisited.ToHashSet().Count;
                    var possibleWaysToGo = edges
                        .Where1(cave => cave == last)
                        .Select2()
                        .Where(cave =>
                        {
                            if (IsLargeCave(cave))
                            {
                                return true;
                            }

                            if (haveVisitedSmallCaveTwice)
                            {
                                return !path.Contains(cave);
                            }
                            else
                            {
                                if (cave is "start" or "end")
                                {
                                    return !path.Contains(cave);
                                }
                                else
                                {
                                    return true;
                                }
                            }
                        });

                    return possibleWaysToGo.Select(next => path.Append(next).ToList());
                }
            };

            var bfs = new BreadthFirstSearch<List<string>>(bfsConfig);
            var paths = bfs.Bfs().ToList();
            return paths.Count;
        }

        private static List<(string, string)> ParseDirectedEdges(string[] input)
        {
            var edges = input
                .Select(line => line.Split("-"))
                .Select(split => (split[0], split[1]))
                .SelectMany(a => new[] { a, (a.Item2, a.Item1) })
                .ToList();
            return edges;
        }

        private bool IsLargeCave(string cave) => char.IsUpper(cave.First());
    }
}