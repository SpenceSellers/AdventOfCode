using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib.BreadthFirstSearch;

namespace AdventOfCode.Days._2021
{
    public class Advent202112 : Problem
    {
        public override object PartOne(string[] input)
        {
            var edges = input
                .Select(line => line.Split("-"))
                .Select(split => (split[0], split[1]))
                .SelectMany(a => new [] {a, (a.Item2, a.Item1)})
                .ToList();

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

        private bool IsLargeCave(string cave) => char.IsUpper(cave.First());

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}