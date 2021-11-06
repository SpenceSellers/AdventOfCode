using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days._2020
{
    public class Advent202020 : Problem
    {
        public override object PartOne(string[] input)
        {
            var grids = ParseGrids(input);

            // Create a map of which sides are shared between which grids
            var sideLookup = new Dictionary<long, HashSet<int>>();
            foreach (var (gridId, grid) in grids)
            {
                var sides = GetSides(grid).Select(Binary.FromBools).ToArray();
                foreach (var side in sides)
                {
                    if (!sideLookup.ContainsKey(side))
                    {
                        sideLookup.Add(side, new HashSet<int>());
                    }

                    // Remember that this side has this ID.
                    sideLookup[side].Add(gridId);
                }
            }

            // Create a map of which grids connect to which other grids
            var connectsTo = grids.Keys.ToDictionary(x => x, _ => new HashSet<int>());
            foreach (var idsConnected in sideLookup.Values)
            {
                foreach (var id in idsConnected)
                {
                    connectsTo[id].UnionWith(idsConnected.Where(otherId => otherId != id));
                }
            }

            // Corner tiles will only connect on two sides
            return connectsTo.Where(kv => kv.Value.Count == 2).Select(kv => kv.Key).Aggregate(1L, (a, b) => a * b)
                .ToString();
        }
        
        public override object PartTwo(string[] input)
        {
            
            var grids = ParseGrids(input);

            var sidesForGrid = new Dictionary<int, HashSet<long>>();
            // Create a map of which sides are shared between which grids
            var gridsForSide = new Dictionary<long, HashSet<int>>();
            foreach (var (gridId, grid) in grids)
            {
                var sides = GetSides(grid).Select(Binary.FromBools).ToArray();
                sidesForGrid.Add(gridId, sides.ToHashSet());
                foreach (var side in sides)
                {
                    if (!gridsForSide.ContainsKey(side))
                    {
                        gridsForSide.Add(side, new HashSet<int>());
                    }

                    // Remember that this side has this ID.
                    gridsForSide[side].Add(gridId);
                }
            }

            // Create a map of which grids connect to which other grids
            var connectsTo = grids.Keys.ToDictionary(x => x, _ => new HashSet<int>());
            foreach (var idsConnected in gridsForSide.Values)
            {
                foreach (var id in idsConnected)
                {
                    connectsTo[id].UnionWith(idsConnected.Where(otherId => otherId != id));
                }
            }

            // Corner tiles will only connect on two sides
            var corners = connectsTo.Where(kv => kv.Value.Count == 2).Select(kv => kv.Key).ToList();
            var firstCorner = corners[2];

            foreach (var theCorner in sidesForGrid[firstCorner])
            {
                var allSides = grids
                    .Where(kv => kv.Key != firstCorner)
                    .SelectMany(kv => AllRotationsAndMirrorings(kv.Value).Select(rotm => (kv.Key, rotm)))
                    .Select(kv => (kv.Key, GetTopSide(kv.rotm)))
                    .Where(kv => kv.Item2 == theCorner);
            }

            return null;
        }

        private Dictionary<int, IDefinedSizeGrid<bool>> ParseGrids(string[] input)
        {
            var chunks = input.SplitList(line => line.Length == 0);
            var grids = new Dictionary<int, IDefinedSizeGrid<bool>>();
            var tileNameRegex = new Regex(@"Tile (\d+):");
            foreach (var chunk in chunks)
            {
                if (chunk.Count == 0) continue;
                var tileId = int.Parse(tileNameRegex.Match(chunk[0]).Groups[1].Value);
                grids[tileId] = SolidGrid<char>.Extract(chunk.Skip(1)).Map(c => c is '#').Solidify();
            }

            return grids;
        }

        private IEnumerable<bool[]> GetSides(IDefinedSizeGrid<bool> grid)
        {
            var size = grid.Region().Width;
            var region = new GridRegion(GridPoint.Origin, size, 1);
            yield return grid.Windowed(region).AllCells().ToArray();
            yield return grid.Windowed(region).AllCells().Reverse().ToArray();
            grid = grid.RotateClockwise();
            yield return grid.Windowed(region).AllCells().ToArray();
            yield return grid.Windowed(region).AllCells().Reverse().ToArray();
            grid = grid.RotateClockwise();
            yield return grid.Windowed(region).AllCells().ToArray();
            yield return grid.Windowed(region).AllCells().Reverse().ToArray();
            grid = grid.RotateClockwise();
            yield return grid.Windowed(region).AllCells().ToArray();
            yield return grid.Windowed(region).AllCells().Reverse().ToArray();
        }

        private long GetTopSide(IDefinedSizeGrid<bool> grid)
        {
            var size = grid.Region().Width;
            var region = new GridRegion(GridPoint.Origin, size, 1);
            return Binary.FromBools(grid.Windowed(region).AllCells());
        }
        
        private IEnumerable<IDefinedSizeGrid<bool>> AllRotationsAndMirrorings(IDefinedSizeGrid<bool> grid)
        {
            for (var i = 0; i < 4; i++)
            {
                yield return grid;
                yield return grid.FlipHorizontally();
                yield return grid.FlipVertically();

                grid = grid.RotateClockwise();
            }
        }
    }
}