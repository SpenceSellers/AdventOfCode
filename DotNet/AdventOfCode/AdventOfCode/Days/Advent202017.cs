using System;
using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days
{
    public class Advent202017 : Problem
    {
        public Advent202017() : base(2020, 17)
        {
        }

        public override string PartOne(string[] input)
        {
            return new GridProblem3D().Solve(input);
        }

        public override string PartTwo(string[] input)
        {
            return new GridProblem4D().Solve(input);
        }
        
        private static HashSet<Point3D> BuildInitialState(SolidGrid<bool> baseGrid)
        {
            var initialState = new HashSet<Point3D>();
            foreach (var point in baseGrid.Region().AllPoints())
            {
                if (baseGrid.Get(point))
                {
                    var point3d = new Point3D(point.X, point.Y, 0);
                    initialState.Add(point3d);
                }
            }

            return initialState;
        }

        private abstract class GridProblem
        {
            protected abstract IEnumerable<Point3D> AdjacentPoints(Point3D activePoint);

            public string Solve(string[] input)
            {
                var baseGrid = SolidGrid<char>.Extract(input).Map(x => x == '#').Solidify();

                var initialState = BuildInitialState(baseGrid);

                var state = initialState;

                for (var i = 0; i < 6; i++)
                {
                    state = NextStep(state);
                }

                return state.Count.ToString();
            }
            
            private HashSet<Point3D> PointsToConsider(HashSet<Point3D> grid)
            {
                var points = new HashSet<Point3D>();
                foreach (var activePoint in grid)
                {
                    points.Add(activePoint);
                    foreach (var adjacentPoint in AdjacentPoints(activePoint))
                    {
                        points.Add(adjacentPoint);
                    }
                }

                return points;
            }

            private HashSet<Point3D> NextStep(HashSet<Point3D> grid)
            {
                var pointsToConsider = PointsToConsider(grid);
                var nextState = new HashSet<Point3D>();
                foreach (var point in pointsToConsider)
                {
                    var currentlyActive = grid.Contains(point);
                    var adjacentPoints = AdjacentPoints(point).Count(grid.Contains);
                    var nowActive = currentlyActive switch
                    {
                        true => adjacentPoints == 2 || adjacentPoints == 3,
                        false => adjacentPoints == 3
                    };
                    if (nowActive)
                    {
                        nextState.Add(point);
                    }
                }

                return nextState;
            }
        }

        private class GridProblem3D : GridProblem
        {
            protected override IEnumerable<Point3D> AdjacentPoints(Point3D point)
            {
                for (var dx = -1; dx <= 1; dx++)
                {
                    for (var dy = -1; dy <= 1; dy++)
                    {
                        for (var dz = -1; dz <= 1; dz++)
                        {
                            if (dx == 0 && dy == 0 && dz == 0) continue;
                            yield return new Point3D(point.X + dx, point.Y + dy, point.Z + dz);
                        }
                    }
                }
            }
        }

        private class GridProblem4D : GridProblem
        {
            protected override IEnumerable<Point3D> AdjacentPoints(Point3D point)
            {
                for (var dx = -1; dx <= 1; dx++)
                {
                    for (var dy = -1; dy <= 1; dy++)
                    {
                        for (var dz = -1; dz <= 1; dz++)
                        {
                            for (var dw = -1; dw <= 1; dw++)
                            {
                                if (dx == 0 && dy == 0 && dz == 0 && dw == 0) continue;
                                yield return new Point3D(point.X + dx, point.Y + dy, point.Z + dz, point.W + dw);
                            }
                        }
                    }
                }
            }
        }
        
        private class Point3D
        {
            public readonly int X;
            public readonly int Y;
            public readonly int Z;
            public readonly int W;

            public Point3D(int x, int y, int z, int w=0)
            {
                X = x;
                Y = y;
                Z = z;
                W = w;
            }

            protected bool Equals(Point3D other)
            {
                return X == other.X && Y == other.Y && Z == other.Z && W == other.W;
            }

            public override bool Equals(object obj)
            {
                if (ReferenceEquals(null, obj)) return false;
                if (ReferenceEquals(this, obj)) return true;
                if (obj.GetType() != this.GetType()) return false;
                return Equals((Point3D) obj);
            }

            public override int GetHashCode()
            {
                return HashCode.Combine(X, Y, Z, W);
            }
        }
    }
}