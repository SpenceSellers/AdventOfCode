using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.AdventLib.AStar;

public class AStarNode
{
    
}
public class AStarSearch<T>
{
    public T Start;
    public T Goal;
    public Func<T, long> Heuristic;
    public Func<T, IEnumerable<(T, long)>> Neighbors;

    private List<T> ReconstructPath(Dictionary<T, T> cameFrom, T current)
    {
        var path = new List<T> {current};
        while (cameFrom.ContainsKey(current))
        {
            current = cameFrom[current];
            path.Add(current);
        }

        path.Reverse();
        return path;
    }

    public List<T> Search()
    {
        var openSet = new HashSet<T> {Start};

        // The cheapest path from start to N currently known
        var cameFrom = new Dictionary<T, T>();

        // The cheapest path from start to N currently known
        var gScore = new Dictionary<T, long>();
        gScore[Start] = 0;

        var fScore = new Dictionary<T, long>();
        fScore[Start] = Heuristic(Start);

        while (openSet.Any())
        {
            Console.Out.WriteLine($"Openset {openSet.Count}");
            var current = openSet.MinBy(x => fScore.GetValueOrDefault(x, long.MaxValue));

            // Todo make this a IsGoal function
            if (current.Equals(Goal))
            {
                return ReconstructPath(cameFrom, current);
            }

            openSet.Remove(current);

            foreach (var (neighbor, weight) in Neighbors(current))
            {
                var tenativeGscore = gScore.GetValueOrDefault(current, long.MaxValue) + weight;
                if (tenativeGscore < gScore.GetValueOrDefault(neighbor, long.MaxValue))
                {
                    cameFrom[neighbor] = current;
                    gScore[neighbor] = tenativeGscore;
                    fScore[neighbor] = tenativeGscore + Heuristic(neighbor);
                    if (!openSet.Contains(neighbor))
                    {
                        openSet.Add(neighbor);
                    }
                }
            }
        }

        throw new Exception("It's impossible to reach the goal");
    }
}