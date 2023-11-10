using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace AdventOfCode.AdventLib.AStar;

public class AStarNode
{
    
}

public class UnreachableGoalException : Exception
{
}

public class AStarSearch<T>
{
    public required T Start { init; get; }
    public required Func<T, bool> IsGoal { init; get; }
    // Leaving out the heuristic creates an Djikstra's search
    
    /// <summary>
    /// It's OK if this is too small, but you'll get the wrong answer if you make the answer too big.
    /// </summary>
    public Func<T, long> Heuristic = _ => 0;

    public required Func<T, IEnumerable<(T neighbor, long weight)>> Neighbors { init; get; }

    public List<T> Search()
    {
        // var openSet = new PriorityQueue<T, long>();
        // openSet.Enqueue(Start, Heuristic(Start));
        var openSet = new HashSet<T> {Start};
        
        // The cheapest path from start to N currently known
        var cameFrom = new Dictionary<T, T>();

        var cheapestToHereSoFar = new Dictionary<T, long>
        {
            [Start] = 0
        };

        var cheapestPathToGoalBestGuess = new Dictionary<T, long>
        {
            [Start] = Heuristic(Start)
        };

        while (openSet.Any())
        {
            // TODO: This is slow, it'd be best to keep the elements in sorted order to begin with.
            var current = openSet.MinBy(x => cheapestPathToGoalBestGuess.GetValueOrDefault(x, long.MaxValue));

            if (IsGoal(current))
            {
                return ReconstructPath(cameFrom, current);
            }

            openSet.Remove(current);

            foreach (var (neighbor, weight) in Neighbors(current))
            {
                var scoreForThisPath = cheapestToHereSoFar.GetValueOrDefault(current, long.MaxValue) + weight;
                if (scoreForThisPath < cheapestToHereSoFar.GetValueOrDefault(neighbor, long.MaxValue))
                {
                    cameFrom[neighbor] = current;
                    cheapestToHereSoFar[neighbor] = scoreForThisPath;
                    cheapestPathToGoalBestGuess[neighbor] = scoreForThisPath + Heuristic(neighbor);
                    if (!openSet.Contains(neighbor))
                    {
                        openSet.Add(neighbor);
                    }
                }
            }
        }

        throw new UnreachableGoalException();
    }

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
}