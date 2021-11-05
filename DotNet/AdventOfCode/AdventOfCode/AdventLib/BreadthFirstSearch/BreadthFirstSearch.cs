using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.AdventLib.BreadthFirstSearch
{
    public class BreadthFirstSearch<TState>
    {
        private readonly BfsSearchConfig<TState> _config;

        public BreadthFirstSearch(BfsSearchConfig<TState> config)
        {
            _config = config;
        }

        /// <summary>
        /// Performs a breadth-first search across the space of all possible JSON strings.
        /// </summary>
        public IEnumerable<TState> Bfs()
        {
            var initialState = _config.InitialState;
            var queue = new Queue<TState>();
            QueueAll(queue, new []{initialState});

            var count = 0;

            while (queue.Any())
            {
                count++;
                var next = queue.Dequeue();

                // Is this one of the winning states we're looking for?
                if (_config.IsSuccessState(next))
                {
                    yield return next;
                }

                // Don't queue if the current node can never lead to a solution node
                if (_config.CanLeadToSuccessState(next))
                {
                    var nextStates = NextStates(next).ToList();
                    QueueAll(queue, nextStates);
                }
            }
        }


        private IEnumerable<TState> NextStates(TState node)
        {
            return _config.NextStates(node);
        }

        private static void QueueAll<T>(Queue<T> queue, IEnumerable<T> items)
        {
            foreach (var item in items)
            {
                queue.Enqueue(item);
            }
        }
    }
}