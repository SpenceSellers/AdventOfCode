using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.AdventLib.BreadthFirstSearch
{
    public class BreadthFirstSearch<TState>
    {
        private readonly BfsSearchConfig<TState> _config;
        private HashSet<object> _seenStates = new();
        private Queue<TState> _queue = new();

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
            QueueAll(new []{initialState});

            while (_queue.Any())
            {
                var next = _queue.Dequeue();

                Console.Out.WriteLine(next);
                // Is this one of the winning states we're looking for?
                if (_config.IsSuccessState(next))
                {
                    yield return next;
                }

                var seenKey = _config.SeenKey(next);
                if (seenKey is not null)
                {
                    _seenStates.Add(seenKey);
                }

                // Don't queue if the current node can never lead to a solution node
                if (_config.CanLeadToSuccessState(next))
                {
                    var nextStates = NextStates(next)
                        .Where(s => !HasSeenItem(s))
                        .ToList();
                    QueueAll(nextStates);
                }
            }
        }

        private bool HasSeenItem(TState item)
        {
            var seenKey = _config.SeenKey(item);
            if (seenKey is null)
            {
                return false;
            }

            return _seenStates.Contains(seenKey);
        }

        private IEnumerable<TState> NextStates(TState node)
        {
            return _config.NextStates(node);
        }

        private void QueueAll(IEnumerable<TState> items)
        {
            foreach (var item in items)
            {
                _queue.Enqueue(item);
            }
        }
    }
}