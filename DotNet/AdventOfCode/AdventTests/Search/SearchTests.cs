using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib.BreadthFirstSearch;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests.Search
{
    public class SearchTests
    {
        [Test]
        public void ShouldFindBasicSolution()
        {
            var config = new BfsSearchConfig<int>
            {
                InitialState = 0,
                IsSuccessState = x => x == 5,
                NextStates = x => new[] { x + 1, x - 1 },
                SeenKey = i => null
            };

            var bfs = new BreadthFirstSearch<int>(config);
            var firstResult = bfs.Bfs().First();
            firstResult.Should().Be(5);
        }

        [Test]
        public void ShouldFindDeepSolution()
        {
            var config = new BfsSearchConfig<int>
            {
                InitialState = 0,
                IsSuccessState = x => x == 1000,
                NextStates = x => new[] { x + 1, x - 1 },
                SeenKey = i => i
            };

            var bfs = new BreadthFirstSearch<int>(config);
            var firstResult = bfs.Bfs().First();
            firstResult.Should().Be(1000);
        }

        private record StateTracker
        {
            public List<string> Actions;
            public int State;
        }

        [Test]
        public void ShouldBeFlexibleEnoughToTrackHistory()
        {
            var config = new BfsSearchConfig<StateTracker>
            {
                InitialState = new StateTracker {State = 1, Actions = new List<string>()},
                IsSuccessState = x => x.State == 1022,
                NextStates = x => new[]
                {
                    new StateTracker {State = x.State * 2, Actions = x.Actions.Append("double").ToList()},
                    new StateTracker {State = x.State - 1, Actions = x.Actions.Append("subtract").ToList()},
                },
                SeenKey = i => i.State
            };

            var bfs = new BreadthFirstSearch<StateTracker>(config);
            var firstResult = bfs.Bfs().First();
            firstResult.State.Should().Be(1022);
            string.Join(", ", firstResult.Actions).Should()
                .Be("double, double, double, double, double, double, double, double, double, subtract, double");
        }
    }
}