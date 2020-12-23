using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202022 : Problem
    {
        public override object PartOne(string[] input)
        {
            var (deck1, deck2) = ParseDecks(input);

            while (true)
            {
                if (deck1.Count == 0 || deck2.Count == 0)
                {
                    break;
                }

                var d1top = PopFirst(deck1);
                var d2top = PopFirst(deck2);

                if (d1top > d2top)
                {
                    deck1.Add(d1top);
                    deck1.Add(d2top);
                }
                else
                {
                    deck2.Add(d2top);
                    deck2.Add(d1top);
                }
            }

            var winningDeck = new[] {deck1, deck2}.First(deck => deck.Any());
            winningDeck.Reverse();
            return winningDeck.Select((x, i) => (i + 1) * x).Sum();
        }
        
        private List<List<int>> ParseDecks(string[] input) =>
            input
                .Where(i => i.Length != 0)
                .SplitList(x => x.Contains("Player"))
                .Select(l => l.Select(int.Parse).ToList())
                .Where(l => l.Any())
                .ToList();

        private static T PopFirst<T>(IList<T> list)
        {
            var first = list[0];
            list.RemoveAt(0);
            return first;
        }

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}