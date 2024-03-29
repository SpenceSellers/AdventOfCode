using System;
using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2020
{
    public class Advent202022 : Problem
    {
        public override object PartOne(string[] input)
        {
            var (deck1, deck2) = ParseDecks(input);

            PlaySingleGame(deck1, deck2);

            var winningDeck = new[] {deck1, deck2}.First(deck => deck.Any());
            winningDeck.Reverse();
            return winningDeck.Select((x, i) => (i + 1) * x).Sum();
        }
        
        public override object PartTwo(string[] input)
        {
            var (deck1, deck2) = ParseDecks(input);
            PlayRecursiveGame(deck1, deck2);
            var winningDeck = new[] {deck1, deck2}.First(deck => deck.Any());
            winningDeck.Reverse();
            return winningDeck.Select((x, i) => (i + 1) * x).Sum();
        }

        private static void PlaySingleGame(IList<int> deck1, IList<int> deck2)
        {
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
        }

        private static List<((List<int>, List<int>), int)> _winnerCache = new();
        
        private static int PlayRecursiveGame(IList<int> deck1, IList<int> deck2)
        {
            var previousRounds = new List<(List<int>, List<int>)>();

            var previndex = _winnerCache.FindIndex(prev => prev.Item1.Item1.SequenceEqual(deck1) && prev.Item1.Item2.SequenceEqual(deck2));
            if (previndex >= 0)
            {
                Console.Out.WriteLine("CACHE HIT");
                return _winnerCache[previndex].Item2;
            }

            bool HavePlayedThisRoundBefore() => previousRounds.Any(prev => prev.Item1.SequenceEqual(deck1) && prev.Item2.SequenceEqual(deck2));

            while (true)
            {
                if (deck1.Count == 0)
                {
                    return 2;
                }

                if (deck2.Count == 0)
                {
                    return 1;
                }

                if (HavePlayedThisRoundBefore())
                {
                    return 1;
                }
                previousRounds.Add((deck1.ToList(), deck2.ToList()));

                var d1top = PopFirst(deck1);
                var d2top = PopFirst(deck2);

                if (d1top > deck1.Count || d2top > deck2.Count)
                {
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
                else
                {
                    var ddd1 = deck1.Take(d1top).ToList();
                    var ddd2 = deck2.Take(d1top).ToList();
                    var winner = PlayRecursiveGame(deck1.Take(d1top).ToList(), deck2.Take(d2top).ToList());
                    _winnerCache.Add(((ddd1, ddd2), winner));
                    if (winner == 1)
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
            }
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

        // private class RecursiveCombatGame
        // {
        //     public List<(List<int>, List<int>)> PreviousRounds = new();
        //
        //     public int PlayGame(List<int> deck1, List<int> deck2)
        //     {
        //         
        //     }
        //     
        // }

    }
}