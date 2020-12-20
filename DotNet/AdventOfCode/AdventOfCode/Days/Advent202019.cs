using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202019 : Problem
    {
        public override string PartOne(string[] input)
        {
            return SolveDay(input);
        }
        
        public override string PartTwo(string[] input)
        {
            var hacked = input.Select(line =>
            {

                if (line.StartsWith("8:"))
                {
                    return "8: 42 | 42 8";
                }
                
                if (line.StartsWith("11:"))
                {
                    return "11: 42 31 | 42 11 31";
                }

                return line;

            });
            // 221 is wrong
            return SolveDay(hacked);
        }

        private string SolveDay(IEnumerable<string> input)
        {
            var (ruleLines, inputLines) = input.SplitList(l => l.Length == 0);

            var ruleLookup = new Dictionary<int, Parser>();
            foreach (var ruleLine in ruleLines)
            {
                var (key, parser) = ParseRule(ruleLine, ruleLookup);
                ruleLookup.Add(key, parser);
            }

            var rootParser = ruleLookup[0];

            var acceptable = inputLines
                .Where(line =>
                {
                    var accepted = rootParser.Accepts(line);
                    Console.Out.WriteLine($"{line.PadRight(20)} - {accepted} - {accepted == line.Length}");
                    return accepted == line.Length;
                })
                .ToList();
            
            return acceptable.Count.ToString();
        }

        private (int, Parser) ParseRule(string line, Dictionary<int, Parser> lookup)
        {
            var (keyString, ruleString) = line.Split(":").Select(s => s.Trim());
            var orPieces = ruleString.Split("|").Select(s => s.Trim());
            var orRules = orPieces.Select(orPiece =>
            {
                var sequencePieces = orPiece.Split(" ");
                var sequenceParsers = sequencePieces.Select<string, Parser>(sequencePiece =>
                {
                    if (int.TryParse(sequencePiece, out var key))
                    {
                        return new LookupParser(key, lookup);
                    }
                    else
                    {
                        return new ConstantParser(sequencePiece[1]);
                    }
                })
                    .ToList();
                return new SequenceParser(sequenceParsers);
            });

            return (int.Parse(keyString), new ChoiceParser(orRules.Cast<Parser>().ToList()));
        }

        private abstract class Parser
        {
            public abstract int Accepts(string s);
        }

        private class ConstantParser : Parser
        {
            private readonly char _character;

            public ConstantParser(char character)
            {
                _character = character;
            }
            
            public override int Accepts(string s)
            {
                if (s.Length == 0)
                {
                    return -1;
                }
                return s[0] == _character ? 1 : -1;
            }

            public override string ToString()
            {
                return $"\"{_character}\"";
            }
        }

        private class SequenceParser : Parser
        {
            private readonly IList<Parser> _parsers;

            public SequenceParser(IList<Parser> parsers)
            {
                _parsers = parsers;
            }
            
            public override int Accepts(string s)
            {
                var accepted = 0;

                foreach (var parser in _parsers)
                {
                    var thisParserAccepted = parser.Accepts(s.Substring(accepted));
                    if (thisParserAccepted < 0)
                    {
                        return -1;
                    }

                    accepted += thisParserAccepted;
                }

                return accepted;
            }

            public override string ToString()
            {
                return string.Join(" ", _parsers);
            }
        }

        private class ChoiceParser : Parser
        {
            private readonly IList<Parser> _parsers;

            public ChoiceParser(IList<Parser> parsers)
            {
                _parsers = parsers;
            }
            
            public override int Accepts(string s)
            {
                // Console.Out.WriteLine(string.Join(',', _parsers.Select(p => p.Accepts(s)).ToList()));
                // return _parsers.Select(p => p.Accepts(s)).Max();
                foreach (var parser in _parsers)
                {
                    var accepted = parser.Accepts(s);
                    // So. We're probably being too "greedy" by accepting the very first parser that works out for us, even if it doesn't parse all the way.
                    // But that's how these rules work right?
                    // We need to be able to backtrack across the whole program.
                    
                    // Going too short needs to be counted as "bad" as going too far.
                    
                    // Crap, it may make sense to return BOTH accepted answers.
                    
                    if (accepted >= 0)
                    {
                        return accepted;
                    }
                }

                return -1;
            }

            public override string ToString()
            {
                return string.Join("|", _parsers);
            }
        }

        private class LookupParser : Parser
        {
            private readonly int _key;
            private readonly Dictionary<int, Parser> _parserTable;

            public LookupParser(int key, Dictionary<int, Parser> parserTable)
            {
                _key = key;
                _parserTable = parserTable;
            }
            
            public override int Accepts(string s)
            {
                return _parserTable[_key].Accepts(s);
            }

            public override string ToString()
            {
                return _key.ToString();
            }
        }
    }
}