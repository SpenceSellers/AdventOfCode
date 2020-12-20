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

            return inputLines
                .Count(line => rootParser.Accepts(line) == line.Length)
                .ToString();
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
                return s[0] == _character ? 1 : -1;
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
                foreach (var parser in _parsers)
                {
                    var accepted = parser.Accepts(s);
                    if (accepted >= 0)
                    {
                        return accepted;
                    }
                }

                return -1;
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
        }
    }
}