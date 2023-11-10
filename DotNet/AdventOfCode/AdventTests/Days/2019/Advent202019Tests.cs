using System.Collections.Generic;
using AdventOfCode.Days._2020;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests.Days._2019
{
    public class Advent202019Tests
    {
        
        [TestCase("c", true)]
        [TestCase("ad", true)]
        [TestCase("add", false)]
        [TestCase("ced", true)]
        [TestCase("cec", false)]
        [TestCase("ce", false)]
        public void LinerarlyParses(string s, bool expectation)
        {
            var dict = new Dictionary<int, RuleParser>();
            dict[0] = new ChoiceParser(new RuleParser[]
            {
                new SequenceParser(new RuleParser[]
                {
                    new LookupParser(1, dict),
                    new ConstantParser('d')
                }),
                new ConstantParser('c')
            });

            dict[1] = new ChoiceParser(new List<RuleParser>
            {
                new ConstantParser('a'),
                new SequenceParser(new List<RuleParser>
                {
                    new ConstantParser('c'),
                    new ConstantParser('e')
                })
            });

            Accepts(dict[0], s).Should().Be(expectation);
        }
        
        [TestCase("c", true)]
        [TestCase("ab", true)]
        [TestCase("aab", true)]
        [TestCase("aaaaaaaaaaaab", true)]
        
        [TestCase("a", false)]
        [TestCase("m", false)]
        [TestCase("aaaaaaaaaaaaaaaa", false)]
        [TestCase("b", false)]
        [TestCase("ba", false)]
        public void RecursivelyParses(string s, bool expectation)
        {
            var dict = new Dictionary<int, RuleParser>();
            dict[0] = new ChoiceParser(new RuleParser[]
            {
                new SequenceParser(new RuleParser[]
                {
                    new LookupParser(1, dict),
                    new ConstantParser('b')
                }),
                new ConstantParser('c')
            });

            dict[1] = new ChoiceParser(new List<RuleParser>
            {
                new ConstantParser('a'),
                new SequenceParser(new List<RuleParser>
                {
                    new ConstantParser('a'),
                    new LookupParser(1, dict)
                })
            });

            Accepts(dict[0], s).Should().Be(expectation);
        }
        
        [TestCase("x", true)]
        [TestCase("oxo", true)]
        [TestCase("ooxoo", true)]
        [TestCase("oooxooo", true)]
        [TestCase("ooooxoooo", true)]
        
        [TestCase("o", false)]
        [TestCase("ox", false)]
        [TestCase("xo", false)]
        [TestCase("oooxoo", false)]
        [TestCase("oooxoooo", false)]
        [TestCase("ooooxoodo", false)]
        public void RecursivelyParsesBalanced(string s, bool expectation)
        {
            var dict = new Dictionary<int, RuleParser>();
            dict[0] = new ChoiceParser(new RuleParser[]
            {
                new SequenceParser(new RuleParser[]
                {
                    new ConstantParser('o'),
                    new LookupParser(0, dict),
                    new ConstantParser('o')
                }),
                new ConstantParser('x')
            });

            Accepts(dict[0], s).Should().Be(expectation);
        }

        private bool Accepts(RuleParser rule, string s)
        {
            var accepts = rule.Accepts(s);
            return accepts == s.Length;
        }
    }
}