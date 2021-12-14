using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode.AdventLib
{
    public static class RegexExtensions
    {
        public static IEnumerable<string> Captures(this Regex regex, string s)
        {
            var match = regex.Match(s);
            return match.Groups.Values.Skip(1).Select(g => g.Value);
        }
    }
}