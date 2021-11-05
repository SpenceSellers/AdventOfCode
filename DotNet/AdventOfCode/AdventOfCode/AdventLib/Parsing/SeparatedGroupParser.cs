using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.AdventLib.Parsing
{
    /// <summary>
    /// Parses groups of input separated by newlines.
    /// </summary>
    public class SeparatedGroupParser
    {
        public IEnumerable<IEnumerable<string>> Parse(IEnumerable<string> input)
        {
            var group = new List<string>();
            foreach (var line in input)
            {
                if (line == "")
                {
                    // We're at the end of a group
                    yield return group;
                    group = new List<string>();
                }
                else
                {
                    group.Add(line);
                }
            }

            // Spit out the last group even if there was no blank line
            if (group.Any())
            {
                yield return group;
            }
        }
    }
}