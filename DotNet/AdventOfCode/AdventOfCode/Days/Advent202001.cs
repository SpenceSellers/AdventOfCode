using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202001 : Problem
    {
        public Advent202001() : base(2020, 01)
        {
        }
        
        public override string PartOne(string[] input)
        {
            var nums = input.Select(int.Parse).ToList();
            for (var i = 0; i < nums.Count; i++)
            {
                for (var j = 0; j < nums.Count; j++)
                {
                    if (i == j) continue;
                    var ivalue = nums[i];
                    var jvalue = nums[j];

                    if (ivalue + jvalue == 2020)
                    {
                        return (ivalue * jvalue).ToString();
                    }
                }
            }

            return null;
        }

        public override string PartTwo(string[] input)
        {
            var nums = input.Select(int.Parse).ToList();
            for (var i = 0; i < nums.Count; i++)
            {
                for (var j = 0; j < nums.Count; j++)
                {
                    for (var k = 0; k < nums.Count; k++)
                    {
                        if (i == j || i == k) continue;
                        var ivalue = nums[i];
                        var jvalue = nums[j];
                        var kvalue = nums[k];

                        if (ivalue + jvalue + kvalue == 2020)
                        {
                            return (ivalue * jvalue * kvalue).ToString();
                        }
                    }
                }
            }

            return null;
        }
    }
}