using System.Collections.Generic;

namespace AdventOfCode
{
    public static class Binary
    {
        public enum Endianness
        {
            BigEndian,
            LittleEndian
        }
        
        public static long FromBools(IEnumerable<bool> list)
        {
            // TODO, endianness
            long accumulate = 0;
            long mask = 1;
            foreach (var b in list)
            {
                if (b)
                {
                    accumulate |= mask;
                }

                mask <<= 1;
            }

            return accumulate;
        }
    }
}