using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2021;

public class Advent202116 : Problem
{
    private class PacketParser
    {
        private readonly string _input;
        private int _pos;

        public PacketParser(string input)
        {
            _input = input;
            _pos = 0;
        }

        public record Packet
        {
            public int Version { get; init; }
            public int Type { get; init; }
            public string Payload { get; init; }
            public List<Packet> ChildPackets { get; init; } = new();

            public long PayloadLong => Convert.ToInt64(Payload, 2);

            public int VersionSum()
            {
                return Version + ChildPackets.Select(c => c.VersionSum()).Sum();
            }

            public long Evaluate()
            {
                var childValues = ChildPackets.Select(p => p.Evaluate()).ToList();
                return Type switch
                {
                    0 => ChildPackets.Sum(p => p.Evaluate()),
                    1 => childValues.Product(),
                    2 => childValues.Min(),
                    3 => childValues.Max(),
                    4 => PayloadLong,
                    5 => childValues[0] > childValues[1] ? 1 : 0,
                    6 => childValues[0] < childValues[1] ? 1 : 0,
                    7 => childValues[0] == childValues[1] ? 1 : 0,
                    _ => throw new Exception($"Unknown packet type {Type}")
                };
            }
        }

        public Packet ParsePacket()
        {
            var version = ReadInt(3);
            var type = ReadInt(3);

            if (type == 4)
            {
                var payload = ParseLiteralPacketData();
                return new Packet
                {
                    Version = version,
                    Type = type,
                    Payload = payload,
                };
            }

            var lengthType = ReadInt(1);
            if (lengthType == 0)
            {
                var childPackets = ParseBitLengthChildPackets();

                return new Packet
                {
                    Version = version,
                    Type = type,
                    ChildPackets = childPackets
                };
            }
            else
            {
                var childPackets = ParseCountedChildPackets();

                return new Packet
                {
                    Version = version,
                    Type = type,
                    ChildPackets = childPackets
                };
            }
        }

        private List<Packet> ParseCountedChildPackets()
        {
            var childPacketCount = ReadInt(11);
            var childPackets = new List<Packet>();
            for (var i = 0; i < childPacketCount; i++)
            {
                childPackets.Add(ParsePacket());
            }

            return childPackets;
        }

        private List<Packet> ParseBitLengthChildPackets()
        {
            var length = ReadInt(15);
            var lengthStart = _pos;
            var childPackets = new List<Packet>();
            while (_pos != lengthStart + length)
            {
                childPackets.Add(ParsePacket());
            }

            return childPackets;
        }

        private string ParseLiteralPacketData()
        {
            var payloadData = new StringBuilder();
            var more = true;
            while (more)
            {
                more = ReadN(1) == "1";
                payloadData.Append(ReadN(4));
            }

            return payloadData.ToString();
        }

        private string ReadN(int n)
        {
            var res = _input[_pos..(_pos + n)];
            _pos += n;
            return res;
        }

        private int ReadInt(int length)
        {
            var bits = ReadN(length);
            return Convert.ToInt32(bits, 2);
        }
    }

    public override object PartOne(string[] input)
    {
        return ParsePacket(input).VersionSum();
    }

    public override object PartTwo(string[] input)
    {
        return ParsePacket(input).Evaluate();
    }

    private static PacketParser.Packet ParsePacket(string[] input)
    {
        var bits = string.Join("", input[0].Select(c => HexLookup[c]));
        var parser = new PacketParser(bits);
        return parser.ParsePacket();
    }

    private static readonly Dictionary<char, string> HexLookup = new()
    {
        { '0', "0000" },
        { '1', "0001" },
        { '2', "0010" },
        { '3', "0011" },
        { '4', "0100" },
        { '5', "0101" },
        { '6', "0110" },
        { '7', "0111" },
        { '8', "1000" },
        { '9', "1001" },
        { 'A', "1010" },
        { 'B', "1011" },
        { 'C', "1100" },
        { 'D', "1101" },
        { 'E', "1110" },
        { 'F', "1111" }
    };
}