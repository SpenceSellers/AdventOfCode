using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;

namespace AdventOfCode.Days._2022;

public class Advent202207 : Problem
{
    public override object PartOne(string[] input)
    {
        var root = BuildFilesystem(input);

        return root.GetAllSubDirectories()
            .Concat(new[] { root })
            .Select(x => x.GetSize())
            .Where(x => x <= 100_000)
            .Sum();
    }

    public override object PartTwo(string[] input)
    {
        var maxSpace = 70000000;
        var updateSpace = 30000000;
        var root = BuildFilesystem(input);
        var currentUsed = root.GetSize();

        var currentFree = maxSpace - currentUsed;
        var needToDelete = updateSpace - currentFree;
        return root.GetAllSubDirectories()
            .Concat(new[] { root })
            .Select(directory => directory.GetSize())
            .Where(size => size >= needToDelete)
            .Min();
    }

    private Directory BuildFilesystem(string[] input)
    {
        var root = new Directory();
        var currentPath = new List<string>();

        foreach (var line in input)
        {
            if (line.StartsWith("$ cd ", StringComparison.Ordinal))
            {
                var newDirectory = line.Split()[2];
                if (newDirectory == "/")
                {
                    currentPath.Clear();
                }
                else if (newDirectory == "..")
                {
                    currentPath.RemoveAt(currentPath.Count - 1);
                }
                else
                {
                    currentPath.Add(newDirectory);
                }
            }
            else if (line.StartsWith("dir", StringComparison.Ordinal))
            {
                // Just ignore it. We build directories by seeing files in them.
            }
            else if (line == "$ ls")
            {
                // Just ignore it
            }
            else
            {
                // This is a file listing
                var pieces = line.Split();
                var size = long.Parse(pieces[0]);
                var fileName = pieces[1];
                root.AddFile(CollectionsMarshal.AsSpan(currentPath), fileName, size);
            }
        }

        return root;
    }

    private class Directory : Entry
    {
        public readonly Dictionary<string, Entry> Members = new();

        public void AddFile(Span<string> path, string filename, long size)
        {
            if (path.Length == 0)
            {
                Members[filename] = new File { Size = size };
                return;
            }

            var nextDirName = path[0];

            if (!Members.ContainsKey(nextDirName))
            {
                Members[nextDirName] = new Directory();
            }

            var nextEntry = Members[nextDirName];
            if (nextEntry is not Directory nextDirectory)
            {
                throw new Exception("Uh oh... we're trying to add a file to a file");
            }

            nextDirectory.AddFile(path[1..], filename, size);
        }

        public override long GetSize()
        {
            return Members.Values.Select(entry => entry.GetSize()).Sum();
        }

        public IEnumerable<Directory> GetAllSubDirectories()
        {
            foreach (var member in Members.Values)
            {
                if (member is Directory directory)
                {
                    yield return directory;
                    foreach (var submember in directory.GetAllSubDirectories())
                    {
                        yield return submember;
                    }
                }
            }
        }
    }

    private class File : Entry
    {
        public required long Size;

        public override long GetSize()
        {
            return Size;
        }
    }

    private abstract class Entry
    {
        public abstract long GetSize();
    }
}