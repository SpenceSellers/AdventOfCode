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
            .Prepend(root)
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
            .Prepend(root)
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
                switch (newDirectory)
                {
                    case "/":
                        currentPath.Clear();
                        break;
                    case "..":
                        currentPath.RemoveAt(currentPath.Count - 1);
                        break;
                    default:
                        currentPath.Add(newDirectory);
                        break;
                }
            }
            else
            {
                var pieces = line.Split();

                // We're going to ignore ls and dir entries. We don't need them.
                if (!char.IsDigit(pieces[0][0])) continue;

                // This is a file listing
                var size = long.Parse(pieces[0]);
                var fileName = pieces[1];
                root.AddFile(CollectionsMarshal.AsSpan(currentPath), fileName, size);
            }
        }

        return root;
    }

    private interface IEntry
    {
        long GetSize();
    }

    private class Directory : IEntry
    {
        private readonly Dictionary<string, IEntry> _members = new();

        public void AddFile(Span<string> path, string filename, long size)
        {
            if (path.Length == 0)
            {
                _members[filename] = new File { Size = size };
                return;
            }

            var nextDirName = path[0];

            if (!_members.ContainsKey(nextDirName))
            {
                _members[nextDirName] = new Directory();
            }

            var nextEntry = _members[nextDirName];
            if (nextEntry is not Directory nextDirectory)
            {
                throw new Exception("Uh oh... we're trying to add a file to a file");
            }

            nextDirectory.AddFile(path[1..], filename, size);
        }

        public virtual long GetSize()
        {
            return _members.Values.Select(entry => entry.GetSize()).Sum();
        }

        /// <summary>
        /// Yields all directories that are contained directly and indirectly in this one. Does not emit itself.
        /// </summary>
        public IEnumerable<Directory> GetAllSubDirectories()
        {
            foreach (var member in _members.Values)
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

    private class File : IEntry
    {
        public required long Size;

        public virtual long GetSize()
        {
            return Size;
        }
    }
}