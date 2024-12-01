from collections import Counter
import math
import advent

def part1(lines: list[str]):
    left: list[int] = []
    right: list[int] = []
    for line in lines:
        l, r = [int(x) for x in line.split(' ') if x != '']
        left.append(l)
        right.append(r)

    left.sort()
    right.sort()

    print(left)
    print(right)

    sum = 0
    for a, b in zip(left, right):
        sum += abs(a - b)
    print(sum)


def part2(lines: list[str]):
    left: list[int] = []
    right: list[int] = []
    for line in lines:
        l, r = [int(x) for x in line.split(' ') if x != '']
        left.append(l)
        right.append(r)

    right_counts = Counter(right)
    sum = 0
    for l in left:
        sum += l * right_counts[l]

    print(sum)



advent.run_advent(1, part2, sample=False)