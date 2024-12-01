from collections import Counter
import advent


def parse_left_right(lines: list[str]) -> tuple[list[int], list[int]]:
    left: list[int] = []
    right: list[int] = []
    for line in lines:
        l, r = [int(x) for x in line.split(" ") if x != ""]
        left.append(l)
        right.append(r)
    return left, right


def part1(lines: list[str]):
    left, right = parse_left_right(lines)

    left.sort()
    right.sort()

    sum = 0
    for a, b in zip(left, right):
        sum += abs(a - b)
    print(sum)


def part2(lines: list[str]):
    left, right = parse_left_right(lines)

    right_counts = Counter(right)
    sum = 0
    for l in left:
        sum += l * right_counts[l]

    print(sum)


advent.run_advent(1, part1, sample=False)
advent.run_advent(2, part2, sample=False)
