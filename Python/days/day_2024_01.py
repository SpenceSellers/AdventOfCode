from collections import Counter
import advent


def parse_left_right(lines: list[str]) -> tuple[list[int], list[int]]:
    left: list[int] = []
    right: list[int] = []
    for line in lines:
        l, r = [int(x) for x in line.split("   ")]
        left.append(l)
        right.append(r)
    return left, right


@advent.part_1
def part1(lines: list[str]):
    left, right = parse_left_right(lines)

    left.sort()
    right.sort()

    return sum(abs(a - b) for a, b in zip(left, right))


@advent.part_2
def part2(lines: list[str]):
    left, right = parse_left_right(lines)

    right_counts = Counter(right)

    return sum(l * right_counts[l] for l in left)


advent.run()
