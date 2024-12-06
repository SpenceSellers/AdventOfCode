import advent
import re


@advent.part_1
def part_1(lines: list[str]):
    ipt = "".join(lines)
    matches = re.findall(r"mul\((\d+),(\d+)\)", ipt)
    return sum(int(a) * int(b) for a, b in matches)


@advent.part_2
def part_2(lines: list[str]):
    ipt = "".join(lines)
    matches = re.findall(r"(mul|do|don\'t)\(((\d+),(\d+))?\)", ipt)
    sum = 0
    enabled = True
    for instruction, _, a, b in matches:
        match instruction:
            case "do":
                enabled = True
            case "don't":
                enabled = False
            case "mul":
                if enabled:
                    sum += int(a) * int(b)
            case _:
                raise ValueError(f"Illegal instruction type {instruction}")

    return sum


advent.run()
