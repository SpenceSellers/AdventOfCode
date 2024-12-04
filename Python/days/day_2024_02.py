from typing import Iterator
import advent


def is_safe(nums: list[int]):
    last = nums[0]
    if last < nums[1]:
        ascending = True
    else:
        ascending = False

    nums = nums[1:]
    for i in nums:
        difference = abs(last - i)
        if difference > 3:
            return False

        if difference == 0:
            return False

        if ascending and i <= last:
            return False

        if not ascending and i >= last:
            return False
        last = i

    return True


def with_one_removed[T](items: list[T]) -> Iterator[list[T]]:
    for i in range(len(items)):
        yield items[:i] + items[i + 1 :]


@advent.part_1
def part_1(lines: list[str]):
    count = 0
    for line in lines:
        numbers = [int(x) for x in line.split()]
        if is_safe(numbers):
            count += 1
    return count


@advent.part_2
def part_2(lines: list[str]):
    count = 0
    for line in lines:
        numbers = [int(x) for x in line.split()]
        for possible in with_one_removed(numbers):
            if is_safe(possible):
                count += 1
                break
    return count


advent.run()
