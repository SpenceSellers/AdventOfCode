import advent
from utils import split_list


def parse(lines: list[str]):
    [order_lines, update_lines] = split_list(lines, lambda line: line == "")
    orderings = [tuple(map(int, line.split("|"))) for line in order_lines]
    orderings = [(a[0], a[1]) for a in orderings]
    updates = [[int(x) for x in line.split(",")] for line in update_lines]
    return orderings, updates


def is_in_order(rules: list[tuple[int, int]], update: list[int]) -> bool:
    return all(conforms_to_rule(rule, update) for rule in rules)


def conforms_to_rule(rule: tuple[int, int], update: list[int]) -> bool:
    try:
        idx_1 = update.index(rule[0])
        idx_2 = update.index(rule[1])
    except ValueError:
        return True

    return idx_1 < idx_2


def get_middle_number[T](items: list[T]) -> T:
    return items[len(items) // 2]


def reorder(rules: list[tuple[int, int]], update: list[int]):
    # Yes, I seriously tried this. One of the inputs in the real input is too long to brute force it.
    #
    # while True:
    #     new_update = update.copy()
    #     random.shuffle(new_update)
    #     if is_in_order(rules, new_update):
    #         return new_update

    new_update: list[int] = []
    for item in update:
        if not new_update:
            new_update.append(item)
            continue

        for i in range(len(new_update) + 1):
            trial = new_update.copy()
            trial.insert(i, item)
            if is_in_order(rules, trial):
                new_update = trial
                break
    return new_update


@advent.part_1
def part_1(lines: list[str]):
    orderings, updates = parse(lines)
    count = 0
    for update in updates:
        if is_in_order(orderings, update):
            count += get_middle_number(update)
    return count


@advent.part_2
def part_2(lines: list[str]):
    orderings, updates = parse(lines)
    count = 0
    for update in updates:
        if not is_in_order(orderings, update):
            new_update = reorder(orderings, update)
            count += get_middle_number(new_update)
    return count


advent.run()
