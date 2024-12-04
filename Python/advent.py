import os
import sys
from typing import Any, Callable
import time
from datetime import timedelta
import humanize
import re
import click
from rich import print
import rich
import rich.panel
import rich.text

part_1_func: Callable[..., Any] | None = None
part_2_func: Callable[..., Any] | None = None


def part_1(f: Callable[..., Any]):
    global part_1_func
    part_1_func = f
    return f


def part_2(f: Callable[..., Any]):
    global part_2_func
    part_2_func = f
    return f


@click.command()
@click.option(
    "--sample/--real",
    "-s",
    default=False,
    help="Whether to use the sample input or the real input",
)
@click.option("--p1", is_flag=True, help="Run part 1?")
@click.option("--p2", is_flag=True, help="Run part 2?")
def run(*, sample: bool = False, p1: bool, p2: bool):
    if not part_1_func and not part_2_func:
        raise ValueError("Neither part 1 nor part 2 defined.")

    if not p1 and not p2:
        # Setting neither defaults to setting all available parts
        p1 = bool(part_1_func)
        p2 = bool(part_2_func)
    if p1:
        if not part_1_func:
            raise ValueError("You asked to run part 1, but part 1 is not defined.")
        run_advent(1, part_1_func, sample=sample)
    if p2:
        if not part_2_func:
            raise ValueError("You asked to run part 2, but part 2 is not defined.")
        run_advent(2, part_2_func, sample=sample)


def run_advent(part: int, func: Callable[..., Any], sample: bool = False):
    match part:
        case 1:
            stars = "⭐"
        case 2:
            stars = "⭐⭐"
        case _:
            raise ValueError(f"Illegal part number: {part}")
    year, day = _parse_day_from_module(_get_entry_point_module())
    file = _read_input_file(year, day, sample)
    input = [line.rstrip() for line in file.splitlines()]
    start = time.time()
    res = func(input)
    end = time.time()
    duration = timedelta(seconds=end - start)
    content = rich.text.Text()
    content.append(str(res))
    content.append("\n")
    content.append(humanize.precisedelta(duration, minimum_unit="milliseconds"))
    print(rich.panel.Panel(content, title=stars, expand=False))


def _read_input_file(year: int, day: int, sample: bool) -> str:
    if sample:
        filename = f"inputs/sample_{year}_{day}.txt"
    else:
        filename = f"inputs/input_{year}_{day}.txt"
    if not os.path.exists(filename):
        print(f"Input file {filename} does not exist. Creating.")
        with open(filename, "w"):
            pass
    with open(filename, "r") as f:
        return f.read()


def _get_entry_point_module():
    """Get the name of the module that was used to start the program."""
    if "__main__" in sys.modules:
        # When run with python -m, the main module's name is stored in __spec__.name
        main_module = sys.modules["__main__"]
        if hasattr(main_module, "__spec__") and main_module.__spec__ is not None:
            return main_module.__spec__.name
    raise ValueError("Cannot get module name")


def _parse_day_from_module(module_name: str) -> tuple[int, int]:
    """
    Parse a module name using regex to extract year and day.

    Args:
        module_name: String like 'days.day_2024_01'

    Returns:
        Tuple of (year, day) as integers
    """
    pattern = r"day_(\d{4})_(\d{2})"
    match = re.search(pattern, module_name)
    if not match:
        raise ValueError(
            f"Could not parse year and day from module name: {module_name}"
        )
    return int(match.group(1)), int(match.group(2))
