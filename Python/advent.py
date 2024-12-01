import os
import sys
from typing import Any, Callable
import time
from datetime import timedelta
import humanize
import re


def run_advent(part: int, func: Callable[..., Any], sample: bool = False):
    year, day = _parse_day_from_module(_get_entry_point_module())
    file = _read_input_file(year, day, sample)
    input = [l.rstrip() for l in file.splitlines()]
    start = time.time()
    res = func(input)
    end = time.time()
    duration = timedelta(seconds=end - start)
    print(humanize.precisedelta(duration, minimum_unit="milliseconds"))
    if res is not None:
        print(res)


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
