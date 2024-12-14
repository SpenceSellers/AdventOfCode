from typing import Callable, Iterable, Iterator


def split_list[T](
    items: Iterable[T], split_by: Callable[[T], bool]
) -> Iterator[list[T]]:
    chunk: list[T] = []
    for i in items:
        if split_by(i):
            yield chunk
            chunk = []
        else:
            chunk.append(i)
    if chunk:
        yield chunk
