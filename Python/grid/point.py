from dataclasses import dataclass

@dataclass(slots=True)
class Point:
    x: int
    y: int

    def __add__(self, other: "Point") -> "Point":
        return Point(self.x + other.x, self.y + other.y)

    def __sub__(self, other: "Point") -> "Point":
        return Point(self.x - other.x, self.y - other.y)

    def __mul__(self, other: int) -> "Point":
        return Point(self.x * other, self.y * other)

    def manhattan_distance_from_origin(self) -> int:
        return abs(self.x) + abs(self.y)

    def manhattan_distance(self, other: "Point") -> int:
        return abs(self.x - other.x) + abs(self.y - other.y)
