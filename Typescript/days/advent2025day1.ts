import input from "../inputs/2025_1.txt" with { type: "text" };
import _ from "lodash";

const lines = input.split("\n").filter(line => line.length > 0);

console.log(lines);

function parseLine(line: string): number {
    let [direction, ...rest] = line.split("");
    if (direction === "R") {
        return parseInt(rest.join(""));
    } else if (direction === "L") {
        return -parseInt(rest.join(""));
    } else {
        throw new Error(`Invalid direction: ${direction}`);
    }
}

const turns = lines.map(parseLine);

function partOne() {
    let c = 50;
    let timesZero = 0;

    for (const turn of turns) {
        c += turn;
        c %= 100;
        if (c === 0) {
            timesZero++;
        }
    }

    console.log("Part One", timesZero)
}

function* toSingleTurn(turn: number): Generator<number> {
    const sign = turn > 0 ? 1 : -1;
    const abs = Math.abs(turn);
    for (let i = 0; i < abs; i++) {
        yield sign;
    }
}

function partTwo() {
    let c = 50;
    let timesZero = 0;
    for (const turn of turns) {
        for (const singleTurn of toSingleTurn(turn)) {
            c += singleTurn;
            c %= 100;
            if (c === 0) {
                timesZero++;
            }
        }
    }

    console.log("Part Two", timesZero)
}
partOne();
partTwo();