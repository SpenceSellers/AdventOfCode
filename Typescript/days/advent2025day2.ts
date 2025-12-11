import _ from "lodash";
import input from "../inputs/2025_2.txt" with { type: "text" };
const line = input.split("\n")[0]!;

const pieces = line.split(',').map(range => range.split('-').map(BigInt));

function isRepeated(n: bigint) {
    const s = n.toString();
    if (s.length % 2 !== 0) {
        return false;
    }
    const firstHalf = s.slice(0, s.length / 2);
    const secondHalf = s.slice(s.length / 2);
    // console.log(s, firstHalf, secondHalf)
    return firstHalf === secondHalf;
}

function isRepeatedN(s: string, length: number) {
    if (s.length % length !== 0) {
        return false;
    }
    let firstPiece = undefined;

    for (let i = 0; i < s.length / length; i++) {
        const piece = s.slice(i * length, (i + 1) * length);
        if (firstPiece === undefined) {
            firstPiece = piece;
        } else if (firstPiece !== piece) {
            return false;
        }
    }
    return true;
}

function isRepeatedAny(n: bigint): boolean {
    const s = n.toString();
    for (let i = 1; i <= s.length / 2; i++) {
        if (isRepeatedN(s, i)) {
            return true;
        }
    }
    return false;
}

function *allNumbers(min: bigint, max: bigint): Generator<bigint> {
    for (let i = min; i <= max; i = i + 1n) {
        yield i;
    }
}

function partOne() {
    const all = pieces.map(range => [...allNumbers(range[0]!, range[1]!).filter(isRepeated)]).flat();
    const res = _.sum(all);
    console.log("Part One", res);
}

function partTwo() {
    const all = pieces.map(range => [...allNumbers(range[0]!, range[1]!).filter(isRepeatedAny)]).flat();
    const res = _.sum(all);
    console.log("Part Two", res);
}

partOne();
partTwo();