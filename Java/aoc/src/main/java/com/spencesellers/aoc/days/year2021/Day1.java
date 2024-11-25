package com.spencesellers.aoc.days.year2021;

import com.spencesellers.aoc.scaffolding.Day;

import java.util.List;

public class Day1 extends Day {
    @Override
    public String getDay() {
        return "2021-01";
    }

    @Override
    public Object partOne(List<String> input) {
        return input.stream().findFirst().orElse("Not found");
    }

    @Override
    public Object partTwo(List<String> input) {
        return null;
    }
}
