package com.spencesellers.aoc.scaffolding;

import com.spencesellers.aoc.inputsources.FileInput;

public class DayRunner {
    private InputSource inputSource = new FileInput("actual");
    public DayRunner useSample() {
        this.inputSource = new FileInput("sample");
        return this;
    }

    public void runDay(Day day) {
        var dayId = day.getDay();
        var input = inputSource.GetInput(dayId);
        Object result = day.partOne(input);
        System.out.printf("Part 1: %s", result);
    }
}
