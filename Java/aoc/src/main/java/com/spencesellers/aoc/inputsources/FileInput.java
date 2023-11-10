package com.spencesellers.aoc.inputsources;

import com.spencesellers.aoc.scaffolding.InputSource;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class FileInput implements InputSource {
    public FileInput(String prefix) {
        this.prefix = prefix;
    }
    private final String prefix;

    @Override
    public List<String> GetInput(String dayId) {
        var f = new File("inputs/" + prefix + "/" + dayId + ".txt");
        System.out.println(f.getAbsolutePath());
        try {
            var stream = new FileInputStream(f);
            return readFromInputStream(stream);
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private List<String> readFromInputStream(InputStream inputStream)
            throws IOException {
        List<String> lines = new ArrayList<>();
        try (BufferedReader br
                     = new BufferedReader(new InputStreamReader(inputStream))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
        }
        return lines;
    }
}
