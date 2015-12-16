package com.company;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Pattern;

//156366
public class Main {

    public static void main(String[] args) throws IOException {
	    final String file_name = "input";
        final String delimiters = "[\\[\\]\",:\\{}a-z.]";
        final String good_stuff = "^(?:(?:\\-{1})?\\d+(?:\\.{1}\\d+)?)$";

        byte[] encoded = Files.readAllBytes(Paths.get(file_name));
        String[] parsed = new String(encoded).replaceAll(delimiters, ",").split(",");

        int summation = 0;
        for(String i : parsed) {
            if(!i.equals("") && i.matches(good_stuff)){
                summation +=  Integer.parseInt(i);
            }
        }
        System.out.print(summation);
    }
}
