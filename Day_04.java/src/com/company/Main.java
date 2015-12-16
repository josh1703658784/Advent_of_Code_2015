package com.company;

import sun.plugin2.message.Message;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class Main {

    public static void main(String[] args) throws NoSuchAlgorithmException {
        String secret_key = "yzbqklnj";
        for(int counter = 0; ; counter++){
            String possible_solution = secret_key + Integer.toString(counter);
            MessageDigest md5er = MessageDigest.getInstance("MD5");
            md5er.update(possible_solution.getBytes());

            byte byteData[] = md5er.digest();

            //convert the byte to hex format
            StringBuffer sb = new StringBuffer();
            for (int i = 0; i < byteData.length; i++) {
                sb.append(Integer.toString((byteData[i] & 0xff) + 0x100, 16).substring(1));
            }

            if(counter % 1000 == 0) {
                System.out.println(counter);
            }
            if(sb.toString().substring(0, 6).equals("000000")){
                System.out.println("Input: " + possible_solution);
                System.out.println("Matcher: " + counter);
                return;
            }
        }
    }
}
