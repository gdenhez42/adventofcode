import java.io.*;
import java.util.List;
import java.util.ArrayList;

class day1 {
    public static void main(String args[]) throws Exception {
        File file = new File("input.txt");
        try (BufferedReader br = new BufferedReader(new FileReader(file))) {
            String st;
            List<Integer> maxCalories = new ArrayList<Integer>();
            int calories = 0;
            while ((st = br.readLine()) != null) {
                if ("".equals(st)) {
                    for (int i = 0; i < maxCalories.size(); i ++) {
                        if (calories > maxCalories.get(i)) {
                            maxCalories.add(i, calories);
                            if (maxCalories.size() > 3) maxCalories.remove(3);
                            break;
                        }
                    }
                    if (maxCalories.size() < 3) maxCalories.add(calories);
                    calories = 0;
                }
                else {
                    calories += Integer.parseInt(st);
                }
            }
            for (int i = 0; i < maxCalories.size(); i ++) {
                if (calories > maxCalories.get(i)) {
                    maxCalories.add(i, calories);
                    if (maxCalories.size() > 3) maxCalories.remove(3);
                    break;
                }
            }
            if (maxCalories.size() < 3) maxCalories.add(calories);

            int totalCalories = 0;
            for (Integer c : maxCalories) {
                totalCalories += c;
            }

            System.out.println(totalCalories);
        }
    }
}