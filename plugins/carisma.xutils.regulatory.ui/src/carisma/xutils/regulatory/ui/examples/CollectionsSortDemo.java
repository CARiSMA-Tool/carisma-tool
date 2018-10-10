package carisma.xutils.regulatory.ui.examples;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

// TODO: Auto-generated Javadoc
/**
 * The Class CollectionsSortDemo.
 */
public class CollectionsSortDemo {
    
    /**
     * The main method.
     *
     * @param args the arguments
     */
    public static void main(String[] args) {
        // List<String> list = new ArrayList<String>(Arrays.asList(new String[]
        // {
        // "Noah", "Abraham", "Isaak", "Ismael", "Moses", "Jesus",
        // "Muhammed" }));
        // Collections.addAll(list, "Saskia", "Regina", "Angela", "Astrid",
        // "Manuela", "Silke", "Linda", "Daniela", "Silvia", "Samah",
        // "Radhia", "Mejda");

        List<String> list = new ArrayList<String>(Arrays.asList(new String[] {
                "345", "1", "456", "3", "test", "992", "87" }));

        Collections.sort(list);
        System.out.println(list);
        list.clear();
        System.out.println(list);

        String test = "187";
        String test2 = "89";
        System.out.println(test.compareTo(test2));

    }
}