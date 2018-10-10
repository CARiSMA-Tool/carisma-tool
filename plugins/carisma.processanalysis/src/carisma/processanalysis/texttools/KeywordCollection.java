package carisma.processanalysis.texttools;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.TreeMap;

import com.csvreader.CsvWriter;

// Key ist jeweils die ID des values

public class KeywordCollection extends TreeMap<String, KeywordSet> {
  
    public KeywordSet getWithDefaultEmpty(String key){
        if(this.containsKey(key))
            return this.get(key);
        else
//            return new KeywordSet(key, new KeywordSet(key)); // ??? haehae?
            return new KeywordSet();
    }
    
    public void writeToCSV(String filename){
    	
    	CsvWriter writer;
		try {
			writer = new CsvWriter(new OutputStreamWriter(new FileOutputStream(filename), "UTF-8"), ',');
//	        CsvWriter writer = new CsvWriter(filename);
	        
	        for(String curKey : this.keySet()){        
	            try {
	                writer.write(curKey);
	                writer.write(this.get(curKey).asCSVLineNoID());
	                writer.endRecord();
	            } catch (IOException e) {
	                // TODO Auto-generated catch block
	                e.printStackTrace();
	            }
	        }

	        writer.close();
		} catch (UnsupportedEncodingException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
