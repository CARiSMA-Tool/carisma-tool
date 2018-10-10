package carisma.processanalysis.texttools;

import java.io.IOException;
import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;

//import carisma.processanalysis.textmodel.ScoredString;
import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.textmodel.WordKind;

import com.csvreader.CsvReader;


public class KeywordSet extends LinkedList<Word> {
    // wuerde es Sinn machen, die ID im Keywordset selbst noch mal zu speichern? -> erst mal rausgenommen
    //private String id; // die ID des Elements, dem die Keywords zugeordnet sind. Z.B. die IRI eines OWLNamedIndividual

//    public String getId() {
//        return id;
//    }
//
//    public void setId(String id) {
//        this.id = id;
//    }
//
//    public KeywordSet(String id) {
//        super();
//        this.id = id;
//    }
//
//  public KeywordSet(String id, Collection<String> values) {
//  super(values);
//  this.id = id;        
//}

    public KeywordSet(Collection<Word> values) {
        super(values);
    }

    public KeywordSet(String valueString) {
    	this(valueString, 100);
    }
    
    public KeywordSet(String valueString, int score) {
        super();
        CsvReader reader = CsvReader.parse(valueString);
        try {
            boolean temp = reader.readRecord();
            for(String curValue : reader.getValues()){
            	this.add(new Word(curValue, WordKind.PATTERNKEYWORD));
            	// TODO: Score geht verloren!
            }
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }        
    }

    public KeywordSet() {
        super();
    }

    public String asCSVLineNoID(){
        String ret = "";
        
//        ret += this.id;
        for(Word curKeyword : this)
            ret += "," + curKeyword.getContent();

        if(ret.length() > 0)
        	ret = ret.substring(1);

        return ret;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }
}
