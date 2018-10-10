package carisma.check.riskfinder.processors;

import java.util.LinkedList;
import java.util.Set;

import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.textmodel.WordSet;

public class StringSetProcessorChain extends LinkedList<StringSetProcessor>{

	public void processAll(WordSet textWords){
		for(StringSetProcessor curProcessor : this){
			curProcessor.process(textWords);
			
			// debug/Demo:
			WordSet templist = textWords.getAsListUniqMaxscore();
			String tempString = templist.toString();
//			System.out.println(tempString);
		}
	}
	
	/**
	 * Liste der Namen der enthaltenen Processors, in der Reihenfolge in der sie auch ausgef�hrt werden.
	 * @return
	 */
	public String getDescription(){
		String ret = "";
		for(StringSetProcessor curProcessor : this){
			ret += ", " + curProcessor.getName();
		}
		ret = ret.substring(2);
		return ret;
	}
}
