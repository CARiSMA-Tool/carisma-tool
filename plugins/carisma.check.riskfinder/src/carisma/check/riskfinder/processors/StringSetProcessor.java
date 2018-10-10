package carisma.check.riskfinder.processors;

import java.util.Set;

//import carisma.processanalysis.textmodel.ScoredString;
import carisma.processanalysis.textmodel.WordSet;

public interface StringSetProcessor {
	public void process(WordSet inputSet);
	
	/**
	 * Der Name des Processors, zur Ausgabe in einer �bersicht �ber die Chains.
	 * @return
	 */
	public String getName();

}
