package carisma.check.sequencediagramcrypto.parser;

import java.util.ArrayList;
import java.util.List;

import carisma.check.sequencediagramcrypto.exception.InvalidSyntaxException;
import carisma.check.sequencediagramcrypto.exception.UnexpectedStringEndException;

public class KnowledgeToCheckParser {
	
	public static List<Object> Parse(String string) throws InvalidSyntaxException, UnexpectedStringEndException {
		
		// TODO: Conditions are currently not parsed and therefor missing, add Parsing for Conditions!
		
		List<Object> vals = new ArrayList<Object>(SyntaxParser.parseVals(new StringBuilder(string)));
		
		return vals;
		
	}
	
}
