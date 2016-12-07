package carisma.check.sequencediagramcrypto.parser;

import carisma.check.sequencediagramcrypto.exception.InvalidSyntaxException;
import carisma.check.sequencediagramcrypto.exception.UnexpectedStringEndException;
import carisma.check.sequencediagramcrypto.knowledge.Knowledge;

public class ArgumentParser {
	
	public static Knowledge Parse(String value) throws InvalidSyntaxException, UnexpectedStringEndException {
		StringBuilder strBuilder = new StringBuilder(value);
		return SyntaxParser.parseVal(strBuilder);
	}
	
}
