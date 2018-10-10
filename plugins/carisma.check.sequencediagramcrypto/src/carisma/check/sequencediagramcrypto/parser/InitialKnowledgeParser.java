package carisma.check.sequencediagramcrypto.parser;

import java.util.List;

import carisma.check.sequencediagramcrypto.exception.InvalidSyntaxException;
import carisma.check.sequencediagramcrypto.exception.UnexpectedStringEndException;
import carisma.check.sequencediagramcrypto.knowledge.Knowledge;

public class InitialKnowledgeParser {
	
	/*-
	 * Valid Syntax: 
	 * InitialKnowledge -> Vals | . 
	 *             Vals -> Val, Vals | Val 
	 *              Val -> string | Unfun | Bifun 
	 *            Unfun -> Unfunname 
	 *        Unfunname -> inv 
	 *            Bifun -> Bifunname(Val, Val) 
	 *        Bifunname -> symenc | symdec | conc | enc | dec | sign | ext
	 **/
	
	/**
	 * @param string
	 * @return
	 * @throws InvalidSyntaxException
	 * @throws UnexpectedStringEndException
	 */
	public static List<Knowledge> Parse(String string) throws InvalidSyntaxException,
			UnexpectedStringEndException {
		return SyntaxParser.parseInitialKnowledge(new StringBuilder(string));
	}
	
}
