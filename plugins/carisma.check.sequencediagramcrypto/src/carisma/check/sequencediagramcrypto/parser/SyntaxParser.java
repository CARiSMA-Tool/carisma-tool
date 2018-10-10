package carisma.check.sequencediagramcrypto.parser;

import java.util.ArrayList;
import java.util.List;

import carisma.check.sequencediagramcrypto.exception.InvalidSyntaxException;
import carisma.check.sequencediagramcrypto.exception.UnexpectedStringEndException;
import carisma.check.sequencediagramcrypto.fol.FOLTerm;
import carisma.check.sequencediagramcrypto.fol.term.Conjunction;
import carisma.check.sequencediagramcrypto.fol.term.Disjunction;
import carisma.check.sequencediagramcrypto.fol.term.False;
import carisma.check.sequencediagramcrypto.fol.term.Implication;
import carisma.check.sequencediagramcrypto.fol.term.KnowledgeEqual;
import carisma.check.sequencediagramcrypto.fol.term.Not;
import carisma.check.sequencediagramcrypto.knowledge.ConcKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.DecKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.EncKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.ExtKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.InvKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.Knowledge;
import carisma.check.sequencediagramcrypto.knowledge.SignKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.SymdecKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.SymencKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.ValueStringKnowledge;

public final class SyntaxParser {
	
	/*-
	 * GUARD SYNTAX:
	 *      Guard -> [ Conditions ] | []
	 * Conditions -> Condition | (Condition Junction Condition)
	 *   Junction -> AND | OR | IMPLIES | XOR
	 *  Condition -> Val = Val | Val != Val | true | false
	 *        Val -> String | Fun
	 *        Fun -> Unfun | Bifun 
	 *      Unfun -> Unfunname ( Val )
	 *  Unfunname -> inv
	 *      Bifun -> Bifunname ( Val , Val )
	 *  Bifunname -> symenc | symdec | conc | enc | dec | sign | ext 
	 *  
	 * INITIAL KNOWLEDGE SYNTAX:
	 * InitialKnowledge -> Vals | . 
	 *             Vals -> Val, Vals | Val 
	 *              Val -> string | Unfun | Bifun 
	 *            Unfun -> Unfunname 
	 *        Unfunname -> inv 
	 *            Bifun -> Bifunname(Val, Val) 
	 *        Bifunname -> symenc | symdec | conc | enc | dec | sign | ext
	 *        
	 * KNOWLEDGE TO CHECK SYNTAX:
	 * SeekKnowledge -> Val | Condition | Condition, SeekKnowledge | Val, SeekKnowledge
	 *     Condition -> Val = Val | Val != Val | true | false
	 *           Val -> String | Fun
	 *           Fun -> Unfun | Bifun 
	 *         Unfun -> Unfunname ( Val )
	 *     Unfunname -> inv
	 *         Bifun -> Bifunname ( Val , Val )
	 *     Bifunname -> symenc | symdec | conc | enc | dec | sign | ext 
	 *    
	 */
	
	public static FOLTerm parseGuard(StringBuilder strBuilder) throws InvalidSyntaxException,
			UnexpectedStringEndException {
		
		if (!strBuilder.toString().startsWith("[") || strBuilder.toString().endsWith("]")) {
			throw new InvalidSyntaxException("[parseGuard] Expected [ at the begin and ] at the end of string "
					+ strBuilder.toString());
		}
		
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		FOLTerm returnTerm = SyntaxParser.parseConditions(strBuilder);
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		strBuilder.delete(0, 1);
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		if (strBuilder.length() != 0) {
			throw new InvalidSyntaxException("[parseGuard] Expected string end but found"
					+ strBuilder.toString());
		}
		
		return returnTerm;
	}
	
	public static List<Knowledge> parseInitialKnowledge(StringBuilder strBuilder)
			throws InvalidSyntaxException, UnexpectedStringEndException {
		
		return SyntaxParser.parseVals(strBuilder);
	}
	
	public static FOLTerm parseConditions(StringBuilder strBuilder) throws InvalidSyntaxException,
			UnexpectedStringEndException {
		
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		
		if (strBuilder.length() < 1) {
			throw new UnexpectedStringEndException();
		}
		
		if (strBuilder.charAt(0) != '(') {
			return SyntaxParser.parseCondition(strBuilder);
		}
		
		strBuilder.delete(0, 1);
		
		FOLTerm returnTerm = new False();
		FOLTerm condition1 = SyntaxParser.parseCondition(strBuilder);
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		
		String currentString = strBuilder.toString();
		
		String junction = "";
		
		if (currentString.startsWith("AND")) {
			junction = "AND";
		}
		else if (currentString.startsWith("OR")) {
			junction = "OR";
		}
		else if (currentString.startsWith("XOR")) {
			junction = "XOR";
		}
		else if (currentString.startsWith("IMPLIES")) {
			junction = "IMPLIES";
		}
		else {
			throw new InvalidSyntaxException("[parseConditions] Found unknown junction " + currentString);
		}
		
		strBuilder.delete(0, junction.length());
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		FOLTerm condition2 = SyntaxParser.parseConditions(strBuilder);
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		
		if (strBuilder.length() < 1 || strBuilder.charAt(0) != ')') {
			throw new InvalidSyntaxException("[parseConditions] Expected ')' but found" + currentString);
		}
		
		if (junction.equals("AND")) {
			returnTerm = new Conjunction(condition1, condition2);
		}
		else if (junction.equals("OR")) {
			returnTerm = new Disjunction(condition1, condition2);
		}
		else if (junction.equals("XOR")) {
			returnTerm = new Disjunction(new Conjunction(condition1, new Not(condition2)),
					new Conjunction(new Not(condition1), condition2));
		}
		else if (junction.equals("IMPLIES")) {
			returnTerm = new Implication(condition1, condition2);
		}
		else {
			throw new InvalidSyntaxException("[parseConditions] Found unknown junction " + currentString);
		}
		
		return returnTerm;
	}
	
	public static FOLTerm parseCondition(StringBuilder strBuilder) throws InvalidSyntaxException,
			UnexpectedStringEndException {
				
		Knowledge val1 = SyntaxParser.parseVal(strBuilder);
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		
		String junction = "";
		if (strBuilder.length() < 1) {
			throw new UnexpectedStringEndException();
		}
		
		if (strBuilder.charAt(0) == '=') {
			junction = "=";
			strBuilder.delete(0, 1);
		}
		else if (strBuilder.length() > 1 && strBuilder.charAt(0) == '!'
				&& strBuilder.charAt(1) == '=') {
			junction = "!=";
			strBuilder.delete(0, 2);
		}
		else {
			throw new InvalidSyntaxException("[parseCondition] Expected = or != but got " + strBuilder.toString());
		}
		
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		Knowledge val2 = SyntaxParser.parseVal(strBuilder);
		
		FOLTerm returnTerm = new False();
		if (junction.equals("=")) {
			returnTerm = new KnowledgeEqual(val1, val2);
		}
		else if (junction.equals("!=")) {
			returnTerm = new Not(new KnowledgeEqual(val1, val2));
		}
		return returnTerm;
	}
	
	public static List<Knowledge> parseVals(StringBuilder strBuilder)
			throws InvalidSyntaxException, UnexpectedStringEndException {
		
		List<Knowledge> knowledge = new ArrayList<Knowledge>();
		
		while (strBuilder.length() > 0) {
			Knowledge val = SyntaxParser.parseVal(strBuilder);
			knowledge.add(val);
			
			SyntaxParser.leftTrimStringBuilder(strBuilder);
			if (strBuilder.length() > 0 && strBuilder.charAt(0) != ',') {
				throw new InvalidSyntaxException("[parseVals] expected ',' but got '" + strBuilder.charAt(0) + "'");
			}
			strBuilder.delete(0, 1);
			SyntaxParser.leftTrimStringBuilder(strBuilder);
		}
		
		return knowledge;
		
	}
	
	public static Knowledge parseVal(StringBuilder strBuilder) throws InvalidSyntaxException,
			UnexpectedStringEndException {
		
		String currentString = strBuilder.toString();
		
		if (currentString.startsWith(InvKnowledge.MSG_NAME + "(")) {
			return SyntaxParser.parseInv(strBuilder);
		}
		else if (currentString.startsWith(SymencKnowledge.MSG_NAME + "(")) {
			return SyntaxParser.parseSymenc(strBuilder);
		}
		else if (currentString.startsWith(SymdecKnowledge.MSG_NAME + "(")) {
			return SyntaxParser.parseSymdec(strBuilder);
		}
		else if (currentString.startsWith(ConcKnowledge.MSG_NAME + "(")) {
			return SyntaxParser.parseConc(strBuilder);
		}
		else if (currentString.startsWith(EncKnowledge.MSG_NAME + "(")) {
			return SyntaxParser.parseEnc(strBuilder);
		}
		else if (currentString.startsWith(DecKnowledge.MSG_NAME + "(")) {
			return SyntaxParser.parseDec(strBuilder);
		}
		else if (currentString.startsWith(SignKnowledge.MSG_NAME + "(")) {
			return SyntaxParser.parseSign(strBuilder);
		}
		else if (currentString.startsWith(ExtKnowledge.MSG_NAME + "(")) {
			return SyntaxParser.parseExt(strBuilder);
		}
		else {
			int firstSpace = strBuilder.indexOf(" ") == -1 ? Integer.MAX_VALUE : strBuilder.indexOf(" ");
			int firstComma = strBuilder.indexOf(",") == -1 ? Integer.MAX_VALUE : strBuilder.indexOf(",");
			int firstBracket = strBuilder.indexOf(")") == -1 ? Integer.MAX_VALUE : strBuilder.indexOf(")");
			int endOfString = Math.min(Math.min(firstSpace, firstBracket), Math.min(firstComma, strBuilder.length()));
			String valueString = strBuilder.substring(0, endOfString);
			strBuilder.delete(0, endOfString);
			
			return new ValueStringKnowledge(valueString);
			
		}
		
	}
	
	private static Knowledge parseExt(StringBuilder strBuilder) throws InvalidSyntaxException,
			UnexpectedStringEndException {
		int parameters = 2;
		String functionname = ExtKnowledge.MSG_NAME;
		
		strBuilder.delete(0, functionname.length() + 1);
		List<Knowledge> valList = SyntaxParser.parseNVal(strBuilder, parameters);
		if (valList.size() > parameters) {
			throw new InvalidSyntaxException("[parseExt] To many parameters where given!");
		}
		// As parseNVal throws Exception on invalid syntaxes, this case should always be true,
		// but is there just in case!
		if (valList.size() < parameters) {
			throw new InvalidSyntaxException("[parseExt] To few parameters where given!");
		}
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		if (strBuilder.length() == 0 || strBuilder.charAt(0) != ')') {
			throw new InvalidSyntaxException("[parseExt] Expected ')' in " + strBuilder.toString());
		}
		strBuilder.delete(0, 1);
		return new ExtKnowledge(valList.get(0), valList.get(1));
	}
	
	private static Knowledge parseSign(StringBuilder strBuilder) throws InvalidSyntaxException,
			UnexpectedStringEndException {
		int parameters = 2;
		String functionname = SignKnowledge.MSG_NAME;
		
		strBuilder.delete(0, functionname.length() + 1);
		List<Knowledge> valList = SyntaxParser.parseNVal(strBuilder, parameters);
		if (valList.size() > parameters) {
			throw new InvalidSyntaxException("[parseSign] To many parameters where given!");
		}
		// As parseNVal throws Exception on invalid syntaxes, this case should always be true,
		// but is there just in case!
		if (valList.size() < parameters) {
			throw new InvalidSyntaxException("[parseSign] To few parameters where given!");
		}
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		if (strBuilder.length() == 0 || strBuilder.charAt(0) != ')') {
			throw new InvalidSyntaxException("[parseSign] Expected ')' in " + strBuilder.toString());
		}
		strBuilder.delete(0, 1);
		return new SignKnowledge(valList.get(0), valList.get(1));
	}
	
	private static Knowledge parseDec(StringBuilder strBuilder) throws InvalidSyntaxException,
			UnexpectedStringEndException {
		int parameters = 2;
		String functionname = DecKnowledge.MSG_NAME;
		
		strBuilder.delete(0, functionname.length() + 1);
		List<Knowledge> valList = SyntaxParser.parseNVal(strBuilder, parameters);
		if (valList.size() > parameters) {
			throw new InvalidSyntaxException("[parseDec] To many parameters where given!");
		}
		// As parseNVal throws Exception on invalid syntaxes, this case should always be true,
		// but is there just in case!
		if (valList.size() < parameters) {
			throw new InvalidSyntaxException("[parseDec] To few parameters where given!");
		}
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		if (strBuilder.length() == 0 || strBuilder.charAt(0) != ')') {
			throw new InvalidSyntaxException("[parseDec] Expected ')' in " + strBuilder.toString());
		}
		strBuilder.delete(0, 1);
		return new DecKnowledge(valList.get(0), valList.get(1));
	}
	
	private static Knowledge parseEnc(StringBuilder strBuilder) throws InvalidSyntaxException,
			UnexpectedStringEndException {
		int parameters = 2;
		String functionname = EncKnowledge.MSG_NAME;
		
		strBuilder.delete(0, functionname.length() + 1);
		List<Knowledge> valList = SyntaxParser.parseNVal(strBuilder, parameters);
		if (valList.size() > parameters) {
			throw new InvalidSyntaxException("[parseEnc] To many parameters where given!");
		}
		// As parseNVal throws Exception on invalid syntaxes, this case should always be true,
		// but is there just in case!
		if (valList.size() < parameters) {
			throw new InvalidSyntaxException("[parseEnc] To few parameters where given!");
		}
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		if (strBuilder.length() == 0 || strBuilder.charAt(0) != ')') {
			throw new InvalidSyntaxException("[parseEnc] Expected ')' in " + strBuilder.toString());
		}
		strBuilder.delete(0, 1);
		return new EncKnowledge(valList.get(0), valList.get(1));
	}
	
	private static Knowledge parseConc(StringBuilder strBuilder) throws InvalidSyntaxException,
			UnexpectedStringEndException {
		int parameters = 2;
		String functionname = ConcKnowledge.MSG_NAME;
		
		strBuilder.delete(0, functionname.length() + 1);
		List<Knowledge> valList = SyntaxParser.parseNVal(strBuilder, parameters);
		if (valList.size() > parameters) {
			throw new InvalidSyntaxException("[parseConc] To many parameters where given!");
		}
		// As parseNVal throws Exception on invalid syntaxes, this case should always be true,
		// but is there just in case!
		if (valList.size() < parameters) {
			throw new InvalidSyntaxException("[parseConc] To few parameters where given!");
		}
		
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		if (strBuilder.length() == 0 || strBuilder.charAt(0) != ')') {
			throw new InvalidSyntaxException("[parseConc] Expected ')' in " + strBuilder.toString());
		}
		strBuilder.delete(0, 1);
		
		return new ConcKnowledge(valList.get(0), valList.get(1));
	}
	
	private static Knowledge parseSymdec(StringBuilder strBuilder) throws InvalidSyntaxException,
			UnexpectedStringEndException {
		int parameters = 2;
		String functionname = SymdecKnowledge.MSG_NAME;
		
		strBuilder.delete(0, functionname.length() + 1);
		List<Knowledge> valList = SyntaxParser.parseNVal(strBuilder, parameters);
		if (valList.size() > parameters) {
			throw new InvalidSyntaxException("[parseSymdec] To many parameters where given!");
		}
		// As parseNVal throws Exception on invalid syntaxes, this case should always be true,
		// but is there just in case!
		if (valList.size() < parameters) {
			throw new InvalidSyntaxException("[parseSymdec] To few parameters where given!");
		}
		
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		if (strBuilder.length() == 0 || strBuilder.charAt(0) != ')') {
			throw new InvalidSyntaxException("[parseSymdec] Expected ')' in " + strBuilder.toString());
		}
		strBuilder.delete(0, 1);
		
		return new SymdecKnowledge(valList.get(0), valList.get(1));
	}
	
	private static Knowledge parseSymenc(StringBuilder strBuilder) throws InvalidSyntaxException,
			UnexpectedStringEndException {
		int parameters = 2;
		String functionname = SymencKnowledge.MSG_NAME;
		
		strBuilder.delete(0, functionname.length() + 1);
		List<Knowledge> valList = SyntaxParser.parseNVal(strBuilder, parameters);
		if (valList.size() > parameters) {
			throw new InvalidSyntaxException("[parseSymenc] To many parameters where given!");
		}
		// As parseNVal throws Exception on invalid syntaxes, this case should always be true,
		// but is there just in case!
		if (valList.size() < parameters) {
			throw new InvalidSyntaxException("[parseSymenc] To few parameters where given!");
		}
		
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		if (strBuilder.length() == 0 || strBuilder.charAt(0) != ')') {
			throw new InvalidSyntaxException("[parseSymenc] Expected ')' in " + strBuilder.toString());
		}
		strBuilder.delete(0, 1);
		
		return new SymencKnowledge(valList.get(0), valList.get(1));
	}
	
	private static Knowledge parseInv(StringBuilder strBuilder) throws InvalidSyntaxException,
			UnexpectedStringEndException {
		int parameters = 1;
		String functionname = InvKnowledge.MSG_NAME;
		
		strBuilder.delete(0, functionname.length() + 1);
		List<Knowledge> valList = SyntaxParser.parseNVal(strBuilder, parameters);
		if (valList.size() > parameters) {
			throw new InvalidSyntaxException("[parseInv] To many parameters where given!");
		}
		// As parseNVal throws Exception on invalid syntaxes, this case should always be true,
		// but is there just in case!
		if (valList.size() < parameters) {
			throw new InvalidSyntaxException("[parseInv] To few parameters where given!");
		}
		
		SyntaxParser.leftTrimStringBuilder(strBuilder);
		if (strBuilder.length() == 0 || strBuilder.charAt(0) != ')') {
			throw new InvalidSyntaxException("[parseInv] Expected ')' in " + strBuilder.toString());
		}
		
		strBuilder.delete(0, 1);
		return new InvKnowledge(valList.get(0));
	}
	
	private static List<Knowledge> parseNVal(StringBuilder strBuilder, int n)
			throws InvalidSyntaxException, UnexpectedStringEndException {
		List<Knowledge> valList = new ArrayList<Knowledge>();
		{
			for (int i = 0; i < n; i++) {
				
				// Remove whitespace to the left
				SyntaxParser.leftTrimStringBuilder(strBuilder);
				// Parse val
				Knowledge val = SyntaxParser.parseVal(strBuilder);
				valList.add(val);
				
				// Remove whitespace to the left
				SyntaxParser.leftTrimStringBuilder(strBuilder);
				
				// If another val is expected after the last parsed val
				if ((i + 1) < n) {
					// the first char now has to be a ',' otherwise the syntax was wrong!
					if (strBuilder.length() == 0 || strBuilder.charAt(0) != ',') {
						throw new InvalidSyntaxException("[parseNVal] Expected ',' in " + strBuilder.toString());
					}
					strBuilder.delete(0, 1);
				}
			}
		}
		return valList;
	}
	
	/**
	 * Removes all whitespace to the left of a given StringBuilder Object
	 * 
	 * @param strBuilder
	 */
	public static void leftTrimStringBuilder(StringBuilder strBuilder) {
		int trimLeft = 0;
		while (trimLeft < strBuilder.length()) {
			// FIXME : Code has problems with Unicode runes with two chars!
			if (!Character.isWhitespace(strBuilder.charAt(trimLeft))) {
				break;
			}
			trimLeft++;
		}
		if(trimLeft > 0){
			strBuilder.delete(0, trimLeft);
		}
	}
	
}
