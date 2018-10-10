package carisma.check.sequencediagramcrypto.message;

import carisma.check.sequencediagramcrypto.exception.InvalidSyntaxException;
import carisma.check.sequencediagramcrypto.exception.UnexpectedStringEndException;
import carisma.check.sequencediagramcrypto.fol.FOLTerm;
import carisma.check.sequencediagramcrypto.fol.term.Knows;
import carisma.check.sequencediagramcrypto.knowledge.Knowledge;
import carisma.check.sequencediagramcrypto.parser.ArgumentParser;

public class ArgumentImp implements Argument {
	
	final private String	value;
	final private FOLTerm	term;
	final private Knowledge	knowledge;
	
	public ArgumentImp(String value) throws InvalidSyntaxException, UnexpectedStringEndException {
		this.value = value;
		this.knowledge = ArgumentParser.Parse(value);
		this.term = new Knows(this.knowledge);
	}
	
	public String value() {
		return this.value;
	}
	
	@Override
	public FOLTerm toFOLTerm() {
		return this.term;
	}
	
	@Override
	public Knowledge toKnowledge() {
		return this.knowledge;
	}
	
}
