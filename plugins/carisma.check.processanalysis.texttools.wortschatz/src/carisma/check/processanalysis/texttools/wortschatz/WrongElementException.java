package carisma.check.processanalysis.texttools.wortschatz;

/**
 * @author dbuerger
 */
public class WrongElementException extends Exception{
	
	private String message = "";
	private static final long serialVersionUID = 2677827524417734111L;
	
	public WrongElementException(String string) {
		message = string;
	}
	
	public String getMessage(){
		return message;
	}

}
