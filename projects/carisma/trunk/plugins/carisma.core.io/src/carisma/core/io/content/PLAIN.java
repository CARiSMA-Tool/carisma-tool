package carisma.core.io.content;

public class PLAIN implements Content {

	public static String ID = "PLAIN";
	
	private String content;
	
	public PLAIN(String content) {
		this.content = content;
	}
	
	@Override
	public String asString() {
		return content;
	}

	@Override
	public String getFormat() {
		return ID;
	}

}
