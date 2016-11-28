package carisma.core.io.content;

public class PLAIN implements Content {

	public static String ID = "PLAIN";
	
	private String text;
	
	protected PLAIN(final String content) {
		this.text = content;
	}
	
	@Override
	public final String asString() {
		return this.text;
	}

	@Override
	public final String getFormat() {
		return ID;
	}

}
