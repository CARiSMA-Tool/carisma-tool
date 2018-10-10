package carisma.core.io.content;

public interface Content {
	public String asString();
	public String getFormat();
	
	public class ContentException extends Exception {

		/**
		 * 
		 */
		private static final long serialVersionUID = 7603921345075517830L;

		public ContentException(Exception e) {
			super(e);
		}
		
	}
}
