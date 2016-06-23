package carisma.core.io.content;

import java.util.Base64;

public class BASE64 implements Content {

	public static String ID = "BASE64";
	
	private byte[] bytes;
	
	protected BASE64(final byte[] base64) {
		this.bytes = base64;
	}
	
	protected BASE64(final String content) {
		byte[] nonBase64 = content.getBytes();
		this.bytes = Base64.getEncoder().encode(nonBase64);
	}
	
	@Override
	public final String asString() {
		return new String(getBytesDecoded());
	}

	@Override
	public final String getFormat() {
		return ID;
	}

	public final byte[] getBytes(){
		return bytes;
	}
	
	public final byte[] getBytesDecoded() {
		return Base64.getDecoder().decode(bytes);
	}

}
