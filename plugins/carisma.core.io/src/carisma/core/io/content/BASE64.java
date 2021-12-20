package carisma.core.io.content;

import jakarta.xml.bind.DatatypeConverter;

public class BASE64 implements Content {

	public static String ID = "BASE64";

	private final byte[] bytes;

	protected BASE64(final byte[] base64) {
		this.bytes = base64;
	}

	protected BASE64(final String content) {
		final var nonBase64 = content.getBytes();
		this.bytes = DatatypeConverter.printBase64Binary(nonBase64).getBytes();
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
		return this.bytes;
	}

	public final byte[] getBytesDecoded() {
		return DatatypeConverter.parseBase64Binary(new String(this.bytes));
	}

}
