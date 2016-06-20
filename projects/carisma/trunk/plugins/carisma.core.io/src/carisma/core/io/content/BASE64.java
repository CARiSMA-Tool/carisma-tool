package carisma.core.io.content;

import java.util.Base64;

public class BASE64 implements Content {

	public static String ID = "BASE64";
	
	private byte[] content;
	
	public BASE64(byte[] base64) {
		this.content = base64;
	}
	
	public BASE64(String content) {
		byte[] bytes = content.getBytes();
		this.content = Base64.getEncoder().encode(bytes);
	}
	
	public BASE64(XML_DOM xml){
		content = Base64.getEncoder().encode(xml.asString().getBytes());
	}
	
	@Override
	public String asString() {
		return new String(getBytesDecoded());
	}

	@Override
	public String getFormat() {
		return ID;
	}

	public byte[] getBytes(){
		return content;
	}
	
	public byte[] getBytesDecoded() {
		return Base64.getDecoder().decode(content);
	}

}
