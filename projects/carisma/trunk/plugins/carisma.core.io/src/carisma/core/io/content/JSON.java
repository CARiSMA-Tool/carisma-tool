package carisma.core.io.content;

import org.json.JSONObject;
import org.json.XML;

public class JSON extends JSONObject implements Content {

	public static final String ID = "JSON";

	public JSON(String document) {
		super(document);
	}

	public JSON(XML_DOM content) {
		super(XML.toJSONObject(content.asString()).toString());
	}

	@Override
	public String getFormat() {
		return ID;
	}

	@Override
	public String asString() {
		return super.toString();
	}
	
}