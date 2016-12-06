package carisma.core.io.content;

import org.json.JSONException;
import org.json.JSONObject;

public class JSON extends JSONObject implements Content {

	public static final String ID = "JSON";

	protected JSON(final String document) throws JSONException {
		super(document);
	}

	@Override
	public final String getFormat() {
		return ID;
	}

	@Override
	public final String asString() {
		return super.toString();
	}
	
}