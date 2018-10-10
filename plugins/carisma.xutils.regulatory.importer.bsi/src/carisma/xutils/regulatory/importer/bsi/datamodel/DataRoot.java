package carisma.xutils.regulatory.importer.bsi.datamodel;

//import com.thoughtworks.xstream.XStream;

public class DataRoot { // TODO: besserer Name. ACHTUNG: auch XML-Alias in RootImporter anpassen!

	private BSICatalogue bsiCatalogue;
	// weitere Eintraege analog fuer BSI-Standards, ISO, etc.

	public DataRoot() {
		super();
		this.bsiCatalogue = null;
	}

	public final BSICatalogue getBsiCatalogue() {
		return bsiCatalogue;
	}

	public final void setBsiCatalogue(final BSICatalogue bsiCatalogue) {
		this.bsiCatalogue = bsiCatalogue;
	}
}
