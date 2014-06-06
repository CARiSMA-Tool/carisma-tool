package carisma.check.riskfinder;

import java.io.File;

public class XMLResult {
	private String xmlResultComplete;
	private String xmlSimpleResult;

	public String getXmlResultComplete() {
		return xmlResultComplete;
	}
	public String getXmlSimpleResult() {
		return xmlSimpleResult;
	}
	
	public void appendXmlResultComplete(String suffix){
		this.xmlResultComplete += suffix;		
	}

	public void appendXmlSimpleResult(String suffix){
		this.xmlSimpleResult += suffix;		
	}
	
	public void init(File ontologyFile, boolean synonymsOnly,
			File stopwordFile, boolean append, int minScore, boolean writeback,
			File xmlResultFile) {
		xmlResultComplete = ""; // xml-String mit den gleichen Informationen,
								// die auch im report stehen
		this.appendXmlResultComplete(xmlResultComplete = "");
		this.appendXmlResultComplete("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n");
		this.appendXmlResultComplete("<RiskfinderResult ");
		this.appendXmlResultComplete("writeback=\"" + writeback);
		this.appendXmlResultComplete("\"  append=\"" + append);
		this.appendXmlResultComplete("\"  stopwords=\"" + stopwordFile);
		this.appendXmlResultComplete("\"  synonymsOnly=\"" + synonymsOnly);
		this.appendXmlResultComplete("\"  ontology=\"" + ontologyFile);
		this.appendXmlResultComplete("\"  minScore=\"" + minScore);
		this.appendXmlResultComplete("\" >\n");

		this.appendXmlSimpleResult(xmlSimpleResult = ""); // xml-String mit vereinfachtem Format, s. Ticket #1138
		this.appendXmlSimpleResult("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n");
		this.appendXmlSimpleResult("<threats>\n");
	}
	
	public void finish(){
		this.appendXmlResultComplete("</RiskfinderResult>");
		this.appendXmlSimpleResult("</threats>");
	}

}
