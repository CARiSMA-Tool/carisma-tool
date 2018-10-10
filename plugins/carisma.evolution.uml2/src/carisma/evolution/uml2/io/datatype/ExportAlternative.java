package carisma.evolution.uml2.io.datatype;

import java.util.ArrayList;
import java.util.List;


public class ExportAlternative {
	
	private List<ExportDeltaElement> elementDescriptions;

	private ExportExtTag exportExtTagValue;
	
	public ExportAlternative(){
		this.elementDescriptions = new ArrayList<>();
		this.exportExtTagValue = null;
	}
	
	public void addExtValue(ExportExtTag extValue){
		this.exportExtTagValue = extValue;
	}
	
	public void add(ExportDeltaElement ele){
		this.elementDescriptions.add(ele); 
	}
	
	
	public List<ExportDeltaElement> getElementDescriptions(){
		return this.elementDescriptions;
	}
	
	public void setExt(ExportExtTag extValue){
		this.exportExtTagValue = extValue;
	}
	
	public ExportExtTag getExt(){
		return this.exportExtTagValue; 
	}
}
