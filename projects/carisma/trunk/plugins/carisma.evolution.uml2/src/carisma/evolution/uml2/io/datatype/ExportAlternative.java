package carisma.evolution.uml2.io.datatype;

import java.util.ArrayList;
import java.util.List;


public class ExportAlternative {
	
	private List<ExportDeltaElement> elementDescriptions;

	private ExportExtTag extValue;
	
	public ExportAlternative(){
		elementDescriptions = new ArrayList<ExportDeltaElement>();
		extValue = null;
	}
	
	public void addExtValue(ExportExtTag extValue){
		this.extValue = extValue;
	}
	
	public void add(ExportDeltaElement ele){
		elementDescriptions.add(ele); 
	}
	
	
	public List<ExportDeltaElement> getElementDescriptions(){
		return elementDescriptions;
	}
	
	public void setExt(ExportExtTag extValue){
		this.extValue = extValue;
	}
	
	public ExportExtTag getExt(){
		return extValue; 
	}
}
