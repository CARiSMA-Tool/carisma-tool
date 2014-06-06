package carisma.evolution.uml2.io.datatype;

import java.util.ArrayList;
import java.util.List;


public class ExportChange {
	private String ref;
	private List<ExportChangeConstraint> constraints;
	private List<ExportAlternative> alternatives;
	
	public ExportChange(){
		ref = "";
		constraints = new ArrayList<ExportChangeConstraint>();
		alternatives = new ArrayList<ExportAlternative>();
	}
	
	public ExportChange(String ref){
		this.ref = ref;
		alternatives = new ArrayList<ExportAlternative>();
	}
	
	public ExportChange(String ref, List<ExportChangeConstraint> constrains){
		this.ref = ref;
		this.constraints = constrains;
		alternatives = new ArrayList<ExportAlternative>();
	}
	
	public void setRef(String ref){
		this.ref = ref;
	}

	
	public void setConstraints(List<ExportChangeConstraint> constraints){
		this.constraints = constraints;
	}
	
	
	public void addAlternative(ExportAlternative alt) {
		alternatives.add(alt);
	}
	
	public List<ExportAlternative> getAlternatives(){
		return alternatives;
	}
	
	public String getRef(){
		return ref;
	}
	
	public List<ExportChangeConstraint> getConstraints(){
		return constraints;
	}
	
}
