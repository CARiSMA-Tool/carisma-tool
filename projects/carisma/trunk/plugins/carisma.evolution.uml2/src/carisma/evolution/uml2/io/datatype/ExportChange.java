package carisma.evolution.uml2.io.datatype;

import java.util.ArrayList;
import java.util.List;


public class ExportChange {
	private String ref;
	private List<ExportChangeConstraint> constraints;
	private List<ExportAlternative> alternatives;
	
	public ExportChange(){
		this.ref = "";
		this.constraints = new ArrayList<>();
		this.alternatives = new ArrayList<>();
	}
	
	public ExportChange(String ref){
		this.ref = ref;
		this.alternatives = new ArrayList<>();
	}
	
	public ExportChange(String ref, List<ExportChangeConstraint> constrains){
		this.ref = ref;
		this.constraints = constrains;
		this.alternatives = new ArrayList<>();
	}
	
	public void setRef(String ref){
		this.ref = ref;
	}

	
	public void setConstraints(List<ExportChangeConstraint> constraints){
		this.constraints = constraints;
	}
	
	
	public void addAlternative(ExportAlternative alt) {
		this.alternatives.add(alt);
	}
	
	public List<ExportAlternative> getAlternatives(){
		return this.alternatives;
	}
	
	public String getRef(){
		return this.ref;
	}
	
	public List<ExportChangeConstraint> getConstraints(){
		return this.constraints;
	}
	
}
