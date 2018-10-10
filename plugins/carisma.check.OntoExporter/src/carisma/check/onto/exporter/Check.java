package carisma.check.onto.exporter;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import carisma.regulatory.ontology.utils.GenericOntologyHelper;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.OutputFileParameter;
import carisma.core.analysis.StringParameter;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CarismaCheck;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

/** This check exports the whole Entire_Law_Ontology.owl in to the DOT format.
 * 
 */
public class Check implements CarismaCheck {

    private String searchPattern = "";

    /** 
     * Output Writer for the new DOT File.
     */
    private StringBuffer dotWriter = new StringBuffer();
    
    
    private GenericOntologyHelper goh;
    private RegulatoryOntologyHelper roh;
    private static final String trimOWLClassNamePattern = ".*\\/";

    
    /** returns the Short name of an OWLClass.
     * 
     * @param owlClass Class for which the short name is to be returned.
     * @return Name of the OWLClass
     */
    private static String getOWLClassName(final OWLClass owlClass) {
        if (owlClass != null) {
            return owlClass.toStringID().replaceAll(trimOWLClassNamePattern, "");
        } else {
            return "";
        }
    }
    private static String getOWLClassName(final String owlClass) {
        if (owlClass != null) {
            return owlClass.replaceAll(trimOWLClassNamePattern, "");
        } else {
            return "";
        }
    }
    private static String getOWLClassName(final OWLIndividual owlClass) {
        if (owlClass != null) {
            return owlClass.toStringID().replaceAll(trimOWLClassNamePattern, "");
        } else {
            return "";
        }
    }

    
    /** Adds all SubClasses to the DOT output File.
     * 
     * @param actualClass the Class for which the Subclasses are searched for.
     * @param goh {@link GenericOntologyHelper}.
     * @param roh {@link RegulatoryOntologyHelper}
     * @throws IOException 
     */
    private void subClasses(final OWLClass actualClass) throws IOException {
        OWLOntology onto = goh.getOntology();
        String actualClassName = getOWLClassName(actualClass); 
        for (OWLClassExpression subClass : actualClass.getSubClasses(onto)) {
            if (!actualClassName.isEmpty()) {
            	String subClassName = getOWLClassName(goh.getOWLClass(subClass.toString()));
                for (OWLIndividual indi : goh.getOWLClass(subClass.toString()).getIndividuals(onto)) {
            		if (subClassName.equals(RegulatoryOntologyHelper.CLS_BSIELEMENT) || subClassName.equals(RegulatoryOntologyHelper.CLS_BSIMEASURE) || subClassName.equals(RegulatoryOntologyHelper.CLS_BSITHREAT)) {
            			handleNamedTextedClass(indi, subClassName, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT, true);
            		} else if (subClassName.equals(RegulatoryOntologyHelper.CLS_LAW) || subClassName.equals(RegulatoryOntologyHelper.CLS_PARAGRAPH)) {
            			handleNamedIndi(indi, subClassName, RegulatoryOntologyHelper.PROP_RULE_TITLE, true);
            		} else if (subClassName.equals(RegulatoryOntologyHelper.CLS_SECTION)) {
            			handleNamedTextedClass(indi, subClassName, RegulatoryOntologyHelper.PROP_SECTION_CONTENT, true);
            		}  else if (subClassName.equals(RegulatoryOntologyHelper.CLS_MARISKBINDING) || subClassName.equals(RegulatoryOntologyHelper.CLS_MARISKCOMMENT)) {
            			handleNamedTextedClass(indi, subClassName, RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT, true);
            		}  else if (subClassName.equals(RegulatoryOntologyHelper.CLS_MARISKCLAUSE)) {
            			handleNamedTextedClass(indi, subClassName, RegulatoryOntologyHelper.PROP_MARISKCLAUSE_NAME, true);
            		} else if (goh.getOWLClass(subClass.toString()).getSuperClasses(onto).contains(goh.getOWLClass(RegulatoryOntologyHelper.CLS_RULEELEMENT))) {
            			handleClassRuleElement(indi, subClassName, true);
            		}
                }
                subClasses(goh.getOWLClass(subClass.toString()));
                if (actualClassName.contains(searchPattern)) {
            		getSuperClass(actualClass);
            	}
            } else {
            	 Logger.log(LogLevel.ERROR, subClass.toString() + " is not Valid Class!");
            }
        }
    }
        
    private void handleNamedIndi(OWLIndividual indi, String className, String propertyName, final boolean needComparison) throws IOException {
    	String name = "";
    	String indiID = indi.toStringID().replaceAll(trimOWLClassNamePattern, "");
    	try {
    		if (!propertyName.isEmpty()) {
    			name = goh.getStringAnnotation(indi.asOWLNamedIndividual(), propertyName);
    			if (needComparison) {
	    			if (!name.contains(searchPattern)) {
	                	return;
	    			} else {
	            		addSimpleLineToFile(indiID, className);
	                 	getSuperClass(goh.getOWLClass(className));
                 	}
                }
    			dotWriter.append("\"" + name + "\"[shape=plaintext];\n");
    			dotWriter.append("\"" + name + "\" -> ");
    		}
            dotWriter.append("\"" + indiID + "\"[shape=box];\n");
            handleProperties(indi);
        } catch (NoSuchPropertyException e) {
			Logger.log(LogLevel.ERROR, "", e);
		}
    }
    
    /** Creates output for the Individuals with a Name and a Text Annotation.
     * 
     * @param indi 
     * @param className  
     * @throws IOException 
     */
    private void handleNamedTextedClass(final OWLIndividual indi, final String className, final String annoName, final boolean needComparison) throws IOException {
        String anno = "";
        String indiID = indi.toStringID().replaceAll(trimOWLClassNamePattern, "");
        try {
            anno = goh.getStringAnnotation(indi.asOWLNamedIndividual(), annoName);
           
            if (needComparison && !anno.contains(searchPattern) && !indiID.contains(searchPattern) ) {
        		return;
            }

            dotWriter.append("\"" + indiID + "\"[shape=box];\n");
            if (needComparison && (anno.contains(searchPattern) || indiID.contains(searchPattern)) ) {
        		getSuperClass(goh.getOWLClass(className));
        		addSimpleLineToFile(indiID, className);
            }
            handleProperties(indi);
        } catch (NoSuchPropertyException e) {
        	Logger.log(LogLevel.ERROR, "No Such Property as " + annoName + ".", e);
        }
    }
    
    private void handleProperties(OWLIndividual indi) throws IOException {
    	handlePropertyAssertions(indi);
    	handleReferredMeasures(indi);
    	handleReferredThreats(indi);
    	handleReferredRules(indi);
    }

    
    private void handleClassRuleElement(OWLIndividual indi, String className, boolean needComparison) throws IOException {
    	String ruleElementType = "";
    	String indiID = indi.toStringID().replaceAll(trimOWLClassNamePattern, "");
        try {
            ruleElementType = goh.getStringAnnotation(indi.asOWLNamedIndividual(), RegulatoryOntologyHelper.PROP_RULEELEMENT_TYPE);
            ruleElementType = ruleElementType.replaceAll("\"", "\\\"");
            if (needComparison) {
	            if (!ruleElementType.contains(searchPattern) && !indiID.contains(searchPattern)) {
	            	return;
	            } else {
	        		getSuperClass(goh.getOWLClass(className));
	        	}
            }
            dotWriter.append("\"ElementType:\\n         " + ruleElementType + "\"[shape=plaintext];\n");
            dotWriter.append("\"" + indiID + "\"[shape=box];\n");
            dotWriter.append("\"ElementType:\\n         " + ruleElementType + "\" -> ");
            dotWriter.append("\"" + indiID + "\" -> " + className + ";\n");
        } catch (NoSuchPropertyException e1) {
        	Logger.log(LogLevel.ERROR, "No Such Property as " + RegulatoryOntologyHelper.PROP_RULEELEMENT_TYPE + ".", e1);
        }
    }

	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		FileWriter dotFileWriter = null;
		try {
			InputFileParameter inputOnto = (InputFileParameter) parameters.get("carisma.check.OntoExporter.inputOntologie");
			OutputFileParameter outputPara = (OutputFileParameter) parameters.get("carisma.check.OntoExporter..outputfile");
			BooleanParameter optionParameter = (BooleanParameter) parameters.get("carisma.check.OntoExporter.optionpara");
			StringParameter searchPattern = (StringParameter) parameters.get("carisma.check.OntoExporter.searchPattern");
			this.searchPattern = searchPattern.getValue();
			if (inputOnto == null || outputPara == null || optionParameter == null || searchPattern == null) {
				throw new IllegalArgumentException("Error on argument 'parameters'.");
			}
            File output = outputPara.getValue();
            if (output.exists() || output.createNewFile()) {
                dotFileWriter = new FileWriter(output);
                goh = new GenericOntologyHelper();
                File x = inputOnto.getValue();
                if (!x.getName().endsWith(".owl")) {
                	throw new IllegalArgumentException("Input file '" + x.getName() + "' has to be an .owl file!!");
                }
                goh.loadOWLOntologyFromFile(x);
                roh = new RegulatoryOntologyHelper(goh);
                dotWriter.append("digraph graphname \n{\n");
                                
                if (optionParameter.getValue()) {
                	getIndividual();
                } else {
                	findKeyWord();
                }
		        dotWriter.append("}");
		        dotFileWriter.append(dotWriter);
            }
            return true;
        } catch (IOException e) {
        	Logger.log(LogLevel.ERROR, "IO Error.", e);
            return false;
        } finally {
            try {
            	if (dotFileWriter != null) {
            		dotFileWriter.flush();
            		dotFileWriter.close();
            	}
            } catch (IOException e) {
            	Logger.log(LogLevel.ERROR, "IO Error.", e);
            }
        }
	}
	
	/** Creates output for all Individuals which contain somehow the searchPattern.
	 * 
	 * @param searchPattern Pattern to look for.
	 * @throws IOException 
	 */
	private void findKeyWord() throws IOException {
        OWLClass rule = roh.getOWLClass(RegulatoryOntologyHelper.CLS_RULE);
        subClasses(rule);
        
        OWLClass ruleElement = roh.getOWLClass(RegulatoryOntologyHelper.CLS_RULEELEMENT);
        subClasses(ruleElement);
	}
	
	private void getSuperClass(OWLClass classExpr) throws IOException {
		for (OWLClassExpression parent : classExpr.getSuperClasses(goh.getOntology())) {
			if (!dotWriter.toString().contains("\"" + getOWLClassName(classExpr).replaceAll(">", "") + "\" -> \"" + getOWLClassName(parent.toString()).replaceAll(">", "") + "\";\n")) {
				dotWriter.append("\"" + getOWLClassName(classExpr).replaceAll(">", "") + "\" -> \"" + getOWLClassName(parent.toString()).replaceAll(">", "") + "\";\n");
				getSuperClass(goh.getOWLClass(parent.toString()));
			}
		}
		if (classExpr.getSuperClasses(goh.getOntology()).isEmpty()) {
			if (!dotWriter.toString().contains("\"" + getOWLClassName(classExpr).replaceAll(">", "") + "\" -> \"Thing\";\n")) {
				dotWriter.append("\"" + getOWLClassName(classExpr).replaceAll(">", "") + "\" -> \"Thing\";\n");
			}
		}
	}
	
	private void handlePropertyAssertions(OWLIndividual indi) throws IOException {
		
		Map<OWLObjectPropertyExpression, Set<OWLIndividual>> props = indi.getObjectPropertyValues(goh.getOntology());
		
		Set<OWLIndividual> activities;
		Set<OWLIndividual> artifacts;
		Set<OWLIndividual> processes;
		Set<OWLIndividual> properties;
		Set<OWLIndividual> roles;

		OWLObjectPropertyExpression activity = goh.getOWLObjectProperty(RegulatoryOntologyHelper.CLS_ACTIVITY);
		OWLObjectPropertyExpression artifact = goh.getOWLObjectProperty(RegulatoryOntologyHelper.CLS_ARTIFACT);
		OWLObjectPropertyExpression process = goh.getOWLObjectProperty(RegulatoryOntologyHelper.CLS_PROCESS);
		OWLObjectPropertyExpression property = goh.getOWLObjectProperty(RegulatoryOntologyHelper.CLS_PROPERTY);
		OWLObjectPropertyExpression role = goh.getOWLObjectProperty(RegulatoryOntologyHelper.CLS_ROLE);
		
		activities = props.get(activity);
		artifacts = props.get(artifact);
		processes = props.get(process);
		properties = props.get(property);
		roles = props.get(role);
		
		if (activities != null && activities.size() > 0) {
			writeSubgraph(2, "#0033ff", "Activities", activities, indi); //blue
			for (OWLIndividual ele : activities) {
				addSimpleLineToFile(getOWLClassName(ele), RegulatoryOntologyHelper.CLS_ACTIVITY);
			}
		}
		
		if (artifacts != null && artifacts.size() > 0) {
			writeSubgraph(3, "#cc0066", "Artifacts", artifacts, indi); // reddish 
			for (OWLIndividual ele : artifacts) {
				addSimpleLineToFile(getOWLClassName(ele), RegulatoryOntologyHelper.CLS_ARTIFACT);
			}
		}
		
		if (processes != null && processes.size() > 0) {
			writeSubgraph(4, "#009966", "Processes", processes, indi); // greenish
			for (OWLIndividual ele : processes) {
				addSimpleLineToFile(getOWLClassName(ele), RegulatoryOntologyHelper.CLS_PROCESS);
			}
		}
		
		if (properties != null && properties.size() > 0) {
			writeSubgraph(5, "#996600", "Properties", properties, indi); // brownish
			for (OWLIndividual ele : properties) {
				addSimpleLineToFile(getOWLClassName(ele), RegulatoryOntologyHelper.CLS_PROPERTY);
			}
		}
		
		if (roles != null && roles.size() > 0) {
			writeSubgraph(6, "#ffff66", "Roles", roles, indi); //
			for (OWLIndividual ele : roles) {
				addSimpleLineToFile(getOWLClassName(ele), RegulatoryOntologyHelper.CLS_ROLE);
			}
		}
		
		handleMARiskProperties(indi);
		
	}
	
	private void 
	handleMARiskProperties(OWLIndividual indi) throws IOException {
		Map<OWLObjectPropertyExpression, Set<OWLIndividual>> props = indi.getObjectPropertyValues(goh.getOntology());
		Set<OWLIndividual> entry= props.get(goh.getOWLObjectProperty(RegulatoryOntologyHelper.REL_MARISKCLAUSE_HASENTRIES));

		if (props.containsKey(goh.getOWLObjectProperty(RegulatoryOntologyHelper.REL_MARISKCLAUSE_HASENTRIES)) && entry.size() > 0) {
			writeSubgraph(7, "", "Entry", entry, indi);
		}
	}
	
	private void handleReferredThreats(OWLIndividual indi) throws IOException {
		OWLObjectPropertyExpression threats = goh.getOWLObjectProperty(RegulatoryOntologyHelper.REL_BSIELEMENT_THREATS);
		Map<OWLObjectPropertyExpression, Set<OWLIndividual>> props = indi.getObjectPropertyValues(goh.getOntology());
		if (props.containsKey(threats) && props.get(threats).size() > 0) {
			writeSubgraph(0, "lightgrey", "Threats", props.get(threats), indi);
		}
	}

	private void writeSubgraph(int clusterNumber,String bgcolor, String label, Set<OWLIndividual> props, OWLIndividual indi
			) throws IOException {
		dotWriter.append("	subgraph cluster" + clusterNumber + " { \n");
		dotWriter.append("		node [style=filled,color=white];\n");
		dotWriter.append("		style=filled;\n");
		dotWriter.append("		color=\"" + bgcolor + "\";\n");
		dotWriter.append("		label=\"" + label + "\";\n");
		List<String> threatNames = new ArrayList<String>();
		for (OWLIndividual prop : props) {
			threatNames.add(getOWLClassName(prop));
			dotWriter.append("		\"" + getOWLClassName(prop) + "\";\n");
		}
		dotWriter.append("	} \n");
		for (String threat : threatNames) {
			addSimpleLineToFile(threat, getOWLClassName(indi));
		}
	}
	
	private void handleReferredMeasures(OWLIndividual indi) throws IOException {
		OWLObjectPropertyExpression measures = goh.getOWLObjectProperty(RegulatoryOntologyHelper.REL_BSIELEMENT_MEASURES);
		Map<OWLObjectPropertyExpression, Set<OWLIndividual>> props = indi.getObjectPropertyValues(goh.getOntology());
		if (props.containsKey(measures) && props.get(measures).size() > 0) {
			writeSubgraph(1, "lightblue", "Measures", props.get(measures), indi);
		}
	}
	
	/** Fetch the referenced individuals for a given one.
	 * 
	 * @param searchPattern
	 * @throws IOException
	 */
	private void handleReferredRules(OWLIndividual indi) throws IOException {
		if (!indi.getObjectPropertyValues(goh.getOntology()).containsKey(goh.getOWLObjectProperty(RegulatoryOntologyHelper.REL_RULE_REFFEREDRULES))) {
			return;
		}
		for (OWLIndividual objectPorperties : indi.getObjectPropertyValues(goh.getOntology()).get(goh.getOWLObjectProperty("Rule/ReferredRules"))) {
			dotWriter.append("\"" + getOWLClassName(objectPorperties.toStringID()) + "\" -> \"" + getOWLClassName(indi.toStringID()) + "\" [style=dotted, arrowhead=onormal];\n");
		}
	}
	
	private void handleAnnotations(final OWLIndividual indi, final String superClass) throws IOException {
		if (superClass.equals(RegulatoryOntologyHelper.CLS_BSIELEMENT) || superClass.equals(RegulatoryOntologyHelper.CLS_BSIMEASURE) || superClass.equals(RegulatoryOntologyHelper.CLS_BSITHREAT)) {
			handleNamedTextedClass(indi, superClass, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT, false);
		} else if (superClass.equals(RegulatoryOntologyHelper.CLS_LAW) || superClass.equals(RegulatoryOntologyHelper.CLS_PARAGRAPH)) {
			handleNamedIndi(indi, superClass, RegulatoryOntologyHelper.PROP_RULE_TITLE, false);
		} else if (superClass.equals(RegulatoryOntologyHelper.CLS_SECTION)) {
			handleNamedTextedClass(indi, superClass, RegulatoryOntologyHelper.PROP_SECTION_CONTENT, false);
		}  else if (superClass.equals(RegulatoryOntologyHelper.CLS_MARISKBINDING) || superClass.equals(RegulatoryOntologyHelper.CLS_MARISKCOMMENT)) {
			handleNamedTextedClass(indi, superClass, RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT, false);
		}  else if (superClass.equals(RegulatoryOntologyHelper.CLS_MARISKCLAUSE)) {
			handleNamedTextedClass(indi, superClass, RegulatoryOntologyHelper.PROP_MARISKCLAUSE_NAME, false);
		} else if (goh.getOWLClass(superClass).getSuperClasses(goh.getOntology()).contains(goh.getOWLClass(RegulatoryOntologyHelper.CLS_RULEELEMENT))) {
			handleClassRuleElement(indi, superClass, true);
		}
	}

	private void addSimpleLineToFile(String parentNode, String childNode) throws IOException {
		dotWriter.append("\"" + parentNode + "\" -> \"" + childNode + "\";\n");
	}
	
	
	private void getIndividual() throws IOException {
		OWLIndividual indi = goh.getIndividualById(searchPattern);
		Set<OWLClassExpression> types = indi.getTypes(goh.getOntology());
		for (OWLClassExpression parent : types) {
			handleAnnotations(indi, getOWLClassName(goh.getOWLClass(parent.toString())));
			dotWriter.append("\"" + getOWLClassName(indi.toStringID()) + "\" -> \"" + getOWLClassName(goh.getOWLClass(parent.toString())) + "\";\n");
			getSuperClass(goh.getOWLClass(parent.toString()));
		}
	}
}