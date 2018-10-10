package carisma.check.onto.exporter;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLOntology;

import carisma.regulatory.ontology.utils.GenericOntologyHelper;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.core.analysis.AnalysisHost;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CarismaCheck;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

/** This check exports the whole Entire_Law_Ontology.owl in to the DOT format.
 * 
 */
public class CopyOfCheck implements CarismaCheck {

    private int numberOfWords = 2;

    /** 
     * Output Writer for the new DOT File.
     */
    private FileWriter dotWriter;
    

    
    /** returns the Short name of an OWLClass.
     * 
     * @param owlClass Class for which the short name is to be returned.
     * @return Name of the OWLClass
     */
    private static String getOWLClassName(final OWLClass owlClass) {
        if (owlClass != null) {
            return owlClass.toStringID().replaceAll(".*\\/", "");
        } else {
            return "";
        }
    }

    
    /** Adds all SubClasses to the DOT output File.
     * 
     * @param actualClass the Class for which the Subclasses are searched for.
     * @param goh {@link GenericOntologyHelper}.
     * @param roh {@link RegulatoryOntologyHelper}
     */
    private void subClasses(final OWLClass actualClass, final GenericOntologyHelper goh, final RegulatoryOntologyHelper roh) {
        OWLOntology onto = goh.getOntology();
        String acutalClassName = getOWLClassName(actualClass); 
        for (OWLClassExpression subClass : actualClass.getSubClasses(onto)) {
            String className = getOWLClassName(goh.getOWLClass(subClass.toString()));
            if (!className.isEmpty() && !acutalClassName.isEmpty()) {
                try {
                    dotWriter.append(className + " -> " + acutalClassName + ";\n");
                } catch (IOException e) {
                	Logger.log(LogLevel.ERROR, "IO Error.", e);
                }
                String subClassName = getOWLClassName(goh.getOWLClass(subClass.toString()));
                numberOfWords = 2;
                for (OWLIndividual indi : goh.getOWLClass(subClass.toString()).getIndividuals(onto)) {
                    if (subClassName.equals("Section")) {
                        handleNamedTextedClass(goh, indi, className);
                    } else if (className.equals("RuleElement")) {
                        handleClassRuleElement(goh, indi, className);
                    } else if (subClassName.equals("BSIRule")) {
                    	handleClassRuleElement(goh, indi, className);
                    } else if (subClassName.equals("Law")) {
                    	handleNamedIndi(goh, indi, className, RegulatoryOntologyHelper.PROP_RULE_TITLE);
                    } else if (subClassName.equals("Paragraph")) {
                    	handleNamedNumberedClass(goh, indi, subClassName);
                    }
                }
                subClasses(goh.getOWLClass(subClass.toString()), goh, roh);
            } else {
                System.err.println(subClass.toString() + " is not Valid Class!");
            }
        }
    }
        
    private void handleNamedIndi(final GenericOntologyHelper goh, OWLIndividual indi, String className, String propertyName) {
    	String name;
    	try {
    		if (!propertyName.isEmpty()) {
    			name = goh.getStringAnnotation(indi.asOWLNamedIndividual(), propertyName);
    			dotWriter.append("\"" + name + "\"[shape=plaintext];\n");
    			dotWriter.append("\"" + name + "\" -> ");
    		}
            dotWriter.append("\"" + indi.toStringID().replaceAll(".*\\/", "") + "\"[shape=box];\n");
            dotWriter.append("\"" + indi.toStringID().replaceAll(".*\\/", "") + "\"" 
                    + " -> " + className + ";\n");
        }  catch (IOException e) {
        	Logger.log(LogLevel.ERROR, "IO Error.", e);
        } catch (NoSuchPropertyException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    }
    
    
    private void handleNamedNumberedClass(final GenericOntologyHelper goh, final OWLIndividual indi, final String className) {
    	String number = "";
        try {
            number = goh.getStringAnnotation(indi.asOWLNamedIndividual(), RegulatoryOntologyHelper.PROP_RULEELEMENT_TYPE);
            handleNamedIndi(goh, indi, className, RegulatoryOntologyHelper.PROP_RULEELEMENT_TYPE);
            dotWriter.append("\"ElementType:\\n         " + number + "\"[shape=plaintext];\n");
            dotWriter.append("\"" + indi.toStringID().replaceAll(".*\\/", "") + "\"[shape=box];\n");
            dotWriter.append("\"ElementType:\\n         " + number + "\" -> \"" + indi.toStringID().replaceAll(".*\\/", "") + "\"");

        } catch (NoSuchPropertyException e1) {
        	Logger.log(LogLevel.ERROR, "No Such Property as 'Section/Text'.", e1);
        } catch (IOException e) {
        	Logger.log(LogLevel.ERROR, "IO Error.", e);
        }
    }
    
    /** Creates output for the Individuals with a Name and a Text Annotation.
     * 
     * @param goh 
     * @param indi 
     * @param className  
     */
    private void handleNamedTextedClass(final GenericOntologyHelper goh, final OWLIndividual indi, final String className) {
        String anno = "";
        try {
            anno = goh.getStringAnnotation(indi.asOWLNamedIndividual(), "Section/Text");
        
            StringBuffer newAnno = new StringBuffer();
            for (String part : anno.split(" ")) {
                newAnno.append(part);
                newAnno.append(" ");
                if (anno.split(" ").length >= 10 && numberOfWords % (anno.split(" ").length / 3) == 0) {
                    newAnno.append("\\n");
                }
                numberOfWords++;
            }

            dotWriter.append("\"" + newAnno.toString() + "\"" + "[shape=plaintext];\n");
            dotWriter.append("\"" + indi.toStringID().replaceAll(".*\\/", "") + "\"[shape=box];\n");
            dotWriter.append("\"" + newAnno.toString() + "\"" + " -> ");
            dotWriter.append("\"" + indi.toStringID().replaceAll(".*\\/", "") + "\"" 
            + " -> " + className + ";\n");
        } catch (NoSuchPropertyException e) {
        	Logger.log(LogLevel.ERROR, "No Such Property as 'Section/Text'.", e);
        } catch (IOException e) {
        	Logger.log(LogLevel.ERROR, "IO Error.", e);
        }
    }
    
    private void handleClassRuleElement(final GenericOntologyHelper goh, OWLIndividual indi, String className) {
    	String ruleElementType = "";
        try {
            ruleElementType = goh.getStringAnnotation(indi.asOWLNamedIndividual(), RegulatoryOntologyHelper.PROP_RULEELEMENT_TYPE);

            dotWriter.append("\"ElementType:\\n         " + ruleElementType + "\"[shape=plaintext];\n");
            dotWriter.append("\"" + indi.toStringID().replaceAll(".*\\/", "") + "\"[shape=box];\n");
            dotWriter.append("\"ElementType:\\n         " + ruleElementType + "\" -> ");
            dotWriter.append("\"" + indi.toStringID().replaceAll(".*\\/", "") + "\"" 
                    + " -> " + className + ";\n");
        } catch (NoSuchPropertyException e1) {
        	Logger.log(LogLevel.ERROR, "No Such Property as 'Section/Text'.", e1);
        } catch (IOException e) {
        	Logger.log(LogLevel.ERROR, "IO Error.", e);
        }
    }
    

	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		try {
            
            File output = new File("C:/TEMP/onto/dotFormat.dot");
            if (output.exists() || output.createNewFile()) {
                dotWriter = new FileWriter(output);
                GenericOntologyHelper goh = new GenericOntologyHelper();
                File x = new File("./resources/Entire_Law_Ontology.owl");
                System.out.println(x.getAbsolutePath());
                goh.loadOWLOntologyFromFile(x);
                RegulatoryOntologyHelper regOntHelper = new RegulatoryOntologyHelper(goh);
                dotWriter.append("digraph graphname \n{\n");
                dotWriter.append("Rule -> Thing;\n");
                
                OWLClass rule = regOntHelper.getOWLClass("Rule");
                subClasses(rule, regOntHelper.getGoh(), regOntHelper);
                
                OWLClass ruleElement = regOntHelper.getOWLClass("RuleElement");
                subClasses(ruleElement, regOntHelper.getGoh(), regOntHelper);
                dotWriter.append("RuleElement -> Thing;\n");                
                
                dotWriter.append("}");
            }
            return true;
        } catch (IOException e) {
        	Logger.log(LogLevel.ERROR, "IO Error.", e);
            return false;
        } finally {
            try {
                dotWriter.flush();
                dotWriter.close();
            } catch (IOException e) {
            	Logger.log(LogLevel.ERROR, "IO Error.", e);
            }
        }
	}
}