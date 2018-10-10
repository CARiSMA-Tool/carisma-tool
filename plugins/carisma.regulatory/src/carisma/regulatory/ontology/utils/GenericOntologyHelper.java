package carisma.regulatory.ontology.utils;

import java.io.File;
import java.lang.annotation.AnnotationFormatError;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.UnknownAnnotationValueException;

import org.semanticweb.HermiT.Reasoner;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAnnotationValue;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLException;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.PrefixManager;
import org.semanticweb.owlapi.reasoner.ConsoleProgressMonitor;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.reasoner.SimpleConfiguration;
import org.semanticweb.owlapi.util.DefaultPrefixManager;

/**
 * Helper class to work conveniently with arbitrary ontologies.
 * @author wenzel
 *
 */
public class GenericOntologyHelper {
    /**
     * The currently loaded ontology.
     */
	private OWLOntology currentOntology = null;
	/**
	 * Manages creation and persistence of ontologies.
	 */
	private OWLOntologyManager manager = null;
	/**
	 * Builds ontology entities, axioms, etc. which can then be added to an ontology using the manager.
	 */
	private OWLDataFactory dataFactory = null;
	/**
	 * Conveniently prefixes classes with their full qualification.
	 */
	private PrefixManager prefixManagerClasses = null;
    /**
     * Conveniently prefixes individuals with their full qualification.
     */
	private PrefixManager prefixManagerIndividuals = null;
    /**
     * Conveniently prefixes annotations with their full qualification.
     */
	private PrefixManager prefixManagerAnnotations = null;
	/**
	 * The used reasoner for querying the ontology.
	 */
	private OWLReasoner reasoner = null;
	/**
	 * Initializes the helper to provide necessary functionality.
	 */
	public GenericOntologyHelper() {
		this.manager = OWLManager.createOWLOntologyManager();
		this.dataFactory = manager.getOWLDataFactory();
	}

    /**
     * Returns the ontology this helper is working with.
     * @return
     */
    public OWLOntology getOntology() {
        return currentOntology;
    }
    
    /**
     * Sets the prefix that is used by the helper to compute IRIs.
     * @param prefix
     */
    public final void setPrefix(final String prefix) {
        prefixManagerClasses = new DefaultPrefixManager(prefix + "classes/");
        prefixManagerIndividuals = new DefaultPrefixManager(prefix + "individuals/");
        prefixManagerAnnotations = new DefaultPrefixManager(prefix + "annotations/");
    }
    
    /**
     * Lazy initialization of the used reasoner including common preparations. 
     */
    private void prepareReasoning() {
        if (true || reasoner == null) {
            OWLReasonerFactory reasonerFactory = new Reasoner.ReasonerFactory();
            ConsoleProgressMonitor progressMonitor = new ConsoleProgressMonitor(); // TODO durch eigenen Monitor ersetzen
            OWLReasonerConfiguration config = new SimpleConfiguration(); //progressMonitor);
            reasoner = reasonerFactory.createReasoner(currentOntology, config);
            reasoner.precomputeInferences();
        } else {
            reasoner.flush();
        }
    }
    
    /**
     * Creates a new, empty ontology.
     * @param ontologyIRIstring
     */
    public final void createNewOntology(final String ontologyIRIstring) {//, String documentIRIstring) {
        IRI ontologyIRI = IRI.create(ontologyIRIstring);
        try {
            this.currentOntology = manager.createOntology(ontologyIRI);
            reasoner = null;
        } catch (OWLOntologyCreationException e) {
            throw new OntologyHelperException(e, "Unable to create new ontology.");
        }
    }
    
	/**
	 * Loads an ontology from a file.
	 * @param file - the ontology file
	 * @throws OntologyHelperException
	 */
	public final void loadOWLOntologyFromFile(final File file) {
		try {
			currentOntology = manager.loadOntologyFromOntologyDocument(file);
			reasoner = null;
		} catch (OWLOntologyCreationException e) {
// Fuer mehr Infos bzgl. eines Bugs eingefuegt (Bug: kann von uns erzeugte Ontologie nicht laden)
		    e.printStackTrace();
			throw new OntologyHelperException(e, "Unable to load ontology.");
		}
	}
	
	/**
	 * removes the given Ontology. If the Ontology is not managed by this goh, nothing happens.
	 * @param ontology
	 */
	public final void removeOWLOntology(OWLOntology ontology) {
	    manager.removeOntology(ontology);
	}
	/**
	 * Saves the current ontology in an OWL file.
	 * @param file
	 */
	public final void saveOntologyToFile(final File file) {
		IRI fileIRI = IRI.create(file);
		try {
			this.manager.saveOntology(this.currentOntology, fileIRI);
		} catch (OWLOntologyStorageException e) {
			throw new OntologyHelperException(e, "Unable to save ontology.");
		}
	}
// OWL CLASS METHODS
	/**
	 * This method adds an axiom to the ontology that declares a given class.
	 * This means that the ontology explicitly contains the given class.
	 * @param className - The class to declare
	 * @return - the axiom declaring the class
	 */
    public OWLDeclarationAxiom declareClass(final String className) {
        OWLDeclarationAxiom classDeclaration = dataFactory.getOWLDeclarationAxiom(getOWLClass(className));
        if (!currentOntology.containsAxiom(classDeclaration)) {
            manager.addAxiom(currentOntology, classDeclaration);
        }
        return classDeclaration;
    }
    
    /**
     * Returns the {@link OWLClass} with the given name. Does not add anything to an ontology. 
     * @param className
     * @return
     */
    public OWLClass getOWLClass(final String className) {
        return dataFactory.getOWLClass(className, this.prefixManagerClasses);
    }
    
    /**
     * Creates a subclass axiom.
     * @param subClass
     * @param superClass
     */
    public OWLSubClassOfAxiom createSubclassOfAxiom(final String subClass, final String superClass) {
        OWLSubClassOfAxiom subClassAxiom = dataFactory.getOWLSubClassOfAxiom(getOWLClass(subClass), getOWLClass(superClass));
        if (!currentOntology.containsAxiom(subClassAxiom)) {
            manager.addAxiom(currentOntology, subClassAxiom);
        }
        return subClassAxiom;
    }

// OWL INDIVIDUAL METHODS
    /**
     * Creates an individual by creating a class assertion about it.
     * This means that using this method you explicitly say "This individual of this class is in this ontology."
     * @param individualId
     * @param className
     * @return
     */
// FIXME: Individuals can be present in the ontology without having a class assertion
    public OWLNamedIndividual createOWLIndividual(final String individualId, final String className) {
        OWLNamedIndividual ind = dataFactory.getOWLNamedIndividual(individualId, this.prefixManagerIndividuals);
        OWLClassAssertionAxiom isOfClassAxiom = dataFactory.getOWLClassAssertionAxiom(getOWLClass(className), ind);
        if (!currentOntology.containsAxiom(isOfClassAxiom)) {
            manager.addAxiom(currentOntology, isOfClassAxiom);            
        }
        return ind;
    }

//FIXME: let removeOWLIndividual remove all references and mentions of the given individual
    /**
     * Removes the assertion that the given individual is of the given class from the ontology.
     * Does not explicitly remove the ability to infer that the individual exists in the ontology.
     * @param individual the individual
     */
    public void removeOWLIndividual(OWLNamedIndividual individual) {
        Set<OWLClassExpression> types = individual.getTypes(currentOntology);
        for (OWLClassExpression expression : types) {   // TODO check if every class has only one class-expression (DW: Do you mean class assertion?)
            this.manager.removeAxiom(currentOntology, dataFactory.getOWLClassAssertionAxiom(expression, individual));
            
            return;
        }      
    }
    
    /**
     * returns the type of an individual as IRI, or null if no type is present
     * @param individual the individual
     * @return the type
     */
    public IRI getTypeOfIndividual(OWLNamedIndividual individual) {
        IRI iri = null;
        Set<OWLClassExpression> types = individual.getTypes(currentOntology);
        for (OWLClassExpression expression : types) {
            iri = expression.asOWLClass().getIRI();
            return iri;    // TODO can individuals have more than one type??? (DW: Yes they can, but do not in our ontology)
        }
        return iri;   
    }
    
    /**
     * Returns all individuals of a given class.
     * @param className
     * @param direct whether only individuals of the given class (true) or also of subclasses (false) are returned.
     * @return
     */
    public Set<OWLNamedIndividual> getIndividuals(String className, boolean direct) {
        prepareReasoning();
        NodeSet<OWLNamedIndividual> individualsNodeSet = reasoner.getInstances(getOWLClass(className), direct);
        Set<OWLNamedIndividual> individuals = individualsNodeSet.getFlattened();
        return individuals;
    }

    // TODO: Mit Vorsicht verwenden. Unklar, ob Ontologie auf korrekte Art abgefragt wird.
    // TODO: Wahrscheinlich wäre es besser, auch die Klasse des gesuchten Individuums mit einzubeziehen
//FIXME: Auch hier sind die impliziten ClassAssertions nicht drin
    public OWLNamedIndividual getIndividualById(final String id) {
        Set<OWLClassAssertionAxiom> axioms = this.currentOntology.getAxioms(AxiomType.CLASS_ASSERTION);
        for (OWLClassAssertionAxiom curAxiom : axioms) {
            if (curAxiom.getIndividual().asOWLNamedIndividual().getIRI().getFragment().equals(id))
             {
                return curAxiom.getIndividual().asOWLNamedIndividual(); //TODO: ???? vermutlich v�lliger Quatsch, aber wie geht es richtig?
            }
        }
        return null;
    }
    
    /**
     * This method searches for all individuals of the given class, and returns the individual
     * with the given name.
     * @param id - The name of the wanted individual
     * @param className - The class of the wanted individual
     * @param direct - If you want only direct instances of the class, set this to true
     * @return - The wanted individual, or null
     */
    public OWLNamedIndividual getIndividualByIdAndClass(final String id, final String className, final boolean direct) {
        prepareReasoning();
        System.out.println(id + ", " + className);
        NodeSet<OWLNamedIndividual> individualsNodeSet = reasoner.getInstances(getOWLClass(className), direct);
        Set<OWLNamedIndividual> individuals = individualsNodeSet.getFlattened();
        for (OWLNamedIndividual ni : individuals) {
            if (ni.getIRI().getFragment().equals(id)) {
                return ni;
            }
        }
        return null;
    }
    
    /**
     * This method searches for a individual with the given id which belongs to the given super class.
     * @param id - the id of the individual
     * @param className - the name of the super class
     * @param direct - If you want only direct instances of the class, set this to true
     * @return The wanted individual, or null
     */
    public OWLNamedIndividual getIndividualByIdAndSuperClass(final String id, final String className, final boolean direct) {
        Set<OWLClassExpression> subClasses = getOWLClass(className).getSubClasses(getOntology());
        for (OWLClassExpression ce : subClasses) {
            OWLNamedIndividual individual = getIndividualByIdAndClass(id, ce.asOWLClass().getIRI().getFragment(), direct);
            if (individual != null) {
                return individual;
            }
        }
        return null;
    }
    /**
     * Returns an individual of the given class which has the given property name and value.
     * @param className
     * @param propertyName
     * @param propertyValue
     * @return - the matching individual
     */
    public OWLNamedIndividual getIndividualByPropertyValueAndClass(final String className, final String propertyName, final String propertyValue) {
        prepareReasoning();
        NodeSet<OWLNamedIndividual> individualsNodeSet = reasoner.getInstances(getOWLClass(className), true);
        Set<OWLNamedIndividual> individuals = individualsNodeSet.getFlattened();
        for (OWLNamedIndividual ni : individuals) {
            OWLLiteral propertyValueLiteral = null;
            try {
                propertyValueLiteral = getAnnotation(ni, propertyName);
            } catch (NoSuchPropertyException e) {
// FIXME: (DW) Not good code; dirty fix
                continue;
            }
            if (propertyValueLiteral != null) {
                if (propertyValueLiteral.toString().equals(propertyValue)) {
                    return ni;
                }
            }
        }
        return null;        
    }
    /**
     * Returns all individuals connected by the given relationship.
     * @param individual OWLNamendIndividual to look connections for
     * @param name type of the OWLNamedIndividuals to look for (e.g. Activity, Property...) TODO wirklich? ist das nicht der Name der OWL-Property?
     * @return returns all the matching OWLNamedIndividuals or null if there are no relations
     */
    //TODO: Exception wird gefangen, dann "null" zurückgegeben public final Set<OWLNamedIndividual> getRelatedIndividuals(final OWLNamedIndividual individual, final String name) {
    public final Set<OWLNamedIndividual> getRelatedIndividuals(final OWLNamedIndividual individual, final String name) {
        prepareReasoning();
        Set<OWLNamedIndividual> individuals = null;
        OWLObjectProperty prop = getOWLObjectProperty(name);
        try { 
            NodeSet<OWLNamedIndividual> individualsNodeSet = reasoner.getObjectPropertyValues(individual, prop);
            individuals = individualsNodeSet.getFlattened();
        } catch (NullPointerException n) {
            return null;
        }
        return individuals;
    }

    /**
     * Returns all individuals that refer to the given individual by the given relationship.
     * @param individual
     * @param name
     * @return set of reverse related individuals or null if none exist
     */
    public final Set<OWLNamedIndividual> getInverseRelatedIndividuals(final OWLNamedIndividual individual, final String name) {
        prepareReasoning();
        Set<OWLNamedIndividual> individuals = null;
        OWLObjectProperty op = getOWLObjectProperty(name);
        Node<OWLObjectPropertyExpression> inverseProp = reasoner.getInverseObjectProperties(op);
        if (inverseProp != null) {
            NodeSet<OWLNamedIndividual> individualsNodeSet = reasoner.getObjectPropertyValues(individual, inverseProp.getRepresentativeElement());
            individuals = individualsNodeSet.getFlattened();
        }
        return individuals;
    }

// OWL Annotation Methods   
    /**
     * Removes the annotation specified by name from the given entity.
     * @param entity
     * @param annotationPropertyName
     */
    public void removeAnnotation(OWLEntity entity, String annotationPropertyName) {
        OWLAnnotationProperty annotationProperty = getOWLAnnotationProperty(annotationPropertyName);
        for (OWLAnnotationAssertionAxiom oaaa : entity.getAnnotationAssertionAxioms(currentOntology)) {
            if (oaaa.getProperty().equals(annotationProperty)) {
                this.manager.removeAxiom(currentOntology, oaaa);
                break;
            }
        }
    }
//FIXME: Can't remove an annotation from an axiom?    
    public void removeAxiomAnnotation(final OWLAxiom annotatedAxiom, final String annotationPropertyName) {
//        OWLAnnotationProperty annotationProperty = getOWLAnnotationProperty(annotationPropertyName);
//        for (OWLAnnotation oa : annotatedAxiom.getAnnotations(annotationProperty)) {
//            if (oa.getProperty().equals(annotationProperty)) {
//                this.manager.removeAxiom(currentOntology, annotatedAxiom.);
//                break;
//            }
//        }       
    }
    
	/**
	 * Returns the {@link OWLAnnotationProperty} with the given name.
	 * Does not add the annotation property to the ontology.
	 * "Annotation properties can be used to provide an annotation for an ontology, axiom, or an IRI." 
	 * @param name
	 * @return
	 */
	public OWLAnnotationProperty getOWLAnnotationProperty(String name) {
		return dataFactory.getOWLAnnotationProperty(name, prefixManagerAnnotations);
	}
	
	/**
	 * Returns the {@link OWLObjectProperty} with the given name.
     * Does not add the annotation property to the ontology.
	 * "Object properties connect pairs of individuals." 
	 * @param name
	 * @return
	 */
	public OWLObjectProperty getOWLObjectProperty(String name) {
        return dataFactory.getOWLObjectProperty(name, prefixManagerAnnotations);
	}
	
    /**
     * Creates an annotation with a value of the supported types (see createOWLLiteral).
     * An existing annotation with the same name will be overwritten.
     * @param ind
     * @param name
     * @param value
     */
	public void createAnnotation(final OWLEntity entityToAnnotate, final String annotationPropertyName, final Object annotationPropertyValue) {
        removeAnnotation(entityToAnnotate, annotationPropertyName);
        OWLLiteral lit = createLiteral(annotationPropertyValue);
        OWLAnnotation anno = dataFactory.getOWLAnnotation(getOWLAnnotationProperty(annotationPropertyName), lit);
        this.manager.addAxiom(currentOntology, dataFactory.getOWLAnnotationAssertionAxiom(entityToAnnotate.getIRI(), anno));
	}
	
	/**
	 * This method checks if an individual has a annotation with a given name attached to it (asserted).
	 * @param individual
	 * @param annotationPropertyName
	 * @return
	 */
	public boolean hasAnnotation(final OWLNamedIndividual individual, final String annotationPropertyName) {
        OWLAnnotationProperty np = getOWLAnnotationProperty(annotationPropertyName);
        for (OWLAnnotationAssertionAxiom oaaa : individual.getAnnotationAssertionAxioms(currentOntology)) {
            if (oaaa.getProperty().equals(np)) {
                return true;
            }
        }
        return false;      	    
	}
	
	/**
	 * Returns the literal of the annotation with the given property name.
	 * @param entity
	 * @param annotationPropertyName
	 * @return
	 */
	public OWLLiteral getAnnotation(final OWLEntity entity, final String annotationPropertyName) throws NoSuchPropertyException {
        OWLAnnotationProperty np = getOWLAnnotationProperty(annotationPropertyName);
//        TODO Klaus R.: keine Fehlerbehandlung, entity kann null sein, annotationPropertyName auch
        for (OWLAnnotationAssertionAxiom oaaa : entity.getAnnotationAssertionAxioms(currentOntology)) {
            if (oaaa.getProperty().equals(np)) {
                OWLLiteral lit = (OWLLiteral) oaaa.getValue();
                return lit;
            }
        }
        throw new NoSuchPropertyException(annotationPropertyName, entity);	    
	}
	
    /**
     * Returns a annotation property value of type String.
     * @param entity
     * @param name
     * @return
     * @throws NoSuchPropertyException
     */
    public final String getStringAnnotation(final OWLEntity entity, final String name) throws NoSuchPropertyException {
        return getAnnotation(entity, name).getLiteral();
    }
    
    /**
     * Returns a annotation property value of type boolean.
     * @param entity
     * @param name
     * @return
     * @throws NoSuchPropertyException
     */
    public boolean getBooleanAnnotation(OWLEntity entity, String name) throws NoSuchPropertyException {
        return getAnnotation(entity, name).parseBoolean();
    }

    /**
     * Returns a annotation property value of type float.
     * @param entity
     * @param name
     * @return
     * @throws NoSuchPropertyException
     */
    public float getFloatAnnotation(OWLEntity entity, String name) throws NoSuchPropertyException {
        return getAnnotation(entity, name).parseFloat();
    }

    /**
     * Returns a annotation property value of type int.
     * @param entity
     * @param name
     * @return
     * @throws NoSuchPropertyException
     */
    public int getIntegerAnnotation(OWLEntity entity, String name) throws NoSuchPropertyException {
        return getAnnotation(entity, name).parseInteger();
    }
    
// OWL Axiom Methods    
    /**
     * removes an axiom from the ontology.
     * @param axiom the axiom
     */
    public void removeAxiom(OWLAxiom axiom) {
        if (currentOntology.containsAxiom(axiom)) {
            this.manager.removeAxiom(currentOntology, axiom);
        }
    }
    
// OWL ObjectProperty (Relationship) Methods
    /**
     * Creates a unidirectional relationship of the given type between two individuals.
     * No annotations are added to this relationship.
     * @param src
     * @param tgt
     * @param relationType
     */
    public OWLObjectPropertyAssertionAxiom createSimpleRelationship(OWLNamedIndividual src, OWLNamedIndividual tgt, String relationType) {
        return createSimpleRelationship(src, tgt, relationType, null);
    }
    /**
     * Creates a unidirectional relationship of the given type between two individuals.
     * The given annotations are added to the relationship.
     * @param src
     * @param tgt
     * @param relationType
     * @param annotations
     * @return
     */
    public OWLObjectPropertyAssertionAxiom createSimpleRelationship(final OWLNamedIndividual src, OWLNamedIndividual tgt, String relationType, final Set<Annotation> annotations) {
// TODO: What if a rule contains two mentions of the same RE? Do we need multiple relations from the rule to the RE?
        System.out.println(src.getIRI().getFragment()
                + ", " + tgt.getIRI().getFragment() + " - " + relationType);
        OWLObjectPropertyAssertionAxiom opaa = null;
        if (annotations == null) {
            opaa = dataFactory.getOWLObjectPropertyAssertionAxiom(getOWLObjectProperty(relationType), src, tgt);
        } else {
            opaa = dataFactory.getOWLObjectPropertyAssertionAxiom(getOWLObjectProperty(relationType), src, tgt, createOWLAnnotations(annotations));            
        }
// FIXME: Do we disallow multiple relationships of the same type between the same individuals?
        manager.addAxiom(currentOntology, opaa);
        return opaa;
    }
    
// Supporting methods    
    /**
     * Creates an OWLLiteral of one of the following supported types:
     * String, boolean, float, integer, double.
     * If the given value object is not of these types, none is created.
     * @param value - the value for the new literal
     * @return - the created OWLLiteral, else null
     */
    public OWLLiteral createLiteral(final Object value) {
        if (value instanceof String) {
            return dataFactory.getOWLLiteral((String) value);
        } else if (value instanceof Boolean) {
            return dataFactory.getOWLLiteral((Boolean) value);
        } else if (value instanceof Float) {
            return dataFactory.getOWLLiteral((Float) value);
        } else if (value instanceof Integer) {
            return dataFactory.getOWLLiteral((Integer) value);
        } else if (value instanceof Double) {
            return dataFactory.getOWLLiteral((Double) value); 
        } else {
            return null;
        }

    }
    /**
     * Creates OWLAnnotations from a set of Annotations.
     * @param annotations - the set of annotations to convert
     * @return - the set of OWLAnnotations
     */
	public Set<OWLAnnotation> createOWLAnnotations(final Set<Annotation> annotations) {
	    Set<OWLAnnotation> owlAnnotations = new HashSet<OWLAnnotation>();
	    for (Annotation annotation : annotations) {
	        owlAnnotations.add(createOWLAnnotation(annotation));
	    }
	    return owlAnnotations;
	}
	/**
	 * Convenience method. Creates an OWLAnnotation from an Annotation class instance.
	 * @param annotation
	 * @return
	 */
	public OWLAnnotation createOWLAnnotation(final Annotation annotation) {
	    return dataFactory.getOWLAnnotation(dataFactory.getOWLAnnotationProperty(annotation.getName(), prefixManagerAnnotations), createLiteral(annotation.getValue()));
	}
	
	
	/**
	 *  creates a string representation of the given value expression
     * @param value the value
     * @return the representation
	 * @throws IndexOutOfBoundsException
	 * @throws UnknownAnnotationValueException
	 */
	public String owlAnnotationValueToString(final OWLAnnotationValue value) throws IndexOutOfBoundsException, 
	                                                                        AnnotationFormatError{
	    String returnValue = "";
	    Set<OWLEntity> signature = value.getSignature();
	    for (OWLEntity entity : signature) {
	        if (entity.toString().equals("xsd:string")) {
	             returnValue = value.toString().substring(1, value.toString().indexOf("\"^^xsd:string"));
	        } else if (entity.toString().equals("xsd:integer")){
	            returnValue = value.toString().substring(1, value.toString().indexOf("\"^^xsd:integer"));
	        } else throw new AnnotationFormatError("Unknown OWLAnnotationValue: " + value.toString());
	    }
	    
	    return returnValue;
	}
	
	/**
	 * creates an integer expression from the given value expression.
	 * @param value
	 * @return
	 * @throws IndexOutOfBoundsException
	 * @throws AnnotationFormatError
	 * @throws NumberFormatException
	 */
	public int owlAnnotationValueToInteger(final OWLAnnotationValue value) throws IndexOutOfBoundsException, 
                                                                            AnnotationFormatError, NumberFormatException{
	    return Integer.parseInt(owlAnnotationValueToString(value));
	}
	
	/**
	 * retrieves a set of annotations, described by type, from a given axiom. 
	 * @param axiom
	 * @param type
	 * @return
	 */
	public Set<OWLAnnotation> getAnnotationsFromAxiom(final OWLAxiom axiom, final String type) {
	    return axiom.getAnnotations(getOWLAnnotationProperty(type));
	}

}