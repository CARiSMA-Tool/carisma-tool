/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.modeltype.owl2;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.OWLXMLOntologyFormat;
import org.semanticweb.owlapi.model.AddOntologyAnnotation;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAnnotationSubject;
import org.semanticweb.owlapi.model.OWLAnonymousIndividual;
import org.semanticweb.owlapi.model.OWLAsymmetricObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataAllValuesFrom;
import org.semanticweb.owlapi.model.OWLDataExactCardinality;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataHasValue;
import org.semanticweb.owlapi.model.OWLDataMaxCardinality;
import org.semanticweb.owlapi.model.OWLDataMinCardinality;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;
import org.semanticweb.owlapi.model.OWLDataPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLDataSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom;
import org.semanticweb.owlapi.model.OWLDisjointClassesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointDataPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointUnionAxiom;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentDataPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalDataPropertyAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLInverseFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLInverseObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLIrreflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLNegativeDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLNegativeObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectAllValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectComplementOf;
import org.semanticweb.owlapi.model.OWLObjectExactCardinality;
import org.semanticweb.owlapi.model.OWLObjectHasSelf;
import org.semanticweb.owlapi.model.OWLObjectHasValue;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectMaxCardinality;
import org.semanticweb.owlapi.model.OWLObjectMinCardinality;
import org.semanticweb.owlapi.model.OWLObjectOneOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLObjectPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.model.OWLReflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLSameIndividualAxiom;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.OWLSubDataPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSymmetricObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLTransitiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.PrefixManager;
import org.semanticweb.owlapi.util.DefaultPrefixManager;
import org.semanticweb.owlapi.util.SimpleIRIMapper;

import carisma.core.models.ModelLoader;
import carisma.modeltype.owl2.catalog.CatalogHandler;
import carisma.modeltype.owl2.model.owl.Annotation;
import carisma.modeltype.owl2.model.owl.AnnotationByConstant;
import carisma.modeltype.owl2.model.owl.AnnotationProperty;
import carisma.modeltype.owl2.model.owl.AnonymousIndividual;
import carisma.modeltype.owl2.model.owl.AsymmetricObjectProperty;
import carisma.modeltype.owl2.model.owl.Axiom;
import carisma.modeltype.owl2.model.owl.ClassAssertion;
import carisma.modeltype.owl2.model.owl.ClassAxiom;
import carisma.modeltype.owl2.model.owl.ClassExpression;
import carisma.modeltype.owl2.model.owl.Constant;
import carisma.modeltype.owl2.model.owl.DataAllValuesFrom;
import carisma.modeltype.owl2.model.owl.DataExactCardinality;
import carisma.modeltype.owl2.model.owl.DataHasValue;
import carisma.modeltype.owl2.model.owl.DataMaxCardinality;
import carisma.modeltype.owl2.model.owl.DataMinCardinality;
import carisma.modeltype.owl2.model.owl.DataProperty;
import carisma.modeltype.owl2.model.owl.DataPropertyAssertion;
import carisma.modeltype.owl2.model.owl.DataPropertyAxiom;
import carisma.modeltype.owl2.model.owl.DataPropertyExpression;
import carisma.modeltype.owl2.model.owl.DataSomeValuesFrom;
import carisma.modeltype.owl2.model.owl.Datatype;
import carisma.modeltype.owl2.model.owl.Declaration;
import carisma.modeltype.owl2.model.owl.DifferentIndividuals;
import carisma.modeltype.owl2.model.owl.DisjointClasses;
import carisma.modeltype.owl2.model.owl.DisjointDataProperties;
import carisma.modeltype.owl2.model.owl.DisjointUnion;
import carisma.modeltype.owl2.model.owl.Entity;
import carisma.modeltype.owl2.model.owl.EntityAnnotation;
import carisma.modeltype.owl2.model.owl.EquivalentClasses;
import carisma.modeltype.owl2.model.owl.EquivalentObjectProperties;
import carisma.modeltype.owl2.model.owl.FunctionalObjectProperty;
import carisma.modeltype.owl2.model.owl.Individual;
import carisma.modeltype.owl2.model.owl.InverseFunctionalObjectProperty;
import carisma.modeltype.owl2.model.owl.InverseObjectProperties;
import carisma.modeltype.owl2.model.owl.IrreflexiveObjectProperty;
import carisma.modeltype.owl2.model.owl.NamedIndividual;
import carisma.modeltype.owl2.model.owl.NegativeDataPropertyAssertion;
import carisma.modeltype.owl2.model.owl.NegativeObjectPropertyAssertion;
import carisma.modeltype.owl2.model.owl.ObjectAllValuesFrom;
import carisma.modeltype.owl2.model.owl.ObjectComplementOf;
import carisma.modeltype.owl2.model.owl.ObjectExactCardinality;
import carisma.modeltype.owl2.model.owl.ObjectExistsSelf;
import carisma.modeltype.owl2.model.owl.ObjectHasValue;
import carisma.modeltype.owl2.model.owl.ObjectIntersectionOf;
import carisma.modeltype.owl2.model.owl.ObjectMaxCardinality;
import carisma.modeltype.owl2.model.owl.ObjectMinCardinality;
import carisma.modeltype.owl2.model.owl.ObjectOneOf;
import carisma.modeltype.owl2.model.owl.ObjectProperty;
import carisma.modeltype.owl2.model.owl.ObjectPropertyAssertion;
import carisma.modeltype.owl2.model.owl.ObjectPropertyDomain;
import carisma.modeltype.owl2.model.owl.ObjectPropertyExpression;
import carisma.modeltype.owl2.model.owl.ObjectPropertyRange;
import carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom;
import carisma.modeltype.owl2.model.owl.ObjectUnionOf;
import carisma.modeltype.owl2.model.owl.Ontology;
import carisma.modeltype.owl2.model.owl.OwlFactory;
import carisma.modeltype.owl2.model.owl.ReflexiveObjectProperty;
import carisma.modeltype.owl2.model.owl.SameIndividual;
import carisma.modeltype.owl2.model.owl.SubClassOf;
import carisma.modeltype.owl2.model.owl.SubObjectPropertyOf;
import carisma.modeltype.owl2.model.owl.SymmetricObjectProperty;
import carisma.modeltype.owl2.model.owl.TransitiveObjectProperty;
import carisma.modeltype.owl2.model.owl.URI;
import carisma.modeltype.owl2.util.Tupel;

/**
 * OWL2 ModelLoader.
 * @author Marcel Michel
 * 
 */
public class OWL2ModelLoader implements ModelLoader {

	private OwlFactory factory = OwlFactory.eINSTANCE;
	
	private List<EObject> currentContainer = null;
	
	private HashMap<String,URI> registredClassIRIs = null;
	
	private List<Tupel<EntityAnnotation, URI>> unboundEntityAnnotations = null;
	
	/**
	 * Loads instances of OWL2 models.
	 * @param file the OWL2 model-file
	 * @exception IOException IOException
	 * @return the new model
	 */
	@Override
	public Resource load(File file) throws IOException {
		Resource resource = null;
		OWLOntologyManager manager =  OWLManager.createOWLOntologyManager();
		
		CatalogHandler handler = new CatalogHandler(new File(file.getParent()));
		List<SimpleIRIMapper> ontologyMapper = handler.getImportIRIMappers();
		for (SimpleIRIMapper mapper : ontologyMapper) {
			manager.addIRIMapper(mapper);
		}
		
		try {
			OWLOntology ontology = manager.loadOntologyFromOntologyDocument(file);
			
			//prefixFun(manager, ontology, file);
			
			Ontology eOntology = transformToEMF(ontology);
			
			/* Create EMF Resource */
			File tmpFile = File.createTempFile(".tmp", ".owl", file.getParentFile());
			ResourceSet resourceSet = new ResourceSetImpl();
			org.eclipse.emf.common.util.URI fileURI = org.eclipse.emf.common.util.URI.createFileURI(tmpFile.getAbsolutePath());
			resource = resourceSet.createResource(fileURI);
			resource.getContents().add(eOntology);
			resource.save(Collections.EMPTY_MAP);
			tmpFile.delete();
			
			// Set the URI to the default model path
			// This is important for writeBack 
			resource.setURI(org.eclipse.emf.common.util.URI.createFileURI(file.getAbsolutePath()));
			
			//Test Model Saver
			/*OWL2ModelSaver saver = new OWL2ModelSaver((Ontology) resource.getContents().get(0));
			saver.saveOntology(file, OWL2FileType.OWL_XML);*/
		
		} catch (Exception e) {//OWLOntologyCreationException e) {
			e.printStackTrace();
			//throw new IOException(e.getMessage(), e.getCause());
		}
		return resource;
	}
	
	@SuppressWarnings("unused")
	private void prefixFun(OWLOntologyManager manager, OWLOntology ontology, File file) throws OWLOntologyStorageException, OWLOntologyCreationException {

		OWLDataFactory factory = manager.getOWLDataFactory();
		
		PrefixManager pm = new DefaultPrefixManager("http://carimsa/");
		 
		OWLAnnotation comment1 = factory.getOWLAnnotation(factory.getOWLAnnotationProperty("evolution", pm), 
				factory.getOWLLiteral("some formated values in here", "en"));
		
		OWLAnnotation comment2 = factory.getOWLAnnotation(factory.getOWLAnnotationProperty("ignore", pm),
				factory.getOWLLiteral(false));
		
		OWLAnnotation comment3 = factory.getOWLAnnotation(
				factory.getOWLAnnotationProperty("superontology", pm),
				factory.getOWLLiteral("mysuperfile.owl"));
		
		OWLAxiom axiom = factory.getOWLAnnotationAssertionAxiom(IRI.create("http://www.co-ode.org/ontologies/pizza/pizza.owl#American"), comment1);
		OWLAxiom axiom2 = factory.getOWLAnnotationAssertionAxiom(IRI.create("http://www.co-ode.org/ontologies/pizza/pizza.owl#American"), comment2);
		
		manager.addAxiom(ontology, axiom);
		manager.addAxiom(ontology, axiom2);
		manager.applyChange(new AddOntologyAnnotation(ontology, comment3));
		
		//manager.saveOntology(ontology, IRI.create(new File(file.getAbsolutePath() + ".new")));
		manager.saveOntology(ontology, new OWLXMLOntologyFormat() , IRI.create(new File(file.getAbsolutePath() + ".new")));
		
		//manager.removeOntology(ontology);
	}

	private Ontology transformToEMF(OWLOntology ontology) {
		
		registredClassIRIs = new HashMap<String, URI>();
		unboundEntityAnnotations = new ArrayList<Tupel<EntityAnnotation,URI>>();
		
		Ontology eOntology = factory.createOntology();
		currentContainer = eOntology.getContainer();
		
		URI eId = factory.createURI();
		URI eVersion = factory.createURI();
		IRI id = ontology.getOntologyID().getOntologyIRI();
		IRI version = ontology.getOntologyID().getVersionIRI();
		
		if (id != null) {
			eId.setValue(id.toString());
			eOntology.setOntologyURI(eId);
		}
		if (version != null) {
			eVersion.setValue(version.toString());
			eOntology.setVersionURI(eVersion);
		}

		convertAxioms(eOntology, ontology);
		convertOntologyAnnotations(eOntology, ontology);
		
		//Now all Entities are transformed and can be linked with the EntityAnnotation Object
		//FIXME: Performance Leak!
		List<Entity> entities = OWL2ModelHelper.getAllElementsOfType(eOntology, Entity.class);
		HashMap<String, Entity> hm = new HashMap<String, Entity>();
		for (Entity e : entities) {
			hm.put(e.getEntityURI().getValue().toString(), e);
		}
		for (Tupel<EntityAnnotation, URI> t : unboundEntityAnnotations) {
			t.getO1().setEntity(hm.get(t.getO2().getValue().toString()));
		}
		
		//Transform all imports (recursively)
		for (OWLOntology importedOntology : ontology.getDirectImports()) {
			eOntology.getImportedOntologies().add(transformToEMF(importedOntology));
		}
		
		return eOntology;
	}

	private void convertOntologyAnnotations(Ontology eOntology, OWLOntology ontology) {
		Iterator<OWLAnnotation> iterator = ontology.getAnnotations().iterator();
		
		boolean idFound = false;
		Set<Annotation> annotations = new HashSet<Annotation>();
		while (iterator.hasNext()) {
			OWLAnnotation owlAnnotation = iterator.next();
			if (!idFound && isIdAnnotation(owlAnnotation)) {
				eOntology.setOntologyId(getValueOfIdAnnotation(owlAnnotation));
				idFound = true;
			} else {
				annotations.add(convertAnnotation(owlAnnotation));
			}
		}
		if (!idFound) {
			eOntology.setOntologyId(EcoreUtil.generateUUID());
		}
		eOntology.getOntologyAnnotations().addAll(annotations);
	}

	//TODO: ObjectAndDataPropertyAxiom?
	private void convertAxioms(Ontology eOntology, OWLOntology ontology) {
		
		Set<OWLAxiom> axioms = ontology.getAxioms();
		Iterator<OWLAxiom> iterator = axioms.iterator();
		while (iterator.hasNext()) {
			OWLAxiom axiom = iterator.next();
			
			Axiom eAxiom = null;
			
			if (axiom instanceof OWLClassAxiom) {
				eAxiom = convertClassAxiom(axiom);
				
			} else if (axiom instanceof OWLAnnotationAssertionAxiom) { // Maps EntityAnnotation
				eAxiom = factory.createEntityAnnotation();
				
				OWLAnnotationSubject subject = ((OWLAnnotationAssertionAxiom) axiom).getSubject();
				if (subject instanceof IRI) {
					URI uri = factory.createURI();
					uri.setValue(((IRI) subject).toString());

					//Resolve later on the required Entity
					//ATM it may not be resolvable
					unboundEntityAnnotations.add(new Tupel<EntityAnnotation, URI>((EntityAnnotation) eAxiom, uri));
					
					//FIXME: One vs Many?
					((EntityAnnotation) eAxiom).getEntityAnnotations().add(
							convertAnnotation(((OWLAnnotationAssertionAxiom) axiom).getAnnotation()));
					
				} else if (subject instanceof OWLAnonymousIndividual) {
					//TODO:
					System.err.println("Error: OWLAnonymousIndividual inside OWLAnnotationAssertion not supported.");
					eAxiom = null;
				}
				
			} else if (axiom instanceof OWLDataPropertyAxiom){
			 	eAxiom = convertDataPropertyAxiom(axiom);
			 	
			} else if (axiom instanceof OWLDeclarationAxiom){
				eAxiom = factory.createDeclaration();
				((Declaration) eAxiom).setEntity(convertEntity(((OWLDeclarationAxiom) axiom).getEntity()));
			 	
			} else {
				
				/* ObjectPropertyAxiom */
				if (axiom instanceof OWLAsymmetricObjectPropertyAxiom) {
					eAxiom = factory.createAsymmetricObjectProperty();
					
					((AsymmetricObjectProperty) eAxiom).setObjectPropertyExpression(
							convertObjectPropertyExpression(((OWLAsymmetricObjectPropertyAxiom) axiom).getProperty()));
					
				} else if (axiom instanceof OWLDisjointDataPropertiesAxiom) {
					eAxiom = factory.createDisjointDataProperties();
					
					List<DataPropertyExpression> dataPropertyExpressions = new ArrayList<DataPropertyExpression>();
					for (OWLDataPropertyExpression d : ((OWLDisjointDataPropertiesAxiom) axiom).getProperties()) {
						dataPropertyExpressions.add(convertDataPropertyExpression(d));
					}
					((DisjointDataProperties) eAxiom).getDataPropertyExpressions().addAll(dataPropertyExpressions);
					
				} else if (axiom instanceof OWLEquivalentObjectPropertiesAxiom) {
					eAxiom = factory.createEquivalentObjectProperties();
					
					List<ObjectPropertyExpression> objectPropertyExpressions = new ArrayList<ObjectPropertyExpression>();
					for (OWLObjectPropertyExpression o : ((OWLEquivalentObjectPropertiesAxiom) axiom).getProperties()) {
						objectPropertyExpressions.add(convertObjectPropertyExpression(o));
					}
					((EquivalentObjectProperties) eAxiom).getObjectPropertyExpressions().addAll(objectPropertyExpressions);
					
				} else if (axiom instanceof OWLFunctionalObjectPropertyAxiom) {
					eAxiom = factory.createFunctionalObjectProperty();
					
					((FunctionalObjectProperty) eAxiom).setObjectPropertyExpression(
							convertObjectPropertyExpression(((OWLFunctionalObjectPropertyAxiom) axiom).getProperty()));
					
				} else if (axiom instanceof OWLInverseFunctionalObjectPropertyAxiom) {
					eAxiom = factory.createInverseFunctionalObjectProperty();
					
					((InverseFunctionalObjectProperty) eAxiom).setObjectPropertyExpression(
							convertObjectPropertyExpression(((OWLInverseFunctionalObjectPropertyAxiom) axiom).getProperty()));
					
				} else if (axiom instanceof OWLObjectPropertyDomainAxiom) {
					eAxiom = factory.createObjectPropertyDomain();
					
					((ObjectPropertyDomain) eAxiom).setDomain(
							convertClassExpression(((OWLObjectPropertyDomainAxiom) axiom).getDomain()));
					((ObjectPropertyDomain) eAxiom).setObjectPropertyExpression(
							convertObjectPropertyExpression(((OWLObjectPropertyDomainAxiom) axiom).getProperty()));
					
				} else if (axiom instanceof OWLSymmetricObjectPropertyAxiom) {
					eAxiom = factory.createSymmetricObjectProperty();
					
					((SymmetricObjectProperty) eAxiom).setObjectPropertyExpression(
							convertObjectPropertyExpression(((OWLSymmetricObjectPropertyAxiom) axiom).getProperty()));
					
				} else if (axiom instanceof OWLReflexiveObjectPropertyAxiom) {
					eAxiom = factory.createReflexiveObjectProperty();
					
					((ReflexiveObjectProperty) eAxiom).setObjectPropertyExpression(
							convertObjectPropertyExpression(((OWLReflexiveObjectPropertyAxiom) axiom).getProperty()));
					
				} else if (axiom instanceof OWLSubObjectPropertyOfAxiom) {
					eAxiom = factory.createSubObjectPropertyOf();
					
					((SubObjectPropertyOf) eAxiom).setSuperObjectPropertyExpression(
							convertObjectPropertyExpression(((OWLSubObjectPropertyOfAxiom) axiom).getSuperProperty()));
					((SubObjectPropertyOf) eAxiom).setSubObjectPropertyExpressions(
							convertObjectPropertyExpression(((OWLSubObjectPropertyOfAxiom) axiom).getSubProperty()));
					
				} else if (axiom instanceof OWLObjectPropertyRangeAxiom) {
					eAxiom = factory.createObjectPropertyRange();
					
					((ObjectPropertyRange) eAxiom).setObjectPropertyExpression(
							convertObjectPropertyExpression(((OWLObjectPropertyRangeAxiom) axiom).getProperty()));
					((ObjectPropertyRange) eAxiom).setRange(
							convertClassExpression(((OWLObjectPropertyRangeAxiom) axiom).getRange()));
					
				} else if (axiom instanceof OWLIrreflexiveObjectPropertyAxiom) {
					eAxiom = factory.createIrreflexiveObjectProperty();
					
					((IrreflexiveObjectProperty) eAxiom).setObjectPropertyExpression(
							convertObjectPropertyExpression(((OWLIrreflexiveObjectPropertyAxiom) axiom).getProperty()));
					
				} else if (axiom instanceof OWLTransitiveObjectPropertyAxiom) {
					eAxiom = factory.createTransitiveObjectProperty();
					
					((TransitiveObjectProperty) eAxiom).setObjectPropertyExpression(
							convertObjectPropertyExpression(((OWLTransitiveObjectPropertyAxiom) axiom).getProperty()));
					
				}  else if (axiom instanceof OWLInverseObjectPropertiesAxiom) {
					eAxiom = factory.createInverseObjectProperties();
					
					((InverseObjectProperties) eAxiom).getInverseObjectProperties().add(
							convertObjectPropertyExpression(((OWLInverseObjectPropertiesAxiom) axiom).getFirstProperty()));
					((InverseObjectProperties) eAxiom).getInverseObjectProperties().add(
							convertObjectPropertyExpression(((OWLInverseObjectPropertiesAxiom) axiom).getSecondProperty()));
				}
				
				/* Assertion */
				if (axiom instanceof OWLNegativeDataPropertyAssertionAxiom) {
					eAxiom = factory.createNegativeDataPropertyAssertion();
					
					DataPropertyExpression dataExpr = convertDataPropertyExpression(((OWLNegativeDataPropertyAssertionAxiom) axiom).getProperty());
					Individual indiv = convertIndividual(((OWLNegativeDataPropertyAssertionAxiom) axiom).getSubject());
					Constant value = convertLiteral(((OWLNegativeDataPropertyAssertionAxiom) axiom).getObject());
					((NegativeDataPropertyAssertion) eAxiom).setDataPropertyExpression(dataExpr);
					((NegativeDataPropertyAssertion) eAxiom).setSourceIndividual(indiv);
					((NegativeDataPropertyAssertion) eAxiom).setTargetValue(value);
					
				} else if (axiom instanceof OWLDifferentIndividualsAxiom) {
					eAxiom = factory.createDifferentIndividuals();
					
					for (OWLIndividual i : ((OWLDifferentIndividualsAxiom) axiom).getIndividuals()) {
						((DifferentIndividuals) eAxiom).getDifferentIndividuals().add(
								(NamedIndividual) convertIndividual(i));
					}
					
					
				} else if (axiom instanceof OWLObjectPropertyAssertionAxiom) {
					eAxiom = factory.createObjectPropertyAssertion();
					
					ObjectPropertyExpression objectPropExpr = convertObjectPropertyExpression(((OWLObjectPropertyAssertionAxiom) axiom).getProperty());
					Individual sourceIndiv = convertIndividual(((OWLObjectPropertyAssertionAxiom) axiom).getSubject());
					Individual targetIndiv = convertIndividual(((OWLObjectPropertyAssertionAxiom) axiom).getObject());
					((ObjectPropertyAssertion) eAxiom).setObjectPropertyExpression(objectPropExpr);
					((ObjectPropertyAssertion) eAxiom).setSourceIndividual(sourceIndiv);
					((ObjectPropertyAssertion) eAxiom).setTargetIndividual(targetIndiv);
					
				} else if (axiom instanceof OWLNegativeObjectPropertyAssertionAxiom) {
					eAxiom = factory.createNegativeObjectPropertyAssertion();
					
					ObjectPropertyExpression objExpr = convertObjectPropertyExpression(((OWLNegativeObjectPropertyAssertionAxiom) axiom).getProperty());
					Individual targetIndiv = convertIndividual(((OWLNegativeObjectPropertyAssertionAxiom) axiom).getObject());
					Individual sourceIndiv = convertIndividual(((OWLNegativeObjectPropertyAssertionAxiom) axiom).getSubject());
					((NegativeObjectPropertyAssertion) eAxiom).setObjectPropertyExpression(objExpr);
					((NegativeObjectPropertyAssertion) eAxiom).setTargetIndividual(targetIndiv);
					((NegativeObjectPropertyAssertion) eAxiom).setSourceIndividual(sourceIndiv);
					
				} else if (axiom instanceof OWLSameIndividualAxiom) {
					eAxiom = factory.createSameIndividual();
					
					for (OWLIndividual individual : ((OWLSameIndividualAxiom) axiom).getIndividuals()) {
						((SameIndividual) eAxiom).getSameIndividuals().add((NamedIndividual) convertIndividual(individual));
					}
					
				} else if (axiom instanceof OWLDataPropertyAssertionAxiom) {
					eAxiom = factory.createDataPropertyAssertion();
					
					DataPropertyExpression dataExpr = convertDataPropertyExpression(((OWLDataPropertyAssertionAxiom) axiom).getProperty());
					Individual individual = convertIndividual(((OWLDataPropertyAssertionAxiom) axiom).getSubject());
					Constant constant = convertLiteral(((OWLDataPropertyAssertionAxiom) axiom).getObject());
					((DataPropertyAssertion) eAxiom).setDataPropertyExpression(dataExpr);
					((DataPropertyAssertion) eAxiom).setSourceIndividual(individual);
					((DataPropertyAssertion) eAxiom).setTargetValue(constant);
					
				} else if (axiom instanceof OWLClassAssertionAxiom) {
					eAxiom = factory.createClassAssertion();
					
					//FIXME: NamedIndividual <-> Individual
					NamedIndividual individual = (NamedIndividual) convertIndividual(((OWLClassAssertionAxiom) axiom).getIndividual());
					ClassExpression clsExpr = convertClassExpression(((OWLClassAssertionAxiom) axiom).getClassExpression());
					((ClassAssertion) eAxiom).setIndividual(individual);
					((ClassAssertion) eAxiom).setClassExpression(clsExpr);
				}
				
			}

			if (eAxiom != null) {
				eOntology.getAxioms().add(eAxiom);
				
				boolean idFound = false;
				Set<Annotation> annotations = new HashSet<Annotation>();
				for (OWLAnnotation owlAnnotation : axiom.getAnnotations()) {
					if (!idFound && isIdAnnotation(owlAnnotation)) {
						eAxiom.setAxiomId(getValueOfIdAnnotation(owlAnnotation));
						idFound = true;
					} else {
						annotations.add(convertAnnotation(owlAnnotation));
					}
				}
				if (!idFound) {
					eAxiom.setAxiomId(EcoreUtil.generateUUID());
				}
				eAxiom.getAxiomAnnotations().addAll(annotations);
			} else {
				System.err.println("Error: Axiom " + axiom.toString() + " not known."); 
			}
		}
	}

	private String getValueOfIdAnnotation(OWLAnnotation owlAnnotation) {
		if (owlAnnotation.getValue() instanceof OWLLiteral) {
			OWLLiteral literal = (OWLLiteral) owlAnnotation.getValue();
			if (owlAnnotation.getProperty().getIRI().toString().equalsIgnoreCase(OWL2ModelHelper.getIdIRI())) {
				return literal.getLiteral();
			}
		}
		return "";
	}

	private boolean isIdAnnotation(OWLAnnotation owlAnnotation) {
		if (owlAnnotation.getProperty().getIRI().toString().equalsIgnoreCase(OWL2ModelHelper.getIdIRI())) {
			return true;
		} else {
			return false;
		}
	}

	private Annotation convertAnnotation(OWLAnnotation owlAnnotation) {
		Annotation eAnnotation = null;
		
		//FIXME: Only AnnotationsByConstant elements are currently supported
		AnnotationByConstant annotation = factory.createAnnotationByConstant();
		AnnotationProperty annotationProperty = factory.createAnnotationProperty();
		URI annotationURI = factory.createURI();
		
		currentContainer.add(annotation);
		currentContainer.add(annotationProperty);
		currentContainer.add(annotationURI);
		
		annotationURI.setValue(owlAnnotation.getProperty().getIRI().toString());
		annotationProperty.setEntityURI(annotationURI);
		
		Constant constant = null; 
		if (owlAnnotation.getValue() instanceof OWLLiteral) {
			constant = convertLiteral((OWLLiteral) owlAnnotation.getValue());

		} else if (owlAnnotation.getValue() instanceof IRI) {
			//TODO
			
		} else if (owlAnnotation.getValue() instanceof AnonymousIndividual) {
			//TODO
			
		}
		
		annotation.setAnnotationProperty(annotationProperty);
		annotation.setAnnotationValue(constant);
		
		eAnnotation = annotation;
		
		if (constant == null) {
			System.err.println("Error: Annotation " + owlAnnotation.toString() + " not known.");
		}
		
		return eAnnotation;
	}

	private ObjectPropertyExpression convertObjectPropertyExpression(
			OWLObjectPropertyExpression objectPropertyExpression) {
		URI uri = transformIRI2URI(((OWLObjectProperty) objectPropertyExpression).getIRI());
		
		ObjectProperty eObjectProperty = factory.createObjectProperty();
		eObjectProperty.setEntityURI(uri);
		
		currentContainer.add(eObjectProperty);
		return eObjectProperty;
	}

	private DataPropertyExpression convertDataPropertyExpression(
			OWLDataPropertyExpression dataPropertyExpression) {
		URI uri = transformIRI2URI(((OWLDataProperty) dataPropertyExpression).getIRI());
		
		DataProperty eDataPropExpr = factory.createDataProperty();
		eDataPropExpr.setEntityURI(uri);
		
		currentContainer.add(eDataPropExpr);
		return eDataPropExpr;
	}

	private Constant convertLiteral(OWLLiteral literal) {
		String value = literal.getLiteral();
		Datatype datatype = convertDataType(literal.getDatatype());
		
		Constant eConstant = factory.createConstant();
		eConstant.setDatatype(datatype);
		eConstant.setLexicalValue(value);
		
		currentContainer.add(eConstant);
		return eConstant;
	}

	private Datatype convertDataType(OWLDatatype datatype) {
		URI uri = transformIRI2URI(datatype.getIRI());
		//TODO: Further information
		
		Datatype eDatatype = factory.createDatatype();
		//FIXME: 
		eDatatype.setArity(0);
		eDatatype.setEntityURI(uri);
		
		currentContainer.add(eDatatype);
		return eDatatype;
	}

	private Entity convertEntity(OWLEntity entity) {
		
		Entity eEntity = null;
		if (entity instanceof OWLAnnotationProperty) {
			eEntity = factory.createAnnotationProperty();
			eEntity.setEntityURI(transformIRI2URI(entity.getIRI()));
			
		} else if (entity instanceof OWLDatatype) {
			//TODO: Further info?
			eEntity = factory.createDatatype();
			eEntity.setEntityURI(transformIRI2URI(entity.getIRI()));
			
		} else if (entity instanceof OWLObjectProperty) {
			eEntity = factory.createObjectProperty();
			eEntity.setEntityURI(transformIRI2URI(((OWLObjectProperty) entity).getIRI()));
			
		} else if (entity instanceof OWLClass) {
			eEntity = createClass((OWLClass) entity);
			
		} else if (entity instanceof OWLNamedIndividual) {
			eEntity = createNamedIndividual((OWLNamedIndividual) entity);
			
		} else if (entity instanceof OWLDataProperty) {
			eEntity = factory.createDataProperty();
			eEntity.setEntityURI(transformIRI2URI(((OWLDataProperty) entity).getIRI()));
		}
		
		if (eEntity == null) {
			System.err.println("Error: ClassExpression " + entity.toString() + " not known."); 
		}
		
		currentContainer.add(eEntity);
		return eEntity;
		
	}

	private DataPropertyAxiom convertDataPropertyAxiom(OWLAxiom axiom) {
		//TODO:
		DataPropertyAxiom eDataPropertyAxiom = null;
		if (axiom instanceof OWLDataPropertyDomainAxiom) {
			eDataPropertyAxiom = factory.createDataPropertyDomain();
		
		} else if (axiom instanceof OWLDataPropertyRangeAxiom) {
			eDataPropertyAxiom = factory.createDataPropertyRange();
			
		} else if (axiom instanceof OWLDisjointDataPropertiesAxiom) {
			eDataPropertyAxiom = factory.createDisjointDataProperties();
			
		} else if (axiom instanceof OWLEquivalentDataPropertiesAxiom) {
			eDataPropertyAxiom = factory.createEquivalentDataProperties();
			
		} else if (axiom instanceof OWLFunctionalDataPropertyAxiom) {
			eDataPropertyAxiom = factory.createFunctionalDataProperty();
			
		} else if (axiom instanceof OWLSubDataPropertyOfAxiom) {
			eDataPropertyAxiom = factory.createSubDataPropertyOf();
		}

		currentContainer.add(eDataPropertyAxiom);
		return eDataPropertyAxiom;
	}

	private ClassAxiom convertClassAxiom(OWLAxiom axiom) {
		
		ClassAxiom eClassAxiom = null;
		if (axiom instanceof OWLDisjointClassesAxiom) {
			eClassAxiom = factory.createDisjointClasses();
			
			for (OWLClassExpression expr : ((OWLDisjointClassesAxiom) axiom).getClassExpressions()) {
				ClassExpression eClassExpression = convertClassExpression(expr);
				((DisjointClasses) eClassAxiom).getDisjointClassExpressions().add(eClassExpression);
			}
			
		} else if (axiom instanceof OWLDisjointUnionAxiom) {
			eClassAxiom = factory.createDisjointUnion();
			
			for (OWLClassExpression expr : ((OWLDisjointUnionAxiom) axiom).getClassExpressions()) {
				ClassExpression eClassExpression = convertClassExpression(expr);
				((DisjointUnion) eClassAxiom).getDisjointClassExpressions().add(eClassExpression);
			}
			((DisjointUnion) eClassAxiom).setUnionClass(createClass(((OWLDisjointUnionAxiom) axiom).getOWLClass()));
			
		} else if (axiom instanceof OWLEquivalentClassesAxiom) {
			eClassAxiom = factory.createEquivalentClasses();
			
			for (OWLClassExpression expr :((OWLEquivalentClassesAxiom) axiom).getClassExpressions()) {
				ClassExpression eClassExpression = convertClassExpression(expr); 
				((EquivalentClasses) eClassAxiom).getEquivalentClassExpressions().add(eClassExpression);
			}
			
		} else if (axiom instanceof OWLSubClassOfAxiom) {
			eClassAxiom = factory.createSubClassOf();
			
			((SubClassOf) eClassAxiom).setSubClassExpression(
					convertClassExpression(((OWLSubClassOfAxiom) axiom).getSubClass()));
			((SubClassOf) eClassAxiom).setSuperClassExpression(
					convertClassExpression(((OWLSubClassOfAxiom) axiom).getSuperClass()));
			
		} 
		
		currentContainer.add(eClassAxiom);
		return eClassAxiom;
	}
	
	private carisma.modeltype.owl2.model.owl.Class createClass(OWLClass owlClass) {
		carisma.modeltype.owl2.model.owl.Class eClass =  factory.createClass();
		eClass.setEntityURI(transformIRI2URI(owlClass.getIRI()));
		currentContainer.add(eClass);
		return eClass;
	}
	
	private ClassExpression convertClassExpression(OWLClassExpression classExpression) {
		ClassExpression eClassExpression =  null;
		
		if (classExpression instanceof OWLClass) {
			eClassExpression =  createClass((OWLClass) classExpression);
			
		} else if (classExpression instanceof OWLObjectIntersectionOf) {
			eClassExpression =  factory.createObjectIntersectionOf();

			for (OWLClassExpression expr : ((OWLObjectIntersectionOf) classExpression).getOperands()) {
					((ObjectIntersectionOf) eClassExpression).getClassExpressions().add(
							convertClassExpression(expr));
			}
			
		} else if (classExpression instanceof OWLObjectUnionOf) {
			eClassExpression =  factory.createObjectUnionOf();
			
			for (OWLClassExpression expr : ((OWLObjectUnionOf) classExpression).getOperands()) {
				((ObjectUnionOf) eClassExpression).getClassExpressions().add(
						convertClassExpression(expr));
			}
			
		} else if (classExpression instanceof OWLObjectOneOf) {
			eClassExpression =  factory.createObjectOneOf();
			
			for (OWLIndividual individual : ((OWLObjectOneOf) classExpression).getIndividuals()) {
				((ObjectOneOf) eClassExpression).getIndividuals().add(convertIndividual(individual));
			}
			
		} else if (classExpression instanceof OWLObjectSomeValuesFrom) {
			eClassExpression =  factory.createObjectSomeValuesFrom();
			
			((ObjectSomeValuesFrom) eClassExpression).setClassExpression(
					convertClassExpression(((OWLObjectSomeValuesFrom) classExpression).getFiller()));

			((ObjectSomeValuesFrom) eClassExpression).setObjectPropertyExpression(
					convertObjectPropertyExpression(
							((OWLObjectSomeValuesFrom) classExpression).getProperty()));

		} else if (classExpression instanceof OWLObjectAllValuesFrom) {
			eClassExpression =  factory.createObjectAllValuesFrom();
			
			((ObjectAllValuesFrom) eClassExpression).setClassExpression(
					convertClassExpression(((OWLObjectAllValuesFrom) classExpression).getFiller()));
			
			((ObjectAllValuesFrom) eClassExpression).setObjectPropertyExpression(
					convertObjectPropertyExpression(
							((OWLObjectAllValuesFrom) classExpression).getProperty()));
			
		} else if (classExpression instanceof OWLObjectHasSelf) { //ObjectExistsSelf
			eClassExpression =  factory.createObjectExistsSelf();
			
			((ObjectExistsSelf) eClassExpression).setObjectPropertyExpression(
					convertObjectPropertyExpression(
							((OWLObjectHasSelf) classExpression).getProperty()));
			
		} else if (classExpression instanceof OWLObjectHasValue) {
			eClassExpression =  factory.createObjectHasValue();
			
			((ObjectHasValue) eClassExpression).setIndividual(
					convertIndividual(((OWLObjectHasValue) classExpression).getValue()));
			
			((ObjectHasValue) eClassExpression).setObjectPropertyExpression(
					convertObjectPropertyExpression(
							((OWLObjectHasValue) classExpression).getProperty()));
			
		} else if (classExpression instanceof OWLObjectMinCardinality) {
			eClassExpression =  factory.createObjectMinCardinality();
			
			((ObjectMinCardinality) eClassExpression).setCardinality(
					((OWLObjectMinCardinality) classExpression).getCardinality());
			
			((ObjectMinCardinality) eClassExpression).setClassExpression(
					convertClassExpression(((OWLObjectMinCardinality) classExpression).getFiller()));

			((ObjectMinCardinality) eClassExpression).setObjectPropertyExpression(
					convertObjectPropertyExpression(
							((OWLObjectMinCardinality) classExpression).getProperty()));
			
		} else if (classExpression instanceof OWLObjectMaxCardinality) {
			eClassExpression =  factory.createObjectMaxCardinality();
			
			((ObjectMaxCardinality) eClassExpression).setCardinality(
					((OWLObjectMaxCardinality) classExpression).getCardinality());
			
			((ObjectMaxCardinality) eClassExpression).setClassExpression(
					convertClassExpression(((OWLObjectMaxCardinality) classExpression).getFiller()));

			((ObjectMaxCardinality) eClassExpression).setObjectPropertyExpression(
					convertObjectPropertyExpression(
							((OWLObjectMaxCardinality) classExpression).getProperty()));
			
		} else if (classExpression instanceof OWLDataSomeValuesFrom) {
			eClassExpression =  factory.createDataSomeValuesFrom();
			
			((DataSomeValuesFrom) eClassExpression).setDataPropertyExpressions(
					convertDataPropertyExpression(
							((OWLDataSomeValuesFrom) classExpression).getProperty()));
			//TODO: DataRange
			//((DataSomeValuesFrom) eClassExpression).getDataRange();
			
		} else if (classExpression instanceof OWLDataAllValuesFrom) {
			eClassExpression =  factory.createDataAllValuesFrom();
			
			((DataAllValuesFrom) eClassExpression).setDataPropertyExpressions(
					convertDataPropertyExpression(
							((OWLDataAllValuesFrom) classExpression).getProperty()));
			//TODO: DataRange
			//((DataAllValuesFrom) eClassExpression).getDataRange();
			
		} else if (classExpression instanceof OWLDataHasValue) {
			eClassExpression =  factory.createDataHasValue();
			
			((DataHasValue) eClassExpression).setDataPropertyExpression(
					convertDataPropertyExpression(
							((OWLDataHasValue) classExpression).getProperty()));

			((DataHasValue) eClassExpression).setConstant(
					convertLiteral(
							((OWLDataHasValue) classExpression).getValue()));
			
		} else if (classExpression instanceof OWLDataMinCardinality) {
			eClassExpression =  factory.createDataMinCardinality();
			
			((DataMinCardinality) eClassExpression).setCardinality(
					((OWLDataMinCardinality) classExpression).getCardinality());
			
			((DataMinCardinality) eClassExpression).setDataPropertyExpression(
					convertDataPropertyExpression(
							((OWLDataMinCardinality) classExpression).getProperty()));

			//TODO:
			//((DataMinCardinality) eClassExpression).getDataRange();
			
		} else if (classExpression instanceof OWLDataMaxCardinality) {
			eClassExpression =  factory.createDataMaxCardinality();
			
			((DataMaxCardinality) eClassExpression).setCardinality(
					((OWLDataMaxCardinality) classExpression).getCardinality());
			
			((DataMaxCardinality) eClassExpression).setDataPropertyExpression(
					convertDataPropertyExpression(
							((OWLDataMaxCardinality) classExpression).getProperty()));
			
			//TODO: 
			//((DataMaxCardinality) eClassExpression).getDataRange();
			
		} else if (classExpression instanceof OWLDataExactCardinality) {
			eClassExpression =  factory.createDataExactCardinality();
			
			((DataExactCardinality) eClassExpression).setCardinality(
					((OWLDataExactCardinality) classExpression).getCardinality());

			((DataExactCardinality) eClassExpression).setDataPropertyExpression(
					convertDataPropertyExpression(
							((OWLDataExactCardinality) classExpression).getProperty()));
			
			//TODO:
			//((DataExactCardinality) eClassExpression).getDataRange();
			
		} else if (classExpression instanceof OWLObjectComplementOf) {
			eClassExpression =  factory.createObjectComplementOf();
			
			((ObjectComplementOf) eClassExpression).setClassExpression(
					convertClassExpression(((OWLObjectComplementOf) classExpression).getOperand()));
			
		} else if (classExpression instanceof OWLObjectExactCardinality) {
			eClassExpression =  factory.createObjectExactCardinality();
			
			//TODO: Is it correct?
			((ObjectExactCardinality) eClassExpression).setClassExpression(
					convertClassExpression(((OWLObjectExactCardinality) classExpression).asIntersectionOfMinMax()));
			
			((ObjectExactCardinality) eClassExpression).setCardinality(
					((OWLObjectExactCardinality) classExpression).getCardinality());
			
			((ObjectExactCardinality) eClassExpression).setObjectPropertyExpression(
					convertObjectPropertyExpression(
							((OWLObjectExactCardinality) classExpression).getProperty()));
		}

		if (eClassExpression == null) {
			System.err.println("Error: ClassExpression " + classExpression.toString() + " not known."); 
		}
		
		currentContainer.add(eClassExpression);
		return eClassExpression;
	}

	private NamedIndividual createNamedIndividual(OWLNamedIndividual namedIndividual) {
		NamedIndividual eNamedIndividual = factory.createNamedIndividual();
		eNamedIndividual.setEntityURI(transformIRI2URI(namedIndividual.getIRI()));
		return eNamedIndividual;
		
	}
	
	private Individual convertIndividual(OWLIndividual individual) {
		
		Individual eIndividual = null;
		
		if (individual instanceof OWLNamedIndividual) {
			eIndividual = createNamedIndividual(((OWLNamedIndividual) individual));
			
		} else if (individual instanceof OWLAnonymousIndividual) {
			eIndividual = factory.createAnonymousIndividual();
			((AnonymousIndividual) eIndividual).setNodeID(
					((OWLAnonymousIndividual) individual).getID().getID());
		}
		
		if (eIndividual == null) {
			System.err.println("Error: ClassExpression " + individual.toString() + " not known."); 
		}
		
		currentContainer.add(eIndividual);
		return eIndividual;
	}

	private URI transformIRI2URI(IRI iri) {
		URI eURI = null;
		
		if (iri != null && registredClassIRIs.containsKey(iri.toString())) {
			eURI = registredClassIRIs.get(iri.toString());
		} else {
			eURI = factory.createURI();
			eURI.setValue(iri == null ? "null" : iri.toString());
			registredClassIRIs.put(iri == null ? "null" : iri.toString(), eURI);
		}
		
		currentContainer.add(eURI);
		return eURI;
	}

}
