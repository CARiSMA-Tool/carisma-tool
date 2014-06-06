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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.AddOntologyAnnotation;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyID;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.model.SetOntologyID;
import org.semanticweb.owlapi.util.OWLOntologyMerger;

import carisma.modeltype.owl2.model.owl.Annotation;
import carisma.modeltype.owl2.model.owl.AnnotationByAnonymousIndividual;
import carisma.modeltype.owl2.model.owl.AnnotationByConstant;
import carisma.modeltype.owl2.model.owl.AnnotationByEntity;
import carisma.modeltype.owl2.model.owl.AnnotationProperty;
import carisma.modeltype.owl2.model.owl.AnonymousIndividual;
import carisma.modeltype.owl2.model.owl.AsymmetricObjectProperty;
import carisma.modeltype.owl2.model.owl.Axiom;
import carisma.modeltype.owl2.model.owl.Class;
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
import carisma.modeltype.owl2.model.owl.ReflexiveObjectProperty;
import carisma.modeltype.owl2.model.owl.SameIndividual;
import carisma.modeltype.owl2.model.owl.SubClassOf;
import carisma.modeltype.owl2.model.owl.SubObjectPropertyOf;
import carisma.modeltype.owl2.model.owl.SymmetricObjectProperty;
import carisma.modeltype.owl2.model.owl.TransitiveObjectProperty;
import carisma.modeltype.owl2.model.owl.URI;
import carisma.modeltype.owl2.type.OWL2FileType;

/**
 * OWL2 ModelSaver.
 * @author Marcel Michel
 * 
 */
public class OWL2ModelSaver {

	private HashMap<String,IRI> registredClassURIs = null;
	
	private OWLOntologyManager manager = null;
	private OWLDataFactory factory = null;
	private OWLOntology currentOntology = null;
	private Ontology eOntology = null;
	
	public OWL2ModelSaver(Ontology eOntology) {
		this.registredClassURIs = new HashMap<String, IRI>();
		this.manager = OWLManager.createOWLOntologyManager();
		this.factory = manager.getOWLDataFactory();
		this.eOntology = eOntology; 
		
		try {
			for (Ontology eImportedOntology : OWL2ModelHelper.getAllImportedOntologies(eOntology)) {
				transformToOWL(eImportedOntology);
			}
			transformToOWL(eOntology);
			
		} catch (OWLOntologyCreationException owlOntologyCreationException) {
			owlOntologyCreationException.printStackTrace();
		}
	}
	
	public boolean saveOntology(File owl2ModelFile, OWL2FileType owl2FileType) {
		try {
			List<Ontology> eImportedOntologies = eOntology.getImportedOntologies();

			List<OWLOntologyChange> importChanges= new ArrayList<OWLOntologyChange>();
			for (Ontology o : eImportedOntologies) {
				importChanges.add(new AddImport(currentOntology, 
						factory.getOWLImportsDeclaration(
								transformURI2IRI(o.getOntologyURI()))));
			}
			manager.applyChanges(importChanges);

			manager.saveOntology(currentOntology, owl2FileType.getOWLOntologyFormat(), IRI.create(owl2ModelFile));
			
			return true;
			
		} catch (OWLOntologyStorageException e) {
			e.printStackTrace();
			return false;
		}
	}
	
	public boolean exportOntology(File owl2ModelFile, OWL2FileType owl2FileType) {
		try {
		
			OWLOntologyMerger merger = new OWLOntologyMerger(manager);
			OWLOntology mergedOntology = merger.createMergedOntology(manager, 
					IRI.create(currentOntology.getOntologyID().getOntologyIRI().toString() + "/merged"));
			
			manager.saveOntology(mergedOntology, owl2FileType.getOWLOntologyFormat(), IRI.create(owl2ModelFile));
					
			return true;
			
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
			return false;
		} catch (OWLOntologyStorageException e) {
			e.printStackTrace();
			return false;
		}
	}
	
	private OWLEntity convertEntity(Entity eEntity) {
		
		OWLEntity owlEntity = null;
		if (eEntity instanceof AnnotationProperty) {
			owlEntity = factory.getOWLAnnotationProperty(transformURI2IRI(eEntity.getEntityURI()));
			
		} else if (eEntity instanceof Datatype) {
			owlEntity = factory.getOWLDatatype(transformURI2IRI(eEntity.getEntityURI()));
			
		} else if (eEntity instanceof ObjectProperty) {
			owlEntity = factory.getOWLObjectProperty(transformURI2IRI(eEntity.getEntityURI()));
			
		} else if (eEntity instanceof carisma.modeltype.owl2.model.owl.Class) {
			owlEntity = factory.getOWLClass(transformURI2IRI(eEntity.getEntityURI()));
			
		} else if (eEntity instanceof NamedIndividual) {
			owlEntity = factory.getOWLNamedIndividual(transformURI2IRI(eEntity.getEntityURI()));
			
		} else if (eEntity instanceof DataProperty) {
			owlEntity = factory.getOWLDataProperty(transformURI2IRI(eEntity.getEntityURI()));
		}
		
		if (owlEntity == null) {
			System.err.println("Error: Entity " + eEntity.toString() + " not converted."); 
		}
		
		return owlEntity;
		
	}
	
	private boolean transformToOWL(Ontology eOntology) throws OWLOntologyCreationException {
		IRI ontologyIRI = transformURI2IRI(eOntology.getOntologyURI());
		currentOntology = manager.createOntology(ontologyIRI);
		
		if (eOntology.getVersionURI() != null) {
			IRI versionIRI = IRI.create(eOntology.getVersionURI().getValue());
			OWLOntologyID newOntologyID = new OWLOntologyID(ontologyIRI, versionIRI);
			SetOntologyID setOntologyID = new SetOntologyID(currentOntology, newOntologyID);
			manager.applyChange(setOntologyID);
		}
		List<AddOntologyAnnotation> addOntologyAnnotations = new ArrayList<AddOntologyAnnotation>();
		for (Annotation a : eOntology.getOntologyAnnotations()) {
			addOntologyAnnotations.add(new AddOntologyAnnotation(currentOntology, convertAnnotation(a)));
		}
		addOntologyAnnotations.add(new AddOntologyAnnotation(currentOntology, convertIdToAnnotation(eOntology.getOntologyId())));
		manager.applyChanges(addOntologyAnnotations);
		
		for (Axiom eAxiom : eOntology.getAxioms()) {
			OWLAxiom owlAxiom = null;
						
			if (eAxiom instanceof ClassAxiom) {
				owlAxiom = convertClassAxiom((ClassAxiom) eAxiom);
				
			} else if (eAxiom instanceof EntityAnnotation) { // Maps OWLAnnotationAssertionAxiom

				OWLAnnotation annotation = null;
				//FIXME: One vs Many
				for (Annotation a : ((EntityAnnotation) eAxiom).getEntityAnnotations()) {
					annotation = convertAnnotation(a);
				}
				
				owlAxiom = factory.getOWLAnnotationAssertionAxiom(
						transformURI2IRI(((EntityAnnotation) eAxiom).getEntity().getEntityURI()), 
						annotation);
				
			} else if (eAxiom instanceof DataProperty){
				//TODO:
				System.err.println("DataProperty is not implemented!");
				
			} else if (eAxiom instanceof Declaration){
				owlAxiom = factory.getOWLDeclarationAxiom(convertEntity(((Declaration) eAxiom).getEntity()));
				
			} else {
				
				/* ObjectPropertyAxiom */
				if (eAxiom instanceof AsymmetricObjectProperty) {
					owlAxiom = factory.getOWLAsymmetricObjectPropertyAxiom(
							convertObjectPropertyExpression(((AsymmetricObjectProperty) eAxiom).getObjectPropertyExpression()));

				} else if (eAxiom instanceof DisjointDataProperties) {
					Set<OWLDataPropertyExpression> dataPropertyExpressions = new HashSet<OWLDataPropertyExpression>();
					for (DataPropertyExpression d :  ((DisjointDataProperties) eAxiom).getDataPropertyExpressions()) {
						dataPropertyExpressions.add(convertDataPropertyExpression(d));
					}
					owlAxiom = factory.getOWLDisjointDataPropertiesAxiom(dataPropertyExpressions);

				} else if (eAxiom instanceof EquivalentObjectProperties) {
					Set<OWLObjectPropertyExpression> objectPropertyExpressions = new HashSet<OWLObjectPropertyExpression>();
					for (ObjectPropertyExpression o : ((EquivalentObjectProperties) eAxiom).getObjectPropertyExpressions()) {
						objectPropertyExpressions.add(convertObjectPropertyExpression(o));
					}
					owlAxiom = factory.getOWLEquivalentObjectPropertiesAxiom(objectPropertyExpressions);
					
				} else if (eAxiom instanceof FunctionalObjectProperty) {
					owlAxiom = factory.getOWLFunctionalObjectPropertyAxiom(
							convertObjectPropertyExpression(((FunctionalObjectProperty) eAxiom).getObjectPropertyExpression()));
					
				} else if (eAxiom instanceof InverseFunctionalObjectProperty) {
					owlAxiom = factory.getOWLInverseFunctionalObjectPropertyAxiom(
							convertObjectPropertyExpression(((InverseFunctionalObjectProperty) eAxiom).getObjectPropertyExpression()));
					
				} else if (eAxiom instanceof ObjectPropertyDomain) {
					owlAxiom = factory.getOWLObjectPropertyDomainAxiom(
							convertObjectPropertyExpression(((ObjectPropertyDomain) eAxiom).getObjectPropertyExpression()), 
							convertClassExpression(((ObjectPropertyDomain) eAxiom).getDomain()));
					
				} else if (eAxiom instanceof SymmetricObjectProperty) {
					owlAxiom = factory.getOWLSymmetricObjectPropertyAxiom(
							convertObjectPropertyExpression(((SymmetricObjectProperty) eAxiom).getObjectPropertyExpression()));
					
				} else if (eAxiom instanceof ReflexiveObjectProperty) {
					owlAxiom = factory.getOWLReflexiveObjectPropertyAxiom(
							convertObjectPropertyExpression(((ReflexiveObjectProperty) eAxiom).getObjectPropertyExpression()));
					
				} else if (eAxiom instanceof SubObjectPropertyOf) {
					owlAxiom = factory.getOWLSubObjectPropertyOfAxiom(
							convertObjectPropertyExpression(((SubObjectPropertyOf) eAxiom).getSubObjectPropertyExpressions()), 
							convertObjectPropertyExpression(((SubObjectPropertyOf) eAxiom).getSuperObjectPropertyExpression()));
					
				} else if (eAxiom instanceof ObjectPropertyRange) {
					owlAxiom = factory.getOWLObjectPropertyRangeAxiom(
							convertObjectPropertyExpression(((ObjectPropertyRange) eAxiom).getObjectPropertyExpression()), 
							convertClassExpression(((ObjectPropertyRange) eAxiom).getRange()));
					
				} else if (eAxiom instanceof IrreflexiveObjectProperty) {
					owlAxiom = factory.getOWLIrreflexiveObjectPropertyAxiom(
							convertObjectPropertyExpression(((IrreflexiveObjectProperty) eAxiom).getObjectPropertyExpression()));
					
				} else if (eAxiom instanceof TransitiveObjectProperty) {
					owlAxiom = factory.getOWLTransitiveObjectPropertyAxiom(
							convertObjectPropertyExpression(((TransitiveObjectProperty) eAxiom).getObjectPropertyExpression()));
					
				}  else if (eAxiom instanceof InverseObjectProperties) {
					owlAxiom = factory.getOWLInverseObjectPropertiesAxiom(
							convertObjectPropertyExpression(((InverseObjectProperties) eAxiom).getInverseObjectProperties().get(0)),
							convertObjectPropertyExpression(((InverseObjectProperties) eAxiom).getInverseObjectProperties().get(1)));
				}
				
				/* Assertion */
				if (eAxiom instanceof NegativeDataPropertyAssertion) {
					OWLDataPropertyExpression dataExpr = convertDataPropertyExpression(((NegativeDataPropertyAssertion) eAxiom).getDataPropertyExpression());
					OWLIndividual indiv = convertIndividual(((NegativeDataPropertyAssertion) eAxiom).getSourceIndividual());
					OWLLiteral literal = convertConstant(((NegativeDataPropertyAssertion) eAxiom).getTargetValue());
					
					owlAxiom = factory.getOWLNegativeDataPropertyAssertionAxiom(dataExpr, indiv, literal);
					
				} else if (eAxiom instanceof DifferentIndividuals) {
					Set<OWLIndividual> individuals = new HashSet<OWLIndividual>();
					for (NamedIndividual i : ((DifferentIndividuals) eAxiom).getDifferentIndividuals()) {
						individuals.add(convertIndividual(i));
					}
					owlAxiom = factory.getOWLDifferentIndividualsAxiom(individuals);
					
				} else if (eAxiom instanceof ObjectPropertyAssertion) {
					OWLObjectPropertyExpression objPropExpr = convertObjectPropertyExpression(((ObjectPropertyAssertion) eAxiom).getObjectPropertyExpression());
					OWLIndividual sourceIndiv = convertIndividual(((ObjectPropertyAssertion) eAxiom).getSourceIndividual());
					OWLIndividual tarIndividual = convertIndividual(((ObjectPropertyAssertion) eAxiom).getTargetIndividual());
					owlAxiom = factory.getOWLObjectPropertyAssertionAxiom(objPropExpr, sourceIndiv, tarIndividual);
					
				} else if (eAxiom instanceof NegativeObjectPropertyAssertion) {
					OWLObjectPropertyExpression objPropExpr = convertObjectPropertyExpression(((NegativeObjectPropertyAssertion) eAxiom).getObjectPropertyExpression());
					OWLIndividual sourceIndiv = convertIndividual(((NegativeObjectPropertyAssertion) eAxiom).getSourceIndividual());
					OWLIndividual targetIndiv = convertIndividual(((NegativeObjectPropertyAssertion) eAxiom).getTargetIndividual());
					owlAxiom = factory.getOWLNegativeObjectPropertyAssertionAxiom(objPropExpr, sourceIndiv, targetIndiv);
					
				} else if (eAxiom instanceof SameIndividual) {
					Set<OWLIndividual> individuals  = new HashSet<OWLIndividual>();
					for (Individual individual : ((SameIndividual) eAxiom).getSameIndividuals()) {
						individuals.add(convertIndividual(individual));
					}
					owlAxiom = factory.getOWLSameIndividualAxiom(individuals);
					
				} else if (eAxiom instanceof DataPropertyAssertion) {
					OWLDataPropertyExpression dataPropExpr = convertDataPropertyExpression(((DataPropertyAssertion) eAxiom).getDataPropertyExpression());
					OWLIndividual individual = convertIndividual(((DataPropertyAssertion) eAxiom).getSourceIndividual());
					OWLLiteral literal = convertConstant(((DataPropertyAssertion) eAxiom).getTargetValue());
					owlAxiom = factory.getOWLDataPropertyAssertionAxiom(dataPropExpr, individual, literal);
					
				} else if (eAxiom instanceof ClassAssertion) {
					OWLClassExpression clsExpr = convertClassExpression(((ClassAssertion) eAxiom).getClassExpression());
					OWLIndividual individual = convertIndividual(((ClassAssertion) eAxiom).getIndividual());
					owlAxiom = factory.getOWLClassAssertionAxiom(clsExpr, individual);
				}
			}

			if (owlAxiom == null) {
				System.err.println("Error: Axiom " + eAxiom.toString() + " not converted.");
			} else {
				Set<OWLAnnotation> owlAnnotations = new HashSet<OWLAnnotation>();
				owlAnnotations.add(convertIdToAnnotation(eAxiom.getAxiomId()));
				for (Annotation a : eAxiom.getAxiomAnnotations()) {
					owlAnnotations.add(convertAnnotation(a));
				}
				manager.addAxiom(currentOntology, owlAxiom.getAnnotatedAxiom(owlAnnotations));
			}
		}
		return true;
	}
		
	private OWLAnnotation convertIdToAnnotation(String axiomId) {
		OWLAnnotationProperty owlAnnotationProperty = factory.getOWLAnnotationProperty(
				IRI.create(OWL2ModelHelper.getIdIRI()));
		OWLLiteral owlLiteral = factory.getOWLLiteral(axiomId);
		return factory.getOWLAnnotation(owlAnnotationProperty, owlLiteral);
	}

	private OWLObjectPropertyExpression convertObjectPropertyExpression(
			ObjectPropertyExpression objectPropertyExpression) {
		IRI iri = transformURI2IRI(((ObjectProperty) objectPropertyExpression).getEntityURI());
		
		return factory.getOWLObjectProperty(iri);
	}

	private OWLLiteral convertConstant(Constant constant) {
		String value = constant.getLexicalValue();
		OWLDatatype datatype = convertDataType(constant.getDatatype());
		
		return factory.getOWLLiteral(value, datatype);
	}

	private OWLDatatype convertDataType(Datatype datatype) {
		IRI iri = transformURI2IRI(datatype.getEntityURI());
		
		return factory.getOWLDatatype(iri);
	}

	private OWLDataPropertyExpression convertDataPropertyExpression(
			DataPropertyExpression dataPropertyExpression) {
		IRI iri = transformURI2IRI(((DataProperty)dataPropertyExpression).getEntityURI());
		
		return factory.getOWLDataProperty(iri);
	}

	private OWLClassAxiom convertClassAxiom(ClassAxiom classAxiom) {
		OWLClassAxiom owlClassAxiom = null;

		if (classAxiom instanceof DisjointClasses) {
			List<ClassExpression> clsExpressions = ((DisjointClasses) classAxiom).getDisjointClassExpressions();
			Set<OWLClassExpression> set = new HashSet<OWLClassExpression>();
			for (ClassExpression expr : clsExpressions) {
				set.add(convertClassExpression(expr));
			}
			owlClassAxiom = factory.getOWLDisjointClassesAxiom(set);
			
		} else if (classAxiom instanceof DisjointUnion) {
			List<ClassExpression> clsExpressions  = ((DisjointUnion) classAxiom).getDisjointClassExpressions();
			Set<OWLClassExpression> set = new HashSet<OWLClassExpression>();
			for (ClassExpression expr : clsExpressions) {
				set.add(convertClassExpression(expr));
			}
			OWLClass owlClass = convertClass(((DisjointUnion) classAxiom).getUnionClass());
			owlClassAxiom = factory.getOWLDisjointUnionAxiom(owlClass, set);
			
		} else if (classAxiom instanceof EquivalentClasses) {
			List<ClassExpression> clsExpressions = ((EquivalentClasses) classAxiom).getEquivalentClassExpressions();
			Set<OWLClassExpression> set = new HashSet<OWLClassExpression>();
			for (ClassExpression expr : clsExpressions) {
				set.add(convertClassExpression(expr));
			}
			owlClassAxiom = factory.getOWLEquivalentClassesAxiom(set);
			
		} else if (classAxiom instanceof SubClassOf) {
			ClassExpression subClassExpressions = ((SubClassOf) classAxiom).getSubClassExpression();
			ClassExpression superClassExpressions = ((SubClassOf) classAxiom).getSuperClassExpression();
			
			owlClassAxiom = factory.getOWLSubClassOfAxiom(convertClassExpression(subClassExpressions),
					convertClassExpression(superClassExpressions));
		} 
		
		if (owlClassAxiom == null) {
			System.err.println("Error: Class " + classAxiom.toString() + " not converted."); 
		}
		
		return owlClassAxiom;
	}
	
	private OWLAnnotation convertAnnotation(Annotation annotation) {
		OWLAnnotation owlAnnotation = null;
		
		OWLAnnotationProperty owlAnnotationProperty = factory.getOWLAnnotationProperty(
				transformURI2IRI(annotation.getAnnotationProperty().getEntityURI())); 
		
		if (annotation instanceof AnnotationByConstant) {
			OWLLiteral owlLiteral = convertConstant(((AnnotationByConstant) annotation).getAnnotationValue());
			owlAnnotation = factory.getOWLAnnotation(owlAnnotationProperty, owlLiteral);
			
		} else if (annotation instanceof AnnotationByAnonymousIndividual) {
			//TODO:
			System.err
				.println("Error: AnnotationByAnonymousIndividual is not supported");
			OWLLiteral owlLiteral = factory.getOWLLiteral("Error: AnnotationByAnonymousIndividual is not supported");
			owlAnnotation = factory.getOWLAnnotation(owlAnnotationProperty, owlLiteral);
			
		} else if (annotation instanceof AnnotationByEntity) {
			//TODO:
			System.err
				.println("Error: AnnotationByEntity is not supported");
			OWLLiteral owlLiteral = factory.getOWLLiteral("Error: AnnotationByEntity is not supported");
			owlAnnotation = factory.getOWLAnnotation(owlAnnotationProperty, owlLiteral);
			
		}
		
		if (owlAnnotation == null) {
			System.err.println("Error: Annotation " + annotation.toString() + " not converted.");
		}
		
		return owlAnnotation;
	}

	private OWLClass convertClass(carisma.modeltype.owl2.model.owl.Class owlClass) {
		return factory.getOWLClass(
				transformURI2IRI(((carisma.modeltype.owl2.model.owl.Class) owlClass).getEntityURI()));
	}
	
	private OWLClassExpression convertClassExpression(ClassExpression classExpression) {
		OWLClassExpression owlClassExpression =  null;
		
		if (classExpression instanceof carisma.modeltype.owl2.model.owl.Class) {
			owlClassExpression = convertClass((Class) classExpression);
			
		} else if (classExpression instanceof ObjectIntersectionOf) {
			Set<OWLClassExpression> set = new HashSet<OWLClassExpression>();
			for (ClassExpression expr : ((ObjectIntersectionOf) classExpression).getClassExpressions()) {
				set.add(convertClassExpression(expr));
			}
			owlClassExpression = factory.getOWLObjectIntersectionOf(set);
			
		} else if (classExpression instanceof ObjectUnionOf) {
			Set<OWLClassExpression> set = new HashSet<OWLClassExpression>();
			for (ClassExpression expr : ((ObjectUnionOf) classExpression).getClassExpressions()) {
				set.add(convertClassExpression(expr));
			}
			owlClassExpression = factory.getOWLObjectUnionOf(set);
			
		} else if (classExpression instanceof ObjectOneOf) {
			Set<OWLIndividual> set = new HashSet<OWLIndividual>();
			for (Individual indiv : ((ObjectOneOf) classExpression).getIndividuals()) {
				set.add(convertIndividual(indiv));
			}
			owlClassExpression = factory.getOWLObjectOneOf(set);
			
		} else if (classExpression instanceof ObjectSomeValuesFrom) {
			OWLClassExpression clsExpr = convertClassExpression(((ObjectSomeValuesFrom) classExpression).getClassExpression());
			OWLObjectPropertyExpression objExpr = convertObjectPropertyExpression(((ObjectSomeValuesFrom) classExpression).getObjectPropertyExpression());
			owlClassExpression = factory.getOWLObjectSomeValuesFrom(objExpr, clsExpr);
			
		} else if (classExpression instanceof ObjectAllValuesFrom) {
			OWLClassExpression clsExpr = convertClassExpression(((ObjectAllValuesFrom) classExpression).getClassExpression());
			OWLObjectPropertyExpression objExpr = convertObjectPropertyExpression(((ObjectAllValuesFrom) classExpression).getObjectPropertyExpression());
			owlClassExpression = factory.getOWLObjectAllValuesFrom(objExpr, clsExpr);
			
		} else if (classExpression instanceof ObjectExistsSelf) { //OWLObjectHasSelf
			OWLObjectPropertyExpression objExpr = convertObjectPropertyExpression(((ObjectExistsSelf) classExpression).getObjectPropertyExpression());
			owlClassExpression = factory.getOWLObjectHasSelf(objExpr);
			
		} else if (classExpression instanceof ObjectHasValue) {
			OWLIndividual individual = convertIndividual(((ObjectHasValue) classExpression).getIndividual());
			OWLObjectPropertyExpression objExpr = convertObjectPropertyExpression(((ObjectHasValue) classExpression).getObjectPropertyExpression());
			owlClassExpression = factory.getOWLObjectHasValue(objExpr, individual);
			
		} else if (classExpression instanceof ObjectMinCardinality) {
			int cardinality = ((ObjectMinCardinality) classExpression).getCardinality();
			OWLClassExpression clsExpr = convertClassExpression(((ObjectMinCardinality) classExpression).getClassExpression());
			OWLObjectPropertyExpression objExpr = convertObjectPropertyExpression(((ObjectMinCardinality) classExpression).getObjectPropertyExpression());
			owlClassExpression = factory.getOWLObjectMinCardinality(cardinality, objExpr, clsExpr);
			
		} else if (classExpression instanceof ObjectMaxCardinality) {
			int cardinality = ((ObjectMaxCardinality) classExpression).getCardinality();
			OWLClassExpression clsExpr = convertClassExpression(((ObjectMaxCardinality) classExpression).getClassExpression());
			OWLObjectPropertyExpression objExpr = convertObjectPropertyExpression(((ObjectMaxCardinality) classExpression).getObjectPropertyExpression());
			owlClassExpression = factory.getOWLObjectMaxCardinality(cardinality, objExpr, clsExpr);
			
		} else if (classExpression instanceof DataSomeValuesFrom) {
			//TODO:
			System.err.println("DataSomeValuesFrom is not implemented!");
			
		} else if (classExpression instanceof DataAllValuesFrom) {
			//TODO:
			System.err.println("DataAllValuesFrom is not implemented!");
			
		} else if (classExpression instanceof DataHasValue) {
			//TODO:
			System.err.println("DataHasValue is not implemented!");
			
		} else if (classExpression instanceof DataMinCardinality) {
			//TODO:
			System.err.println("DataMinCardinality is not implemented!");
			
		} else if (classExpression instanceof DataMaxCardinality) {
			//TODO:
			System.err.println("DataMaxCardinality is not implemented!");
			
		} else if (classExpression instanceof DataExactCardinality) {
			//TODO:
			System.err.println("DataExactCardinality is not implemented!");
			
		} else if (classExpression instanceof ObjectComplementOf) {
			owlClassExpression = factory.getOWLObjectComplementOf(
					convertClassExpression(((ObjectComplementOf) classExpression).getClassExpression()));
			
		} else if (classExpression instanceof ObjectExactCardinality) {
			int cardinality = ((ObjectExactCardinality) classExpression).getCardinality();
			OWLClassExpression clsExpr = convertClassExpression(((ObjectExactCardinality) classExpression).getClassExpression());
			OWLObjectPropertyExpression objExpr = convertObjectPropertyExpression(((ObjectExactCardinality) classExpression).getObjectPropertyExpression());
			owlClassExpression = factory.getOWLObjectExactCardinality(cardinality, objExpr, clsExpr);
					
		}

		if (owlClassExpression == null) {
			System.err.println("Error: ClassExpression " + classExpression.toString() + " not converted."); 
		}
		
		return owlClassExpression;
	}
	
	private OWLIndividual convertIndividual(Individual individual) {
			
		OWLIndividual owlIndividual = null;
		
		if (individual instanceof NamedIndividual) {
			owlIndividual = factory.getOWLNamedIndividual(
					transformURI2IRI(((NamedIndividual) individual).getEntityURI())); 
			
		} else if (individual instanceof AnonymousIndividual) {
			owlIndividual = factory.getOWLAnonymousIndividual(
					((AnonymousIndividual) individual).getNodeID()); 
		}
		
		if (owlIndividual == null) {
			System.err.println("Error: Individual " + individual.toString() + " not converted."); 
		}
		
		return owlIndividual;
	}

	private IRI transformURI2IRI(URI uri) {
		IRI iri = null;
		if (uri != null && registredClassURIs.containsKey(uri.getValue())) {
			iri = registredClassURIs.get(uri.getValue());
		} else {
			iri = IRI.create(uri == null ? "null" : uri.getValue());
			registredClassURIs.put(uri == null ? "null" : uri.getValue(), iri);
		}
		
		return iri;
	}
	
}
