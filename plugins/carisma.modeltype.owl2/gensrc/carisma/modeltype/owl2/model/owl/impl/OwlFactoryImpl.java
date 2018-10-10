/**
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     {SecSE group} - initial API and implementation and/or initial documentation
 */
package carisma.modeltype.owl2.model.owl.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;

import carisma.modeltype.owl2.model.owl.AbbreviatedURI;
import carisma.modeltype.owl2.model.owl.Annotation;
import carisma.modeltype.owl2.model.owl.AnnotationByAnonymousIndividual;
import carisma.modeltype.owl2.model.owl.AnnotationByConstant;
import carisma.modeltype.owl2.model.owl.AnnotationByEntity;
import carisma.modeltype.owl2.model.owl.AnnotationProperty;
import carisma.modeltype.owl2.model.owl.AnonymousIndividual;
import carisma.modeltype.owl2.model.owl.AnonymousIndividualAnnotation;
import carisma.modeltype.owl2.model.owl.AsymmetricObjectProperty;
import carisma.modeltype.owl2.model.owl.ClassAssertion;
import carisma.modeltype.owl2.model.owl.Constant;
import carisma.modeltype.owl2.model.owl.DataAllValuesFrom;
import carisma.modeltype.owl2.model.owl.DataComplementOf;
import carisma.modeltype.owl2.model.owl.DataExactCardinality;
import carisma.modeltype.owl2.model.owl.DataHasValue;
import carisma.modeltype.owl2.model.owl.DataMaxCardinality;
import carisma.modeltype.owl2.model.owl.DataMinCardinality;
import carisma.modeltype.owl2.model.owl.DataOneOf;
import carisma.modeltype.owl2.model.owl.DataProperty;
import carisma.modeltype.owl2.model.owl.DataPropertyAssertion;
import carisma.modeltype.owl2.model.owl.DataPropertyDomain;
import carisma.modeltype.owl2.model.owl.DataPropertyRange;
import carisma.modeltype.owl2.model.owl.DataSomeValuesFrom;
import carisma.modeltype.owl2.model.owl.Datatype;
import carisma.modeltype.owl2.model.owl.DatatypeRestriction;
import carisma.modeltype.owl2.model.owl.Declaration;
import carisma.modeltype.owl2.model.owl.DifferentIndividuals;
import carisma.modeltype.owl2.model.owl.DisjointClasses;
import carisma.modeltype.owl2.model.owl.DisjointDataProperties;
import carisma.modeltype.owl2.model.owl.DisjointObjectProperties;
import carisma.modeltype.owl2.model.owl.DisjointUnion;
import carisma.modeltype.owl2.model.owl.EntityAnnotation;
import carisma.modeltype.owl2.model.owl.EquivalentClasses;
import carisma.modeltype.owl2.model.owl.EquivalentDataProperties;
import carisma.modeltype.owl2.model.owl.EquivalentObjectProperties;
import carisma.modeltype.owl2.model.owl.FacetConstantPair;
import carisma.modeltype.owl2.model.owl.FullURI;
import carisma.modeltype.owl2.model.owl.FunctionalDataProperty;
import carisma.modeltype.owl2.model.owl.FunctionalObjectProperty;
import carisma.modeltype.owl2.model.owl.InverseFunctionalObjectProperty;
import carisma.modeltype.owl2.model.owl.InverseObjectProperties;
import carisma.modeltype.owl2.model.owl.InverseObjectProperty;
import carisma.modeltype.owl2.model.owl.IrreflexiveObjectProperty;
import carisma.modeltype.owl2.model.owl.KeyFor;
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
import carisma.modeltype.owl2.model.owl.ObjectPropertyRange;
import carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom;
import carisma.modeltype.owl2.model.owl.ObjectUnionOf;
import carisma.modeltype.owl2.model.owl.Ontology;
import carisma.modeltype.owl2.model.owl.OwlFactory;
import carisma.modeltype.owl2.model.owl.OwlPackage;
import carisma.modeltype.owl2.model.owl.ReflexiveObjectProperty;
import carisma.modeltype.owl2.model.owl.SameIndividual;
import carisma.modeltype.owl2.model.owl.SubClassOf;
import carisma.modeltype.owl2.model.owl.SubDataPropertyOf;
import carisma.modeltype.owl2.model.owl.SubObjectProperty;
import carisma.modeltype.owl2.model.owl.SubObjectPropertyOf;
import carisma.modeltype.owl2.model.owl.SymmetricObjectProperty;
import carisma.modeltype.owl2.model.owl.TransitiveObjectProperty;
import carisma.modeltype.owl2.model.owl.URI;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class OwlFactoryImpl extends EFactoryImpl implements OwlFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static OwlFactory init() {
		try {
			OwlFactory theOwlFactory = (OwlFactory)EPackage.Registry.INSTANCE.getEFactory("http:///owl.ecore"); 
			if (theOwlFactory != null) {
				return theOwlFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new OwlFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public OwlFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
			case OwlPackage.ANNOTATION: return createAnnotation();
			case OwlPackage.ANNOTATION_PROPERTY: return createAnnotationProperty();
			case OwlPackage.URI: return createURI();
			case OwlPackage.CONSTANT: return createConstant();
			case OwlPackage.DATATYPE: return createDatatype();
			case OwlPackage.ASYMMETRIC_OBJECT_PROPERTY: return createAsymmetricObjectProperty();
			case OwlPackage.OBJECT_PROPERTY: return createObjectProperty();
			case OwlPackage.INVERSE_OBJECT_PROPERTY: return createInverseObjectProperty();
			case OwlPackage.CLASS: return createClass();
			case OwlPackage.OBJECT_INTERSECTION_OF: return createObjectIntersectionOf();
			case OwlPackage.OBJECT_UNION_OF: return createObjectUnionOf();
			case OwlPackage.OBJECT_ONE_OF: return createObjectOneOf();
			case OwlPackage.NAMED_INDIVIDUAL: return createNamedIndividual();
			case OwlPackage.OBJECT_SOME_VALUES_FROM: return createObjectSomeValuesFrom();
			case OwlPackage.OBJECT_ALL_VALUES_FROM: return createObjectAllValuesFrom();
			case OwlPackage.OBJECT_EXISTS_SELF: return createObjectExistsSelf();
			case OwlPackage.OBJECT_HAS_VALUE: return createObjectHasValue();
			case OwlPackage.OBJECT_MIN_CARDINALITY: return createObjectMinCardinality();
			case OwlPackage.OBJECT_MAX_CARDINALITY: return createObjectMaxCardinality();
			case OwlPackage.DATA_SOME_VALUES_FROM: return createDataSomeValuesFrom();
			case OwlPackage.DATA_PROPERTY: return createDataProperty();
			case OwlPackage.DATA_ONE_OF: return createDataOneOf();
			case OwlPackage.DATATYPE_RESTRICTION: return createDatatypeRestriction();
			case OwlPackage.FACET_CONSTANT_PAIR: return createFacetConstantPair();
			case OwlPackage.DATA_ALL_VALUES_FROM: return createDataAllValuesFrom();
			case OwlPackage.DATA_HAS_VALUE: return createDataHasValue();
			case OwlPackage.DATA_MIN_CARDINALITY: return createDataMinCardinality();
			case OwlPackage.DATA_MAX_CARDINALITY: return createDataMaxCardinality();
			case OwlPackage.DATA_EXACT_CARDINALITY: return createDataExactCardinality();
			case OwlPackage.NEGATIVE_DATA_PROPERTY_ASSERTION: return createNegativeDataPropertyAssertion();
			case OwlPackage.DATA_PROPERTY_DOMAIN: return createDataPropertyDomain();
			case OwlPackage.DATA_PROPERTY_RANGE: return createDataPropertyRange();
			case OwlPackage.DIFFERENT_INDIVIDUALS: return createDifferentIndividuals();
			case OwlPackage.DISJOINT_CLASSES: return createDisjointClasses();
			case OwlPackage.DISJOINT_DATA_PROPERTIES: return createDisjointDataProperties();
			case OwlPackage.DISJOINT_OBJECT_PROPERTIES: return createDisjointObjectProperties();
			case OwlPackage.DISJOINT_UNION: return createDisjointUnion();
			case OwlPackage.EQUIVALENT_CLASSES: return createEquivalentClasses();
			case OwlPackage.EQUIVALENT_DATA_PROPERTIES: return createEquivalentDataProperties();
			case OwlPackage.FUNCTIONAL_DATA_PROPERTY: return createFunctionalDataProperty();
			case OwlPackage.EQUIVALENT_OBJECT_PROPERTIES: return createEquivalentObjectProperties();
			case OwlPackage.FUNCTIONAL_OBJECT_PROPERTY: return createFunctionalObjectProperty();
			case OwlPackage.INVERSE_FUNCTIONAL_OBJECT_PROPERTY: return createInverseFunctionalObjectProperty();
			case OwlPackage.OBJECT_PROPERTY_ASSERTION: return createObjectPropertyAssertion();
			case OwlPackage.NEGATIVE_OBJECT_PROPERTY_ASSERTION: return createNegativeObjectPropertyAssertion();
			case OwlPackage.OBJECT_PROPERTY_DOMAIN: return createObjectPropertyDomain();
			case OwlPackage.SYMMETRIC_OBJECT_PROPERTY: return createSymmetricObjectProperty();
			case OwlPackage.REFLEXIVE_OBJECT_PROPERTY: return createReflexiveObjectProperty();
			case OwlPackage.SUB_DATA_PROPERTY_OF: return createSubDataPropertyOf();
			case OwlPackage.SAME_INDIVIDUAL: return createSameIndividual();
			case OwlPackage.SUB_OBJECT_PROPERTY_OF: return createSubObjectPropertyOf();
			case OwlPackage.OBJECT_COMPLEMENT_OF: return createObjectComplementOf();
			case OwlPackage.ONTOLOGY: return createOntology();
			case OwlPackage.OBJECT_PROPERTY_RANGE: return createObjectPropertyRange();
			case OwlPackage.DATA_PROPERTY_ASSERTION: return createDataPropertyAssertion();
			case OwlPackage.CLASS_ASSERTION: return createClassAssertion();
			case OwlPackage.IRREFLEXIVE_OBJECT_PROPERTY: return createIrreflexiveObjectProperty();
			case OwlPackage.OBJECT_EXACT_CARDINALITY: return createObjectExactCardinality();
			case OwlPackage.DATA_COMPLEMENT_OF: return createDataComplementOf();
			case OwlPackage.SUB_CLASS_OF: return createSubClassOf();
			case OwlPackage.SUB_OBJECT_PROPERTY: return createSubObjectProperty();
			case OwlPackage.TRANSITIVE_OBJECT_PROPERTY: return createTransitiveObjectProperty();
			case OwlPackage.ENTITY_ANNOTATION: return createEntityAnnotation();
			case OwlPackage.FULL_URI: return createFullURI();
			case OwlPackage.ABBREVIATED_URI: return createAbbreviatedURI();
			case OwlPackage.INVERSE_OBJECT_PROPERTIES: return createInverseObjectProperties();
			case OwlPackage.ANNOTATION_BY_CONSTANT: return createAnnotationByConstant();
			case OwlPackage.ANNOTATION_BY_ENTITY: return createAnnotationByEntity();
			case OwlPackage.ANNOTATION_BY_ANONYMOUS_INDIVIDUAL: return createAnnotationByAnonymousIndividual();
			case OwlPackage.ANONYMOUS_INDIVIDUAL: return createAnonymousIndividual();
			case OwlPackage.DECLARATION: return createDeclaration();
			case OwlPackage.KEY_FOR: return createKeyFor();
			case OwlPackage.ANONYMOUS_INDIVIDUAL_ANNOTATION: return createAnonymousIndividualAnnotation();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Annotation createAnnotation() {
		AnnotationImpl annotation = new AnnotationImpl();
		return annotation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public AnnotationProperty createAnnotationProperty() {
		AnnotationPropertyImpl annotationProperty = new AnnotationPropertyImpl();
		return annotationProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public URI createURI() {
		URIImpl uri = new URIImpl();
		return uri;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Constant createConstant() {
		ConstantImpl constant = new ConstantImpl();
		return constant;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Datatype createDatatype() {
		DatatypeImpl datatype = new DatatypeImpl();
		return datatype;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public AsymmetricObjectProperty createAsymmetricObjectProperty() {
		AsymmetricObjectPropertyImpl asymmetricObjectProperty = new AsymmetricObjectPropertyImpl();
		return asymmetricObjectProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectProperty createObjectProperty() {
		ObjectPropertyImpl objectProperty = new ObjectPropertyImpl();
		return objectProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public InverseObjectProperty createInverseObjectProperty() {
		InverseObjectPropertyImpl inverseObjectProperty = new InverseObjectPropertyImpl();
		return inverseObjectProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public carisma.modeltype.owl2.model.owl.Class createClass() {
		ClassImpl class_ = new ClassImpl();
		return class_;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectIntersectionOf createObjectIntersectionOf() {
		ObjectIntersectionOfImpl objectIntersectionOf = new ObjectIntersectionOfImpl();
		return objectIntersectionOf;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectUnionOf createObjectUnionOf() {
		ObjectUnionOfImpl objectUnionOf = new ObjectUnionOfImpl();
		return objectUnionOf;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectOneOf createObjectOneOf() {
		ObjectOneOfImpl objectOneOf = new ObjectOneOfImpl();
		return objectOneOf;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NamedIndividual createNamedIndividual() {
		NamedIndividualImpl namedIndividual = new NamedIndividualImpl();
		return namedIndividual;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectSomeValuesFrom createObjectSomeValuesFrom() {
		ObjectSomeValuesFromImpl objectSomeValuesFrom = new ObjectSomeValuesFromImpl();
		return objectSomeValuesFrom;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectAllValuesFrom createObjectAllValuesFrom() {
		ObjectAllValuesFromImpl objectAllValuesFrom = new ObjectAllValuesFromImpl();
		return objectAllValuesFrom;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectExistsSelf createObjectExistsSelf() {
		ObjectExistsSelfImpl objectExistsSelf = new ObjectExistsSelfImpl();
		return objectExistsSelf;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectHasValue createObjectHasValue() {
		ObjectHasValueImpl objectHasValue = new ObjectHasValueImpl();
		return objectHasValue;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectMinCardinality createObjectMinCardinality() {
		ObjectMinCardinalityImpl objectMinCardinality = new ObjectMinCardinalityImpl();
		return objectMinCardinality;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectMaxCardinality createObjectMaxCardinality() {
		ObjectMaxCardinalityImpl objectMaxCardinality = new ObjectMaxCardinalityImpl();
		return objectMaxCardinality;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataSomeValuesFrom createDataSomeValuesFrom() {
		DataSomeValuesFromImpl dataSomeValuesFrom = new DataSomeValuesFromImpl();
		return dataSomeValuesFrom;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataProperty createDataProperty() {
		DataPropertyImpl dataProperty = new DataPropertyImpl();
		return dataProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataOneOf createDataOneOf() {
		DataOneOfImpl dataOneOf = new DataOneOfImpl();
		return dataOneOf;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DatatypeRestriction createDatatypeRestriction() {
		DatatypeRestrictionImpl datatypeRestriction = new DatatypeRestrictionImpl();
		return datatypeRestriction;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FacetConstantPair createFacetConstantPair() {
		FacetConstantPairImpl facetConstantPair = new FacetConstantPairImpl();
		return facetConstantPair;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataAllValuesFrom createDataAllValuesFrom() {
		DataAllValuesFromImpl dataAllValuesFrom = new DataAllValuesFromImpl();
		return dataAllValuesFrom;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataHasValue createDataHasValue() {
		DataHasValueImpl dataHasValue = new DataHasValueImpl();
		return dataHasValue;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataMinCardinality createDataMinCardinality() {
		DataMinCardinalityImpl dataMinCardinality = new DataMinCardinalityImpl();
		return dataMinCardinality;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataMaxCardinality createDataMaxCardinality() {
		DataMaxCardinalityImpl dataMaxCardinality = new DataMaxCardinalityImpl();
		return dataMaxCardinality;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataExactCardinality createDataExactCardinality() {
		DataExactCardinalityImpl dataExactCardinality = new DataExactCardinalityImpl();
		return dataExactCardinality;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NegativeDataPropertyAssertion createNegativeDataPropertyAssertion() {
		NegativeDataPropertyAssertionImpl negativeDataPropertyAssertion = new NegativeDataPropertyAssertionImpl();
		return negativeDataPropertyAssertion;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataPropertyDomain createDataPropertyDomain() {
		DataPropertyDomainImpl dataPropertyDomain = new DataPropertyDomainImpl();
		return dataPropertyDomain;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataPropertyRange createDataPropertyRange() {
		DataPropertyRangeImpl dataPropertyRange = new DataPropertyRangeImpl();
		return dataPropertyRange;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DifferentIndividuals createDifferentIndividuals() {
		DifferentIndividualsImpl differentIndividuals = new DifferentIndividualsImpl();
		return differentIndividuals;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DisjointClasses createDisjointClasses() {
		DisjointClassesImpl disjointClasses = new DisjointClassesImpl();
		return disjointClasses;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DisjointDataProperties createDisjointDataProperties() {
		DisjointDataPropertiesImpl disjointDataProperties = new DisjointDataPropertiesImpl();
		return disjointDataProperties;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DisjointObjectProperties createDisjointObjectProperties() {
		DisjointObjectPropertiesImpl disjointObjectProperties = new DisjointObjectPropertiesImpl();
		return disjointObjectProperties;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DisjointUnion createDisjointUnion() {
		DisjointUnionImpl disjointUnion = new DisjointUnionImpl();
		return disjointUnion;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EquivalentClasses createEquivalentClasses() {
		EquivalentClassesImpl equivalentClasses = new EquivalentClassesImpl();
		return equivalentClasses;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EquivalentDataProperties createEquivalentDataProperties() {
		EquivalentDataPropertiesImpl equivalentDataProperties = new EquivalentDataPropertiesImpl();
		return equivalentDataProperties;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FunctionalDataProperty createFunctionalDataProperty() {
		FunctionalDataPropertyImpl functionalDataProperty = new FunctionalDataPropertyImpl();
		return functionalDataProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EquivalentObjectProperties createEquivalentObjectProperties() {
		EquivalentObjectPropertiesImpl equivalentObjectProperties = new EquivalentObjectPropertiesImpl();
		return equivalentObjectProperties;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FunctionalObjectProperty createFunctionalObjectProperty() {
		FunctionalObjectPropertyImpl functionalObjectProperty = new FunctionalObjectPropertyImpl();
		return functionalObjectProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public InverseFunctionalObjectProperty createInverseFunctionalObjectProperty() {
		InverseFunctionalObjectPropertyImpl inverseFunctionalObjectProperty = new InverseFunctionalObjectPropertyImpl();
		return inverseFunctionalObjectProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectPropertyAssertion createObjectPropertyAssertion() {
		ObjectPropertyAssertionImpl objectPropertyAssertion = new ObjectPropertyAssertionImpl();
		return objectPropertyAssertion;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NegativeObjectPropertyAssertion createNegativeObjectPropertyAssertion() {
		NegativeObjectPropertyAssertionImpl negativeObjectPropertyAssertion = new NegativeObjectPropertyAssertionImpl();
		return negativeObjectPropertyAssertion;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectPropertyDomain createObjectPropertyDomain() {
		ObjectPropertyDomainImpl objectPropertyDomain = new ObjectPropertyDomainImpl();
		return objectPropertyDomain;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SymmetricObjectProperty createSymmetricObjectProperty() {
		SymmetricObjectPropertyImpl symmetricObjectProperty = new SymmetricObjectPropertyImpl();
		return symmetricObjectProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ReflexiveObjectProperty createReflexiveObjectProperty() {
		ReflexiveObjectPropertyImpl reflexiveObjectProperty = new ReflexiveObjectPropertyImpl();
		return reflexiveObjectProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SubDataPropertyOf createSubDataPropertyOf() {
		SubDataPropertyOfImpl subDataPropertyOf = new SubDataPropertyOfImpl();
		return subDataPropertyOf;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SameIndividual createSameIndividual() {
		SameIndividualImpl sameIndividual = new SameIndividualImpl();
		return sameIndividual;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SubObjectPropertyOf createSubObjectPropertyOf() {
		SubObjectPropertyOfImpl subObjectPropertyOf = new SubObjectPropertyOfImpl();
		return subObjectPropertyOf;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectComplementOf createObjectComplementOf() {
		ObjectComplementOfImpl objectComplementOf = new ObjectComplementOfImpl();
		return objectComplementOf;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Ontology createOntology() {
		OntologyImpl ontology = new OntologyImpl();
		return ontology;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectPropertyRange createObjectPropertyRange() {
		ObjectPropertyRangeImpl objectPropertyRange = new ObjectPropertyRangeImpl();
		return objectPropertyRange;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataPropertyAssertion createDataPropertyAssertion() {
		DataPropertyAssertionImpl dataPropertyAssertion = new DataPropertyAssertionImpl();
		return dataPropertyAssertion;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ClassAssertion createClassAssertion() {
		ClassAssertionImpl classAssertion = new ClassAssertionImpl();
		return classAssertion;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public IrreflexiveObjectProperty createIrreflexiveObjectProperty() {
		IrreflexiveObjectPropertyImpl irreflexiveObjectProperty = new IrreflexiveObjectPropertyImpl();
		return irreflexiveObjectProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectExactCardinality createObjectExactCardinality() {
		ObjectExactCardinalityImpl objectExactCardinality = new ObjectExactCardinalityImpl();
		return objectExactCardinality;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataComplementOf createDataComplementOf() {
		DataComplementOfImpl dataComplementOf = new DataComplementOfImpl();
		return dataComplementOf;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SubClassOf createSubClassOf() {
		SubClassOfImpl subClassOf = new SubClassOfImpl();
		return subClassOf;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SubObjectProperty createSubObjectProperty() {
		SubObjectPropertyImpl subObjectProperty = new SubObjectPropertyImpl();
		return subObjectProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TransitiveObjectProperty createTransitiveObjectProperty() {
		TransitiveObjectPropertyImpl transitiveObjectProperty = new TransitiveObjectPropertyImpl();
		return transitiveObjectProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EntityAnnotation createEntityAnnotation() {
		EntityAnnotationImpl entityAnnotation = new EntityAnnotationImpl();
		return entityAnnotation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FullURI createFullURI() {
		FullURIImpl fullURI = new FullURIImpl();
		return fullURI;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public AbbreviatedURI createAbbreviatedURI() {
		AbbreviatedURIImpl abbreviatedURI = new AbbreviatedURIImpl();
		return abbreviatedURI;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public InverseObjectProperties createInverseObjectProperties() {
		InverseObjectPropertiesImpl inverseObjectProperties = new InverseObjectPropertiesImpl();
		return inverseObjectProperties;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public AnnotationByConstant createAnnotationByConstant() {
		AnnotationByConstantImpl annotationByConstant = new AnnotationByConstantImpl();
		return annotationByConstant;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public AnnotationByEntity createAnnotationByEntity() {
		AnnotationByEntityImpl annotationByEntity = new AnnotationByEntityImpl();
		return annotationByEntity;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public AnnotationByAnonymousIndividual createAnnotationByAnonymousIndividual() {
		AnnotationByAnonymousIndividualImpl annotationByAnonymousIndividual = new AnnotationByAnonymousIndividualImpl();
		return annotationByAnonymousIndividual;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public AnonymousIndividual createAnonymousIndividual() {
		AnonymousIndividualImpl anonymousIndividual = new AnonymousIndividualImpl();
		return anonymousIndividual;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Declaration createDeclaration() {
		DeclarationImpl declaration = new DeclarationImpl();
		return declaration;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public KeyFor createKeyFor() {
		KeyForImpl keyFor = new KeyForImpl();
		return keyFor;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public AnonymousIndividualAnnotation createAnonymousIndividualAnnotation() {
		AnonymousIndividualAnnotationImpl anonymousIndividualAnnotation = new AnonymousIndividualAnnotationImpl();
		return anonymousIndividualAnnotation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public OwlPackage getOwlPackage() {
		return (OwlPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static OwlPackage getPackage() {
		return OwlPackage.eINSTANCE;
	}

} //OwlFactoryImpl
