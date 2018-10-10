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
package carisma.modeltype.owl2.model.owl.util;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.util.Switch;

import carisma.modeltype.owl2.model.owl.AbbreviatedURI;
import carisma.modeltype.owl2.model.owl.Annotation;
import carisma.modeltype.owl2.model.owl.AnnotationByAnonymousIndividual;
import carisma.modeltype.owl2.model.owl.AnnotationByConstant;
import carisma.modeltype.owl2.model.owl.AnnotationByEntity;
import carisma.modeltype.owl2.model.owl.AnnotationProperty;
import carisma.modeltype.owl2.model.owl.AnonymousIndividual;
import carisma.modeltype.owl2.model.owl.AnonymousIndividualAnnotation;
import carisma.modeltype.owl2.model.owl.Assertion;
import carisma.modeltype.owl2.model.owl.AsymmetricObjectProperty;
import carisma.modeltype.owl2.model.owl.Axiom;
import carisma.modeltype.owl2.model.owl.ClassAssertion;
import carisma.modeltype.owl2.model.owl.ClassAxiom;
import carisma.modeltype.owl2.model.owl.ClassExpression;
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
import carisma.modeltype.owl2.model.owl.DataPropertyAxiom;
import carisma.modeltype.owl2.model.owl.DataPropertyDomain;
import carisma.modeltype.owl2.model.owl.DataPropertyExpression;
import carisma.modeltype.owl2.model.owl.DataPropertyRange;
import carisma.modeltype.owl2.model.owl.DataRange;
import carisma.modeltype.owl2.model.owl.DataSomeValuesFrom;
import carisma.modeltype.owl2.model.owl.Datatype;
import carisma.modeltype.owl2.model.owl.DatatypeRestriction;
import carisma.modeltype.owl2.model.owl.Declaration;
import carisma.modeltype.owl2.model.owl.DifferentIndividuals;
import carisma.modeltype.owl2.model.owl.DisjointClasses;
import carisma.modeltype.owl2.model.owl.DisjointDataProperties;
import carisma.modeltype.owl2.model.owl.DisjointObjectProperties;
import carisma.modeltype.owl2.model.owl.DisjointUnion;
import carisma.modeltype.owl2.model.owl.Entity;
import carisma.modeltype.owl2.model.owl.EntityAnnotation;
import carisma.modeltype.owl2.model.owl.EquivalentClasses;
import carisma.modeltype.owl2.model.owl.EquivalentDataProperties;
import carisma.modeltype.owl2.model.owl.EquivalentObjectProperties;
import carisma.modeltype.owl2.model.owl.FacetConstantPair;
import carisma.modeltype.owl2.model.owl.FullURI;
import carisma.modeltype.owl2.model.owl.FunctionalDataProperty;
import carisma.modeltype.owl2.model.owl.FunctionalObjectProperty;
import carisma.modeltype.owl2.model.owl.Individual;
import carisma.modeltype.owl2.model.owl.InverseFunctionalObjectProperty;
import carisma.modeltype.owl2.model.owl.InverseObjectProperties;
import carisma.modeltype.owl2.model.owl.InverseObjectProperty;
import carisma.modeltype.owl2.model.owl.IrreflexiveObjectProperty;
import carisma.modeltype.owl2.model.owl.KeyFor;
import carisma.modeltype.owl2.model.owl.NamedIndividual;
import carisma.modeltype.owl2.model.owl.NegativeDataPropertyAssertion;
import carisma.modeltype.owl2.model.owl.NegativeObjectPropertyAssertion;
import carisma.modeltype.owl2.model.owl.ObjectAllValuesFrom;
import carisma.modeltype.owl2.model.owl.ObjectAndDataPropertyAxiom;
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
import carisma.modeltype.owl2.model.owl.ObjectPropertyAxiom;
import carisma.modeltype.owl2.model.owl.ObjectPropertyDomain;
import carisma.modeltype.owl2.model.owl.ObjectPropertyExpression;
import carisma.modeltype.owl2.model.owl.ObjectPropertyRange;
import carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom;
import carisma.modeltype.owl2.model.owl.ObjectUnionOf;
import carisma.modeltype.owl2.model.owl.Ontology;
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
 * The <b>Switch</b> for the model's inheritance hierarchy.
 * It supports the call {@link #doSwitch(EObject) doSwitch(object)}
 * to invoke the <code>caseXXX</code> method for each class of the model,
 * starting with the actual class of the object
 * and proceeding up the inheritance hierarchy
 * until a non-null result is returned,
 * which is the result of the switch.
 * <!-- end-user-doc -->
 * @see carisma.modeltype.owl2.model.owl.OwlPackage
 * @generated
 */
public class OwlSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static OwlPackage modelPackage;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public OwlSwitch() {
		if (modelPackage == null) {
			modelPackage = OwlPackage.eINSTANCE;
		}
	}

	/**
	 * Checks whether this is a switch for the given package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @parameter ePackage the package in question.
	 * @return whether this is a switch for the given package.
	 * @generated
	 */
	@Override
	protected boolean isSwitchFor(EPackage ePackage) {
		return ePackage == modelPackage;
	}

	/**
	 * Calls <code>caseXXX</code> for each class of the model until one returns a non null result; it yields that result.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the first non-null result returned by a <code>caseXXX</code> call.
	 * @generated
	 */
	@Override
	protected T doSwitch(int classifierID, EObject theEObject) {
		switch (classifierID) {
			case OwlPackage.ASSERTION: {
				Assertion assertion = (Assertion)theEObject;
				T result = caseAssertion(assertion);
				if (result == null) result = caseAxiom(assertion);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.AXIOM: {
				Axiom axiom = (Axiom)theEObject;
				T result = caseAxiom(axiom);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.ANNOTATION: {
				Annotation annotation = (Annotation)theEObject;
				T result = caseAnnotation(annotation);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.ANNOTATION_PROPERTY: {
				AnnotationProperty annotationProperty = (AnnotationProperty)theEObject;
				T result = caseAnnotationProperty(annotationProperty);
				if (result == null) result = caseEntity(annotationProperty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.ENTITY: {
				Entity entity = (Entity)theEObject;
				T result = caseEntity(entity);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.URI: {
				URI uri = (URI)theEObject;
				T result = caseURI(uri);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.CONSTANT: {
				Constant constant = (Constant)theEObject;
				T result = caseConstant(constant);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATATYPE: {
				Datatype datatype = (Datatype)theEObject;
				T result = caseDatatype(datatype);
				if (result == null) result = caseDataRange(datatype);
				if (result == null) result = caseEntity(datatype);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_RANGE: {
				DataRange dataRange = (DataRange)theEObject;
				T result = caseDataRange(dataRange);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_PROPERTY_AXIOM: {
				DataPropertyAxiom dataPropertyAxiom = (DataPropertyAxiom)theEObject;
				T result = caseDataPropertyAxiom(dataPropertyAxiom);
				if (result == null) result = caseAxiom(dataPropertyAxiom);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_PROPERTY_AXIOM: {
				ObjectPropertyAxiom objectPropertyAxiom = (ObjectPropertyAxiom)theEObject;
				T result = caseObjectPropertyAxiom(objectPropertyAxiom);
				if (result == null) result = caseAxiom(objectPropertyAxiom);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.CLASS_EXPRESSION: {
				ClassExpression classExpression = (ClassExpression)theEObject;
				T result = caseClassExpression(classExpression);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.CLASS_AXIOM: {
				ClassAxiom classAxiom = (ClassAxiom)theEObject;
				T result = caseClassAxiom(classAxiom);
				if (result == null) result = caseAxiom(classAxiom);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_PROPERTY_EXPRESSION: {
				DataPropertyExpression dataPropertyExpression = (DataPropertyExpression)theEObject;
				T result = caseDataPropertyExpression(dataPropertyExpression);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_PROPERTY_EXPRESSION: {
				ObjectPropertyExpression objectPropertyExpression = (ObjectPropertyExpression)theEObject;
				T result = caseObjectPropertyExpression(objectPropertyExpression);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.ASYMMETRIC_OBJECT_PROPERTY: {
				AsymmetricObjectProperty asymmetricObjectProperty = (AsymmetricObjectProperty)theEObject;
				T result = caseAsymmetricObjectProperty(asymmetricObjectProperty);
				if (result == null) result = caseObjectPropertyAxiom(asymmetricObjectProperty);
				if (result == null) result = caseAxiom(asymmetricObjectProperty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_PROPERTY: {
				ObjectProperty objectProperty = (ObjectProperty)theEObject;
				T result = caseObjectProperty(objectProperty);
				if (result == null) result = caseEntity(objectProperty);
				if (result == null) result = caseObjectPropertyExpression(objectProperty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.INVERSE_OBJECT_PROPERTY: {
				InverseObjectProperty inverseObjectProperty = (InverseObjectProperty)theEObject;
				T result = caseInverseObjectProperty(inverseObjectProperty);
				if (result == null) result = caseObjectPropertyExpression(inverseObjectProperty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.CLASS: {
				carisma.modeltype.owl2.model.owl.Class class_ = (carisma.modeltype.owl2.model.owl.Class)theEObject;
				T result = caseClass(class_);
				if (result == null) result = caseEntity(class_);
				if (result == null) result = caseClassExpression(class_);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_INTERSECTION_OF: {
				ObjectIntersectionOf objectIntersectionOf = (ObjectIntersectionOf)theEObject;
				T result = caseObjectIntersectionOf(objectIntersectionOf);
				if (result == null) result = caseClassExpression(objectIntersectionOf);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_UNION_OF: {
				ObjectUnionOf objectUnionOf = (ObjectUnionOf)theEObject;
				T result = caseObjectUnionOf(objectUnionOf);
				if (result == null) result = caseClassExpression(objectUnionOf);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_ONE_OF: {
				ObjectOneOf objectOneOf = (ObjectOneOf)theEObject;
				T result = caseObjectOneOf(objectOneOf);
				if (result == null) result = caseClassExpression(objectOneOf);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.INDIVIDUAL: {
				Individual individual = (Individual)theEObject;
				T result = caseIndividual(individual);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.NAMED_INDIVIDUAL: {
				NamedIndividual namedIndividual = (NamedIndividual)theEObject;
				T result = caseNamedIndividual(namedIndividual);
				if (result == null) result = caseEntity(namedIndividual);
				if (result == null) result = caseIndividual(namedIndividual);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_SOME_VALUES_FROM: {
				ObjectSomeValuesFrom objectSomeValuesFrom = (ObjectSomeValuesFrom)theEObject;
				T result = caseObjectSomeValuesFrom(objectSomeValuesFrom);
				if (result == null) result = caseClassExpression(objectSomeValuesFrom);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_ALL_VALUES_FROM: {
				ObjectAllValuesFrom objectAllValuesFrom = (ObjectAllValuesFrom)theEObject;
				T result = caseObjectAllValuesFrom(objectAllValuesFrom);
				if (result == null) result = caseClassExpression(objectAllValuesFrom);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_EXISTS_SELF: {
				ObjectExistsSelf objectExistsSelf = (ObjectExistsSelf)theEObject;
				T result = caseObjectExistsSelf(objectExistsSelf);
				if (result == null) result = caseClassExpression(objectExistsSelf);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_HAS_VALUE: {
				ObjectHasValue objectHasValue = (ObjectHasValue)theEObject;
				T result = caseObjectHasValue(objectHasValue);
				if (result == null) result = caseClassExpression(objectHasValue);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_MIN_CARDINALITY: {
				ObjectMinCardinality objectMinCardinality = (ObjectMinCardinality)theEObject;
				T result = caseObjectMinCardinality(objectMinCardinality);
				if (result == null) result = caseClassExpression(objectMinCardinality);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_MAX_CARDINALITY: {
				ObjectMaxCardinality objectMaxCardinality = (ObjectMaxCardinality)theEObject;
				T result = caseObjectMaxCardinality(objectMaxCardinality);
				if (result == null) result = caseClassExpression(objectMaxCardinality);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_SOME_VALUES_FROM: {
				DataSomeValuesFrom dataSomeValuesFrom = (DataSomeValuesFrom)theEObject;
				T result = caseDataSomeValuesFrom(dataSomeValuesFrom);
				if (result == null) result = caseClassExpression(dataSomeValuesFrom);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_PROPERTY: {
				DataProperty dataProperty = (DataProperty)theEObject;
				T result = caseDataProperty(dataProperty);
				if (result == null) result = caseEntity(dataProperty);
				if (result == null) result = caseDataPropertyExpression(dataProperty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_ONE_OF: {
				DataOneOf dataOneOf = (DataOneOf)theEObject;
				T result = caseDataOneOf(dataOneOf);
				if (result == null) result = caseDataRange(dataOneOf);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATATYPE_RESTRICTION: {
				DatatypeRestriction datatypeRestriction = (DatatypeRestriction)theEObject;
				T result = caseDatatypeRestriction(datatypeRestriction);
				if (result == null) result = caseDataRange(datatypeRestriction);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.FACET_CONSTANT_PAIR: {
				FacetConstantPair facetConstantPair = (FacetConstantPair)theEObject;
				T result = caseFacetConstantPair(facetConstantPair);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_ALL_VALUES_FROM: {
				DataAllValuesFrom dataAllValuesFrom = (DataAllValuesFrom)theEObject;
				T result = caseDataAllValuesFrom(dataAllValuesFrom);
				if (result == null) result = caseClassExpression(dataAllValuesFrom);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_HAS_VALUE: {
				DataHasValue dataHasValue = (DataHasValue)theEObject;
				T result = caseDataHasValue(dataHasValue);
				if (result == null) result = caseClassExpression(dataHasValue);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_MIN_CARDINALITY: {
				DataMinCardinality dataMinCardinality = (DataMinCardinality)theEObject;
				T result = caseDataMinCardinality(dataMinCardinality);
				if (result == null) result = caseClassExpression(dataMinCardinality);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_MAX_CARDINALITY: {
				DataMaxCardinality dataMaxCardinality = (DataMaxCardinality)theEObject;
				T result = caseDataMaxCardinality(dataMaxCardinality);
				if (result == null) result = caseClassExpression(dataMaxCardinality);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_EXACT_CARDINALITY: {
				DataExactCardinality dataExactCardinality = (DataExactCardinality)theEObject;
				T result = caseDataExactCardinality(dataExactCardinality);
				if (result == null) result = caseClassExpression(dataExactCardinality);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.NEGATIVE_DATA_PROPERTY_ASSERTION: {
				NegativeDataPropertyAssertion negativeDataPropertyAssertion = (NegativeDataPropertyAssertion)theEObject;
				T result = caseNegativeDataPropertyAssertion(negativeDataPropertyAssertion);
				if (result == null) result = caseAssertion(negativeDataPropertyAssertion);
				if (result == null) result = caseAxiom(negativeDataPropertyAssertion);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_PROPERTY_DOMAIN: {
				DataPropertyDomain dataPropertyDomain = (DataPropertyDomain)theEObject;
				T result = caseDataPropertyDomain(dataPropertyDomain);
				if (result == null) result = caseDataPropertyAxiom(dataPropertyDomain);
				if (result == null) result = caseAxiom(dataPropertyDomain);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_PROPERTY_RANGE: {
				DataPropertyRange dataPropertyRange = (DataPropertyRange)theEObject;
				T result = caseDataPropertyRange(dataPropertyRange);
				if (result == null) result = caseDataPropertyAxiom(dataPropertyRange);
				if (result == null) result = caseAxiom(dataPropertyRange);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DIFFERENT_INDIVIDUALS: {
				DifferentIndividuals differentIndividuals = (DifferentIndividuals)theEObject;
				T result = caseDifferentIndividuals(differentIndividuals);
				if (result == null) result = caseAssertion(differentIndividuals);
				if (result == null) result = caseAxiom(differentIndividuals);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DISJOINT_CLASSES: {
				DisjointClasses disjointClasses = (DisjointClasses)theEObject;
				T result = caseDisjointClasses(disjointClasses);
				if (result == null) result = caseClassAxiom(disjointClasses);
				if (result == null) result = caseAxiom(disjointClasses);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DISJOINT_DATA_PROPERTIES: {
				DisjointDataProperties disjointDataProperties = (DisjointDataProperties)theEObject;
				T result = caseDisjointDataProperties(disjointDataProperties);
				if (result == null) result = caseDataPropertyAxiom(disjointDataProperties);
				if (result == null) result = caseAxiom(disjointDataProperties);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DISJOINT_OBJECT_PROPERTIES: {
				DisjointObjectProperties disjointObjectProperties = (DisjointObjectProperties)theEObject;
				T result = caseDisjointObjectProperties(disjointObjectProperties);
				if (result == null) result = caseObjectPropertyAxiom(disjointObjectProperties);
				if (result == null) result = caseAxiom(disjointObjectProperties);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DISJOINT_UNION: {
				DisjointUnion disjointUnion = (DisjointUnion)theEObject;
				T result = caseDisjointUnion(disjointUnion);
				if (result == null) result = caseClassAxiom(disjointUnion);
				if (result == null) result = caseAxiom(disjointUnion);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.EQUIVALENT_CLASSES: {
				EquivalentClasses equivalentClasses = (EquivalentClasses)theEObject;
				T result = caseEquivalentClasses(equivalentClasses);
				if (result == null) result = caseClassAxiom(equivalentClasses);
				if (result == null) result = caseAxiom(equivalentClasses);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.EQUIVALENT_DATA_PROPERTIES: {
				EquivalentDataProperties equivalentDataProperties = (EquivalentDataProperties)theEObject;
				T result = caseEquivalentDataProperties(equivalentDataProperties);
				if (result == null) result = caseDataPropertyAxiom(equivalentDataProperties);
				if (result == null) result = caseAxiom(equivalentDataProperties);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.FUNCTIONAL_DATA_PROPERTY: {
				FunctionalDataProperty functionalDataProperty = (FunctionalDataProperty)theEObject;
				T result = caseFunctionalDataProperty(functionalDataProperty);
				if (result == null) result = caseDataPropertyAxiom(functionalDataProperty);
				if (result == null) result = caseAxiom(functionalDataProperty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.EQUIVALENT_OBJECT_PROPERTIES: {
				EquivalentObjectProperties equivalentObjectProperties = (EquivalentObjectProperties)theEObject;
				T result = caseEquivalentObjectProperties(equivalentObjectProperties);
				if (result == null) result = caseObjectPropertyAxiom(equivalentObjectProperties);
				if (result == null) result = caseAxiom(equivalentObjectProperties);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.FUNCTIONAL_OBJECT_PROPERTY: {
				FunctionalObjectProperty functionalObjectProperty = (FunctionalObjectProperty)theEObject;
				T result = caseFunctionalObjectProperty(functionalObjectProperty);
				if (result == null) result = caseObjectPropertyAxiom(functionalObjectProperty);
				if (result == null) result = caseAxiom(functionalObjectProperty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.INVERSE_FUNCTIONAL_OBJECT_PROPERTY: {
				InverseFunctionalObjectProperty inverseFunctionalObjectProperty = (InverseFunctionalObjectProperty)theEObject;
				T result = caseInverseFunctionalObjectProperty(inverseFunctionalObjectProperty);
				if (result == null) result = caseObjectPropertyAxiom(inverseFunctionalObjectProperty);
				if (result == null) result = caseAxiom(inverseFunctionalObjectProperty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_PROPERTY_ASSERTION: {
				ObjectPropertyAssertion objectPropertyAssertion = (ObjectPropertyAssertion)theEObject;
				T result = caseObjectPropertyAssertion(objectPropertyAssertion);
				if (result == null) result = caseAssertion(objectPropertyAssertion);
				if (result == null) result = caseAxiom(objectPropertyAssertion);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.NEGATIVE_OBJECT_PROPERTY_ASSERTION: {
				NegativeObjectPropertyAssertion negativeObjectPropertyAssertion = (NegativeObjectPropertyAssertion)theEObject;
				T result = caseNegativeObjectPropertyAssertion(negativeObjectPropertyAssertion);
				if (result == null) result = caseAssertion(negativeObjectPropertyAssertion);
				if (result == null) result = caseAxiom(negativeObjectPropertyAssertion);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_PROPERTY_DOMAIN: {
				ObjectPropertyDomain objectPropertyDomain = (ObjectPropertyDomain)theEObject;
				T result = caseObjectPropertyDomain(objectPropertyDomain);
				if (result == null) result = caseObjectPropertyAxiom(objectPropertyDomain);
				if (result == null) result = caseAxiom(objectPropertyDomain);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.SYMMETRIC_OBJECT_PROPERTY: {
				SymmetricObjectProperty symmetricObjectProperty = (SymmetricObjectProperty)theEObject;
				T result = caseSymmetricObjectProperty(symmetricObjectProperty);
				if (result == null) result = caseObjectPropertyAxiom(symmetricObjectProperty);
				if (result == null) result = caseAxiom(symmetricObjectProperty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.REFLEXIVE_OBJECT_PROPERTY: {
				ReflexiveObjectProperty reflexiveObjectProperty = (ReflexiveObjectProperty)theEObject;
				T result = caseReflexiveObjectProperty(reflexiveObjectProperty);
				if (result == null) result = caseObjectPropertyAxiom(reflexiveObjectProperty);
				if (result == null) result = caseAxiom(reflexiveObjectProperty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.SUB_DATA_PROPERTY_OF: {
				SubDataPropertyOf subDataPropertyOf = (SubDataPropertyOf)theEObject;
				T result = caseSubDataPropertyOf(subDataPropertyOf);
				if (result == null) result = caseDataPropertyAxiom(subDataPropertyOf);
				if (result == null) result = caseAxiom(subDataPropertyOf);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.SAME_INDIVIDUAL: {
				SameIndividual sameIndividual = (SameIndividual)theEObject;
				T result = caseSameIndividual(sameIndividual);
				if (result == null) result = caseAssertion(sameIndividual);
				if (result == null) result = caseAxiom(sameIndividual);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.SUB_OBJECT_PROPERTY_OF: {
				SubObjectPropertyOf subObjectPropertyOf = (SubObjectPropertyOf)theEObject;
				T result = caseSubObjectPropertyOf(subObjectPropertyOf);
				if (result == null) result = caseObjectPropertyAxiom(subObjectPropertyOf);
				if (result == null) result = caseAxiom(subObjectPropertyOf);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_COMPLEMENT_OF: {
				ObjectComplementOf objectComplementOf = (ObjectComplementOf)theEObject;
				T result = caseObjectComplementOf(objectComplementOf);
				if (result == null) result = caseClassExpression(objectComplementOf);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.ONTOLOGY: {
				Ontology ontology = (Ontology)theEObject;
				T result = caseOntology(ontology);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_PROPERTY_RANGE: {
				ObjectPropertyRange objectPropertyRange = (ObjectPropertyRange)theEObject;
				T result = caseObjectPropertyRange(objectPropertyRange);
				if (result == null) result = caseObjectPropertyAxiom(objectPropertyRange);
				if (result == null) result = caseAxiom(objectPropertyRange);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_PROPERTY_ASSERTION: {
				DataPropertyAssertion dataPropertyAssertion = (DataPropertyAssertion)theEObject;
				T result = caseDataPropertyAssertion(dataPropertyAssertion);
				if (result == null) result = caseAssertion(dataPropertyAssertion);
				if (result == null) result = caseAxiom(dataPropertyAssertion);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.CLASS_ASSERTION: {
				ClassAssertion classAssertion = (ClassAssertion)theEObject;
				T result = caseClassAssertion(classAssertion);
				if (result == null) result = caseAssertion(classAssertion);
				if (result == null) result = caseAxiom(classAssertion);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.IRREFLEXIVE_OBJECT_PROPERTY: {
				IrreflexiveObjectProperty irreflexiveObjectProperty = (IrreflexiveObjectProperty)theEObject;
				T result = caseIrreflexiveObjectProperty(irreflexiveObjectProperty);
				if (result == null) result = caseObjectPropertyAxiom(irreflexiveObjectProperty);
				if (result == null) result = caseAxiom(irreflexiveObjectProperty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_EXACT_CARDINALITY: {
				ObjectExactCardinality objectExactCardinality = (ObjectExactCardinality)theEObject;
				T result = caseObjectExactCardinality(objectExactCardinality);
				if (result == null) result = caseClassExpression(objectExactCardinality);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DATA_COMPLEMENT_OF: {
				DataComplementOf dataComplementOf = (DataComplementOf)theEObject;
				T result = caseDataComplementOf(dataComplementOf);
				if (result == null) result = caseDataRange(dataComplementOf);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.SUB_CLASS_OF: {
				SubClassOf subClassOf = (SubClassOf)theEObject;
				T result = caseSubClassOf(subClassOf);
				if (result == null) result = caseClassAxiom(subClassOf);
				if (result == null) result = caseAxiom(subClassOf);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.SUB_OBJECT_PROPERTY: {
				SubObjectProperty subObjectProperty = (SubObjectProperty)theEObject;
				T result = caseSubObjectProperty(subObjectProperty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.TRANSITIVE_OBJECT_PROPERTY: {
				TransitiveObjectProperty transitiveObjectProperty = (TransitiveObjectProperty)theEObject;
				T result = caseTransitiveObjectProperty(transitiveObjectProperty);
				if (result == null) result = caseObjectPropertyAxiom(transitiveObjectProperty);
				if (result == null) result = caseAxiom(transitiveObjectProperty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.ENTITY_ANNOTATION: {
				EntityAnnotation entityAnnotation = (EntityAnnotation)theEObject;
				T result = caseEntityAnnotation(entityAnnotation);
				if (result == null) result = caseAxiom(entityAnnotation);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.FULL_URI: {
				FullURI fullURI = (FullURI)theEObject;
				T result = caseFullURI(fullURI);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.ABBREVIATED_URI: {
				AbbreviatedURI abbreviatedURI = (AbbreviatedURI)theEObject;
				T result = caseAbbreviatedURI(abbreviatedURI);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.INVERSE_OBJECT_PROPERTIES: {
				InverseObjectProperties inverseObjectProperties = (InverseObjectProperties)theEObject;
				T result = caseInverseObjectProperties(inverseObjectProperties);
				if (result == null) result = caseObjectPropertyAxiom(inverseObjectProperties);
				if (result == null) result = caseAxiom(inverseObjectProperties);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.ANNOTATION_BY_CONSTANT: {
				AnnotationByConstant annotationByConstant = (AnnotationByConstant)theEObject;
				T result = caseAnnotationByConstant(annotationByConstant);
				if (result == null) result = caseAnnotation(annotationByConstant);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.ANNOTATION_BY_ENTITY: {
				AnnotationByEntity annotationByEntity = (AnnotationByEntity)theEObject;
				T result = caseAnnotationByEntity(annotationByEntity);
				if (result == null) result = caseAnnotation(annotationByEntity);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.ANNOTATION_BY_ANONYMOUS_INDIVIDUAL: {
				AnnotationByAnonymousIndividual annotationByAnonymousIndividual = (AnnotationByAnonymousIndividual)theEObject;
				T result = caseAnnotationByAnonymousIndividual(annotationByAnonymousIndividual);
				if (result == null) result = caseAnnotation(annotationByAnonymousIndividual);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.ANONYMOUS_INDIVIDUAL: {
				AnonymousIndividual anonymousIndividual = (AnonymousIndividual)theEObject;
				T result = caseAnonymousIndividual(anonymousIndividual);
				if (result == null) result = caseIndividual(anonymousIndividual);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.DECLARATION: {
				Declaration declaration = (Declaration)theEObject;
				T result = caseDeclaration(declaration);
				if (result == null) result = caseAxiom(declaration);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.OBJECT_AND_DATA_PROPERTY_AXIOM: {
				ObjectAndDataPropertyAxiom objectAndDataPropertyAxiom = (ObjectAndDataPropertyAxiom)theEObject;
				T result = caseObjectAndDataPropertyAxiom(objectAndDataPropertyAxiom);
				if (result == null) result = caseAxiom(objectAndDataPropertyAxiom);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.KEY_FOR: {
				KeyFor keyFor = (KeyFor)theEObject;
				T result = caseKeyFor(keyFor);
				if (result == null) result = caseObjectAndDataPropertyAxiom(keyFor);
				if (result == null) result = caseAxiom(keyFor);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case OwlPackage.ANONYMOUS_INDIVIDUAL_ANNOTATION: {
				AnonymousIndividualAnnotation anonymousIndividualAnnotation = (AnonymousIndividualAnnotation)theEObject;
				T result = caseAnonymousIndividualAnnotation(anonymousIndividualAnnotation);
				if (result == null) result = caseAxiom(anonymousIndividualAnnotation);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			default: return defaultCase(theEObject);
		}
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Assertion</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Assertion</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAssertion(Assertion object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Axiom</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Axiom</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAxiom(Axiom object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Annotation</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Annotation</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAnnotation(Annotation object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Annotation Property</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Annotation Property</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAnnotationProperty(AnnotationProperty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Entity</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Entity</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseEntity(Entity object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>URI</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>URI</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseURI(URI object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Constant</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Constant</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseConstant(Constant object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Datatype</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Datatype</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDatatype(Datatype object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data Range</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data Range</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataRange(DataRange object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data Property Axiom</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data Property Axiom</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataPropertyAxiom(DataPropertyAxiom object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Property Axiom</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Property Axiom</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectPropertyAxiom(ObjectPropertyAxiom object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Class Expression</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Class Expression</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseClassExpression(ClassExpression object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Class Axiom</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Class Axiom</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseClassAxiom(ClassAxiom object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data Property Expression</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data Property Expression</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataPropertyExpression(DataPropertyExpression object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Property Expression</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Property Expression</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectPropertyExpression(ObjectPropertyExpression object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Asymmetric Object Property</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Asymmetric Object Property</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAsymmetricObjectProperty(AsymmetricObjectProperty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Property</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Property</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectProperty(ObjectProperty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Inverse Object Property</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Inverse Object Property</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseInverseObjectProperty(InverseObjectProperty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Class</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Class</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseClass(carisma.modeltype.owl2.model.owl.Class object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Intersection Of</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Intersection Of</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectIntersectionOf(ObjectIntersectionOf object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Union Of</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Union Of</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectUnionOf(ObjectUnionOf object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object One Of</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object One Of</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectOneOf(ObjectOneOf object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Individual</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Individual</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseIndividual(Individual object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Named Individual</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Named Individual</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseNamedIndividual(NamedIndividual object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Some Values From</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Some Values From</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectSomeValuesFrom(ObjectSomeValuesFrom object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object All Values From</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object All Values From</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectAllValuesFrom(ObjectAllValuesFrom object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Exists Self</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Exists Self</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectExistsSelf(ObjectExistsSelf object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Has Value</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Has Value</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectHasValue(ObjectHasValue object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Min Cardinality</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Min Cardinality</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectMinCardinality(ObjectMinCardinality object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Max Cardinality</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Max Cardinality</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectMaxCardinality(ObjectMaxCardinality object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data Some Values From</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data Some Values From</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataSomeValuesFrom(DataSomeValuesFrom object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data Property</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data Property</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataProperty(DataProperty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data One Of</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data One Of</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataOneOf(DataOneOf object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Datatype Restriction</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Datatype Restriction</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDatatypeRestriction(DatatypeRestriction object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Facet Constant Pair</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Facet Constant Pair</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseFacetConstantPair(FacetConstantPair object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data All Values From</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data All Values From</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataAllValuesFrom(DataAllValuesFrom object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data Has Value</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data Has Value</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataHasValue(DataHasValue object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data Min Cardinality</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data Min Cardinality</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataMinCardinality(DataMinCardinality object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data Max Cardinality</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data Max Cardinality</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataMaxCardinality(DataMaxCardinality object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data Exact Cardinality</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data Exact Cardinality</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataExactCardinality(DataExactCardinality object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Negative Data Property Assertion</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Negative Data Property Assertion</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseNegativeDataPropertyAssertion(NegativeDataPropertyAssertion object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data Property Domain</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data Property Domain</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataPropertyDomain(DataPropertyDomain object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data Property Range</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data Property Range</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataPropertyRange(DataPropertyRange object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Different Individuals</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Different Individuals</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDifferentIndividuals(DifferentIndividuals object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Disjoint Classes</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Disjoint Classes</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDisjointClasses(DisjointClasses object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Disjoint Data Properties</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Disjoint Data Properties</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDisjointDataProperties(DisjointDataProperties object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Disjoint Object Properties</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Disjoint Object Properties</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDisjointObjectProperties(DisjointObjectProperties object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Disjoint Union</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Disjoint Union</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDisjointUnion(DisjointUnion object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Equivalent Classes</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Equivalent Classes</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseEquivalentClasses(EquivalentClasses object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Equivalent Data Properties</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Equivalent Data Properties</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseEquivalentDataProperties(EquivalentDataProperties object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Functional Data Property</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Functional Data Property</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseFunctionalDataProperty(FunctionalDataProperty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Equivalent Object Properties</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Equivalent Object Properties</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseEquivalentObjectProperties(EquivalentObjectProperties object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Functional Object Property</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Functional Object Property</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseFunctionalObjectProperty(FunctionalObjectProperty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Inverse Functional Object Property</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Inverse Functional Object Property</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseInverseFunctionalObjectProperty(InverseFunctionalObjectProperty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Property Assertion</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Property Assertion</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectPropertyAssertion(ObjectPropertyAssertion object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Negative Object Property Assertion</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Negative Object Property Assertion</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseNegativeObjectPropertyAssertion(NegativeObjectPropertyAssertion object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Property Domain</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Property Domain</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectPropertyDomain(ObjectPropertyDomain object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Symmetric Object Property</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Symmetric Object Property</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSymmetricObjectProperty(SymmetricObjectProperty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Reflexive Object Property</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Reflexive Object Property</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseReflexiveObjectProperty(ReflexiveObjectProperty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Sub Data Property Of</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Sub Data Property Of</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSubDataPropertyOf(SubDataPropertyOf object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Same Individual</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Same Individual</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSameIndividual(SameIndividual object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Sub Object Property Of</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Sub Object Property Of</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSubObjectPropertyOf(SubObjectPropertyOf object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Complement Of</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Complement Of</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectComplementOf(ObjectComplementOf object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Ontology</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Ontology</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseOntology(Ontology object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Property Range</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Property Range</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectPropertyRange(ObjectPropertyRange object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data Property Assertion</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data Property Assertion</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataPropertyAssertion(DataPropertyAssertion object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Class Assertion</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Class Assertion</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseClassAssertion(ClassAssertion object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Irreflexive Object Property</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Irreflexive Object Property</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseIrreflexiveObjectProperty(IrreflexiveObjectProperty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object Exact Cardinality</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object Exact Cardinality</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectExactCardinality(ObjectExactCardinality object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Data Complement Of</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Data Complement Of</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDataComplementOf(DataComplementOf object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Sub Class Of</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Sub Class Of</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSubClassOf(SubClassOf object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Sub Object Property</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Sub Object Property</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSubObjectProperty(SubObjectProperty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Transitive Object Property</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Transitive Object Property</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseTransitiveObjectProperty(TransitiveObjectProperty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Entity Annotation</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Entity Annotation</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseEntityAnnotation(EntityAnnotation object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Full URI</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Full URI</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseFullURI(FullURI object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Abbreviated URI</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Abbreviated URI</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAbbreviatedURI(AbbreviatedURI object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Inverse Object Properties</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Inverse Object Properties</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseInverseObjectProperties(InverseObjectProperties object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Annotation By Constant</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Annotation By Constant</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAnnotationByConstant(AnnotationByConstant object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Annotation By Entity</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Annotation By Entity</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAnnotationByEntity(AnnotationByEntity object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Annotation By Anonymous Individual</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Annotation By Anonymous Individual</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAnnotationByAnonymousIndividual(AnnotationByAnonymousIndividual object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Anonymous Individual</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Anonymous Individual</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAnonymousIndividual(AnonymousIndividual object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Declaration</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Declaration</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDeclaration(Declaration object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Object And Data Property Axiom</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Object And Data Property Axiom</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseObjectAndDataPropertyAxiom(ObjectAndDataPropertyAxiom object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Key For</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Key For</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseKeyFor(KeyFor object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Anonymous Individual Annotation</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Anonymous Individual Annotation</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAnonymousIndividualAnnotation(AnonymousIndividualAnnotation object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch, but this is the last case anyway.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	public T defaultCase(EObject object) {
		return null;
	}

} //OwlSwitch
