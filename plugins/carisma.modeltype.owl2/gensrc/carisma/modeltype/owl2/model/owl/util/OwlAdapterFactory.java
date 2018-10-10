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

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;
import org.eclipse.emf.ecore.EObject;

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
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see carisma.modeltype.owl2.model.owl.OwlPackage
 * @generated
 */
public class OwlAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static OwlPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public OwlAdapterFactory() {
		if (modelPackage == null) {
			modelPackage = OwlPackage.eINSTANCE;
		}
	}

	/**
	 * Returns whether this factory is applicable for the type of the object.
	 * <!-- begin-user-doc -->
	 * This implementation returns <code>true</code> if the object is either the model's package or is an instance object of the model.
	 * <!-- end-user-doc -->
	 * @return whether this factory is applicable for the type of the object.
	 * @generated
	 */
	@Override
	public boolean isFactoryForType(Object object) {
		if (object == modelPackage) {
			return true;
		}
		if (object instanceof EObject) {
			return ((EObject)object).eClass().getEPackage() == modelPackage;
		}
		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected OwlSwitch<Adapter> modelSwitch =
		new OwlSwitch<Adapter>() {
			@Override
			public Adapter caseAssertion(Assertion object) {
				return createAssertionAdapter();
			}
			@Override
			public Adapter caseAxiom(Axiom object) {
				return createAxiomAdapter();
			}
			@Override
			public Adapter caseAnnotation(Annotation object) {
				return createAnnotationAdapter();
			}
			@Override
			public Adapter caseAnnotationProperty(AnnotationProperty object) {
				return createAnnotationPropertyAdapter();
			}
			@Override
			public Adapter caseEntity(Entity object) {
				return createEntityAdapter();
			}
			@Override
			public Adapter caseURI(URI object) {
				return createURIAdapter();
			}
			@Override
			public Adapter caseConstant(Constant object) {
				return createConstantAdapter();
			}
			@Override
			public Adapter caseDatatype(Datatype object) {
				return createDatatypeAdapter();
			}
			@Override
			public Adapter caseDataRange(DataRange object) {
				return createDataRangeAdapter();
			}
			@Override
			public Adapter caseDataPropertyAxiom(DataPropertyAxiom object) {
				return createDataPropertyAxiomAdapter();
			}
			@Override
			public Adapter caseObjectPropertyAxiom(ObjectPropertyAxiom object) {
				return createObjectPropertyAxiomAdapter();
			}
			@Override
			public Adapter caseClassExpression(ClassExpression object) {
				return createClassExpressionAdapter();
			}
			@Override
			public Adapter caseClassAxiom(ClassAxiom object) {
				return createClassAxiomAdapter();
			}
			@Override
			public Adapter caseDataPropertyExpression(DataPropertyExpression object) {
				return createDataPropertyExpressionAdapter();
			}
			@Override
			public Adapter caseObjectPropertyExpression(ObjectPropertyExpression object) {
				return createObjectPropertyExpressionAdapter();
			}
			@Override
			public Adapter caseAsymmetricObjectProperty(AsymmetricObjectProperty object) {
				return createAsymmetricObjectPropertyAdapter();
			}
			@Override
			public Adapter caseObjectProperty(ObjectProperty object) {
				return createObjectPropertyAdapter();
			}
			@Override
			public Adapter caseInverseObjectProperty(InverseObjectProperty object) {
				return createInverseObjectPropertyAdapter();
			}
			@Override
			public Adapter caseClass(carisma.modeltype.owl2.model.owl.Class object) {
				return createClassAdapter();
			}
			@Override
			public Adapter caseObjectIntersectionOf(ObjectIntersectionOf object) {
				return createObjectIntersectionOfAdapter();
			}
			@Override
			public Adapter caseObjectUnionOf(ObjectUnionOf object) {
				return createObjectUnionOfAdapter();
			}
			@Override
			public Adapter caseObjectOneOf(ObjectOneOf object) {
				return createObjectOneOfAdapter();
			}
			@Override
			public Adapter caseIndividual(Individual object) {
				return createIndividualAdapter();
			}
			@Override
			public Adapter caseNamedIndividual(NamedIndividual object) {
				return createNamedIndividualAdapter();
			}
			@Override
			public Adapter caseObjectSomeValuesFrom(ObjectSomeValuesFrom object) {
				return createObjectSomeValuesFromAdapter();
			}
			@Override
			public Adapter caseObjectAllValuesFrom(ObjectAllValuesFrom object) {
				return createObjectAllValuesFromAdapter();
			}
			@Override
			public Adapter caseObjectExistsSelf(ObjectExistsSelf object) {
				return createObjectExistsSelfAdapter();
			}
			@Override
			public Adapter caseObjectHasValue(ObjectHasValue object) {
				return createObjectHasValueAdapter();
			}
			@Override
			public Adapter caseObjectMinCardinality(ObjectMinCardinality object) {
				return createObjectMinCardinalityAdapter();
			}
			@Override
			public Adapter caseObjectMaxCardinality(ObjectMaxCardinality object) {
				return createObjectMaxCardinalityAdapter();
			}
			@Override
			public Adapter caseDataSomeValuesFrom(DataSomeValuesFrom object) {
				return createDataSomeValuesFromAdapter();
			}
			@Override
			public Adapter caseDataProperty(DataProperty object) {
				return createDataPropertyAdapter();
			}
			@Override
			public Adapter caseDataOneOf(DataOneOf object) {
				return createDataOneOfAdapter();
			}
			@Override
			public Adapter caseDatatypeRestriction(DatatypeRestriction object) {
				return createDatatypeRestrictionAdapter();
			}
			@Override
			public Adapter caseFacetConstantPair(FacetConstantPair object) {
				return createFacetConstantPairAdapter();
			}
			@Override
			public Adapter caseDataAllValuesFrom(DataAllValuesFrom object) {
				return createDataAllValuesFromAdapter();
			}
			@Override
			public Adapter caseDataHasValue(DataHasValue object) {
				return createDataHasValueAdapter();
			}
			@Override
			public Adapter caseDataMinCardinality(DataMinCardinality object) {
				return createDataMinCardinalityAdapter();
			}
			@Override
			public Adapter caseDataMaxCardinality(DataMaxCardinality object) {
				return createDataMaxCardinalityAdapter();
			}
			@Override
			public Adapter caseDataExactCardinality(DataExactCardinality object) {
				return createDataExactCardinalityAdapter();
			}
			@Override
			public Adapter caseNegativeDataPropertyAssertion(NegativeDataPropertyAssertion object) {
				return createNegativeDataPropertyAssertionAdapter();
			}
			@Override
			public Adapter caseDataPropertyDomain(DataPropertyDomain object) {
				return createDataPropertyDomainAdapter();
			}
			@Override
			public Adapter caseDataPropertyRange(DataPropertyRange object) {
				return createDataPropertyRangeAdapter();
			}
			@Override
			public Adapter caseDifferentIndividuals(DifferentIndividuals object) {
				return createDifferentIndividualsAdapter();
			}
			@Override
			public Adapter caseDisjointClasses(DisjointClasses object) {
				return createDisjointClassesAdapter();
			}
			@Override
			public Adapter caseDisjointDataProperties(DisjointDataProperties object) {
				return createDisjointDataPropertiesAdapter();
			}
			@Override
			public Adapter caseDisjointObjectProperties(DisjointObjectProperties object) {
				return createDisjointObjectPropertiesAdapter();
			}
			@Override
			public Adapter caseDisjointUnion(DisjointUnion object) {
				return createDisjointUnionAdapter();
			}
			@Override
			public Adapter caseEquivalentClasses(EquivalentClasses object) {
				return createEquivalentClassesAdapter();
			}
			@Override
			public Adapter caseEquivalentDataProperties(EquivalentDataProperties object) {
				return createEquivalentDataPropertiesAdapter();
			}
			@Override
			public Adapter caseFunctionalDataProperty(FunctionalDataProperty object) {
				return createFunctionalDataPropertyAdapter();
			}
			@Override
			public Adapter caseEquivalentObjectProperties(EquivalentObjectProperties object) {
				return createEquivalentObjectPropertiesAdapter();
			}
			@Override
			public Adapter caseFunctionalObjectProperty(FunctionalObjectProperty object) {
				return createFunctionalObjectPropertyAdapter();
			}
			@Override
			public Adapter caseInverseFunctionalObjectProperty(InverseFunctionalObjectProperty object) {
				return createInverseFunctionalObjectPropertyAdapter();
			}
			@Override
			public Adapter caseObjectPropertyAssertion(ObjectPropertyAssertion object) {
				return createObjectPropertyAssertionAdapter();
			}
			@Override
			public Adapter caseNegativeObjectPropertyAssertion(NegativeObjectPropertyAssertion object) {
				return createNegativeObjectPropertyAssertionAdapter();
			}
			@Override
			public Adapter caseObjectPropertyDomain(ObjectPropertyDomain object) {
				return createObjectPropertyDomainAdapter();
			}
			@Override
			public Adapter caseSymmetricObjectProperty(SymmetricObjectProperty object) {
				return createSymmetricObjectPropertyAdapter();
			}
			@Override
			public Adapter caseReflexiveObjectProperty(ReflexiveObjectProperty object) {
				return createReflexiveObjectPropertyAdapter();
			}
			@Override
			public Adapter caseSubDataPropertyOf(SubDataPropertyOf object) {
				return createSubDataPropertyOfAdapter();
			}
			@Override
			public Adapter caseSameIndividual(SameIndividual object) {
				return createSameIndividualAdapter();
			}
			@Override
			public Adapter caseSubObjectPropertyOf(SubObjectPropertyOf object) {
				return createSubObjectPropertyOfAdapter();
			}
			@Override
			public Adapter caseObjectComplementOf(ObjectComplementOf object) {
				return createObjectComplementOfAdapter();
			}
			@Override
			public Adapter caseOntology(Ontology object) {
				return createOntologyAdapter();
			}
			@Override
			public Adapter caseObjectPropertyRange(ObjectPropertyRange object) {
				return createObjectPropertyRangeAdapter();
			}
			@Override
			public Adapter caseDataPropertyAssertion(DataPropertyAssertion object) {
				return createDataPropertyAssertionAdapter();
			}
			@Override
			public Adapter caseClassAssertion(ClassAssertion object) {
				return createClassAssertionAdapter();
			}
			@Override
			public Adapter caseIrreflexiveObjectProperty(IrreflexiveObjectProperty object) {
				return createIrreflexiveObjectPropertyAdapter();
			}
			@Override
			public Adapter caseObjectExactCardinality(ObjectExactCardinality object) {
				return createObjectExactCardinalityAdapter();
			}
			@Override
			public Adapter caseDataComplementOf(DataComplementOf object) {
				return createDataComplementOfAdapter();
			}
			@Override
			public Adapter caseSubClassOf(SubClassOf object) {
				return createSubClassOfAdapter();
			}
			@Override
			public Adapter caseSubObjectProperty(SubObjectProperty object) {
				return createSubObjectPropertyAdapter();
			}
			@Override
			public Adapter caseTransitiveObjectProperty(TransitiveObjectProperty object) {
				return createTransitiveObjectPropertyAdapter();
			}
			@Override
			public Adapter caseEntityAnnotation(EntityAnnotation object) {
				return createEntityAnnotationAdapter();
			}
			@Override
			public Adapter caseFullURI(FullURI object) {
				return createFullURIAdapter();
			}
			@Override
			public Adapter caseAbbreviatedURI(AbbreviatedURI object) {
				return createAbbreviatedURIAdapter();
			}
			@Override
			public Adapter caseInverseObjectProperties(InverseObjectProperties object) {
				return createInverseObjectPropertiesAdapter();
			}
			@Override
			public Adapter caseAnnotationByConstant(AnnotationByConstant object) {
				return createAnnotationByConstantAdapter();
			}
			@Override
			public Adapter caseAnnotationByEntity(AnnotationByEntity object) {
				return createAnnotationByEntityAdapter();
			}
			@Override
			public Adapter caseAnnotationByAnonymousIndividual(AnnotationByAnonymousIndividual object) {
				return createAnnotationByAnonymousIndividualAdapter();
			}
			@Override
			public Adapter caseAnonymousIndividual(AnonymousIndividual object) {
				return createAnonymousIndividualAdapter();
			}
			@Override
			public Adapter caseDeclaration(Declaration object) {
				return createDeclarationAdapter();
			}
			@Override
			public Adapter caseObjectAndDataPropertyAxiom(ObjectAndDataPropertyAxiom object) {
				return createObjectAndDataPropertyAxiomAdapter();
			}
			@Override
			public Adapter caseKeyFor(KeyFor object) {
				return createKeyForAdapter();
			}
			@Override
			public Adapter caseAnonymousIndividualAnnotation(AnonymousIndividualAnnotation object) {
				return createAnonymousIndividualAnnotationAdapter();
			}
			@Override
			public Adapter defaultCase(EObject object) {
				return createEObjectAdapter();
			}
		};

	/**
	 * Creates an adapter for the <code>target</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param target the object to adapt.
	 * @return the adapter for the <code>target</code>.
	 * @generated
	 */
	@Override
	public Adapter createAdapter(Notifier target) {
		return modelSwitch.doSwitch((EObject)target);
	}


	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.Assertion <em>Assertion</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.Assertion
	 * @generated
	 */
	public Adapter createAssertionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.Axiom <em>Axiom</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.Axiom
	 * @generated
	 */
	public Adapter createAxiomAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.Annotation <em>Annotation</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.Annotation
	 * @generated
	 */
	public Adapter createAnnotationAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.AnnotationProperty <em>Annotation Property</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.AnnotationProperty
	 * @generated
	 */
	public Adapter createAnnotationPropertyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.Entity <em>Entity</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.Entity
	 * @generated
	 */
	public Adapter createEntityAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.URI <em>URI</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.URI
	 * @generated
	 */
	public Adapter createURIAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.Constant <em>Constant</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.Constant
	 * @generated
	 */
	public Adapter createConstantAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.Datatype <em>Datatype</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.Datatype
	 * @generated
	 */
	public Adapter createDatatypeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataRange <em>Data Range</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataRange
	 * @generated
	 */
	public Adapter createDataRangeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataPropertyAxiom <em>Data Property Axiom</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyAxiom
	 * @generated
	 */
	public Adapter createDataPropertyAxiomAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyAxiom <em>Object Property Axiom</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyAxiom
	 * @generated
	 */
	public Adapter createObjectPropertyAxiomAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ClassExpression <em>Class Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ClassExpression
	 * @generated
	 */
	public Adapter createClassExpressionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ClassAxiom <em>Class Axiom</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ClassAxiom
	 * @generated
	 */
	public Adapter createClassAxiomAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataPropertyExpression <em>Data Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyExpression
	 * @generated
	 */
	public Adapter createDataPropertyExpressionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyExpression
	 * @generated
	 */
	public Adapter createObjectPropertyExpressionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.AsymmetricObjectProperty <em>Asymmetric Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.AsymmetricObjectProperty
	 * @generated
	 */
	public Adapter createAsymmetricObjectPropertyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectProperty <em>Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectProperty
	 * @generated
	 */
	public Adapter createObjectPropertyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.InverseObjectProperty <em>Inverse Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.InverseObjectProperty
	 * @generated
	 */
	public Adapter createInverseObjectPropertyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.Class <em>Class</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.Class
	 * @generated
	 */
	public Adapter createClassAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectIntersectionOf <em>Object Intersection Of</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectIntersectionOf
	 * @generated
	 */
	public Adapter createObjectIntersectionOfAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectUnionOf <em>Object Union Of</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectUnionOf
	 * @generated
	 */
	public Adapter createObjectUnionOfAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectOneOf <em>Object One Of</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectOneOf
	 * @generated
	 */
	public Adapter createObjectOneOfAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.Individual <em>Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.Individual
	 * @generated
	 */
	public Adapter createIndividualAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.NamedIndividual <em>Named Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.NamedIndividual
	 * @generated
	 */
	public Adapter createNamedIndividualAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom <em>Object Some Values From</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom
	 * @generated
	 */
	public Adapter createObjectSomeValuesFromAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectAllValuesFrom <em>Object All Values From</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectAllValuesFrom
	 * @generated
	 */
	public Adapter createObjectAllValuesFromAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectExistsSelf <em>Object Exists Self</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectExistsSelf
	 * @generated
	 */
	public Adapter createObjectExistsSelfAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectHasValue <em>Object Has Value</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectHasValue
	 * @generated
	 */
	public Adapter createObjectHasValueAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectMinCardinality <em>Object Min Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectMinCardinality
	 * @generated
	 */
	public Adapter createObjectMinCardinalityAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectMaxCardinality <em>Object Max Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectMaxCardinality
	 * @generated
	 */
	public Adapter createObjectMaxCardinalityAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataSomeValuesFrom <em>Data Some Values From</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataSomeValuesFrom
	 * @generated
	 */
	public Adapter createDataSomeValuesFromAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataProperty <em>Data Property</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataProperty
	 * @generated
	 */
	public Adapter createDataPropertyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataOneOf <em>Data One Of</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataOneOf
	 * @generated
	 */
	public Adapter createDataOneOfAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DatatypeRestriction <em>Datatype Restriction</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DatatypeRestriction
	 * @generated
	 */
	public Adapter createDatatypeRestrictionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.FacetConstantPair <em>Facet Constant Pair</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.FacetConstantPair
	 * @generated
	 */
	public Adapter createFacetConstantPairAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataAllValuesFrom <em>Data All Values From</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataAllValuesFrom
	 * @generated
	 */
	public Adapter createDataAllValuesFromAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataHasValue <em>Data Has Value</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataHasValue
	 * @generated
	 */
	public Adapter createDataHasValueAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataMinCardinality <em>Data Min Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataMinCardinality
	 * @generated
	 */
	public Adapter createDataMinCardinalityAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataMaxCardinality <em>Data Max Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataMaxCardinality
	 * @generated
	 */
	public Adapter createDataMaxCardinalityAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataExactCardinality <em>Data Exact Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataExactCardinality
	 * @generated
	 */
	public Adapter createDataExactCardinalityAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.NegativeDataPropertyAssertion <em>Negative Data Property Assertion</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.NegativeDataPropertyAssertion
	 * @generated
	 */
	public Adapter createNegativeDataPropertyAssertionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataPropertyDomain <em>Data Property Domain</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyDomain
	 * @generated
	 */
	public Adapter createDataPropertyDomainAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataPropertyRange <em>Data Property Range</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyRange
	 * @generated
	 */
	public Adapter createDataPropertyRangeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DifferentIndividuals <em>Different Individuals</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DifferentIndividuals
	 * @generated
	 */
	public Adapter createDifferentIndividualsAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DisjointClasses <em>Disjoint Classes</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DisjointClasses
	 * @generated
	 */
	public Adapter createDisjointClassesAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DisjointDataProperties <em>Disjoint Data Properties</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DisjointDataProperties
	 * @generated
	 */
	public Adapter createDisjointDataPropertiesAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DisjointObjectProperties <em>Disjoint Object Properties</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DisjointObjectProperties
	 * @generated
	 */
	public Adapter createDisjointObjectPropertiesAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DisjointUnion <em>Disjoint Union</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DisjointUnion
	 * @generated
	 */
	public Adapter createDisjointUnionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.EquivalentClasses <em>Equivalent Classes</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.EquivalentClasses
	 * @generated
	 */
	public Adapter createEquivalentClassesAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.EquivalentDataProperties <em>Equivalent Data Properties</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.EquivalentDataProperties
	 * @generated
	 */
	public Adapter createEquivalentDataPropertiesAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.FunctionalDataProperty <em>Functional Data Property</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.FunctionalDataProperty
	 * @generated
	 */
	public Adapter createFunctionalDataPropertyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.EquivalentObjectProperties <em>Equivalent Object Properties</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.EquivalentObjectProperties
	 * @generated
	 */
	public Adapter createEquivalentObjectPropertiesAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.FunctionalObjectProperty <em>Functional Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.FunctionalObjectProperty
	 * @generated
	 */
	public Adapter createFunctionalObjectPropertyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.InverseFunctionalObjectProperty <em>Inverse Functional Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.InverseFunctionalObjectProperty
	 * @generated
	 */
	public Adapter createInverseFunctionalObjectPropertyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyAssertion <em>Object Property Assertion</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyAssertion
	 * @generated
	 */
	public Adapter createObjectPropertyAssertionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.NegativeObjectPropertyAssertion <em>Negative Object Property Assertion</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.NegativeObjectPropertyAssertion
	 * @generated
	 */
	public Adapter createNegativeObjectPropertyAssertionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyDomain <em>Object Property Domain</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyDomain
	 * @generated
	 */
	public Adapter createObjectPropertyDomainAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.SymmetricObjectProperty <em>Symmetric Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.SymmetricObjectProperty
	 * @generated
	 */
	public Adapter createSymmetricObjectPropertyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ReflexiveObjectProperty <em>Reflexive Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ReflexiveObjectProperty
	 * @generated
	 */
	public Adapter createReflexiveObjectPropertyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.SubDataPropertyOf <em>Sub Data Property Of</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.SubDataPropertyOf
	 * @generated
	 */
	public Adapter createSubDataPropertyOfAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.SameIndividual <em>Same Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.SameIndividual
	 * @generated
	 */
	public Adapter createSameIndividualAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.SubObjectPropertyOf <em>Sub Object Property Of</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.SubObjectPropertyOf
	 * @generated
	 */
	public Adapter createSubObjectPropertyOfAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectComplementOf <em>Object Complement Of</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectComplementOf
	 * @generated
	 */
	public Adapter createObjectComplementOfAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.Ontology <em>Ontology</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.Ontology
	 * @generated
	 */
	public Adapter createOntologyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyRange <em>Object Property Range</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyRange
	 * @generated
	 */
	public Adapter createObjectPropertyRangeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataPropertyAssertion <em>Data Property Assertion</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyAssertion
	 * @generated
	 */
	public Adapter createDataPropertyAssertionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ClassAssertion <em>Class Assertion</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ClassAssertion
	 * @generated
	 */
	public Adapter createClassAssertionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.IrreflexiveObjectProperty <em>Irreflexive Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.IrreflexiveObjectProperty
	 * @generated
	 */
	public Adapter createIrreflexiveObjectPropertyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectExactCardinality <em>Object Exact Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectExactCardinality
	 * @generated
	 */
	public Adapter createObjectExactCardinalityAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.DataComplementOf <em>Data Complement Of</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.DataComplementOf
	 * @generated
	 */
	public Adapter createDataComplementOfAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.SubClassOf <em>Sub Class Of</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.SubClassOf
	 * @generated
	 */
	public Adapter createSubClassOfAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.SubObjectProperty <em>Sub Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.SubObjectProperty
	 * @generated
	 */
	public Adapter createSubObjectPropertyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.TransitiveObjectProperty <em>Transitive Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.TransitiveObjectProperty
	 * @generated
	 */
	public Adapter createTransitiveObjectPropertyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.EntityAnnotation <em>Entity Annotation</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.EntityAnnotation
	 * @generated
	 */
	public Adapter createEntityAnnotationAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.FullURI <em>Full URI</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.FullURI
	 * @generated
	 */
	public Adapter createFullURIAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.AbbreviatedURI <em>Abbreviated URI</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.AbbreviatedURI
	 * @generated
	 */
	public Adapter createAbbreviatedURIAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.InverseObjectProperties <em>Inverse Object Properties</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.InverseObjectProperties
	 * @generated
	 */
	public Adapter createInverseObjectPropertiesAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.AnnotationByConstant <em>Annotation By Constant</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.AnnotationByConstant
	 * @generated
	 */
	public Adapter createAnnotationByConstantAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.AnnotationByEntity <em>Annotation By Entity</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.AnnotationByEntity
	 * @generated
	 */
	public Adapter createAnnotationByEntityAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.AnnotationByAnonymousIndividual <em>Annotation By Anonymous Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.AnnotationByAnonymousIndividual
	 * @generated
	 */
	public Adapter createAnnotationByAnonymousIndividualAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.AnonymousIndividual <em>Anonymous Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.AnonymousIndividual
	 * @generated
	 */
	public Adapter createAnonymousIndividualAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.Declaration <em>Declaration</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.Declaration
	 * @generated
	 */
	public Adapter createDeclarationAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.ObjectAndDataPropertyAxiom <em>Object And Data Property Axiom</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.ObjectAndDataPropertyAxiom
	 * @generated
	 */
	public Adapter createObjectAndDataPropertyAxiomAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.KeyFor <em>Key For</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.KeyFor
	 * @generated
	 */
	public Adapter createKeyForAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.modeltype.owl2.model.owl.AnonymousIndividualAnnotation <em>Anonymous Individual Annotation</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.modeltype.owl2.model.owl.AnonymousIndividualAnnotation
	 * @generated
	 */
	public Adapter createAnonymousIndividualAnnotationAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for the default case.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @generated
	 */
	public Adapter createEObjectAdapter() {
		return null;
	}

} //OwlAdapterFactory
