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
package carisma.modeltype.owl2.model.owl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see carisma.modeltype.owl2.model.owl.OwlFactory
 * @model kind="package"
 * @generated
 */
public interface OwlPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "owl";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http:///owl.ecore";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "owl";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	OwlPackage eINSTANCE = carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl.init();

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.AxiomImpl <em>Axiom</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.AxiomImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAxiom()
	 * @generated
	 */
	int AXIOM = 1;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AXIOM__AXIOM_ANNOTATIONS = 0;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AXIOM__AXIOM_ID = 1;

	/**
	 * The number of structural features of the '<em>Axiom</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AXIOM_FEATURE_COUNT = 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.AssertionImpl <em>Assertion</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.AssertionImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAssertion()
	 * @generated
	 */
	int ASSERTION = 0;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSERTION__AXIOM_ANNOTATIONS = AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSERTION__AXIOM_ID = AXIOM__AXIOM_ID;

	/**
	 * The number of structural features of the '<em>Assertion</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSERTION_FEATURE_COUNT = AXIOM_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.AnnotationImpl <em>Annotation</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.AnnotationImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnnotation()
	 * @generated
	 */
	int ANNOTATION = 2;

	/**
	 * The feature id for the '<em><b>Annotation Property</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION__ANNOTATION_PROPERTY = 0;

	/**
	 * The number of structural features of the '<em>Annotation</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.EntityImpl <em>Entity</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.EntityImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getEntity()
	 * @generated
	 */
	int ENTITY = 4;

	/**
	 * The feature id for the '<em><b>Entity URI</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENTITY__ENTITY_URI = 0;

	/**
	 * The number of structural features of the '<em>Entity</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENTITY_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.AnnotationPropertyImpl <em>Annotation Property</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.AnnotationPropertyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnnotationProperty()
	 * @generated
	 */
	int ANNOTATION_PROPERTY = 3;

	/**
	 * The feature id for the '<em><b>Entity URI</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_PROPERTY__ENTITY_URI = ENTITY__ENTITY_URI;

	/**
	 * The number of structural features of the '<em>Annotation Property</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_PROPERTY_FEATURE_COUNT = ENTITY_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.URIImpl <em>URI</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.URIImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getURI()
	 * @generated
	 */
	int URI = 5;

	/**
	 * The feature id for the '<em><b>Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int URI__VALUE = 0;

	/**
	 * The number of structural features of the '<em>URI</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int URI_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ConstantImpl <em>Constant</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ConstantImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getConstant()
	 * @generated
	 */
	int CONSTANT = 6;

	/**
	 * The feature id for the '<em><b>Lexical Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTANT__LEXICAL_VALUE = 0;

	/**
	 * The feature id for the '<em><b>Datatype</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTANT__DATATYPE = 1;

	/**
	 * The number of structural features of the '<em>Constant</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTANT_FEATURE_COUNT = 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataRangeImpl <em>Data Range</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataRangeImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataRange()
	 * @generated
	 */
	int DATA_RANGE = 8;

	/**
	 * The feature id for the '<em><b>Arity</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_RANGE__ARITY = 0;

	/**
	 * The number of structural features of the '<em>Data Range</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_RANGE_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DatatypeImpl <em>Datatype</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DatatypeImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDatatype()
	 * @generated
	 */
	int DATATYPE = 7;

	/**
	 * The feature id for the '<em><b>Arity</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATYPE__ARITY = DATA_RANGE__ARITY;

	/**
	 * The feature id for the '<em><b>Entity URI</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATYPE__ENTITY_URI = DATA_RANGE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Datatype</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATYPE_FEATURE_COUNT = DATA_RANGE_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyAxiomImpl <em>Data Property Axiom</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataPropertyAxiomImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataPropertyAxiom()
	 * @generated
	 */
	int DATA_PROPERTY_AXIOM = 9;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_AXIOM__AXIOM_ANNOTATIONS = AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_AXIOM__AXIOM_ID = AXIOM__AXIOM_ID;

	/**
	 * The number of structural features of the '<em>Data Property Axiom</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_AXIOM_FEATURE_COUNT = AXIOM_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectPropertyAxiomImpl <em>Object Property Axiom</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectPropertyAxiomImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectPropertyAxiom()
	 * @generated
	 */
	int OBJECT_PROPERTY_AXIOM = 10;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS = AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_AXIOM__AXIOM_ID = AXIOM__AXIOM_ID;

	/**
	 * The number of structural features of the '<em>Object Property Axiom</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_AXIOM_FEATURE_COUNT = AXIOM_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ClassExpressionImpl <em>Class Expression</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ClassExpressionImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getClassExpression()
	 * @generated
	 */
	int CLASS_EXPRESSION = 11;

	/**
	 * The number of structural features of the '<em>Class Expression</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_EXPRESSION_FEATURE_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ClassAxiomImpl <em>Class Axiom</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ClassAxiomImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getClassAxiom()
	 * @generated
	 */
	int CLASS_AXIOM = 12;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_AXIOM__AXIOM_ANNOTATIONS = AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_AXIOM__AXIOM_ID = AXIOM__AXIOM_ID;

	/**
	 * The number of structural features of the '<em>Class Axiom</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_AXIOM_FEATURE_COUNT = AXIOM_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyExpressionImpl <em>Data Property Expression</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataPropertyExpressionImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataPropertyExpression()
	 * @generated
	 */
	int DATA_PROPERTY_EXPRESSION = 13;

	/**
	 * The number of structural features of the '<em>Data Property Expression</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_EXPRESSION_FEATURE_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectPropertyExpressionImpl <em>Object Property Expression</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectPropertyExpressionImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectPropertyExpression()
	 * @generated
	 */
	int OBJECT_PROPERTY_EXPRESSION = 14;

	/**
	 * The number of structural features of the '<em>Object Property Expression</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_EXPRESSION_FEATURE_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.AsymmetricObjectPropertyImpl <em>Asymmetric Object Property</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.AsymmetricObjectPropertyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAsymmetricObjectProperty()
	 * @generated
	 */
	int ASYMMETRIC_OBJECT_PROPERTY = 15;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASYMMETRIC_OBJECT_PROPERTY__AXIOM_ANNOTATIONS = OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASYMMETRIC_OBJECT_PROPERTY__AXIOM_ID = OBJECT_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASYMMETRIC_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Asymmetric Object Property</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASYMMETRIC_OBJECT_PROPERTY_FEATURE_COUNT = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectPropertyImpl <em>Object Property</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectPropertyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectProperty()
	 * @generated
	 */
	int OBJECT_PROPERTY = 16;

	/**
	 * The feature id for the '<em><b>Entity URI</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY__ENTITY_URI = ENTITY__ENTITY_URI;

	/**
	 * The number of structural features of the '<em>Object Property</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_FEATURE_COUNT = ENTITY_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.InverseObjectPropertyImpl <em>Inverse Object Property</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.InverseObjectPropertyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getInverseObjectProperty()
	 * @generated
	 */
	int INVERSE_OBJECT_PROPERTY = 17;

	/**
	 * The feature id for the '<em><b>Object Property</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INVERSE_OBJECT_PROPERTY__OBJECT_PROPERTY = OBJECT_PROPERTY_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Inverse Object Property</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INVERSE_OBJECT_PROPERTY_FEATURE_COUNT = OBJECT_PROPERTY_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ClassImpl <em>Class</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ClassImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getClass_()
	 * @generated
	 */
	int CLASS = 18;

	/**
	 * The feature id for the '<em><b>Entity URI</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS__ENTITY_URI = ENTITY__ENTITY_URI;

	/**
	 * The number of structural features of the '<em>Class</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_FEATURE_COUNT = ENTITY_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectIntersectionOfImpl <em>Object Intersection Of</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectIntersectionOfImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectIntersectionOf()
	 * @generated
	 */
	int OBJECT_INTERSECTION_OF = 19;

	/**
	 * The feature id for the '<em><b>Class Expressions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_INTERSECTION_OF__CLASS_EXPRESSIONS = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Object Intersection Of</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_INTERSECTION_OF_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectUnionOfImpl <em>Object Union Of</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectUnionOfImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectUnionOf()
	 * @generated
	 */
	int OBJECT_UNION_OF = 20;

	/**
	 * The feature id for the '<em><b>Class Expressions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_UNION_OF__CLASS_EXPRESSIONS = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Object Union Of</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_UNION_OF_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectOneOfImpl <em>Object One Of</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectOneOfImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectOneOf()
	 * @generated
	 */
	int OBJECT_ONE_OF = 21;

	/**
	 * The feature id for the '<em><b>Individuals</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_ONE_OF__INDIVIDUALS = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Object One Of</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_ONE_OF_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.IndividualImpl <em>Individual</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.IndividualImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getIndividual()
	 * @generated
	 */
	int INDIVIDUAL = 22;

	/**
	 * The number of structural features of the '<em>Individual</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INDIVIDUAL_FEATURE_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.NamedIndividualImpl <em>Named Individual</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.NamedIndividualImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getNamedIndividual()
	 * @generated
	 */
	int NAMED_INDIVIDUAL = 23;

	/**
	 * The feature id for the '<em><b>Entity URI</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NAMED_INDIVIDUAL__ENTITY_URI = ENTITY__ENTITY_URI;

	/**
	 * The number of structural features of the '<em>Named Individual</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NAMED_INDIVIDUAL_FEATURE_COUNT = ENTITY_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectSomeValuesFromImpl <em>Object Some Values From</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectSomeValuesFromImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectSomeValuesFrom()
	 * @generated
	 */
	int OBJECT_SOME_VALUES_FROM = 24;

	/**
	 * The feature id for the '<em><b>Class Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_SOME_VALUES_FROM__CLASS_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_SOME_VALUES_FROM__OBJECT_PROPERTY_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Object Some Values From</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_SOME_VALUES_FROM_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectAllValuesFromImpl <em>Object All Values From</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectAllValuesFromImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectAllValuesFrom()
	 * @generated
	 */
	int OBJECT_ALL_VALUES_FROM = 25;

	/**
	 * The feature id for the '<em><b>Class Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_ALL_VALUES_FROM__CLASS_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_ALL_VALUES_FROM__OBJECT_PROPERTY_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Object All Values From</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_ALL_VALUES_FROM_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectExistsSelfImpl <em>Object Exists Self</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectExistsSelfImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectExistsSelf()
	 * @generated
	 */
	int OBJECT_EXISTS_SELF = 26;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_EXISTS_SELF__OBJECT_PROPERTY_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Object Exists Self</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_EXISTS_SELF_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectHasValueImpl <em>Object Has Value</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectHasValueImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectHasValue()
	 * @generated
	 */
	int OBJECT_HAS_VALUE = 27;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_HAS_VALUE__OBJECT_PROPERTY_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Individual</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_HAS_VALUE__INDIVIDUAL = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Object Has Value</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_HAS_VALUE_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectMinCardinalityImpl <em>Object Min Cardinality</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectMinCardinalityImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectMinCardinality()
	 * @generated
	 */
	int OBJECT_MIN_CARDINALITY = 28;

	/**
	 * The feature id for the '<em><b>Cardinality</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_MIN_CARDINALITY__CARDINALITY = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Class Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_MIN_CARDINALITY__CLASS_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_MIN_CARDINALITY__OBJECT_PROPERTY_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Object Min Cardinality</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_MIN_CARDINALITY_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectMaxCardinalityImpl <em>Object Max Cardinality</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectMaxCardinalityImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectMaxCardinality()
	 * @generated
	 */
	int OBJECT_MAX_CARDINALITY = 29;

	/**
	 * The feature id for the '<em><b>Cardinality</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_MAX_CARDINALITY__CARDINALITY = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Class Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_MAX_CARDINALITY__CLASS_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_MAX_CARDINALITY__OBJECT_PROPERTY_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Object Max Cardinality</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_MAX_CARDINALITY_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataSomeValuesFromImpl <em>Data Some Values From</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataSomeValuesFromImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataSomeValuesFrom()
	 * @generated
	 */
	int DATA_SOME_VALUES_FROM = 30;

	/**
	 * The feature id for the '<em><b>Data Range</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_SOME_VALUES_FROM__DATA_RANGE = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Data Property Expressions</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_SOME_VALUES_FROM__DATA_PROPERTY_EXPRESSIONS = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Data Some Values From</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_SOME_VALUES_FROM_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyImpl <em>Data Property</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataPropertyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataProperty()
	 * @generated
	 */
	int DATA_PROPERTY = 31;

	/**
	 * The feature id for the '<em><b>Entity URI</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY__ENTITY_URI = ENTITY__ENTITY_URI;

	/**
	 * The number of structural features of the '<em>Data Property</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_FEATURE_COUNT = ENTITY_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataOneOfImpl <em>Data One Of</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataOneOfImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataOneOf()
	 * @generated
	 */
	int DATA_ONE_OF = 32;

	/**
	 * The feature id for the '<em><b>Arity</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_ONE_OF__ARITY = DATA_RANGE__ARITY;

	/**
	 * The feature id for the '<em><b>Constants</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_ONE_OF__CONSTANTS = DATA_RANGE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Data One Of</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_ONE_OF_FEATURE_COUNT = DATA_RANGE_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DatatypeRestrictionImpl <em>Datatype Restriction</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DatatypeRestrictionImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDatatypeRestriction()
	 * @generated
	 */
	int DATATYPE_RESTRICTION = 33;

	/**
	 * The feature id for the '<em><b>Arity</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATYPE_RESTRICTION__ARITY = DATA_RANGE__ARITY;

	/**
	 * The feature id for the '<em><b>Datatype</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATYPE_RESTRICTION__DATATYPE = DATA_RANGE_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Restrictions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATYPE_RESTRICTION__RESTRICTIONS = DATA_RANGE_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Datatype Restriction</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATYPE_RESTRICTION_FEATURE_COUNT = DATA_RANGE_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.FacetConstantPairImpl <em>Facet Constant Pair</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.FacetConstantPairImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getFacetConstantPair()
	 * @generated
	 */
	int FACET_CONSTANT_PAIR = 34;

	/**
	 * The feature id for the '<em><b>Constant</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FACET_CONSTANT_PAIR__CONSTANT = 0;

	/**
	 * The feature id for the '<em><b>Facet</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FACET_CONSTANT_PAIR__FACET = 1;

	/**
	 * The number of structural features of the '<em>Facet Constant Pair</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FACET_CONSTANT_PAIR_FEATURE_COUNT = 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataAllValuesFromImpl <em>Data All Values From</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataAllValuesFromImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataAllValuesFrom()
	 * @generated
	 */
	int DATA_ALL_VALUES_FROM = 35;

	/**
	 * The feature id for the '<em><b>Data Range</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_ALL_VALUES_FROM__DATA_RANGE = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Data Property Expressions</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_ALL_VALUES_FROM__DATA_PROPERTY_EXPRESSIONS = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Data All Values From</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_ALL_VALUES_FROM_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataHasValueImpl <em>Data Has Value</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataHasValueImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataHasValue()
	 * @generated
	 */
	int DATA_HAS_VALUE = 36;

	/**
	 * The feature id for the '<em><b>Constant</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_HAS_VALUE__CONSTANT = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Data Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_HAS_VALUE__DATA_PROPERTY_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Data Has Value</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_HAS_VALUE_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataMinCardinalityImpl <em>Data Min Cardinality</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataMinCardinalityImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataMinCardinality()
	 * @generated
	 */
	int DATA_MIN_CARDINALITY = 37;

	/**
	 * The feature id for the '<em><b>Cardinality</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_MIN_CARDINALITY__CARDINALITY = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Data Range</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_MIN_CARDINALITY__DATA_RANGE = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Data Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_MIN_CARDINALITY__DATA_PROPERTY_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Data Min Cardinality</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_MIN_CARDINALITY_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataMaxCardinalityImpl <em>Data Max Cardinality</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataMaxCardinalityImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataMaxCardinality()
	 * @generated
	 */
	int DATA_MAX_CARDINALITY = 38;

	/**
	 * The feature id for the '<em><b>Cardinality</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_MAX_CARDINALITY__CARDINALITY = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Data Range</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_MAX_CARDINALITY__DATA_RANGE = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Data Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_MAX_CARDINALITY__DATA_PROPERTY_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Data Max Cardinality</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_MAX_CARDINALITY_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataExactCardinalityImpl <em>Data Exact Cardinality</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataExactCardinalityImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataExactCardinality()
	 * @generated
	 */
	int DATA_EXACT_CARDINALITY = 39;

	/**
	 * The feature id for the '<em><b>Cardinality</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_EXACT_CARDINALITY__CARDINALITY = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Data Range</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_EXACT_CARDINALITY__DATA_RANGE = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Data Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_EXACT_CARDINALITY__DATA_PROPERTY_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Data Exact Cardinality</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_EXACT_CARDINALITY_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.NegativeDataPropertyAssertionImpl <em>Negative Data Property Assertion</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.NegativeDataPropertyAssertionImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getNegativeDataPropertyAssertion()
	 * @generated
	 */
	int NEGATIVE_DATA_PROPERTY_ASSERTION = 40;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NEGATIVE_DATA_PROPERTY_ASSERTION__AXIOM_ANNOTATIONS = ASSERTION__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NEGATIVE_DATA_PROPERTY_ASSERTION__AXIOM_ID = ASSERTION__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Data Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NEGATIVE_DATA_PROPERTY_ASSERTION__DATA_PROPERTY_EXPRESSION = ASSERTION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Target Value</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NEGATIVE_DATA_PROPERTY_ASSERTION__TARGET_VALUE = ASSERTION_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Source Individual</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NEGATIVE_DATA_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL = ASSERTION_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Negative Data Property Assertion</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NEGATIVE_DATA_PROPERTY_ASSERTION_FEATURE_COUNT = ASSERTION_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyDomainImpl <em>Data Property Domain</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataPropertyDomainImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataPropertyDomain()
	 * @generated
	 */
	int DATA_PROPERTY_DOMAIN = 41;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_DOMAIN__AXIOM_ANNOTATIONS = DATA_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_DOMAIN__AXIOM_ID = DATA_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Domain</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_DOMAIN__DOMAIN = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Data Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_DOMAIN__DATA_PROPERTY_EXPRESSION = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Data Property Domain</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_DOMAIN_FEATURE_COUNT = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyRangeImpl <em>Data Property Range</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataPropertyRangeImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataPropertyRange()
	 * @generated
	 */
	int DATA_PROPERTY_RANGE = 42;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_RANGE__AXIOM_ANNOTATIONS = DATA_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_RANGE__AXIOM_ID = DATA_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Range</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_RANGE__RANGE = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Data Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_RANGE__DATA_PROPERTY_EXPRESSION = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Data Property Range</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_RANGE_FEATURE_COUNT = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DifferentIndividualsImpl <em>Different Individuals</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DifferentIndividualsImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDifferentIndividuals()
	 * @generated
	 */
	int DIFFERENT_INDIVIDUALS = 43;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DIFFERENT_INDIVIDUALS__AXIOM_ANNOTATIONS = ASSERTION__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DIFFERENT_INDIVIDUALS__AXIOM_ID = ASSERTION__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Different Individuals</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DIFFERENT_INDIVIDUALS__DIFFERENT_INDIVIDUALS = ASSERTION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Different Individuals</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DIFFERENT_INDIVIDUALS_FEATURE_COUNT = ASSERTION_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DisjointClassesImpl <em>Disjoint Classes</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DisjointClassesImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDisjointClasses()
	 * @generated
	 */
	int DISJOINT_CLASSES = 44;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_CLASSES__AXIOM_ANNOTATIONS = CLASS_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_CLASSES__AXIOM_ID = CLASS_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Disjoint Class Expressions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_CLASSES__DISJOINT_CLASS_EXPRESSIONS = CLASS_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Disjoint Classes</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_CLASSES_FEATURE_COUNT = CLASS_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DisjointDataPropertiesImpl <em>Disjoint Data Properties</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DisjointDataPropertiesImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDisjointDataProperties()
	 * @generated
	 */
	int DISJOINT_DATA_PROPERTIES = 45;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_DATA_PROPERTIES__AXIOM_ANNOTATIONS = DATA_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_DATA_PROPERTIES__AXIOM_ID = DATA_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Data Property Expressions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_DATA_PROPERTIES__DATA_PROPERTY_EXPRESSIONS = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Disjoint Data Properties</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_DATA_PROPERTIES_FEATURE_COUNT = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DisjointObjectPropertiesImpl <em>Disjoint Object Properties</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DisjointObjectPropertiesImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDisjointObjectProperties()
	 * @generated
	 */
	int DISJOINT_OBJECT_PROPERTIES = 46;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_OBJECT_PROPERTIES__AXIOM_ANNOTATIONS = OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_OBJECT_PROPERTIES__AXIOM_ID = OBJECT_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Object Property Expressions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_OBJECT_PROPERTIES__OBJECT_PROPERTY_EXPRESSIONS = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Disjoint Object Properties</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_OBJECT_PROPERTIES_FEATURE_COUNT = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DisjointUnionImpl <em>Disjoint Union</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DisjointUnionImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDisjointUnion()
	 * @generated
	 */
	int DISJOINT_UNION = 47;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_UNION__AXIOM_ANNOTATIONS = CLASS_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_UNION__AXIOM_ID = CLASS_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Union Class</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_UNION__UNION_CLASS = CLASS_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Disjoint Class Expressions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_UNION__DISJOINT_CLASS_EXPRESSIONS = CLASS_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Disjoint Union</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DISJOINT_UNION_FEATURE_COUNT = CLASS_AXIOM_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.EquivalentClassesImpl <em>Equivalent Classes</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.EquivalentClassesImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getEquivalentClasses()
	 * @generated
	 */
	int EQUIVALENT_CLASSES = 48;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUIVALENT_CLASSES__AXIOM_ANNOTATIONS = CLASS_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUIVALENT_CLASSES__AXIOM_ID = CLASS_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Equivalent Class Expressions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUIVALENT_CLASSES__EQUIVALENT_CLASS_EXPRESSIONS = CLASS_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Equivalent Classes</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUIVALENT_CLASSES_FEATURE_COUNT = CLASS_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.EquivalentDataPropertiesImpl <em>Equivalent Data Properties</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.EquivalentDataPropertiesImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getEquivalentDataProperties()
	 * @generated
	 */
	int EQUIVALENT_DATA_PROPERTIES = 49;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUIVALENT_DATA_PROPERTIES__AXIOM_ANNOTATIONS = DATA_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUIVALENT_DATA_PROPERTIES__AXIOM_ID = DATA_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Data Property Expressions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUIVALENT_DATA_PROPERTIES__DATA_PROPERTY_EXPRESSIONS = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Equivalent Data Properties</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUIVALENT_DATA_PROPERTIES_FEATURE_COUNT = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.FunctionalDataPropertyImpl <em>Functional Data Property</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.FunctionalDataPropertyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getFunctionalDataProperty()
	 * @generated
	 */
	int FUNCTIONAL_DATA_PROPERTY = 50;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FUNCTIONAL_DATA_PROPERTY__AXIOM_ANNOTATIONS = DATA_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FUNCTIONAL_DATA_PROPERTY__AXIOM_ID = DATA_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Data Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FUNCTIONAL_DATA_PROPERTY__DATA_PROPERTY_EXPRESSION = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Functional Data Property</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FUNCTIONAL_DATA_PROPERTY_FEATURE_COUNT = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.EquivalentObjectPropertiesImpl <em>Equivalent Object Properties</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.EquivalentObjectPropertiesImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getEquivalentObjectProperties()
	 * @generated
	 */
	int EQUIVALENT_OBJECT_PROPERTIES = 51;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUIVALENT_OBJECT_PROPERTIES__AXIOM_ANNOTATIONS = OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUIVALENT_OBJECT_PROPERTIES__AXIOM_ID = OBJECT_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Object Property Expressions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUIVALENT_OBJECT_PROPERTIES__OBJECT_PROPERTY_EXPRESSIONS = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Equivalent Object Properties</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUIVALENT_OBJECT_PROPERTIES_FEATURE_COUNT = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.FunctionalObjectPropertyImpl <em>Functional Object Property</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.FunctionalObjectPropertyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getFunctionalObjectProperty()
	 * @generated
	 */
	int FUNCTIONAL_OBJECT_PROPERTY = 52;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FUNCTIONAL_OBJECT_PROPERTY__AXIOM_ANNOTATIONS = OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FUNCTIONAL_OBJECT_PROPERTY__AXIOM_ID = OBJECT_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FUNCTIONAL_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Functional Object Property</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FUNCTIONAL_OBJECT_PROPERTY_FEATURE_COUNT = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.InverseFunctionalObjectPropertyImpl <em>Inverse Functional Object Property</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.InverseFunctionalObjectPropertyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getInverseFunctionalObjectProperty()
	 * @generated
	 */
	int INVERSE_FUNCTIONAL_OBJECT_PROPERTY = 53;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INVERSE_FUNCTIONAL_OBJECT_PROPERTY__AXIOM_ANNOTATIONS = OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INVERSE_FUNCTIONAL_OBJECT_PROPERTY__AXIOM_ID = OBJECT_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INVERSE_FUNCTIONAL_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Inverse Functional Object Property</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INVERSE_FUNCTIONAL_OBJECT_PROPERTY_FEATURE_COUNT = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectPropertyAssertionImpl <em>Object Property Assertion</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectPropertyAssertionImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectPropertyAssertion()
	 * @generated
	 */
	int OBJECT_PROPERTY_ASSERTION = 54;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_ASSERTION__AXIOM_ANNOTATIONS = ASSERTION__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_ASSERTION__AXIOM_ID = ASSERTION__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_ASSERTION__OBJECT_PROPERTY_EXPRESSION = ASSERTION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Source Individual</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL = ASSERTION_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Target Individual</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_ASSERTION__TARGET_INDIVIDUAL = ASSERTION_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Object Property Assertion</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_ASSERTION_FEATURE_COUNT = ASSERTION_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.NegativeObjectPropertyAssertionImpl <em>Negative Object Property Assertion</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.NegativeObjectPropertyAssertionImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getNegativeObjectPropertyAssertion()
	 * @generated
	 */
	int NEGATIVE_OBJECT_PROPERTY_ASSERTION = 55;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NEGATIVE_OBJECT_PROPERTY_ASSERTION__AXIOM_ANNOTATIONS = ASSERTION__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NEGATIVE_OBJECT_PROPERTY_ASSERTION__AXIOM_ID = ASSERTION__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NEGATIVE_OBJECT_PROPERTY_ASSERTION__OBJECT_PROPERTY_EXPRESSION = ASSERTION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Source Individual</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NEGATIVE_OBJECT_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL = ASSERTION_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Target Individual</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NEGATIVE_OBJECT_PROPERTY_ASSERTION__TARGET_INDIVIDUAL = ASSERTION_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Negative Object Property Assertion</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NEGATIVE_OBJECT_PROPERTY_ASSERTION_FEATURE_COUNT = ASSERTION_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectPropertyDomainImpl <em>Object Property Domain</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectPropertyDomainImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectPropertyDomain()
	 * @generated
	 */
	int OBJECT_PROPERTY_DOMAIN = 56;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_DOMAIN__AXIOM_ANNOTATIONS = OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_DOMAIN__AXIOM_ID = OBJECT_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Domain</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_DOMAIN__DOMAIN = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_DOMAIN__OBJECT_PROPERTY_EXPRESSION = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Object Property Domain</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_DOMAIN_FEATURE_COUNT = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.SymmetricObjectPropertyImpl <em>Symmetric Object Property</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.SymmetricObjectPropertyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getSymmetricObjectProperty()
	 * @generated
	 */
	int SYMMETRIC_OBJECT_PROPERTY = 57;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SYMMETRIC_OBJECT_PROPERTY__AXIOM_ANNOTATIONS = OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SYMMETRIC_OBJECT_PROPERTY__AXIOM_ID = OBJECT_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SYMMETRIC_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Symmetric Object Property</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SYMMETRIC_OBJECT_PROPERTY_FEATURE_COUNT = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ReflexiveObjectPropertyImpl <em>Reflexive Object Property</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ReflexiveObjectPropertyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getReflexiveObjectProperty()
	 * @generated
	 */
	int REFLEXIVE_OBJECT_PROPERTY = 58;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFLEXIVE_OBJECT_PROPERTY__AXIOM_ANNOTATIONS = OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFLEXIVE_OBJECT_PROPERTY__AXIOM_ID = OBJECT_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFLEXIVE_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Reflexive Object Property</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFLEXIVE_OBJECT_PROPERTY_FEATURE_COUNT = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.SubDataPropertyOfImpl <em>Sub Data Property Of</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.SubDataPropertyOfImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getSubDataPropertyOf()
	 * @generated
	 */
	int SUB_DATA_PROPERTY_OF = 59;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_DATA_PROPERTY_OF__AXIOM_ANNOTATIONS = DATA_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_DATA_PROPERTY_OF__AXIOM_ID = DATA_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Super Data Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_DATA_PROPERTY_OF__SUPER_DATA_PROPERTY_EXPRESSION = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Sub Data Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_DATA_PROPERTY_OF__SUB_DATA_PROPERTY_EXPRESSION = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Sub Data Property Of</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_DATA_PROPERTY_OF_FEATURE_COUNT = DATA_PROPERTY_AXIOM_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.SameIndividualImpl <em>Same Individual</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.SameIndividualImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getSameIndividual()
	 * @generated
	 */
	int SAME_INDIVIDUAL = 60;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SAME_INDIVIDUAL__AXIOM_ANNOTATIONS = ASSERTION__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SAME_INDIVIDUAL__AXIOM_ID = ASSERTION__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Same Individuals</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SAME_INDIVIDUAL__SAME_INDIVIDUALS = ASSERTION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Same Individual</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SAME_INDIVIDUAL_FEATURE_COUNT = ASSERTION_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.SubObjectPropertyOfImpl <em>Sub Object Property Of</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.SubObjectPropertyOfImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getSubObjectPropertyOf()
	 * @generated
	 */
	int SUB_OBJECT_PROPERTY_OF = 61;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_OBJECT_PROPERTY_OF__AXIOM_ANNOTATIONS = OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_OBJECT_PROPERTY_OF__AXIOM_ID = OBJECT_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Super Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_OBJECT_PROPERTY_OF__SUPER_OBJECT_PROPERTY_EXPRESSION = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Sub Object Property Expressions</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_OBJECT_PROPERTY_OF__SUB_OBJECT_PROPERTY_EXPRESSIONS = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Sub Object Property Of</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_OBJECT_PROPERTY_OF_FEATURE_COUNT = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectComplementOfImpl <em>Object Complement Of</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectComplementOfImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectComplementOf()
	 * @generated
	 */
	int OBJECT_COMPLEMENT_OF = 62;

	/**
	 * The feature id for the '<em><b>Class Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_COMPLEMENT_OF__CLASS_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Object Complement Of</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_COMPLEMENT_OF_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.OntologyImpl <em>Ontology</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.OntologyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getOntology()
	 * @generated
	 */
	int ONTOLOGY = 63;

	/**
	 * The feature id for the '<em><b>Ontology Annotations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ONTOLOGY__ONTOLOGY_ANNOTATIONS = 0;

	/**
	 * The feature id for the '<em><b>Axioms</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ONTOLOGY__AXIOMS = 1;

	/**
	 * The feature id for the '<em><b>Imported Ontologies</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ONTOLOGY__IMPORTED_ONTOLOGIES = 2;

	/**
	 * The feature id for the '<em><b>Ontology URI</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ONTOLOGY__ONTOLOGY_URI = 3;

	/**
	 * The feature id for the '<em><b>Version URI</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ONTOLOGY__VERSION_URI = 4;

	/**
	 * The feature id for the '<em><b>Container</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ONTOLOGY__CONTAINER = 5;

	/**
	 * The feature id for the '<em><b>Ontology Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ONTOLOGY__ONTOLOGY_ID = 6;

	/**
	 * The number of structural features of the '<em>Ontology</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ONTOLOGY_FEATURE_COUNT = 7;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectPropertyRangeImpl <em>Object Property Range</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectPropertyRangeImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectPropertyRange()
	 * @generated
	 */
	int OBJECT_PROPERTY_RANGE = 64;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_RANGE__AXIOM_ANNOTATIONS = OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_RANGE__AXIOM_ID = OBJECT_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Range</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_RANGE__RANGE = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_RANGE__OBJECT_PROPERTY_EXPRESSION = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Object Property Range</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_PROPERTY_RANGE_FEATURE_COUNT = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyAssertionImpl <em>Data Property Assertion</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataPropertyAssertionImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataPropertyAssertion()
	 * @generated
	 */
	int DATA_PROPERTY_ASSERTION = 65;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_ASSERTION__AXIOM_ANNOTATIONS = ASSERTION__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_ASSERTION__AXIOM_ID = ASSERTION__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Data Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_ASSERTION__DATA_PROPERTY_EXPRESSION = ASSERTION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Target Value</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_ASSERTION__TARGET_VALUE = ASSERTION_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Source Individual</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL = ASSERTION_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Data Property Assertion</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_PROPERTY_ASSERTION_FEATURE_COUNT = ASSERTION_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ClassAssertionImpl <em>Class Assertion</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ClassAssertionImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getClassAssertion()
	 * @generated
	 */
	int CLASS_ASSERTION = 66;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_ASSERTION__AXIOM_ANNOTATIONS = ASSERTION__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_ASSERTION__AXIOM_ID = ASSERTION__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Individual</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_ASSERTION__INDIVIDUAL = ASSERTION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Class Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_ASSERTION__CLASS_EXPRESSION = ASSERTION_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Class Assertion</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_ASSERTION_FEATURE_COUNT = ASSERTION_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.IrreflexiveObjectPropertyImpl <em>Irreflexive Object Property</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.IrreflexiveObjectPropertyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getIrreflexiveObjectProperty()
	 * @generated
	 */
	int IRREFLEXIVE_OBJECT_PROPERTY = 67;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IRREFLEXIVE_OBJECT_PROPERTY__AXIOM_ANNOTATIONS = OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IRREFLEXIVE_OBJECT_PROPERTY__AXIOM_ID = OBJECT_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IRREFLEXIVE_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Irreflexive Object Property</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IRREFLEXIVE_OBJECT_PROPERTY_FEATURE_COUNT = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectExactCardinalityImpl <em>Object Exact Cardinality</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectExactCardinalityImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectExactCardinality()
	 * @generated
	 */
	int OBJECT_EXACT_CARDINALITY = 68;

	/**
	 * The feature id for the '<em><b>Cardinality</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_EXACT_CARDINALITY__CARDINALITY = CLASS_EXPRESSION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Class Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_EXACT_CARDINALITY__CLASS_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_EXACT_CARDINALITY__OBJECT_PROPERTY_EXPRESSION = CLASS_EXPRESSION_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Object Exact Cardinality</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_EXACT_CARDINALITY_FEATURE_COUNT = CLASS_EXPRESSION_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DataComplementOfImpl <em>Data Complement Of</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DataComplementOfImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataComplementOf()
	 * @generated
	 */
	int DATA_COMPLEMENT_OF = 69;

	/**
	 * The feature id for the '<em><b>Arity</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_COMPLEMENT_OF__ARITY = DATA_RANGE__ARITY;

	/**
	 * The feature id for the '<em><b>Data Range</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_COMPLEMENT_OF__DATA_RANGE = DATA_RANGE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Data Complement Of</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATA_COMPLEMENT_OF_FEATURE_COUNT = DATA_RANGE_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.SubClassOfImpl <em>Sub Class Of</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.SubClassOfImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getSubClassOf()
	 * @generated
	 */
	int SUB_CLASS_OF = 70;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_CLASS_OF__AXIOM_ANNOTATIONS = CLASS_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_CLASS_OF__AXIOM_ID = CLASS_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Sub Class Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_CLASS_OF__SUB_CLASS_EXPRESSION = CLASS_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Super Class Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_CLASS_OF__SUPER_CLASS_EXPRESSION = CLASS_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Sub Class Of</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_CLASS_OF_FEATURE_COUNT = CLASS_AXIOM_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.SubObjectPropertyImpl <em>Sub Object Property</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.SubObjectPropertyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getSubObjectProperty()
	 * @generated
	 */
	int SUB_OBJECT_PROPERTY = 71;

	/**
	 * The number of structural features of the '<em>Sub Object Property</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SUB_OBJECT_PROPERTY_FEATURE_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.TransitiveObjectPropertyImpl <em>Transitive Object Property</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.TransitiveObjectPropertyImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getTransitiveObjectProperty()
	 * @generated
	 */
	int TRANSITIVE_OBJECT_PROPERTY = 72;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSITIVE_OBJECT_PROPERTY__AXIOM_ANNOTATIONS = OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSITIVE_OBJECT_PROPERTY__AXIOM_ID = OBJECT_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSITIVE_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Transitive Object Property</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSITIVE_OBJECT_PROPERTY_FEATURE_COUNT = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.EntityAnnotationImpl <em>Entity Annotation</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.EntityAnnotationImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getEntityAnnotation()
	 * @generated
	 */
	int ENTITY_ANNOTATION = 73;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENTITY_ANNOTATION__AXIOM_ANNOTATIONS = AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENTITY_ANNOTATION__AXIOM_ID = AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Entity</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENTITY_ANNOTATION__ENTITY = AXIOM_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Entity Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENTITY_ANNOTATION__ENTITY_ANNOTATIONS = AXIOM_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Entity Annotation</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENTITY_ANNOTATION_FEATURE_COUNT = AXIOM_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.FullURIImpl <em>Full URI</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.FullURIImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getFullURI()
	 * @generated
	 */
	int FULL_URI = 74;

	/**
	 * The feature id for the '<em><b>Iri</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FULL_URI__IRI = 0;

	/**
	 * The number of structural features of the '<em>Full URI</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FULL_URI_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.AbbreviatedURIImpl <em>Abbreviated URI</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.AbbreviatedURIImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAbbreviatedURI()
	 * @generated
	 */
	int ABBREVIATED_URI = 75;

	/**
	 * The feature id for the '<em><b>Local Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABBREVIATED_URI__LOCAL_NAME = 0;

	/**
	 * The number of structural features of the '<em>Abbreviated URI</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABBREVIATED_URI_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.InverseObjectPropertiesImpl <em>Inverse Object Properties</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.InverseObjectPropertiesImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getInverseObjectProperties()
	 * @generated
	 */
	int INVERSE_OBJECT_PROPERTIES = 76;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INVERSE_OBJECT_PROPERTIES__AXIOM_ANNOTATIONS = OBJECT_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INVERSE_OBJECT_PROPERTIES__AXIOM_ID = OBJECT_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Inverse Object Properties</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INVERSE_OBJECT_PROPERTIES__INVERSE_OBJECT_PROPERTIES = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Inverse Object Properties</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INVERSE_OBJECT_PROPERTIES_FEATURE_COUNT = OBJECT_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.AnnotationByConstantImpl <em>Annotation By Constant</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.AnnotationByConstantImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnnotationByConstant()
	 * @generated
	 */
	int ANNOTATION_BY_CONSTANT = 77;

	/**
	 * The feature id for the '<em><b>Annotation Property</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_BY_CONSTANT__ANNOTATION_PROPERTY = ANNOTATION__ANNOTATION_PROPERTY;

	/**
	 * The feature id for the '<em><b>Annotation Value</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_BY_CONSTANT__ANNOTATION_VALUE = ANNOTATION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Annotation By Constant</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_BY_CONSTANT_FEATURE_COUNT = ANNOTATION_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.AnnotationByEntityImpl <em>Annotation By Entity</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.AnnotationByEntityImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnnotationByEntity()
	 * @generated
	 */
	int ANNOTATION_BY_ENTITY = 78;

	/**
	 * The feature id for the '<em><b>Annotation Property</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_BY_ENTITY__ANNOTATION_PROPERTY = ANNOTATION__ANNOTATION_PROPERTY;

	/**
	 * The feature id for the '<em><b>Annotation Value</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_BY_ENTITY__ANNOTATION_VALUE = ANNOTATION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Annotation By Entity</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_BY_ENTITY_FEATURE_COUNT = ANNOTATION_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.AnnotationByAnonymousIndividualImpl <em>Annotation By Anonymous Individual</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.AnnotationByAnonymousIndividualImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnnotationByAnonymousIndividual()
	 * @generated
	 */
	int ANNOTATION_BY_ANONYMOUS_INDIVIDUAL = 79;

	/**
	 * The feature id for the '<em><b>Annotation Property</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_BY_ANONYMOUS_INDIVIDUAL__ANNOTATION_PROPERTY = ANNOTATION__ANNOTATION_PROPERTY;

	/**
	 * The feature id for the '<em><b>Annotation Value</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_BY_ANONYMOUS_INDIVIDUAL__ANNOTATION_VALUE = ANNOTATION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Annotation By Anonymous Individual</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_BY_ANONYMOUS_INDIVIDUAL_FEATURE_COUNT = ANNOTATION_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.AnonymousIndividualImpl <em>Anonymous Individual</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.AnonymousIndividualImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnonymousIndividual()
	 * @generated
	 */
	int ANONYMOUS_INDIVIDUAL = 80;

	/**
	 * The feature id for the '<em><b>Node ID</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANONYMOUS_INDIVIDUAL__NODE_ID = INDIVIDUAL_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Anonymous Individual</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANONYMOUS_INDIVIDUAL_FEATURE_COUNT = INDIVIDUAL_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.DeclarationImpl <em>Declaration</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.DeclarationImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDeclaration()
	 * @generated
	 */
	int DECLARATION = 81;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DECLARATION__AXIOM_ANNOTATIONS = AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DECLARATION__AXIOM_ID = AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Entity</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DECLARATION__ENTITY = AXIOM_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Declaration</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DECLARATION_FEATURE_COUNT = AXIOM_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectAndDataPropertyAxiomImpl <em>Object And Data Property Axiom</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.ObjectAndDataPropertyAxiomImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectAndDataPropertyAxiom()
	 * @generated
	 */
	int OBJECT_AND_DATA_PROPERTY_AXIOM = 82;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_AND_DATA_PROPERTY_AXIOM__AXIOM_ANNOTATIONS = AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_AND_DATA_PROPERTY_AXIOM__AXIOM_ID = AXIOM__AXIOM_ID;

	/**
	 * The number of structural features of the '<em>Object And Data Property Axiom</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OBJECT_AND_DATA_PROPERTY_AXIOM_FEATURE_COUNT = AXIOM_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.KeyForImpl <em>Key For</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.KeyForImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getKeyFor()
	 * @generated
	 */
	int KEY_FOR = 83;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int KEY_FOR__AXIOM_ANNOTATIONS = OBJECT_AND_DATA_PROPERTY_AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int KEY_FOR__AXIOM_ID = OBJECT_AND_DATA_PROPERTY_AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Class Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int KEY_FOR__CLASS_EXPRESSION = OBJECT_AND_DATA_PROPERTY_AXIOM_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Data Property Expressions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int KEY_FOR__DATA_PROPERTY_EXPRESSIONS = OBJECT_AND_DATA_PROPERTY_AXIOM_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Object Property Expressions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int KEY_FOR__OBJECT_PROPERTY_EXPRESSIONS = OBJECT_AND_DATA_PROPERTY_AXIOM_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Key For</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int KEY_FOR_FEATURE_COUNT = OBJECT_AND_DATA_PROPERTY_AXIOM_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link carisma.modeltype.owl2.model.owl.impl.AnonymousIndividualAnnotationImpl <em>Anonymous Individual Annotation</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.owl2.model.owl.impl.AnonymousIndividualAnnotationImpl
	 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnonymousIndividualAnnotation()
	 * @generated
	 */
	int ANONYMOUS_INDIVIDUAL_ANNOTATION = 84;

	/**
	 * The feature id for the '<em><b>Axiom Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANONYMOUS_INDIVIDUAL_ANNOTATION__AXIOM_ANNOTATIONS = AXIOM__AXIOM_ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANONYMOUS_INDIVIDUAL_ANNOTATION__AXIOM_ID = AXIOM__AXIOM_ID;

	/**
	 * The feature id for the '<em><b>Anonymous Individual</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANONYMOUS_INDIVIDUAL_ANNOTATION__ANONYMOUS_INDIVIDUAL = AXIOM_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Anonymous Indiviudal Annotations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANONYMOUS_INDIVIDUAL_ANNOTATION__ANONYMOUS_INDIVIUDAL_ANNOTATIONS = AXIOM_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Anonymous Individual Annotation</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANONYMOUS_INDIVIDUAL_ANNOTATION_FEATURE_COUNT = AXIOM_FEATURE_COUNT + 2;


	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.Assertion <em>Assertion</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Assertion</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Assertion
	 * @generated
	 */
	EClass getAssertion();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.Axiom <em>Axiom</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Axiom</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Axiom
	 * @generated
	 */
	EClass getAxiom();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.Axiom#getAxiomAnnotations <em>Axiom Annotations</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Axiom Annotations</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Axiom#getAxiomAnnotations()
	 * @see #getAxiom()
	 * @generated
	 */
	EReference getAxiom_AxiomAnnotations();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.Axiom#getAxiomId <em>Axiom Id</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Axiom Id</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Axiom#getAxiomId()
	 * @see #getAxiom()
	 * @generated
	 */
	EAttribute getAxiom_AxiomId();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.Annotation <em>Annotation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Annotation</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Annotation
	 * @generated
	 */
	EClass getAnnotation();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.Annotation#getAnnotationProperty <em>Annotation Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Annotation Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Annotation#getAnnotationProperty()
	 * @see #getAnnotation()
	 * @generated
	 */
	EReference getAnnotation_AnnotationProperty();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.AnnotationProperty <em>Annotation Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Annotation Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AnnotationProperty
	 * @generated
	 */
	EClass getAnnotationProperty();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.Entity <em>Entity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Entity</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Entity
	 * @generated
	 */
	EClass getEntity();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.Entity#getEntityURI <em>Entity URI</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Entity URI</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Entity#getEntityURI()
	 * @see #getEntity()
	 * @generated
	 */
	EReference getEntity_EntityURI();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.URI <em>URI</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>URI</em>'.
	 * @see carisma.modeltype.owl2.model.owl.URI
	 * @generated
	 */
	EClass getURI();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.URI#getValue <em>Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Value</em>'.
	 * @see carisma.modeltype.owl2.model.owl.URI#getValue()
	 * @see #getURI()
	 * @generated
	 */
	EAttribute getURI_Value();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.Constant <em>Constant</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Constant</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Constant
	 * @generated
	 */
	EClass getConstant();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.Constant#getLexicalValue <em>Lexical Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Lexical Value</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Constant#getLexicalValue()
	 * @see #getConstant()
	 * @generated
	 */
	EAttribute getConstant_LexicalValue();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.Constant#getDatatype <em>Datatype</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Datatype</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Constant#getDatatype()
	 * @see #getConstant()
	 * @generated
	 */
	EReference getConstant_Datatype();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.Datatype <em>Datatype</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Datatype</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Datatype
	 * @generated
	 */
	EClass getDatatype();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataRange <em>Data Range</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data Range</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataRange
	 * @generated
	 */
	EClass getDataRange();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.DataRange#getArity <em>Arity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Arity</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataRange#getArity()
	 * @see #getDataRange()
	 * @generated
	 */
	EAttribute getDataRange_Arity();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataPropertyAxiom <em>Data Property Axiom</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data Property Axiom</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyAxiom
	 * @generated
	 */
	EClass getDataPropertyAxiom();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyAxiom <em>Object Property Axiom</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Property Axiom</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyAxiom
	 * @generated
	 */
	EClass getObjectPropertyAxiom();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ClassExpression <em>Class Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Class Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ClassExpression
	 * @generated
	 */
	EClass getClassExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ClassAxiom <em>Class Axiom</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Class Axiom</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ClassAxiom
	 * @generated
	 */
	EClass getClassAxiom();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataPropertyExpression <em>Data Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyExpression
	 * @generated
	 */
	EClass getDataPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyExpression
	 * @generated
	 */
	EClass getObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.AsymmetricObjectProperty <em>Asymmetric Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Asymmetric Object Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AsymmetricObjectProperty
	 * @generated
	 */
	EClass getAsymmetricObjectProperty();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.AsymmetricObjectProperty#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AsymmetricObjectProperty#getObjectPropertyExpression()
	 * @see #getAsymmetricObjectProperty()
	 * @generated
	 */
	EReference getAsymmetricObjectProperty_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectProperty <em>Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectProperty
	 * @generated
	 */
	EClass getObjectProperty();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.InverseObjectProperty <em>Inverse Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Inverse Object Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.InverseObjectProperty
	 * @generated
	 */
	EClass getInverseObjectProperty();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.InverseObjectProperty#getObjectProperty <em>Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.InverseObjectProperty#getObjectProperty()
	 * @see #getInverseObjectProperty()
	 * @generated
	 */
	EReference getInverseObjectProperty_ObjectProperty();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.Class <em>Class</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Class</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Class
	 * @generated
	 */
	EClass getClass_();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectIntersectionOf <em>Object Intersection Of</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Intersection Of</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectIntersectionOf
	 * @generated
	 */
	EClass getObjectIntersectionOf();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.ObjectIntersectionOf#getClassExpressions <em>Class Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Class Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectIntersectionOf#getClassExpressions()
	 * @see #getObjectIntersectionOf()
	 * @generated
	 */
	EReference getObjectIntersectionOf_ClassExpressions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectUnionOf <em>Object Union Of</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Union Of</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectUnionOf
	 * @generated
	 */
	EClass getObjectUnionOf();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.ObjectUnionOf#getClassExpressions <em>Class Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Class Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectUnionOf#getClassExpressions()
	 * @see #getObjectUnionOf()
	 * @generated
	 */
	EReference getObjectUnionOf_ClassExpressions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectOneOf <em>Object One Of</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object One Of</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectOneOf
	 * @generated
	 */
	EClass getObjectOneOf();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.ObjectOneOf#getIndividuals <em>Individuals</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Individuals</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectOneOf#getIndividuals()
	 * @see #getObjectOneOf()
	 * @generated
	 */
	EReference getObjectOneOf_Individuals();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.Individual <em>Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Individual
	 * @generated
	 */
	EClass getIndividual();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.NamedIndividual <em>Named Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Named Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.NamedIndividual
	 * @generated
	 */
	EClass getNamedIndividual();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom <em>Object Some Values From</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Some Values From</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom
	 * @generated
	 */
	EClass getObjectSomeValuesFrom();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom#getClassExpression <em>Class Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Class Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom#getClassExpression()
	 * @see #getObjectSomeValuesFrom()
	 * @generated
	 */
	EReference getObjectSomeValuesFrom_ClassExpression();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom#getObjectPropertyExpression()
	 * @see #getObjectSomeValuesFrom()
	 * @generated
	 */
	EReference getObjectSomeValuesFrom_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectAllValuesFrom <em>Object All Values From</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object All Values From</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectAllValuesFrom
	 * @generated
	 */
	EClass getObjectAllValuesFrom();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectAllValuesFrom#getClassExpression <em>Class Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Class Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectAllValuesFrom#getClassExpression()
	 * @see #getObjectAllValuesFrom()
	 * @generated
	 */
	EReference getObjectAllValuesFrom_ClassExpression();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectAllValuesFrom#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectAllValuesFrom#getObjectPropertyExpression()
	 * @see #getObjectAllValuesFrom()
	 * @generated
	 */
	EReference getObjectAllValuesFrom_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectExistsSelf <em>Object Exists Self</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Exists Self</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectExistsSelf
	 * @generated
	 */
	EClass getObjectExistsSelf();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectExistsSelf#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectExistsSelf#getObjectPropertyExpression()
	 * @see #getObjectExistsSelf()
	 * @generated
	 */
	EReference getObjectExistsSelf_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectHasValue <em>Object Has Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Has Value</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectHasValue
	 * @generated
	 */
	EClass getObjectHasValue();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectHasValue#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectHasValue#getObjectPropertyExpression()
	 * @see #getObjectHasValue()
	 * @generated
	 */
	EReference getObjectHasValue_ObjectPropertyExpression();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectHasValue#getIndividual <em>Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectHasValue#getIndividual()
	 * @see #getObjectHasValue()
	 * @generated
	 */
	EReference getObjectHasValue_Individual();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectMinCardinality <em>Object Min Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Min Cardinality</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectMinCardinality
	 * @generated
	 */
	EClass getObjectMinCardinality();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.ObjectMinCardinality#getCardinality <em>Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Cardinality</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectMinCardinality#getCardinality()
	 * @see #getObjectMinCardinality()
	 * @generated
	 */
	EAttribute getObjectMinCardinality_Cardinality();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectMinCardinality#getClassExpression <em>Class Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Class Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectMinCardinality#getClassExpression()
	 * @see #getObjectMinCardinality()
	 * @generated
	 */
	EReference getObjectMinCardinality_ClassExpression();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectMinCardinality#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectMinCardinality#getObjectPropertyExpression()
	 * @see #getObjectMinCardinality()
	 * @generated
	 */
	EReference getObjectMinCardinality_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectMaxCardinality <em>Object Max Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Max Cardinality</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectMaxCardinality
	 * @generated
	 */
	EClass getObjectMaxCardinality();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.ObjectMaxCardinality#getCardinality <em>Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Cardinality</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectMaxCardinality#getCardinality()
	 * @see #getObjectMaxCardinality()
	 * @generated
	 */
	EAttribute getObjectMaxCardinality_Cardinality();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectMaxCardinality#getClassExpression <em>Class Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Class Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectMaxCardinality#getClassExpression()
	 * @see #getObjectMaxCardinality()
	 * @generated
	 */
	EReference getObjectMaxCardinality_ClassExpression();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectMaxCardinality#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectMaxCardinality#getObjectPropertyExpression()
	 * @see #getObjectMaxCardinality()
	 * @generated
	 */
	EReference getObjectMaxCardinality_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataSomeValuesFrom <em>Data Some Values From</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data Some Values From</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataSomeValuesFrom
	 * @generated
	 */
	EClass getDataSomeValuesFrom();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataSomeValuesFrom#getDataRange <em>Data Range</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Range</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataSomeValuesFrom#getDataRange()
	 * @see #getDataSomeValuesFrom()
	 * @generated
	 */
	EReference getDataSomeValuesFrom_DataRange();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataSomeValuesFrom#getDataPropertyExpressions <em>Data Property Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Property Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataSomeValuesFrom#getDataPropertyExpressions()
	 * @see #getDataSomeValuesFrom()
	 * @generated
	 */
	EReference getDataSomeValuesFrom_DataPropertyExpressions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataProperty <em>Data Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataProperty
	 * @generated
	 */
	EClass getDataProperty();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataOneOf <em>Data One Of</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data One Of</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataOneOf
	 * @generated
	 */
	EClass getDataOneOf();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.DataOneOf#getConstants <em>Constants</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Constants</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataOneOf#getConstants()
	 * @see #getDataOneOf()
	 * @generated
	 */
	EReference getDataOneOf_Constants();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DatatypeRestriction <em>Datatype Restriction</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Datatype Restriction</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DatatypeRestriction
	 * @generated
	 */
	EClass getDatatypeRestriction();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DatatypeRestriction#getDatatype <em>Datatype</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Datatype</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DatatypeRestriction#getDatatype()
	 * @see #getDatatypeRestriction()
	 * @generated
	 */
	EReference getDatatypeRestriction_Datatype();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.DatatypeRestriction#getRestrictions <em>Restrictions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Restrictions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DatatypeRestriction#getRestrictions()
	 * @see #getDatatypeRestriction()
	 * @generated
	 */
	EReference getDatatypeRestriction_Restrictions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.FacetConstantPair <em>Facet Constant Pair</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Facet Constant Pair</em>'.
	 * @see carisma.modeltype.owl2.model.owl.FacetConstantPair
	 * @generated
	 */
	EClass getFacetConstantPair();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.FacetConstantPair#getConstant <em>Constant</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Constant</em>'.
	 * @see carisma.modeltype.owl2.model.owl.FacetConstantPair#getConstant()
	 * @see #getFacetConstantPair()
	 * @generated
	 */
	EReference getFacetConstantPair_Constant();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.FacetConstantPair#getFacet <em>Facet</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Facet</em>'.
	 * @see carisma.modeltype.owl2.model.owl.FacetConstantPair#getFacet()
	 * @see #getFacetConstantPair()
	 * @generated
	 */
	EAttribute getFacetConstantPair_Facet();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataAllValuesFrom <em>Data All Values From</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data All Values From</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataAllValuesFrom
	 * @generated
	 */
	EClass getDataAllValuesFrom();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataAllValuesFrom#getDataRange <em>Data Range</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Range</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataAllValuesFrom#getDataRange()
	 * @see #getDataAllValuesFrom()
	 * @generated
	 */
	EReference getDataAllValuesFrom_DataRange();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataAllValuesFrom#getDataPropertyExpressions <em>Data Property Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Property Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataAllValuesFrom#getDataPropertyExpressions()
	 * @see #getDataAllValuesFrom()
	 * @generated
	 */
	EReference getDataAllValuesFrom_DataPropertyExpressions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataHasValue <em>Data Has Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data Has Value</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataHasValue
	 * @generated
	 */
	EClass getDataHasValue();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataHasValue#getConstant <em>Constant</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Constant</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataHasValue#getConstant()
	 * @see #getDataHasValue()
	 * @generated
	 */
	EReference getDataHasValue_Constant();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataHasValue#getDataPropertyExpression <em>Data Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataHasValue#getDataPropertyExpression()
	 * @see #getDataHasValue()
	 * @generated
	 */
	EReference getDataHasValue_DataPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataMinCardinality <em>Data Min Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data Min Cardinality</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataMinCardinality
	 * @generated
	 */
	EClass getDataMinCardinality();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.DataMinCardinality#getCardinality <em>Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Cardinality</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataMinCardinality#getCardinality()
	 * @see #getDataMinCardinality()
	 * @generated
	 */
	EAttribute getDataMinCardinality_Cardinality();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataMinCardinality#getDataRange <em>Data Range</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Range</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataMinCardinality#getDataRange()
	 * @see #getDataMinCardinality()
	 * @generated
	 */
	EReference getDataMinCardinality_DataRange();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataMinCardinality#getDataPropertyExpression <em>Data Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataMinCardinality#getDataPropertyExpression()
	 * @see #getDataMinCardinality()
	 * @generated
	 */
	EReference getDataMinCardinality_DataPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataMaxCardinality <em>Data Max Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data Max Cardinality</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataMaxCardinality
	 * @generated
	 */
	EClass getDataMaxCardinality();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.DataMaxCardinality#getCardinality <em>Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Cardinality</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataMaxCardinality#getCardinality()
	 * @see #getDataMaxCardinality()
	 * @generated
	 */
	EAttribute getDataMaxCardinality_Cardinality();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataMaxCardinality#getDataRange <em>Data Range</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Range</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataMaxCardinality#getDataRange()
	 * @see #getDataMaxCardinality()
	 * @generated
	 */
	EReference getDataMaxCardinality_DataRange();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataMaxCardinality#getDataPropertyExpression <em>Data Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataMaxCardinality#getDataPropertyExpression()
	 * @see #getDataMaxCardinality()
	 * @generated
	 */
	EReference getDataMaxCardinality_DataPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataExactCardinality <em>Data Exact Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data Exact Cardinality</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataExactCardinality
	 * @generated
	 */
	EClass getDataExactCardinality();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.DataExactCardinality#getCardinality <em>Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Cardinality</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataExactCardinality#getCardinality()
	 * @see #getDataExactCardinality()
	 * @generated
	 */
	EAttribute getDataExactCardinality_Cardinality();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataExactCardinality#getDataRange <em>Data Range</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Range</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataExactCardinality#getDataRange()
	 * @see #getDataExactCardinality()
	 * @generated
	 */
	EReference getDataExactCardinality_DataRange();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataExactCardinality#getDataPropertyExpression <em>Data Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataExactCardinality#getDataPropertyExpression()
	 * @see #getDataExactCardinality()
	 * @generated
	 */
	EReference getDataExactCardinality_DataPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.NegativeDataPropertyAssertion <em>Negative Data Property Assertion</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Negative Data Property Assertion</em>'.
	 * @see carisma.modeltype.owl2.model.owl.NegativeDataPropertyAssertion
	 * @generated
	 */
	EClass getNegativeDataPropertyAssertion();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.NegativeDataPropertyAssertion#getDataPropertyExpression <em>Data Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.NegativeDataPropertyAssertion#getDataPropertyExpression()
	 * @see #getNegativeDataPropertyAssertion()
	 * @generated
	 */
	EReference getNegativeDataPropertyAssertion_DataPropertyExpression();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.NegativeDataPropertyAssertion#getTargetValue <em>Target Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Target Value</em>'.
	 * @see carisma.modeltype.owl2.model.owl.NegativeDataPropertyAssertion#getTargetValue()
	 * @see #getNegativeDataPropertyAssertion()
	 * @generated
	 */
	EReference getNegativeDataPropertyAssertion_TargetValue();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.NegativeDataPropertyAssertion#getSourceIndividual <em>Source Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Source Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.NegativeDataPropertyAssertion#getSourceIndividual()
	 * @see #getNegativeDataPropertyAssertion()
	 * @generated
	 */
	EReference getNegativeDataPropertyAssertion_SourceIndividual();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataPropertyDomain <em>Data Property Domain</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data Property Domain</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyDomain
	 * @generated
	 */
	EClass getDataPropertyDomain();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataPropertyDomain#getDomain <em>Domain</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Domain</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyDomain#getDomain()
	 * @see #getDataPropertyDomain()
	 * @generated
	 */
	EReference getDataPropertyDomain_Domain();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataPropertyDomain#getDataPropertyExpression <em>Data Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyDomain#getDataPropertyExpression()
	 * @see #getDataPropertyDomain()
	 * @generated
	 */
	EReference getDataPropertyDomain_DataPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataPropertyRange <em>Data Property Range</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data Property Range</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyRange
	 * @generated
	 */
	EClass getDataPropertyRange();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataPropertyRange#getRange <em>Range</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Range</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyRange#getRange()
	 * @see #getDataPropertyRange()
	 * @generated
	 */
	EReference getDataPropertyRange_Range();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataPropertyRange#getDataPropertyExpression <em>Data Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyRange#getDataPropertyExpression()
	 * @see #getDataPropertyRange()
	 * @generated
	 */
	EReference getDataPropertyRange_DataPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DifferentIndividuals <em>Different Individuals</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Different Individuals</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DifferentIndividuals
	 * @generated
	 */
	EClass getDifferentIndividuals();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.DifferentIndividuals#getDifferentIndividuals <em>Different Individuals</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Different Individuals</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DifferentIndividuals#getDifferentIndividuals()
	 * @see #getDifferentIndividuals()
	 * @generated
	 */
	EReference getDifferentIndividuals_DifferentIndividuals();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DisjointClasses <em>Disjoint Classes</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Disjoint Classes</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DisjointClasses
	 * @generated
	 */
	EClass getDisjointClasses();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.DisjointClasses#getDisjointClassExpressions <em>Disjoint Class Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Disjoint Class Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DisjointClasses#getDisjointClassExpressions()
	 * @see #getDisjointClasses()
	 * @generated
	 */
	EReference getDisjointClasses_DisjointClassExpressions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DisjointDataProperties <em>Disjoint Data Properties</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Disjoint Data Properties</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DisjointDataProperties
	 * @generated
	 */
	EClass getDisjointDataProperties();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.DisjointDataProperties#getDataPropertyExpressions <em>Data Property Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Data Property Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DisjointDataProperties#getDataPropertyExpressions()
	 * @see #getDisjointDataProperties()
	 * @generated
	 */
	EReference getDisjointDataProperties_DataPropertyExpressions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DisjointObjectProperties <em>Disjoint Object Properties</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Disjoint Object Properties</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DisjointObjectProperties
	 * @generated
	 */
	EClass getDisjointObjectProperties();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.DisjointObjectProperties#getObjectPropertyExpressions <em>Object Property Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Object Property Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DisjointObjectProperties#getObjectPropertyExpressions()
	 * @see #getDisjointObjectProperties()
	 * @generated
	 */
	EReference getDisjointObjectProperties_ObjectPropertyExpressions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DisjointUnion <em>Disjoint Union</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Disjoint Union</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DisjointUnion
	 * @generated
	 */
	EClass getDisjointUnion();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DisjointUnion#getUnionClass <em>Union Class</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Union Class</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DisjointUnion#getUnionClass()
	 * @see #getDisjointUnion()
	 * @generated
	 */
	EReference getDisjointUnion_UnionClass();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.DisjointUnion#getDisjointClassExpressions <em>Disjoint Class Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Disjoint Class Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DisjointUnion#getDisjointClassExpressions()
	 * @see #getDisjointUnion()
	 * @generated
	 */
	EReference getDisjointUnion_DisjointClassExpressions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.EquivalentClasses <em>Equivalent Classes</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Equivalent Classes</em>'.
	 * @see carisma.modeltype.owl2.model.owl.EquivalentClasses
	 * @generated
	 */
	EClass getEquivalentClasses();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.EquivalentClasses#getEquivalentClassExpressions <em>Equivalent Class Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Equivalent Class Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.EquivalentClasses#getEquivalentClassExpressions()
	 * @see #getEquivalentClasses()
	 * @generated
	 */
	EReference getEquivalentClasses_EquivalentClassExpressions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.EquivalentDataProperties <em>Equivalent Data Properties</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Equivalent Data Properties</em>'.
	 * @see carisma.modeltype.owl2.model.owl.EquivalentDataProperties
	 * @generated
	 */
	EClass getEquivalentDataProperties();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.EquivalentDataProperties#getDataPropertyExpressions <em>Data Property Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Data Property Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.EquivalentDataProperties#getDataPropertyExpressions()
	 * @see #getEquivalentDataProperties()
	 * @generated
	 */
	EReference getEquivalentDataProperties_DataPropertyExpressions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.FunctionalDataProperty <em>Functional Data Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Functional Data Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.FunctionalDataProperty
	 * @generated
	 */
	EClass getFunctionalDataProperty();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.FunctionalDataProperty#getDataPropertyExpression <em>Data Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.FunctionalDataProperty#getDataPropertyExpression()
	 * @see #getFunctionalDataProperty()
	 * @generated
	 */
	EReference getFunctionalDataProperty_DataPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.EquivalentObjectProperties <em>Equivalent Object Properties</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Equivalent Object Properties</em>'.
	 * @see carisma.modeltype.owl2.model.owl.EquivalentObjectProperties
	 * @generated
	 */
	EClass getEquivalentObjectProperties();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.EquivalentObjectProperties#getObjectPropertyExpressions <em>Object Property Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Object Property Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.EquivalentObjectProperties#getObjectPropertyExpressions()
	 * @see #getEquivalentObjectProperties()
	 * @generated
	 */
	EReference getEquivalentObjectProperties_ObjectPropertyExpressions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.FunctionalObjectProperty <em>Functional Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Functional Object Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.FunctionalObjectProperty
	 * @generated
	 */
	EClass getFunctionalObjectProperty();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.FunctionalObjectProperty#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.FunctionalObjectProperty#getObjectPropertyExpression()
	 * @see #getFunctionalObjectProperty()
	 * @generated
	 */
	EReference getFunctionalObjectProperty_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.InverseFunctionalObjectProperty <em>Inverse Functional Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Inverse Functional Object Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.InverseFunctionalObjectProperty
	 * @generated
	 */
	EClass getInverseFunctionalObjectProperty();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.InverseFunctionalObjectProperty#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.InverseFunctionalObjectProperty#getObjectPropertyExpression()
	 * @see #getInverseFunctionalObjectProperty()
	 * @generated
	 */
	EReference getInverseFunctionalObjectProperty_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyAssertion <em>Object Property Assertion</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Property Assertion</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyAssertion
	 * @generated
	 */
	EClass getObjectPropertyAssertion();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyAssertion#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyAssertion#getObjectPropertyExpression()
	 * @see #getObjectPropertyAssertion()
	 * @generated
	 */
	EReference getObjectPropertyAssertion_ObjectPropertyExpression();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyAssertion#getSourceIndividual <em>Source Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Source Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyAssertion#getSourceIndividual()
	 * @see #getObjectPropertyAssertion()
	 * @generated
	 */
	EReference getObjectPropertyAssertion_SourceIndividual();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyAssertion#getTargetIndividual <em>Target Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Target Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyAssertion#getTargetIndividual()
	 * @see #getObjectPropertyAssertion()
	 * @generated
	 */
	EReference getObjectPropertyAssertion_TargetIndividual();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.NegativeObjectPropertyAssertion <em>Negative Object Property Assertion</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Negative Object Property Assertion</em>'.
	 * @see carisma.modeltype.owl2.model.owl.NegativeObjectPropertyAssertion
	 * @generated
	 */
	EClass getNegativeObjectPropertyAssertion();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.NegativeObjectPropertyAssertion#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.NegativeObjectPropertyAssertion#getObjectPropertyExpression()
	 * @see #getNegativeObjectPropertyAssertion()
	 * @generated
	 */
	EReference getNegativeObjectPropertyAssertion_ObjectPropertyExpression();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.NegativeObjectPropertyAssertion#getSourceIndividual <em>Source Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Source Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.NegativeObjectPropertyAssertion#getSourceIndividual()
	 * @see #getNegativeObjectPropertyAssertion()
	 * @generated
	 */
	EReference getNegativeObjectPropertyAssertion_SourceIndividual();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.NegativeObjectPropertyAssertion#getTargetIndividual <em>Target Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Target Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.NegativeObjectPropertyAssertion#getTargetIndividual()
	 * @see #getNegativeObjectPropertyAssertion()
	 * @generated
	 */
	EReference getNegativeObjectPropertyAssertion_TargetIndividual();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyDomain <em>Object Property Domain</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Property Domain</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyDomain
	 * @generated
	 */
	EClass getObjectPropertyDomain();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyDomain#getDomain <em>Domain</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Domain</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyDomain#getDomain()
	 * @see #getObjectPropertyDomain()
	 * @generated
	 */
	EReference getObjectPropertyDomain_Domain();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyDomain#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyDomain#getObjectPropertyExpression()
	 * @see #getObjectPropertyDomain()
	 * @generated
	 */
	EReference getObjectPropertyDomain_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.SymmetricObjectProperty <em>Symmetric Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Symmetric Object Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SymmetricObjectProperty
	 * @generated
	 */
	EClass getSymmetricObjectProperty();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.SymmetricObjectProperty#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SymmetricObjectProperty#getObjectPropertyExpression()
	 * @see #getSymmetricObjectProperty()
	 * @generated
	 */
	EReference getSymmetricObjectProperty_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ReflexiveObjectProperty <em>Reflexive Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Reflexive Object Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ReflexiveObjectProperty
	 * @generated
	 */
	EClass getReflexiveObjectProperty();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ReflexiveObjectProperty#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ReflexiveObjectProperty#getObjectPropertyExpression()
	 * @see #getReflexiveObjectProperty()
	 * @generated
	 */
	EReference getReflexiveObjectProperty_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.SubDataPropertyOf <em>Sub Data Property Of</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Sub Data Property Of</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SubDataPropertyOf
	 * @generated
	 */
	EClass getSubDataPropertyOf();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.SubDataPropertyOf#getSuperDataPropertyExpression <em>Super Data Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Super Data Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SubDataPropertyOf#getSuperDataPropertyExpression()
	 * @see #getSubDataPropertyOf()
	 * @generated
	 */
	EReference getSubDataPropertyOf_SuperDataPropertyExpression();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.SubDataPropertyOf#getSubDataPropertyExpression <em>Sub Data Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Sub Data Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SubDataPropertyOf#getSubDataPropertyExpression()
	 * @see #getSubDataPropertyOf()
	 * @generated
	 */
	EReference getSubDataPropertyOf_SubDataPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.SameIndividual <em>Same Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Same Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SameIndividual
	 * @generated
	 */
	EClass getSameIndividual();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.SameIndividual#getSameIndividuals <em>Same Individuals</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Same Individuals</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SameIndividual#getSameIndividuals()
	 * @see #getSameIndividual()
	 * @generated
	 */
	EReference getSameIndividual_SameIndividuals();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.SubObjectPropertyOf <em>Sub Object Property Of</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Sub Object Property Of</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SubObjectPropertyOf
	 * @generated
	 */
	EClass getSubObjectPropertyOf();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.SubObjectPropertyOf#getSuperObjectPropertyExpression <em>Super Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Super Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SubObjectPropertyOf#getSuperObjectPropertyExpression()
	 * @see #getSubObjectPropertyOf()
	 * @generated
	 */
	EReference getSubObjectPropertyOf_SuperObjectPropertyExpression();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.SubObjectPropertyOf#getSubObjectPropertyExpressions <em>Sub Object Property Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Sub Object Property Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SubObjectPropertyOf#getSubObjectPropertyExpressions()
	 * @see #getSubObjectPropertyOf()
	 * @generated
	 */
	EReference getSubObjectPropertyOf_SubObjectPropertyExpressions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectComplementOf <em>Object Complement Of</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Complement Of</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectComplementOf
	 * @generated
	 */
	EClass getObjectComplementOf();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectComplementOf#getClassExpression <em>Class Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Class Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectComplementOf#getClassExpression()
	 * @see #getObjectComplementOf()
	 * @generated
	 */
	EReference getObjectComplementOf_ClassExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.Ontology <em>Ontology</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Ontology</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Ontology
	 * @generated
	 */
	EClass getOntology();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.modeltype.owl2.model.owl.Ontology#getOntologyAnnotations <em>Ontology Annotations</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Ontology Annotations</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Ontology#getOntologyAnnotations()
	 * @see #getOntology()
	 * @generated
	 */
	EReference getOntology_OntologyAnnotations();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.modeltype.owl2.model.owl.Ontology#getAxioms <em>Axioms</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Axioms</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Ontology#getAxioms()
	 * @see #getOntology()
	 * @generated
	 */
	EReference getOntology_Axioms();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.modeltype.owl2.model.owl.Ontology#getImportedOntologies <em>Imported Ontologies</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Imported Ontologies</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Ontology#getImportedOntologies()
	 * @see #getOntology()
	 * @generated
	 */
	EReference getOntology_ImportedOntologies();

	/**
	 * Returns the meta object for the containment reference '{@link carisma.modeltype.owl2.model.owl.Ontology#getOntologyURI <em>Ontology URI</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Ontology URI</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Ontology#getOntologyURI()
	 * @see #getOntology()
	 * @generated
	 */
	EReference getOntology_OntologyURI();

	/**
	 * Returns the meta object for the containment reference '{@link carisma.modeltype.owl2.model.owl.Ontology#getVersionURI <em>Version URI</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Version URI</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Ontology#getVersionURI()
	 * @see #getOntology()
	 * @generated
	 */
	EReference getOntology_VersionURI();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.modeltype.owl2.model.owl.Ontology#getContainer <em>Container</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Container</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Ontology#getContainer()
	 * @see #getOntology()
	 * @generated
	 */
	EReference getOntology_Container();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.Ontology#getOntologyId <em>Ontology Id</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Ontology Id</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Ontology#getOntologyId()
	 * @see #getOntology()
	 * @generated
	 */
	EAttribute getOntology_OntologyId();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyRange <em>Object Property Range</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Property Range</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyRange
	 * @generated
	 */
	EClass getObjectPropertyRange();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyRange#getRange <em>Range</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Range</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyRange#getRange()
	 * @see #getObjectPropertyRange()
	 * @generated
	 */
	EReference getObjectPropertyRange_Range();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectPropertyRange#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectPropertyRange#getObjectPropertyExpression()
	 * @see #getObjectPropertyRange()
	 * @generated
	 */
	EReference getObjectPropertyRange_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataPropertyAssertion <em>Data Property Assertion</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data Property Assertion</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyAssertion
	 * @generated
	 */
	EClass getDataPropertyAssertion();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataPropertyAssertion#getDataPropertyExpression <em>Data Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyAssertion#getDataPropertyExpression()
	 * @see #getDataPropertyAssertion()
	 * @generated
	 */
	EReference getDataPropertyAssertion_DataPropertyExpression();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataPropertyAssertion#getTargetValue <em>Target Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Target Value</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyAssertion#getTargetValue()
	 * @see #getDataPropertyAssertion()
	 * @generated
	 */
	EReference getDataPropertyAssertion_TargetValue();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataPropertyAssertion#getSourceIndividual <em>Source Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Source Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataPropertyAssertion#getSourceIndividual()
	 * @see #getDataPropertyAssertion()
	 * @generated
	 */
	EReference getDataPropertyAssertion_SourceIndividual();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ClassAssertion <em>Class Assertion</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Class Assertion</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ClassAssertion
	 * @generated
	 */
	EClass getClassAssertion();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ClassAssertion#getIndividual <em>Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ClassAssertion#getIndividual()
	 * @see #getClassAssertion()
	 * @generated
	 */
	EReference getClassAssertion_Individual();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ClassAssertion#getClassExpression <em>Class Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Class Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ClassAssertion#getClassExpression()
	 * @see #getClassAssertion()
	 * @generated
	 */
	EReference getClassAssertion_ClassExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.IrreflexiveObjectProperty <em>Irreflexive Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Irreflexive Object Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.IrreflexiveObjectProperty
	 * @generated
	 */
	EClass getIrreflexiveObjectProperty();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.IrreflexiveObjectProperty#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.IrreflexiveObjectProperty#getObjectPropertyExpression()
	 * @see #getIrreflexiveObjectProperty()
	 * @generated
	 */
	EReference getIrreflexiveObjectProperty_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectExactCardinality <em>Object Exact Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object Exact Cardinality</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectExactCardinality
	 * @generated
	 */
	EClass getObjectExactCardinality();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.ObjectExactCardinality#getCardinality <em>Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Cardinality</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectExactCardinality#getCardinality()
	 * @see #getObjectExactCardinality()
	 * @generated
	 */
	EAttribute getObjectExactCardinality_Cardinality();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectExactCardinality#getClassExpression <em>Class Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Class Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectExactCardinality#getClassExpression()
	 * @see #getObjectExactCardinality()
	 * @generated
	 */
	EReference getObjectExactCardinality_ClassExpression();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.ObjectExactCardinality#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectExactCardinality#getObjectPropertyExpression()
	 * @see #getObjectExactCardinality()
	 * @generated
	 */
	EReference getObjectExactCardinality_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.DataComplementOf <em>Data Complement Of</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Data Complement Of</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataComplementOf
	 * @generated
	 */
	EClass getDataComplementOf();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.DataComplementOf#getDataRange <em>Data Range</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Data Range</em>'.
	 * @see carisma.modeltype.owl2.model.owl.DataComplementOf#getDataRange()
	 * @see #getDataComplementOf()
	 * @generated
	 */
	EReference getDataComplementOf_DataRange();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.SubClassOf <em>Sub Class Of</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Sub Class Of</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SubClassOf
	 * @generated
	 */
	EClass getSubClassOf();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.SubClassOf#getSubClassExpression <em>Sub Class Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Sub Class Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SubClassOf#getSubClassExpression()
	 * @see #getSubClassOf()
	 * @generated
	 */
	EReference getSubClassOf_SubClassExpression();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.SubClassOf#getSuperClassExpression <em>Super Class Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Super Class Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SubClassOf#getSuperClassExpression()
	 * @see #getSubClassOf()
	 * @generated
	 */
	EReference getSubClassOf_SuperClassExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.SubObjectProperty <em>Sub Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Sub Object Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.SubObjectProperty
	 * @generated
	 */
	EClass getSubObjectProperty();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.TransitiveObjectProperty <em>Transitive Object Property</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Transitive Object Property</em>'.
	 * @see carisma.modeltype.owl2.model.owl.TransitiveObjectProperty
	 * @generated
	 */
	EClass getTransitiveObjectProperty();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.TransitiveObjectProperty#getObjectPropertyExpression <em>Object Property Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Object Property Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.TransitiveObjectProperty#getObjectPropertyExpression()
	 * @see #getTransitiveObjectProperty()
	 * @generated
	 */
	EReference getTransitiveObjectProperty_ObjectPropertyExpression();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.EntityAnnotation <em>Entity Annotation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Entity Annotation</em>'.
	 * @see carisma.modeltype.owl2.model.owl.EntityAnnotation
	 * @generated
	 */
	EClass getEntityAnnotation();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.EntityAnnotation#getEntity <em>Entity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Entity</em>'.
	 * @see carisma.modeltype.owl2.model.owl.EntityAnnotation#getEntity()
	 * @see #getEntityAnnotation()
	 * @generated
	 */
	EReference getEntityAnnotation_Entity();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.EntityAnnotation#getEntityAnnotations <em>Entity Annotations</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Entity Annotations</em>'.
	 * @see carisma.modeltype.owl2.model.owl.EntityAnnotation#getEntityAnnotations()
	 * @see #getEntityAnnotation()
	 * @generated
	 */
	EReference getEntityAnnotation_EntityAnnotations();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.FullURI <em>Full URI</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Full URI</em>'.
	 * @see carisma.modeltype.owl2.model.owl.FullURI
	 * @generated
	 */
	EClass getFullURI();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.FullURI#getIri <em>Iri</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Iri</em>'.
	 * @see carisma.modeltype.owl2.model.owl.FullURI#getIri()
	 * @see #getFullURI()
	 * @generated
	 */
	EAttribute getFullURI_Iri();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.AbbreviatedURI <em>Abbreviated URI</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Abbreviated URI</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AbbreviatedURI
	 * @generated
	 */
	EClass getAbbreviatedURI();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.AbbreviatedURI#getLocalName <em>Local Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Local Name</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AbbreviatedURI#getLocalName()
	 * @see #getAbbreviatedURI()
	 * @generated
	 */
	EAttribute getAbbreviatedURI_LocalName();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.InverseObjectProperties <em>Inverse Object Properties</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Inverse Object Properties</em>'.
	 * @see carisma.modeltype.owl2.model.owl.InverseObjectProperties
	 * @generated
	 */
	EClass getInverseObjectProperties();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.InverseObjectProperties#getInverseObjectProperties <em>Inverse Object Properties</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Inverse Object Properties</em>'.
	 * @see carisma.modeltype.owl2.model.owl.InverseObjectProperties#getInverseObjectProperties()
	 * @see #getInverseObjectProperties()
	 * @generated
	 */
	EReference getInverseObjectProperties_InverseObjectProperties();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.AnnotationByConstant <em>Annotation By Constant</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Annotation By Constant</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AnnotationByConstant
	 * @generated
	 */
	EClass getAnnotationByConstant();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.AnnotationByConstant#getAnnotationValue <em>Annotation Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Annotation Value</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AnnotationByConstant#getAnnotationValue()
	 * @see #getAnnotationByConstant()
	 * @generated
	 */
	EReference getAnnotationByConstant_AnnotationValue();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.AnnotationByEntity <em>Annotation By Entity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Annotation By Entity</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AnnotationByEntity
	 * @generated
	 */
	EClass getAnnotationByEntity();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.AnnotationByEntity#getAnnotationValue <em>Annotation Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Annotation Value</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AnnotationByEntity#getAnnotationValue()
	 * @see #getAnnotationByEntity()
	 * @generated
	 */
	EReference getAnnotationByEntity_AnnotationValue();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.AnnotationByAnonymousIndividual <em>Annotation By Anonymous Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Annotation By Anonymous Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AnnotationByAnonymousIndividual
	 * @generated
	 */
	EClass getAnnotationByAnonymousIndividual();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.AnnotationByAnonymousIndividual#getAnnotationValue <em>Annotation Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Annotation Value</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AnnotationByAnonymousIndividual#getAnnotationValue()
	 * @see #getAnnotationByAnonymousIndividual()
	 * @generated
	 */
	EReference getAnnotationByAnonymousIndividual_AnnotationValue();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.AnonymousIndividual <em>Anonymous Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Anonymous Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AnonymousIndividual
	 * @generated
	 */
	EClass getAnonymousIndividual();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.owl2.model.owl.AnonymousIndividual#getNodeID <em>Node ID</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Node ID</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AnonymousIndividual#getNodeID()
	 * @see #getAnonymousIndividual()
	 * @generated
	 */
	EAttribute getAnonymousIndividual_NodeID();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.Declaration <em>Declaration</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Declaration</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Declaration
	 * @generated
	 */
	EClass getDeclaration();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.Declaration#getEntity <em>Entity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Entity</em>'.
	 * @see carisma.modeltype.owl2.model.owl.Declaration#getEntity()
	 * @see #getDeclaration()
	 * @generated
	 */
	EReference getDeclaration_Entity();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.ObjectAndDataPropertyAxiom <em>Object And Data Property Axiom</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Object And Data Property Axiom</em>'.
	 * @see carisma.modeltype.owl2.model.owl.ObjectAndDataPropertyAxiom
	 * @generated
	 */
	EClass getObjectAndDataPropertyAxiom();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.KeyFor <em>Key For</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Key For</em>'.
	 * @see carisma.modeltype.owl2.model.owl.KeyFor
	 * @generated
	 */
	EClass getKeyFor();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.KeyFor#getClassExpression <em>Class Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Class Expression</em>'.
	 * @see carisma.modeltype.owl2.model.owl.KeyFor#getClassExpression()
	 * @see #getKeyFor()
	 * @generated
	 */
	EReference getKeyFor_ClassExpression();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.KeyFor#getDataPropertyExpressions <em>Data Property Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Data Property Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.KeyFor#getDataPropertyExpressions()
	 * @see #getKeyFor()
	 * @generated
	 */
	EReference getKeyFor_DataPropertyExpressions();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.KeyFor#getObjectPropertyExpressions <em>Object Property Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Object Property Expressions</em>'.
	 * @see carisma.modeltype.owl2.model.owl.KeyFor#getObjectPropertyExpressions()
	 * @see #getKeyFor()
	 * @generated
	 */
	EReference getKeyFor_ObjectPropertyExpressions();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.owl2.model.owl.AnonymousIndividualAnnotation <em>Anonymous Individual Annotation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Anonymous Individual Annotation</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AnonymousIndividualAnnotation
	 * @generated
	 */
	EClass getAnonymousIndividualAnnotation();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.owl2.model.owl.AnonymousIndividualAnnotation#getAnonymousIndividual <em>Anonymous Individual</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Anonymous Individual</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AnonymousIndividualAnnotation#getAnonymousIndividual()
	 * @see #getAnonymousIndividualAnnotation()
	 * @generated
	 */
	EReference getAnonymousIndividualAnnotation_AnonymousIndividual();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.owl2.model.owl.AnonymousIndividualAnnotation#getAnonymousIndiviudalAnnotations <em>Anonymous Indiviudal Annotations</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Anonymous Indiviudal Annotations</em>'.
	 * @see carisma.modeltype.owl2.model.owl.AnonymousIndividualAnnotation#getAnonymousIndiviudalAnnotations()
	 * @see #getAnonymousIndividualAnnotation()
	 * @generated
	 */
	EReference getAnonymousIndividualAnnotation_AnonymousIndiviudalAnnotations();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	OwlFactory getOwlFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.AssertionImpl <em>Assertion</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.AssertionImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAssertion()
		 * @generated
		 */
		EClass ASSERTION = eINSTANCE.getAssertion();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.AxiomImpl <em>Axiom</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.AxiomImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAxiom()
		 * @generated
		 */
		EClass AXIOM = eINSTANCE.getAxiom();

		/**
		 * The meta object literal for the '<em><b>Axiom Annotations</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference AXIOM__AXIOM_ANNOTATIONS = eINSTANCE.getAxiom_AxiomAnnotations();

		/**
		 * The meta object literal for the '<em><b>Axiom Id</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute AXIOM__AXIOM_ID = eINSTANCE.getAxiom_AxiomId();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.AnnotationImpl <em>Annotation</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.AnnotationImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnnotation()
		 * @generated
		 */
		EClass ANNOTATION = eINSTANCE.getAnnotation();

		/**
		 * The meta object literal for the '<em><b>Annotation Property</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ANNOTATION__ANNOTATION_PROPERTY = eINSTANCE.getAnnotation_AnnotationProperty();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.AnnotationPropertyImpl <em>Annotation Property</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.AnnotationPropertyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnnotationProperty()
		 * @generated
		 */
		EClass ANNOTATION_PROPERTY = eINSTANCE.getAnnotationProperty();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.EntityImpl <em>Entity</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.EntityImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getEntity()
		 * @generated
		 */
		EClass ENTITY = eINSTANCE.getEntity();

		/**
		 * The meta object literal for the '<em><b>Entity URI</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ENTITY__ENTITY_URI = eINSTANCE.getEntity_EntityURI();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.URIImpl <em>URI</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.URIImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getURI()
		 * @generated
		 */
		EClass URI = eINSTANCE.getURI();

		/**
		 * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute URI__VALUE = eINSTANCE.getURI_Value();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ConstantImpl <em>Constant</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ConstantImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getConstant()
		 * @generated
		 */
		EClass CONSTANT = eINSTANCE.getConstant();

		/**
		 * The meta object literal for the '<em><b>Lexical Value</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CONSTANT__LEXICAL_VALUE = eINSTANCE.getConstant_LexicalValue();

		/**
		 * The meta object literal for the '<em><b>Datatype</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CONSTANT__DATATYPE = eINSTANCE.getConstant_Datatype();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DatatypeImpl <em>Datatype</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DatatypeImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDatatype()
		 * @generated
		 */
		EClass DATATYPE = eINSTANCE.getDatatype();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataRangeImpl <em>Data Range</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataRangeImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataRange()
		 * @generated
		 */
		EClass DATA_RANGE = eINSTANCE.getDataRange();

		/**
		 * The meta object literal for the '<em><b>Arity</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute DATA_RANGE__ARITY = eINSTANCE.getDataRange_Arity();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyAxiomImpl <em>Data Property Axiom</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataPropertyAxiomImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataPropertyAxiom()
		 * @generated
		 */
		EClass DATA_PROPERTY_AXIOM = eINSTANCE.getDataPropertyAxiom();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectPropertyAxiomImpl <em>Object Property Axiom</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectPropertyAxiomImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectPropertyAxiom()
		 * @generated
		 */
		EClass OBJECT_PROPERTY_AXIOM = eINSTANCE.getObjectPropertyAxiom();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ClassExpressionImpl <em>Class Expression</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ClassExpressionImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getClassExpression()
		 * @generated
		 */
		EClass CLASS_EXPRESSION = eINSTANCE.getClassExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ClassAxiomImpl <em>Class Axiom</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ClassAxiomImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getClassAxiom()
		 * @generated
		 */
		EClass CLASS_AXIOM = eINSTANCE.getClassAxiom();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyExpressionImpl <em>Data Property Expression</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataPropertyExpressionImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataPropertyExpression()
		 * @generated
		 */
		EClass DATA_PROPERTY_EXPRESSION = eINSTANCE.getDataPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectPropertyExpressionImpl <em>Object Property Expression</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectPropertyExpressionImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectPropertyExpression()
		 * @generated
		 */
		EClass OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.AsymmetricObjectPropertyImpl <em>Asymmetric Object Property</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.AsymmetricObjectPropertyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAsymmetricObjectProperty()
		 * @generated
		 */
		EClass ASYMMETRIC_OBJECT_PROPERTY = eINSTANCE.getAsymmetricObjectProperty();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ASYMMETRIC_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getAsymmetricObjectProperty_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectPropertyImpl <em>Object Property</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectPropertyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectProperty()
		 * @generated
		 */
		EClass OBJECT_PROPERTY = eINSTANCE.getObjectProperty();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.InverseObjectPropertyImpl <em>Inverse Object Property</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.InverseObjectPropertyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getInverseObjectProperty()
		 * @generated
		 */
		EClass INVERSE_OBJECT_PROPERTY = eINSTANCE.getInverseObjectProperty();

		/**
		 * The meta object literal for the '<em><b>Object Property</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference INVERSE_OBJECT_PROPERTY__OBJECT_PROPERTY = eINSTANCE.getInverseObjectProperty_ObjectProperty();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ClassImpl <em>Class</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ClassImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getClass_()
		 * @generated
		 */
		EClass CLASS = eINSTANCE.getClass_();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectIntersectionOfImpl <em>Object Intersection Of</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectIntersectionOfImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectIntersectionOf()
		 * @generated
		 */
		EClass OBJECT_INTERSECTION_OF = eINSTANCE.getObjectIntersectionOf();

		/**
		 * The meta object literal for the '<em><b>Class Expressions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_INTERSECTION_OF__CLASS_EXPRESSIONS = eINSTANCE.getObjectIntersectionOf_ClassExpressions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectUnionOfImpl <em>Object Union Of</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectUnionOfImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectUnionOf()
		 * @generated
		 */
		EClass OBJECT_UNION_OF = eINSTANCE.getObjectUnionOf();

		/**
		 * The meta object literal for the '<em><b>Class Expressions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_UNION_OF__CLASS_EXPRESSIONS = eINSTANCE.getObjectUnionOf_ClassExpressions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectOneOfImpl <em>Object One Of</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectOneOfImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectOneOf()
		 * @generated
		 */
		EClass OBJECT_ONE_OF = eINSTANCE.getObjectOneOf();

		/**
		 * The meta object literal for the '<em><b>Individuals</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_ONE_OF__INDIVIDUALS = eINSTANCE.getObjectOneOf_Individuals();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.IndividualImpl <em>Individual</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.IndividualImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getIndividual()
		 * @generated
		 */
		EClass INDIVIDUAL = eINSTANCE.getIndividual();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.NamedIndividualImpl <em>Named Individual</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.NamedIndividualImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getNamedIndividual()
		 * @generated
		 */
		EClass NAMED_INDIVIDUAL = eINSTANCE.getNamedIndividual();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectSomeValuesFromImpl <em>Object Some Values From</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectSomeValuesFromImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectSomeValuesFrom()
		 * @generated
		 */
		EClass OBJECT_SOME_VALUES_FROM = eINSTANCE.getObjectSomeValuesFrom();

		/**
		 * The meta object literal for the '<em><b>Class Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_SOME_VALUES_FROM__CLASS_EXPRESSION = eINSTANCE.getObjectSomeValuesFrom_ClassExpression();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_SOME_VALUES_FROM__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getObjectSomeValuesFrom_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectAllValuesFromImpl <em>Object All Values From</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectAllValuesFromImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectAllValuesFrom()
		 * @generated
		 */
		EClass OBJECT_ALL_VALUES_FROM = eINSTANCE.getObjectAllValuesFrom();

		/**
		 * The meta object literal for the '<em><b>Class Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_ALL_VALUES_FROM__CLASS_EXPRESSION = eINSTANCE.getObjectAllValuesFrom_ClassExpression();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_ALL_VALUES_FROM__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getObjectAllValuesFrom_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectExistsSelfImpl <em>Object Exists Self</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectExistsSelfImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectExistsSelf()
		 * @generated
		 */
		EClass OBJECT_EXISTS_SELF = eINSTANCE.getObjectExistsSelf();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_EXISTS_SELF__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getObjectExistsSelf_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectHasValueImpl <em>Object Has Value</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectHasValueImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectHasValue()
		 * @generated
		 */
		EClass OBJECT_HAS_VALUE = eINSTANCE.getObjectHasValue();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_HAS_VALUE__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getObjectHasValue_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '<em><b>Individual</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_HAS_VALUE__INDIVIDUAL = eINSTANCE.getObjectHasValue_Individual();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectMinCardinalityImpl <em>Object Min Cardinality</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectMinCardinalityImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectMinCardinality()
		 * @generated
		 */
		EClass OBJECT_MIN_CARDINALITY = eINSTANCE.getObjectMinCardinality();

		/**
		 * The meta object literal for the '<em><b>Cardinality</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute OBJECT_MIN_CARDINALITY__CARDINALITY = eINSTANCE.getObjectMinCardinality_Cardinality();

		/**
		 * The meta object literal for the '<em><b>Class Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_MIN_CARDINALITY__CLASS_EXPRESSION = eINSTANCE.getObjectMinCardinality_ClassExpression();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_MIN_CARDINALITY__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getObjectMinCardinality_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectMaxCardinalityImpl <em>Object Max Cardinality</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectMaxCardinalityImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectMaxCardinality()
		 * @generated
		 */
		EClass OBJECT_MAX_CARDINALITY = eINSTANCE.getObjectMaxCardinality();

		/**
		 * The meta object literal for the '<em><b>Cardinality</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute OBJECT_MAX_CARDINALITY__CARDINALITY = eINSTANCE.getObjectMaxCardinality_Cardinality();

		/**
		 * The meta object literal for the '<em><b>Class Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_MAX_CARDINALITY__CLASS_EXPRESSION = eINSTANCE.getObjectMaxCardinality_ClassExpression();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_MAX_CARDINALITY__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getObjectMaxCardinality_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataSomeValuesFromImpl <em>Data Some Values From</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataSomeValuesFromImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataSomeValuesFrom()
		 * @generated
		 */
		EClass DATA_SOME_VALUES_FROM = eINSTANCE.getDataSomeValuesFrom();

		/**
		 * The meta object literal for the '<em><b>Data Range</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_SOME_VALUES_FROM__DATA_RANGE = eINSTANCE.getDataSomeValuesFrom_DataRange();

		/**
		 * The meta object literal for the '<em><b>Data Property Expressions</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_SOME_VALUES_FROM__DATA_PROPERTY_EXPRESSIONS = eINSTANCE.getDataSomeValuesFrom_DataPropertyExpressions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyImpl <em>Data Property</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataPropertyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataProperty()
		 * @generated
		 */
		EClass DATA_PROPERTY = eINSTANCE.getDataProperty();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataOneOfImpl <em>Data One Of</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataOneOfImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataOneOf()
		 * @generated
		 */
		EClass DATA_ONE_OF = eINSTANCE.getDataOneOf();

		/**
		 * The meta object literal for the '<em><b>Constants</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_ONE_OF__CONSTANTS = eINSTANCE.getDataOneOf_Constants();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DatatypeRestrictionImpl <em>Datatype Restriction</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DatatypeRestrictionImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDatatypeRestriction()
		 * @generated
		 */
		EClass DATATYPE_RESTRICTION = eINSTANCE.getDatatypeRestriction();

		/**
		 * The meta object literal for the '<em><b>Datatype</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATATYPE_RESTRICTION__DATATYPE = eINSTANCE.getDatatypeRestriction_Datatype();

		/**
		 * The meta object literal for the '<em><b>Restrictions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATATYPE_RESTRICTION__RESTRICTIONS = eINSTANCE.getDatatypeRestriction_Restrictions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.FacetConstantPairImpl <em>Facet Constant Pair</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.FacetConstantPairImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getFacetConstantPair()
		 * @generated
		 */
		EClass FACET_CONSTANT_PAIR = eINSTANCE.getFacetConstantPair();

		/**
		 * The meta object literal for the '<em><b>Constant</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference FACET_CONSTANT_PAIR__CONSTANT = eINSTANCE.getFacetConstantPair_Constant();

		/**
		 * The meta object literal for the '<em><b>Facet</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute FACET_CONSTANT_PAIR__FACET = eINSTANCE.getFacetConstantPair_Facet();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataAllValuesFromImpl <em>Data All Values From</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataAllValuesFromImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataAllValuesFrom()
		 * @generated
		 */
		EClass DATA_ALL_VALUES_FROM = eINSTANCE.getDataAllValuesFrom();

		/**
		 * The meta object literal for the '<em><b>Data Range</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_ALL_VALUES_FROM__DATA_RANGE = eINSTANCE.getDataAllValuesFrom_DataRange();

		/**
		 * The meta object literal for the '<em><b>Data Property Expressions</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_ALL_VALUES_FROM__DATA_PROPERTY_EXPRESSIONS = eINSTANCE.getDataAllValuesFrom_DataPropertyExpressions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataHasValueImpl <em>Data Has Value</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataHasValueImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataHasValue()
		 * @generated
		 */
		EClass DATA_HAS_VALUE = eINSTANCE.getDataHasValue();

		/**
		 * The meta object literal for the '<em><b>Constant</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_HAS_VALUE__CONSTANT = eINSTANCE.getDataHasValue_Constant();

		/**
		 * The meta object literal for the '<em><b>Data Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_HAS_VALUE__DATA_PROPERTY_EXPRESSION = eINSTANCE.getDataHasValue_DataPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataMinCardinalityImpl <em>Data Min Cardinality</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataMinCardinalityImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataMinCardinality()
		 * @generated
		 */
		EClass DATA_MIN_CARDINALITY = eINSTANCE.getDataMinCardinality();

		/**
		 * The meta object literal for the '<em><b>Cardinality</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute DATA_MIN_CARDINALITY__CARDINALITY = eINSTANCE.getDataMinCardinality_Cardinality();

		/**
		 * The meta object literal for the '<em><b>Data Range</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_MIN_CARDINALITY__DATA_RANGE = eINSTANCE.getDataMinCardinality_DataRange();

		/**
		 * The meta object literal for the '<em><b>Data Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_MIN_CARDINALITY__DATA_PROPERTY_EXPRESSION = eINSTANCE.getDataMinCardinality_DataPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataMaxCardinalityImpl <em>Data Max Cardinality</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataMaxCardinalityImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataMaxCardinality()
		 * @generated
		 */
		EClass DATA_MAX_CARDINALITY = eINSTANCE.getDataMaxCardinality();

		/**
		 * The meta object literal for the '<em><b>Cardinality</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute DATA_MAX_CARDINALITY__CARDINALITY = eINSTANCE.getDataMaxCardinality_Cardinality();

		/**
		 * The meta object literal for the '<em><b>Data Range</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_MAX_CARDINALITY__DATA_RANGE = eINSTANCE.getDataMaxCardinality_DataRange();

		/**
		 * The meta object literal for the '<em><b>Data Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_MAX_CARDINALITY__DATA_PROPERTY_EXPRESSION = eINSTANCE.getDataMaxCardinality_DataPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataExactCardinalityImpl <em>Data Exact Cardinality</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataExactCardinalityImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataExactCardinality()
		 * @generated
		 */
		EClass DATA_EXACT_CARDINALITY = eINSTANCE.getDataExactCardinality();

		/**
		 * The meta object literal for the '<em><b>Cardinality</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute DATA_EXACT_CARDINALITY__CARDINALITY = eINSTANCE.getDataExactCardinality_Cardinality();

		/**
		 * The meta object literal for the '<em><b>Data Range</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_EXACT_CARDINALITY__DATA_RANGE = eINSTANCE.getDataExactCardinality_DataRange();

		/**
		 * The meta object literal for the '<em><b>Data Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_EXACT_CARDINALITY__DATA_PROPERTY_EXPRESSION = eINSTANCE.getDataExactCardinality_DataPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.NegativeDataPropertyAssertionImpl <em>Negative Data Property Assertion</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.NegativeDataPropertyAssertionImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getNegativeDataPropertyAssertion()
		 * @generated
		 */
		EClass NEGATIVE_DATA_PROPERTY_ASSERTION = eINSTANCE.getNegativeDataPropertyAssertion();

		/**
		 * The meta object literal for the '<em><b>Data Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference NEGATIVE_DATA_PROPERTY_ASSERTION__DATA_PROPERTY_EXPRESSION = eINSTANCE.getNegativeDataPropertyAssertion_DataPropertyExpression();

		/**
		 * The meta object literal for the '<em><b>Target Value</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference NEGATIVE_DATA_PROPERTY_ASSERTION__TARGET_VALUE = eINSTANCE.getNegativeDataPropertyAssertion_TargetValue();

		/**
		 * The meta object literal for the '<em><b>Source Individual</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference NEGATIVE_DATA_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL = eINSTANCE.getNegativeDataPropertyAssertion_SourceIndividual();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyDomainImpl <em>Data Property Domain</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataPropertyDomainImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataPropertyDomain()
		 * @generated
		 */
		EClass DATA_PROPERTY_DOMAIN = eINSTANCE.getDataPropertyDomain();

		/**
		 * The meta object literal for the '<em><b>Domain</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_PROPERTY_DOMAIN__DOMAIN = eINSTANCE.getDataPropertyDomain_Domain();

		/**
		 * The meta object literal for the '<em><b>Data Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_PROPERTY_DOMAIN__DATA_PROPERTY_EXPRESSION = eINSTANCE.getDataPropertyDomain_DataPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyRangeImpl <em>Data Property Range</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataPropertyRangeImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataPropertyRange()
		 * @generated
		 */
		EClass DATA_PROPERTY_RANGE = eINSTANCE.getDataPropertyRange();

		/**
		 * The meta object literal for the '<em><b>Range</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_PROPERTY_RANGE__RANGE = eINSTANCE.getDataPropertyRange_Range();

		/**
		 * The meta object literal for the '<em><b>Data Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_PROPERTY_RANGE__DATA_PROPERTY_EXPRESSION = eINSTANCE.getDataPropertyRange_DataPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DifferentIndividualsImpl <em>Different Individuals</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DifferentIndividualsImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDifferentIndividuals()
		 * @generated
		 */
		EClass DIFFERENT_INDIVIDUALS = eINSTANCE.getDifferentIndividuals();

		/**
		 * The meta object literal for the '<em><b>Different Individuals</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DIFFERENT_INDIVIDUALS__DIFFERENT_INDIVIDUALS = eINSTANCE.getDifferentIndividuals_DifferentIndividuals();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DisjointClassesImpl <em>Disjoint Classes</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DisjointClassesImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDisjointClasses()
		 * @generated
		 */
		EClass DISJOINT_CLASSES = eINSTANCE.getDisjointClasses();

		/**
		 * The meta object literal for the '<em><b>Disjoint Class Expressions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DISJOINT_CLASSES__DISJOINT_CLASS_EXPRESSIONS = eINSTANCE.getDisjointClasses_DisjointClassExpressions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DisjointDataPropertiesImpl <em>Disjoint Data Properties</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DisjointDataPropertiesImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDisjointDataProperties()
		 * @generated
		 */
		EClass DISJOINT_DATA_PROPERTIES = eINSTANCE.getDisjointDataProperties();

		/**
		 * The meta object literal for the '<em><b>Data Property Expressions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DISJOINT_DATA_PROPERTIES__DATA_PROPERTY_EXPRESSIONS = eINSTANCE.getDisjointDataProperties_DataPropertyExpressions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DisjointObjectPropertiesImpl <em>Disjoint Object Properties</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DisjointObjectPropertiesImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDisjointObjectProperties()
		 * @generated
		 */
		EClass DISJOINT_OBJECT_PROPERTIES = eINSTANCE.getDisjointObjectProperties();

		/**
		 * The meta object literal for the '<em><b>Object Property Expressions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DISJOINT_OBJECT_PROPERTIES__OBJECT_PROPERTY_EXPRESSIONS = eINSTANCE.getDisjointObjectProperties_ObjectPropertyExpressions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DisjointUnionImpl <em>Disjoint Union</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DisjointUnionImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDisjointUnion()
		 * @generated
		 */
		EClass DISJOINT_UNION = eINSTANCE.getDisjointUnion();

		/**
		 * The meta object literal for the '<em><b>Union Class</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DISJOINT_UNION__UNION_CLASS = eINSTANCE.getDisjointUnion_UnionClass();

		/**
		 * The meta object literal for the '<em><b>Disjoint Class Expressions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DISJOINT_UNION__DISJOINT_CLASS_EXPRESSIONS = eINSTANCE.getDisjointUnion_DisjointClassExpressions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.EquivalentClassesImpl <em>Equivalent Classes</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.EquivalentClassesImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getEquivalentClasses()
		 * @generated
		 */
		EClass EQUIVALENT_CLASSES = eINSTANCE.getEquivalentClasses();

		/**
		 * The meta object literal for the '<em><b>Equivalent Class Expressions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EQUIVALENT_CLASSES__EQUIVALENT_CLASS_EXPRESSIONS = eINSTANCE.getEquivalentClasses_EquivalentClassExpressions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.EquivalentDataPropertiesImpl <em>Equivalent Data Properties</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.EquivalentDataPropertiesImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getEquivalentDataProperties()
		 * @generated
		 */
		EClass EQUIVALENT_DATA_PROPERTIES = eINSTANCE.getEquivalentDataProperties();

		/**
		 * The meta object literal for the '<em><b>Data Property Expressions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EQUIVALENT_DATA_PROPERTIES__DATA_PROPERTY_EXPRESSIONS = eINSTANCE.getEquivalentDataProperties_DataPropertyExpressions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.FunctionalDataPropertyImpl <em>Functional Data Property</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.FunctionalDataPropertyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getFunctionalDataProperty()
		 * @generated
		 */
		EClass FUNCTIONAL_DATA_PROPERTY = eINSTANCE.getFunctionalDataProperty();

		/**
		 * The meta object literal for the '<em><b>Data Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference FUNCTIONAL_DATA_PROPERTY__DATA_PROPERTY_EXPRESSION = eINSTANCE.getFunctionalDataProperty_DataPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.EquivalentObjectPropertiesImpl <em>Equivalent Object Properties</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.EquivalentObjectPropertiesImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getEquivalentObjectProperties()
		 * @generated
		 */
		EClass EQUIVALENT_OBJECT_PROPERTIES = eINSTANCE.getEquivalentObjectProperties();

		/**
		 * The meta object literal for the '<em><b>Object Property Expressions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EQUIVALENT_OBJECT_PROPERTIES__OBJECT_PROPERTY_EXPRESSIONS = eINSTANCE.getEquivalentObjectProperties_ObjectPropertyExpressions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.FunctionalObjectPropertyImpl <em>Functional Object Property</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.FunctionalObjectPropertyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getFunctionalObjectProperty()
		 * @generated
		 */
		EClass FUNCTIONAL_OBJECT_PROPERTY = eINSTANCE.getFunctionalObjectProperty();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference FUNCTIONAL_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getFunctionalObjectProperty_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.InverseFunctionalObjectPropertyImpl <em>Inverse Functional Object Property</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.InverseFunctionalObjectPropertyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getInverseFunctionalObjectProperty()
		 * @generated
		 */
		EClass INVERSE_FUNCTIONAL_OBJECT_PROPERTY = eINSTANCE.getInverseFunctionalObjectProperty();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference INVERSE_FUNCTIONAL_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getInverseFunctionalObjectProperty_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectPropertyAssertionImpl <em>Object Property Assertion</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectPropertyAssertionImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectPropertyAssertion()
		 * @generated
		 */
		EClass OBJECT_PROPERTY_ASSERTION = eINSTANCE.getObjectPropertyAssertion();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_PROPERTY_ASSERTION__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getObjectPropertyAssertion_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '<em><b>Source Individual</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL = eINSTANCE.getObjectPropertyAssertion_SourceIndividual();

		/**
		 * The meta object literal for the '<em><b>Target Individual</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_PROPERTY_ASSERTION__TARGET_INDIVIDUAL = eINSTANCE.getObjectPropertyAssertion_TargetIndividual();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.NegativeObjectPropertyAssertionImpl <em>Negative Object Property Assertion</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.NegativeObjectPropertyAssertionImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getNegativeObjectPropertyAssertion()
		 * @generated
		 */
		EClass NEGATIVE_OBJECT_PROPERTY_ASSERTION = eINSTANCE.getNegativeObjectPropertyAssertion();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference NEGATIVE_OBJECT_PROPERTY_ASSERTION__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getNegativeObjectPropertyAssertion_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '<em><b>Source Individual</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference NEGATIVE_OBJECT_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL = eINSTANCE.getNegativeObjectPropertyAssertion_SourceIndividual();

		/**
		 * The meta object literal for the '<em><b>Target Individual</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference NEGATIVE_OBJECT_PROPERTY_ASSERTION__TARGET_INDIVIDUAL = eINSTANCE.getNegativeObjectPropertyAssertion_TargetIndividual();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectPropertyDomainImpl <em>Object Property Domain</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectPropertyDomainImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectPropertyDomain()
		 * @generated
		 */
		EClass OBJECT_PROPERTY_DOMAIN = eINSTANCE.getObjectPropertyDomain();

		/**
		 * The meta object literal for the '<em><b>Domain</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_PROPERTY_DOMAIN__DOMAIN = eINSTANCE.getObjectPropertyDomain_Domain();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_PROPERTY_DOMAIN__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getObjectPropertyDomain_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.SymmetricObjectPropertyImpl <em>Symmetric Object Property</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.SymmetricObjectPropertyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getSymmetricObjectProperty()
		 * @generated
		 */
		EClass SYMMETRIC_OBJECT_PROPERTY = eINSTANCE.getSymmetricObjectProperty();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SYMMETRIC_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getSymmetricObjectProperty_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ReflexiveObjectPropertyImpl <em>Reflexive Object Property</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ReflexiveObjectPropertyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getReflexiveObjectProperty()
		 * @generated
		 */
		EClass REFLEXIVE_OBJECT_PROPERTY = eINSTANCE.getReflexiveObjectProperty();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference REFLEXIVE_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getReflexiveObjectProperty_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.SubDataPropertyOfImpl <em>Sub Data Property Of</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.SubDataPropertyOfImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getSubDataPropertyOf()
		 * @generated
		 */
		EClass SUB_DATA_PROPERTY_OF = eINSTANCE.getSubDataPropertyOf();

		/**
		 * The meta object literal for the '<em><b>Super Data Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SUB_DATA_PROPERTY_OF__SUPER_DATA_PROPERTY_EXPRESSION = eINSTANCE.getSubDataPropertyOf_SuperDataPropertyExpression();

		/**
		 * The meta object literal for the '<em><b>Sub Data Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SUB_DATA_PROPERTY_OF__SUB_DATA_PROPERTY_EXPRESSION = eINSTANCE.getSubDataPropertyOf_SubDataPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.SameIndividualImpl <em>Same Individual</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.SameIndividualImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getSameIndividual()
		 * @generated
		 */
		EClass SAME_INDIVIDUAL = eINSTANCE.getSameIndividual();

		/**
		 * The meta object literal for the '<em><b>Same Individuals</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SAME_INDIVIDUAL__SAME_INDIVIDUALS = eINSTANCE.getSameIndividual_SameIndividuals();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.SubObjectPropertyOfImpl <em>Sub Object Property Of</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.SubObjectPropertyOfImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getSubObjectPropertyOf()
		 * @generated
		 */
		EClass SUB_OBJECT_PROPERTY_OF = eINSTANCE.getSubObjectPropertyOf();

		/**
		 * The meta object literal for the '<em><b>Super Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SUB_OBJECT_PROPERTY_OF__SUPER_OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getSubObjectPropertyOf_SuperObjectPropertyExpression();

		/**
		 * The meta object literal for the '<em><b>Sub Object Property Expressions</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SUB_OBJECT_PROPERTY_OF__SUB_OBJECT_PROPERTY_EXPRESSIONS = eINSTANCE.getSubObjectPropertyOf_SubObjectPropertyExpressions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectComplementOfImpl <em>Object Complement Of</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectComplementOfImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectComplementOf()
		 * @generated
		 */
		EClass OBJECT_COMPLEMENT_OF = eINSTANCE.getObjectComplementOf();

		/**
		 * The meta object literal for the '<em><b>Class Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_COMPLEMENT_OF__CLASS_EXPRESSION = eINSTANCE.getObjectComplementOf_ClassExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.OntologyImpl <em>Ontology</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.OntologyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getOntology()
		 * @generated
		 */
		EClass ONTOLOGY = eINSTANCE.getOntology();

		/**
		 * The meta object literal for the '<em><b>Ontology Annotations</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ONTOLOGY__ONTOLOGY_ANNOTATIONS = eINSTANCE.getOntology_OntologyAnnotations();

		/**
		 * The meta object literal for the '<em><b>Axioms</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ONTOLOGY__AXIOMS = eINSTANCE.getOntology_Axioms();

		/**
		 * The meta object literal for the '<em><b>Imported Ontologies</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ONTOLOGY__IMPORTED_ONTOLOGIES = eINSTANCE.getOntology_ImportedOntologies();

		/**
		 * The meta object literal for the '<em><b>Ontology URI</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ONTOLOGY__ONTOLOGY_URI = eINSTANCE.getOntology_OntologyURI();

		/**
		 * The meta object literal for the '<em><b>Version URI</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ONTOLOGY__VERSION_URI = eINSTANCE.getOntology_VersionURI();

		/**
		 * The meta object literal for the '<em><b>Container</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ONTOLOGY__CONTAINER = eINSTANCE.getOntology_Container();

		/**
		 * The meta object literal for the '<em><b>Ontology Id</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ONTOLOGY__ONTOLOGY_ID = eINSTANCE.getOntology_OntologyId();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectPropertyRangeImpl <em>Object Property Range</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectPropertyRangeImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectPropertyRange()
		 * @generated
		 */
		EClass OBJECT_PROPERTY_RANGE = eINSTANCE.getObjectPropertyRange();

		/**
		 * The meta object literal for the '<em><b>Range</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_PROPERTY_RANGE__RANGE = eINSTANCE.getObjectPropertyRange_Range();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_PROPERTY_RANGE__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getObjectPropertyRange_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyAssertionImpl <em>Data Property Assertion</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataPropertyAssertionImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataPropertyAssertion()
		 * @generated
		 */
		EClass DATA_PROPERTY_ASSERTION = eINSTANCE.getDataPropertyAssertion();

		/**
		 * The meta object literal for the '<em><b>Data Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_PROPERTY_ASSERTION__DATA_PROPERTY_EXPRESSION = eINSTANCE.getDataPropertyAssertion_DataPropertyExpression();

		/**
		 * The meta object literal for the '<em><b>Target Value</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_PROPERTY_ASSERTION__TARGET_VALUE = eINSTANCE.getDataPropertyAssertion_TargetValue();

		/**
		 * The meta object literal for the '<em><b>Source Individual</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL = eINSTANCE.getDataPropertyAssertion_SourceIndividual();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ClassAssertionImpl <em>Class Assertion</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ClassAssertionImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getClassAssertion()
		 * @generated
		 */
		EClass CLASS_ASSERTION = eINSTANCE.getClassAssertion();

		/**
		 * The meta object literal for the '<em><b>Individual</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CLASS_ASSERTION__INDIVIDUAL = eINSTANCE.getClassAssertion_Individual();

		/**
		 * The meta object literal for the '<em><b>Class Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CLASS_ASSERTION__CLASS_EXPRESSION = eINSTANCE.getClassAssertion_ClassExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.IrreflexiveObjectPropertyImpl <em>Irreflexive Object Property</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.IrreflexiveObjectPropertyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getIrreflexiveObjectProperty()
		 * @generated
		 */
		EClass IRREFLEXIVE_OBJECT_PROPERTY = eINSTANCE.getIrreflexiveObjectProperty();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference IRREFLEXIVE_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getIrreflexiveObjectProperty_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectExactCardinalityImpl <em>Object Exact Cardinality</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectExactCardinalityImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectExactCardinality()
		 * @generated
		 */
		EClass OBJECT_EXACT_CARDINALITY = eINSTANCE.getObjectExactCardinality();

		/**
		 * The meta object literal for the '<em><b>Cardinality</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute OBJECT_EXACT_CARDINALITY__CARDINALITY = eINSTANCE.getObjectExactCardinality_Cardinality();

		/**
		 * The meta object literal for the '<em><b>Class Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_EXACT_CARDINALITY__CLASS_EXPRESSION = eINSTANCE.getObjectExactCardinality_ClassExpression();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OBJECT_EXACT_CARDINALITY__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getObjectExactCardinality_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DataComplementOfImpl <em>Data Complement Of</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DataComplementOfImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDataComplementOf()
		 * @generated
		 */
		EClass DATA_COMPLEMENT_OF = eINSTANCE.getDataComplementOf();

		/**
		 * The meta object literal for the '<em><b>Data Range</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATA_COMPLEMENT_OF__DATA_RANGE = eINSTANCE.getDataComplementOf_DataRange();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.SubClassOfImpl <em>Sub Class Of</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.SubClassOfImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getSubClassOf()
		 * @generated
		 */
		EClass SUB_CLASS_OF = eINSTANCE.getSubClassOf();

		/**
		 * The meta object literal for the '<em><b>Sub Class Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SUB_CLASS_OF__SUB_CLASS_EXPRESSION = eINSTANCE.getSubClassOf_SubClassExpression();

		/**
		 * The meta object literal for the '<em><b>Super Class Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SUB_CLASS_OF__SUPER_CLASS_EXPRESSION = eINSTANCE.getSubClassOf_SuperClassExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.SubObjectPropertyImpl <em>Sub Object Property</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.SubObjectPropertyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getSubObjectProperty()
		 * @generated
		 */
		EClass SUB_OBJECT_PROPERTY = eINSTANCE.getSubObjectProperty();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.TransitiveObjectPropertyImpl <em>Transitive Object Property</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.TransitiveObjectPropertyImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getTransitiveObjectProperty()
		 * @generated
		 */
		EClass TRANSITIVE_OBJECT_PROPERTY = eINSTANCE.getTransitiveObjectProperty();

		/**
		 * The meta object literal for the '<em><b>Object Property Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TRANSITIVE_OBJECT_PROPERTY__OBJECT_PROPERTY_EXPRESSION = eINSTANCE.getTransitiveObjectProperty_ObjectPropertyExpression();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.EntityAnnotationImpl <em>Entity Annotation</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.EntityAnnotationImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getEntityAnnotation()
		 * @generated
		 */
		EClass ENTITY_ANNOTATION = eINSTANCE.getEntityAnnotation();

		/**
		 * The meta object literal for the '<em><b>Entity</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ENTITY_ANNOTATION__ENTITY = eINSTANCE.getEntityAnnotation_Entity();

		/**
		 * The meta object literal for the '<em><b>Entity Annotations</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ENTITY_ANNOTATION__ENTITY_ANNOTATIONS = eINSTANCE.getEntityAnnotation_EntityAnnotations();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.FullURIImpl <em>Full URI</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.FullURIImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getFullURI()
		 * @generated
		 */
		EClass FULL_URI = eINSTANCE.getFullURI();

		/**
		 * The meta object literal for the '<em><b>Iri</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute FULL_URI__IRI = eINSTANCE.getFullURI_Iri();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.AbbreviatedURIImpl <em>Abbreviated URI</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.AbbreviatedURIImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAbbreviatedURI()
		 * @generated
		 */
		EClass ABBREVIATED_URI = eINSTANCE.getAbbreviatedURI();

		/**
		 * The meta object literal for the '<em><b>Local Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ABBREVIATED_URI__LOCAL_NAME = eINSTANCE.getAbbreviatedURI_LocalName();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.InverseObjectPropertiesImpl <em>Inverse Object Properties</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.InverseObjectPropertiesImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getInverseObjectProperties()
		 * @generated
		 */
		EClass INVERSE_OBJECT_PROPERTIES = eINSTANCE.getInverseObjectProperties();

		/**
		 * The meta object literal for the '<em><b>Inverse Object Properties</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference INVERSE_OBJECT_PROPERTIES__INVERSE_OBJECT_PROPERTIES = eINSTANCE.getInverseObjectProperties_InverseObjectProperties();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.AnnotationByConstantImpl <em>Annotation By Constant</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.AnnotationByConstantImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnnotationByConstant()
		 * @generated
		 */
		EClass ANNOTATION_BY_CONSTANT = eINSTANCE.getAnnotationByConstant();

		/**
		 * The meta object literal for the '<em><b>Annotation Value</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ANNOTATION_BY_CONSTANT__ANNOTATION_VALUE = eINSTANCE.getAnnotationByConstant_AnnotationValue();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.AnnotationByEntityImpl <em>Annotation By Entity</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.AnnotationByEntityImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnnotationByEntity()
		 * @generated
		 */
		EClass ANNOTATION_BY_ENTITY = eINSTANCE.getAnnotationByEntity();

		/**
		 * The meta object literal for the '<em><b>Annotation Value</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ANNOTATION_BY_ENTITY__ANNOTATION_VALUE = eINSTANCE.getAnnotationByEntity_AnnotationValue();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.AnnotationByAnonymousIndividualImpl <em>Annotation By Anonymous Individual</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.AnnotationByAnonymousIndividualImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnnotationByAnonymousIndividual()
		 * @generated
		 */
		EClass ANNOTATION_BY_ANONYMOUS_INDIVIDUAL = eINSTANCE.getAnnotationByAnonymousIndividual();

		/**
		 * The meta object literal for the '<em><b>Annotation Value</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ANNOTATION_BY_ANONYMOUS_INDIVIDUAL__ANNOTATION_VALUE = eINSTANCE.getAnnotationByAnonymousIndividual_AnnotationValue();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.AnonymousIndividualImpl <em>Anonymous Individual</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.AnonymousIndividualImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnonymousIndividual()
		 * @generated
		 */
		EClass ANONYMOUS_INDIVIDUAL = eINSTANCE.getAnonymousIndividual();

		/**
		 * The meta object literal for the '<em><b>Node ID</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ANONYMOUS_INDIVIDUAL__NODE_ID = eINSTANCE.getAnonymousIndividual_NodeID();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.DeclarationImpl <em>Declaration</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.DeclarationImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getDeclaration()
		 * @generated
		 */
		EClass DECLARATION = eINSTANCE.getDeclaration();

		/**
		 * The meta object literal for the '<em><b>Entity</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DECLARATION__ENTITY = eINSTANCE.getDeclaration_Entity();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.ObjectAndDataPropertyAxiomImpl <em>Object And Data Property Axiom</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.ObjectAndDataPropertyAxiomImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getObjectAndDataPropertyAxiom()
		 * @generated
		 */
		EClass OBJECT_AND_DATA_PROPERTY_AXIOM = eINSTANCE.getObjectAndDataPropertyAxiom();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.KeyForImpl <em>Key For</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.KeyForImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getKeyFor()
		 * @generated
		 */
		EClass KEY_FOR = eINSTANCE.getKeyFor();

		/**
		 * The meta object literal for the '<em><b>Class Expression</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference KEY_FOR__CLASS_EXPRESSION = eINSTANCE.getKeyFor_ClassExpression();

		/**
		 * The meta object literal for the '<em><b>Data Property Expressions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference KEY_FOR__DATA_PROPERTY_EXPRESSIONS = eINSTANCE.getKeyFor_DataPropertyExpressions();

		/**
		 * The meta object literal for the '<em><b>Object Property Expressions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference KEY_FOR__OBJECT_PROPERTY_EXPRESSIONS = eINSTANCE.getKeyFor_ObjectPropertyExpressions();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.owl2.model.owl.impl.AnonymousIndividualAnnotationImpl <em>Anonymous Individual Annotation</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.owl2.model.owl.impl.AnonymousIndividualAnnotationImpl
		 * @see carisma.modeltype.owl2.model.owl.impl.OwlPackageImpl#getAnonymousIndividualAnnotation()
		 * @generated
		 */
		EClass ANONYMOUS_INDIVIDUAL_ANNOTATION = eINSTANCE.getAnonymousIndividualAnnotation();

		/**
		 * The meta object literal for the '<em><b>Anonymous Individual</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ANONYMOUS_INDIVIDUAL_ANNOTATION__ANONYMOUS_INDIVIDUAL = eINSTANCE.getAnonymousIndividualAnnotation_AnonymousIndividual();

		/**
		 * The meta object literal for the '<em><b>Anonymous Indiviudal Annotations</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ANONYMOUS_INDIVIDUAL_ANNOTATION__ANONYMOUS_INDIVIUDAL_ANNOTATIONS = eINSTANCE.getAnonymousIndividualAnnotation_AnonymousIndiviudalAnnotations();

	}

} //OwlPackage
