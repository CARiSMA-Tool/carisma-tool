/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package carisma.regulatory.ruleallocator.datamodel.impl;


import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

import carisma.regulatory.ruleallocator.datamodel.*;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class DatamodelFactoryImpl extends EFactoryImpl implements DatamodelFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static DatamodelFactory init() {
		try {
			DatamodelFactory theModel2Factory = (DatamodelFactory)EPackage.Registry.INSTANCE.getEFactory("http://model2/1.0"); 
			if (theModel2Factory != null) {
				return theModel2Factory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new DatamodelFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DatamodelFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
			case DatamodelPackage.RULE_ELEMENT: return createRuleElement();
			case DatamodelPackage.RULE_ELEMENT_TYPE: return createRuleElementType();
			case DatamodelPackage.SITUATION: return createSituation();
			case DatamodelPackage.ALLOCATION: return createAllocation();
			case DatamodelPackage.BPMN_ELEMENT: return createBPMNElement();
			case DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE: return createRuleElementAssosiationType();
			case DatamodelPackage.RULE_ELEMENT_ASSOSATION: return createRuleElementAssosation();
			case DatamodelPackage.CONTAINER: return createContainer();
			case DatamodelPackage.MODEL_ELEMENT_TYPE: return createModelElementType();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElement createRuleElement() {
		RuleElementImpl ruleElement = new RuleElementImpl();
		return ruleElement;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElementType createRuleElementType() {
		RuleElementTypeImpl ruleElementType = new RuleElementTypeImpl();
		return ruleElementType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Situation createSituation() {
		SituationImpl situation = new SituationImpl();
		return situation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Allocation createAllocation() {
		AllocationImpl allocation = new AllocationImpl();
		return allocation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public BPMNElement createBPMNElement() {
		BPMNElementImpl bpmnElement = new BPMNElementImpl();
		return bpmnElement;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElementAssociationType createRuleElementAssosiationType() {
		RuleElementAssociationTypeImpl ruleElementAssosiationType = new RuleElementAssociationTypeImpl();
		return ruleElementAssosiationType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElementAssociation createRuleElementAssosation() {
		RuleElementAssociationImpl ruleElementAssosation = new RuleElementAssociationImpl();
		return ruleElementAssosation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public carisma.regulatory.ruleallocator.datamodel.Container createContainer() {
		ContainerImpl container = new ContainerImpl();
		return container;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ModelElementType createModelElementType() {
		ModelElementTypeImpl modelElementType = new ModelElementTypeImpl();
		return modelElementType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DatamodelPackage getModel2Package() {
		return (DatamodelPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	public static DatamodelPackage getPackage() {
		return DatamodelPackage.eINSTANCE;
	}

} //Model2FactoryImpl
