/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package carisma.regulatory.ruleallocator.datamodel.util;


import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

import carisma.regulatory.ruleallocator.datamodel.*;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage
 * @generated
 */
public class DatamodelAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static DatamodelPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DatamodelAdapterFactory() {
		if (modelPackage == null) {
			modelPackage = DatamodelPackage.eINSTANCE;
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
	protected DatamodelSwitch modelSwitch =
		new DatamodelSwitch() {
			public Object caseRuleElement(RuleElement object) {
				return createRuleElementAdapter();
			}
			public Object caseRuleElementType(RuleElementType object) {
				return createRuleElementTypeAdapter();
			}
			public Object caseSituation(Situation object) {
				return createSituationAdapter();
			}
			public Object caseAllocation(Allocation object) {
				return createAllocationAdapter();
			}
			public Object caseBPMNElement(BPMNElement object) {
				return createBPMNElementAdapter();
			}
			public Object caseRuleElementAssosiationType(RuleElementAssociationType object) {
				return createRuleElementAssosiationTypeAdapter();
			}
			public Object caseRuleElementAssosation(RuleElementAssociation object) {
				return createRuleElementAssosationAdapter();
			}
			public Object caseContainer(Container object) {
				return createContainerAdapter();
			}
			public Object caseModelElementType(ModelElementType object) {
				return createModelElementTypeAdapter();
			}
			public Object defaultCase(EObject object) {
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
	public Adapter createAdapter(Notifier target) {
		return (Adapter)modelSwitch.doSwitch((EObject)target);
	}


	/**
	 * Creates a new adapter for an object of class '{@link carisma.regulatory.ruleallocator.datamodel.RuleElement <em>Rule Element</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElement
	 * @generated
	 */
	public Adapter createRuleElementAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementType <em>Rule Element Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElementType
	 * @generated
	 */
	public Adapter createRuleElementTypeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.regulatory.ruleallocator.datamodel.Situation <em>Situation</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.regulatory.ruleallocator.datamodel.Situation
	 * @generated
	 */
	public Adapter createSituationAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.regulatory.ruleallocator.datamodel.Allocation <em>Allocation</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.regulatory.ruleallocator.datamodel.Allocation
	 * @generated
	 */
	public Adapter createAllocationAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement <em>BPMN Element</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.regulatory.ruleallocator.datamodel.BPMNElement
	 * @generated
	 */
	public Adapter createBPMNElementAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType <em>Rule Element Assosiation Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType
	 * @generated
	 */
	public Adapter createRuleElementAssosiationTypeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation <em>Rule Element Assosation</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation
	 * @generated
	 */
	public Adapter createRuleElementAssosationAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.regulatory.ruleallocator.datamodel.Container <em>Container</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.regulatory.ruleallocator.datamodel.Container
	 * @generated
	 */
	public Adapter createContainerAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.regulatory.ruleallocator.datamodel.ModelElementType <em>Model Element Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.regulatory.ruleallocator.datamodel.ModelElementType
	 * @generated
	 */
	public Adapter createModelElementTypeAdapter() {
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

} //Model2AdapterFactory
