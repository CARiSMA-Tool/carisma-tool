/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package carisma.regulatory.ruleallocator.datamodel;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage
 * @generated
 */
public interface DatamodelFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	DatamodelFactory eINSTANCE = carisma.regulatory.ruleallocator.datamodel.impl.DatamodelFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>Rule Element</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Rule Element</em>'.
	 * @generated
	 */
	RuleElement createRuleElement();

	/**
	 * Returns a new object of class '<em>Rule Element Type</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Rule Element Type</em>'.
	 * @generated
	 */
	RuleElementType createRuleElementType();

	/**
	 * Returns a new object of class '<em>Situation</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Situation</em>'.
	 * @generated
	 */
	Situation createSituation();

	/**
	 * Returns a new object of class '<em>Allocation</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Allocation</em>'.
	 * @generated
	 */
	Allocation createAllocation();

	/**
	 * Returns a new object of class '<em>BPMN Element</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>BPMN Element</em>'.
	 * @generated
	 */
	BPMNElement createBPMNElement();

	/**
	 * Returns a new object of class '<em>Rule Element Assosiation Type</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Rule Element Assosiation Type</em>'.
	 * @generated
	 */
	RuleElementAssociationType createRuleElementAssosiationType();

	/**
	 * Returns a new object of class '<em>Rule Element Assosation</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Rule Element Assosation</em>'.
	 * @generated
	 */
	RuleElementAssociation createRuleElementAssosation();

	/**
	 * Returns a new object of class '<em>Container</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Container</em>'.
	 * @generated
	 */
	Container createContainer();

	/**
	 * Returns a new object of class '<em>Model Element Type</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Model Element Type</em>'.
	 * @generated
	 */
	ModelElementType createModelElementType();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	DatamodelPackage getModel2Package();

} //Model2Factory
