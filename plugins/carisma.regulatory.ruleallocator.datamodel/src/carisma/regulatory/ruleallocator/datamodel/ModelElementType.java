/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package carisma.regulatory.ruleallocator.datamodel;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Model Element Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.ModelElementType#getAssociatsWith <em>Associats With</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.ModelElementType#getName <em>Name</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getModelElementType()
 * @model
 * @generated
 */
public interface ModelElementType extends EObject {
	/**
	 * Returns the value of the '<em><b>Associats With</b></em>' reference list.
	 * The list contents are of type {@link carisma.regulatory.ruleallocator.datamodel.RuleElementType}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Associats With</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Associats With</em>' reference list.
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getModelElementType_AssociatsWith()
	 * @model type="model2.RuleElementType" required="true"
	 * @generated
	 */
	EList getAssociatsWith();

	/**
	 * Returns the value of the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Name</em>' attribute.
	 * @see #setName(String)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getModelElementType_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.ModelElementType#getName <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Name</em>' attribute.
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

} // ModelElementType
