/**
 */
package carisma.profile.umlsec;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Action;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>rbac</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * in UML2 > can only be used in activity diagrams
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.rbac#getProtectedactions <em>Protectedactions</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rbac#getRole <em>Role</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rbac#getRight <em>Right</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rbac#getBase_Package <em>Base Package</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.UmlsecPackage#getrbac()
 * @model
 * @generated
 */
public interface rbac extends EObject {
	/**
	 * Returns the value of the '<em><b>Protectedactions</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.uml2.uml.Action}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Protectedactions</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Protectedactions</em>' reference list.
	 * @see carisma.profile.umlsec.UmlsecPackage#getrbac_Protectedactions()
	 * @model ordered="false"
	 *        annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='protected actions'"
	 * @generated
	 */
	EList<Action> getProtectedactions();

	/**
	 * Retrieves the first {@link org.eclipse.uml2.uml.Action} with the specified '<em><b>Name</b></em>' from the '<em><b>Protectedactions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param name The '<em><b>Name</b></em>' of the {@link org.eclipse.uml2.uml.Action} to retrieve, or <code>null</code>.
	 * @return The first {@link org.eclipse.uml2.uml.Action} with the specified '<em><b>Name</b></em>', or <code>null</code>.
	 * @see #getProtectedactions()
	 * @generated
	 */
	Action getProtectedactions(String name);

	/**
	 * Retrieves the first {@link org.eclipse.uml2.uml.Action} with the specified '<em><b>Name</b></em>' from the '<em><b>Protectedactions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param name The '<em><b>Name</b></em>' of the {@link org.eclipse.uml2.uml.Action} to retrieve, or <code>null</code>.
	 * @param ignoreCase Whether to ignore case in {@link java.lang.String} comparisons.
	 * @param eClass The Ecore class of the {@link org.eclipse.uml2.uml.Action} to retrieve, or <code>null</code>.
	 * @return The first {@link org.eclipse.uml2.uml.Action} with the specified '<em><b>Name</b></em>', or <code>null</code>.
	 * @see #getProtectedactions()
	 * @generated
	 */
	Action getProtectedactions(String name, boolean ignoreCase, EClass eClass);

	/**
	 * Returns the value of the '<em><b>Role</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Role</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Role</em>' attribute list.
	 * @see carisma.profile.umlsec.UmlsecPackage#getrbac_Role()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	EList<String> getRole();

	/**
	 * Returns the value of the '<em><b>Right</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Right</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Right</em>' attribute list.
	 * @see carisma.profile.umlsec.UmlsecPackage#getrbac_Right()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getRight();

	/**
	 * Returns the value of the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Package</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Package</em>' reference.
	 * @see #setBase_Package(org.eclipse.uml2.uml.Package)
	 * @see carisma.profile.umlsec.UmlsecPackage#getrbac_Base_Package()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	org.eclipse.uml2.uml.Package getBase_Package();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.rbac#getBase_Package <em>Base Package</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Package</em>' reference.
	 * @see #getBase_Package()
	 * @generated
	 */
	void setBase_Package(org.eclipse.uml2.uml.Package value);

} // rbac
