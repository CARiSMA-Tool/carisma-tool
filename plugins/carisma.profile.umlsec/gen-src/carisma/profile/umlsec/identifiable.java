/**
 */
package carisma.profile.umlsec;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Element;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>identifiable</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * allows us to assign an
 * identifier to a model element
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.identifiable#getId <em>Id</em>}</li>
 *   <li>{@link carisma.profile.umlsec.identifiable#getBase_Element <em>Base Element</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.UmlsecPackage#getidentifiable()
 * @model
 * @generated
 */
public interface identifiable extends EObject {
	/**
	 * Returns the value of the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Id</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Id</em>' attribute.
	 * @see #setId(String)
	 * @see carisma.profile.umlsec.UmlsecPackage#getidentifiable_Id()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getId();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.identifiable#getId <em>Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Id</em>' attribute.
	 * @see #getId()
	 * @generated
	 */
	void setId(String value);

	/**
	 * Returns the value of the '<em><b>Base Element</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Element</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Element</em>' reference.
	 * @see #setBase_Element(Element)
	 * @see carisma.profile.umlsec.UmlsecPackage#getidentifiable_Base_Element()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Element getBase_Element();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.identifiable#getBase_Element <em>Base Element</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Element</em>' reference.
	 * @see #getBase_Element()
	 * @generated
	 */
	void setBase_Element(Element value);

} // identifiable
