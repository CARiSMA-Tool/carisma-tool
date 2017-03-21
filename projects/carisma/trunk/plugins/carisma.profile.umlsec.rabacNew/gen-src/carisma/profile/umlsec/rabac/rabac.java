/**
 */
package carisma.profile.umlsec.rabac;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>rabac</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.rabac.rabac#getRoles <em>Roles</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rabac.rabac#getRights <em>Rights</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rabac.rabac#getRh <em>Rh</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rabac.rabac#getSsd <em>Ssd</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rabac.rabac#getBase_Class <em>Base Class</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rabac.rabac#getDsd <em>Dsd</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rabac.rabac#getAttributeFilters <em>Attribute Filters</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rabac.rabac#getBase_Package <em>Base Package</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.rabac.RabacPackage#getrabac()
 * @model
 * @generated
 */
public interface rabac extends EObject {
	/**
	 * Returns the value of the '<em><b>Roles</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Roles</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Roles</em>' attribute.
	 * @see #setRoles(String)
	 * @see carisma.profile.umlsec.rabac.RabacPackage#getrabac_Roles()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getRoles();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.rabac.rabac#getRoles <em>Roles</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Roles</em>' attribute.
	 * @see #getRoles()
	 * @generated
	 */
	void setRoles(String value);

	/**
	 * Returns the value of the '<em><b>Rights</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Rights</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Rights</em>' attribute.
	 * @see #setRights(String)
	 * @see carisma.profile.umlsec.rabac.RabacPackage#getrabac_Rights()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getRights();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.rabac.rabac#getRights <em>Rights</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Rights</em>' attribute.
	 * @see #getRights()
	 * @generated
	 */
	void setRights(String value);

	/**
	 * Returns the value of the '<em><b>Rh</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Rh</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Rh</em>' attribute.
	 * @see #setRh(String)
	 * @see carisma.profile.umlsec.rabac.RabacPackage#getrabac_Rh()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	String getRh();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.rabac.rabac#getRh <em>Rh</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Rh</em>' attribute.
	 * @see #getRh()
	 * @generated
	 */
	void setRh(String value);

	/**
	 * Returns the value of the '<em><b>Ssd</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ssd</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ssd</em>' attribute.
	 * @see #setSsd(String)
	 * @see carisma.profile.umlsec.rabac.RabacPackage#getrabac_Ssd()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	String getSsd();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.rabac.rabac#getSsd <em>Ssd</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ssd</em>' attribute.
	 * @see #getSsd()
	 * @generated
	 */
	void setSsd(String value);

	/**
	 * Returns the value of the '<em><b>Base Class</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Class</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Class</em>' reference.
	 * @see #setBase_Class(org.eclipse.uml2.uml.Class)
	 * @see carisma.profile.umlsec.rabac.RabacPackage#getrabac_Base_Class()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	org.eclipse.uml2.uml.Class getBase_Class();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.rabac.rabac#getBase_Class <em>Base Class</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Class</em>' reference.
	 * @see #getBase_Class()
	 * @generated
	 */
	void setBase_Class(org.eclipse.uml2.uml.Class value);

	/**
	 * Returns the value of the '<em><b>Dsd</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Dsd</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Dsd</em>' attribute.
	 * @see #setDsd(String)
	 * @see carisma.profile.umlsec.rabac.RabacPackage#getrabac_Dsd()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	String getDsd();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.rabac.rabac#getDsd <em>Dsd</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Dsd</em>' attribute.
	 * @see #getDsd()
	 * @generated
	 */
	void setDsd(String value);

	/**
	 * Returns the value of the '<em><b>Attribute Filters</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Attribute Filters</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Attribute Filters</em>' attribute.
	 * @see #setAttributeFilters(String)
	 * @see carisma.profile.umlsec.rabac.RabacPackage#getrabac_AttributeFilters()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	String getAttributeFilters();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.rabac.rabac#getAttributeFilters <em>Attribute Filters</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Attribute Filters</em>' attribute.
	 * @see #getAttributeFilters()
	 * @generated
	 */
	void setAttributeFilters(String value);

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
	 * @see carisma.profile.umlsec.rabac.RabacPackage#getrabac_Base_Package()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	org.eclipse.uml2.uml.Package getBase_Package();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.rabac.rabac#getBase_Package <em>Base Package</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Package</em>' reference.
	 * @see #getBase_Package()
	 * @generated
	 */
	void setBase_Package(org.eclipse.uml2.uml.Package value);

} // rabac
