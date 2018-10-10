/**
 */
package carisma.profile.umlsec.enc;

import carisma.profile.umlsec.secrecy;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>secrecyenc</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.enc.secrecyenc#getTime <em>Time</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.enc.EncPackage#getsecrecyenc()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='secrecy enc'"
 * @generated
 */
public interface secrecyenc extends secrecy {
	/**
	 * Returns the value of the '<em><b>Time</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Time</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Time</em>' attribute.
	 * @see #setTime(String)
	 * @see carisma.profile.umlsec.enc.EncPackage#getsecrecyenc_Time()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getTime();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.enc.secrecyenc#getTime <em>Time</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Time</em>' attribute.
	 * @see #getTime()
	 * @generated
	 */
	void setTime(String value);

} // secrecyenc
