/**
 */
package carisma.profile.umlsec.enc.util;

import carisma.profile.umlsec.enc.*;

import carisma.profile.umlsec.encrypted;
import carisma.profile.umlsec.secrecy;
import carisma.profile.umlsec.securelinks;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.util.Switch;

/**
 * <!-- begin-user-doc -->
 * The <b>Switch</b> for the model's inheritance hierarchy.
 * It supports the call {@link #doSwitch(EObject) doSwitch(object)}
 * to invoke the <code>caseXXX</code> method for each class of the model,
 * starting with the actual class of the object
 * and proceeding up the inheritance hierarchy
 * until a non-null result is returned,
 * which is the result of the switch.
 * <!-- end-user-doc -->
 * @see carisma.profile.umlsec.enc.EncPackage
 * @generated
 */
public class EncSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static EncPackage modelPackage;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EncSwitch() {
		if (modelPackage == null) {
			modelPackage = EncPackage.eINSTANCE;
		}
	}

	/**
	 * Checks whether this is a switch for the given package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param ePackage the package in question.
	 * @return whether this is a switch for the given package.
	 * @generated
	 */
	@Override
	protected boolean isSwitchFor(EPackage ePackage) {
		return ePackage == modelPackage;
	}

	/**
	 * Calls <code>caseXXX</code> for each class of the model until one returns a non null result; it yields that result.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the first non-null result returned by a <code>caseXXX</code> call.
	 * @generated
	 */
	@Override
	protected T doSwitch(int classifierID, EObject theEObject) {
		switch (classifierID) {
			case EncPackage.SECURELINKSENC: {
				securelinksenc securelinksenc = (securelinksenc)theEObject;
				T result = casesecurelinksenc(securelinksenc);
				if (result == null) result = casesecurelinks(securelinksenc);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case EncPackage.SECRECYENC: {
				secrecyenc secrecyenc = (secrecyenc)theEObject;
				T result = casesecrecyenc(secrecyenc);
				if (result == null) result = casesecrecy(secrecyenc);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case EncPackage.ENCRYPTEDENC: {
				encryptedenc encryptedenc = (encryptedenc)theEObject;
				T result = caseencryptedenc(encryptedenc);
				if (result == null) result = caseencrypted(encryptedenc);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case EncPackage.ENCRYPTEDPERSISTENCE: {
				encryptedpersistence encryptedpersistence = (encryptedpersistence)theEObject;
				T result = caseencryptedpersistence(encryptedpersistence);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			default: return defaultCase(theEObject);
		}
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>securelinksenc</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>securelinksenc</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casesecurelinksenc(securelinksenc object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>secrecyenc</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>secrecyenc</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casesecrecyenc(secrecyenc object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>encryptedenc</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>encryptedenc</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseencryptedenc(encryptedenc object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>encryptedpersistence</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>encryptedpersistence</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseencryptedpersistence(encryptedpersistence object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>securelinks</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>securelinks</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casesecurelinks(securelinks object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>secrecy</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>secrecy</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casesecrecy(secrecy object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>encrypted</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>encrypted</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseencrypted(encrypted object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch, but this is the last case anyway.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	public T defaultCase(EObject object) {
		return null;
	}

} //EncSwitch
