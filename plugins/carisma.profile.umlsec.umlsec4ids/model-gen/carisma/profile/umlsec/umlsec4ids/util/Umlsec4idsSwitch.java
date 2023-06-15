/**
 */
package carisma.profile.umlsec.umlsec4ids.util;

import carisma.profile.umlsec.umlsec4ids.*;

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
 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage
 * @generated
 */
public class Umlsec4idsSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static Umlsec4idsPackage modelPackage;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Umlsec4idsSwitch() {
		if (modelPackage == null) {
			modelPackage = Umlsec4idsPackage.eINSTANCE;
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
			case Umlsec4idsPackage.BASEFREE: {
				basefree basefree = (basefree)theEObject;
				T result = casebasefree(basefree);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case Umlsec4idsPackage.BASE: {
				base base = (base)theEObject;
				T result = casebase(base);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case Umlsec4idsPackage.TRUST: {
				trust trust = (trust)theEObject;
				T result = casetrust(trust);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case Umlsec4idsPackage.TRUSTPLUS: {
				trustplus trustplus = (trustplus)theEObject;
				T result = casetrustplus(trustplus);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case Umlsec4idsPackage.DATAPROVENANCETRACKING: {
				dataprovenancetracking dataprovenancetracking = (dataprovenancetracking)theEObject;
				T result = casedataprovenancetracking(dataprovenancetracking);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case Umlsec4idsPackage.OWNER: {
				Owner owner = (Owner)theEObject;
				T result = caseOwner(owner);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case Umlsec4idsPackage.CONSUMER: {
				Consumer consumer = (Consumer)theEObject;
				T result = caseConsumer(consumer);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case Umlsec4idsPackage.X509: {
				X509 x509 = (X509)theEObject;
				T result = caseX509(x509);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case Umlsec4idsPackage.X509TLS: {
				X509TLS x509TLS = (X509TLS)theEObject;
				T result = caseX509TLS(x509TLS);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case Umlsec4idsPackage.DATAUSAGECONTROL: {
				datausagecontrol datausagecontrol = (datausagecontrol)theEObject;
				T result = casedatausagecontrol(datausagecontrol);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case Umlsec4idsPackage.VERIFIED: {
				verified verified = (verified)theEObject;
				T result = caseverified(verified);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case Umlsec4idsPackage.ISOLATED: {
				isolated isolated = (isolated)theEObject;
				T result = caseisolated(isolated);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case Umlsec4idsPackage.ENCRYPTION: {
				encryption encryption = (encryption)theEObject;
				T result = caseencryption(encryption);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case Umlsec4idsPackage.CERTIFIED: {
				certified certified = (certified)theEObject;
				T result = casecertified(certified);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			default: return defaultCase(theEObject);
		}
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>basefree</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>basefree</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casebasefree(basefree object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>base</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>base</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casebase(base object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>trust</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>trust</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casetrust(trust object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>trustplus</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>trustplus</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casetrustplus(trustplus object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>dataprovenancetracking</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>dataprovenancetracking</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casedataprovenancetracking(dataprovenancetracking object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Owner</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Owner</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseOwner(Owner object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Consumer</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Consumer</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseConsumer(Consumer object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>X509</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>X509</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseX509(X509 object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>X509TLS</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>X509TLS</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseX509TLS(X509TLS object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>datausagecontrol</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>datausagecontrol</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casedatausagecontrol(datausagecontrol object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>verified</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>verified</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseverified(verified object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>isolated</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>isolated</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseisolated(isolated object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>encryption</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>encryption</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseencryption(encryption object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>certified</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>certified</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casecertified(certified object) {
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

} //Umlsec4idsSwitch
