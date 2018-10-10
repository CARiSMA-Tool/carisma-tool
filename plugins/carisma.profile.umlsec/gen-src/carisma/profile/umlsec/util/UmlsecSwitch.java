/**
 */
package carisma.profile.umlsec.util;

import carisma.profile.umlsec.*;

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
 * @see carisma.profile.umlsec.UmlsecPackage
 * @generated
 */
public class UmlsecSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static UmlsecPackage modelPackage;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public UmlsecSwitch() {
		if (modelPackage == null) {
			modelPackage = UmlsecPackage.eINSTANCE;
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
			case UmlsecPackage.PROTECTEDACTION: {
				protectedaction protectedaction = (protectedaction)theEObject;
				T result = caseprotectedaction(protectedaction);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.RBAC: {
				rbac rbac = (rbac)theEObject;
				T result = caserbac(rbac);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.SAP_TRANSACTION: {
				SAPTransaction sapTransaction = (SAPTransaction)theEObject;
				T result = caseSAPTransaction(sapTransaction);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.ALLOWEDUSERS: {
				allowedusers allowedusers = (allowedusers)theEObject;
				T result = caseallowedusers(allowedusers);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.SEPERATIONOFDUTY: {
				seperationofduty seperationofduty = (seperationofduty)theEObject;
				T result = caseseperationofduty(seperationofduty);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.USEDBY: {
				usedby usedby = (usedby)theEObject;
				T result = caseusedby(usedby);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.IDENTIFIABLE: {
				identifiable identifiable = (identifiable)theEObject;
				T result = caseidentifiable(identifiable);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.SECUREDEPENDENCY: {
				securedependency securedependency = (securedependency)theEObject;
				T result = casesecuredependency(securedependency);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.CALL: {
				call call = (call)theEObject;
				T result = casecall(call);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.SEND: {
				send send = (send)theEObject;
				T result = casesend(send);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.SECRECY: {
				secrecy secrecy = (secrecy)theEObject;
				T result = casesecrecy(secrecy);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.INTEGRITY: {
				integrity integrity = (integrity)theEObject;
				T result = caseintegrity(integrity);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.HIGH: {
				high high = (high)theEObject;
				T result = casehigh(high);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.ENCRYPTED: {
				encrypted encrypted = (encrypted)theEObject;
				T result = caseencrypted(encrypted);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.WIRE: {
				wire wire = (wire)theEObject;
				T result = casewire(wire);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.INTERNET: {
				Internet internet = (Internet)theEObject;
				T result = caseInternet(internet);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.LAN: {
				LAN lan = (LAN)theEObject;
				T result = caseLAN(lan);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.SMARTCARD: {
				smartcard smartcard = (smartcard)theEObject;
				T result = casesmartcard(smartcard);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.PO_SDEVICE: {
				POSdevice poSdevice = (POSdevice)theEObject;
				T result = casePOSdevice(poSdevice);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.ISSUERNODE: {
				issuernode issuernode = (issuernode)theEObject;
				T result = caseissuernode(issuernode);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.CRITICAL: {
				critical critical = (critical)theEObject;
				T result = casecritical(critical);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.DATASECURITY: {
				datasecurity datasecurity = (datasecurity)theEObject;
				T result = casedatasecurity(datasecurity);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.SECURELINKS: {
				securelinks securelinks = (securelinks)theEObject;
				T result = casesecurelinks(securelinks);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.FAIREXCHANGE: {
				fairexchange fairexchange = (fairexchange)theEObject;
				T result = casefairexchange(fairexchange);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.PROVABLE: {
				provable provable = (provable)theEObject;
				T result = caseprovable(provable);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.NODOWNFLOW: {
				nodownflow nodownflow = (nodownflow)theEObject;
				T result = casenodownflow(nodownflow);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.NOUPFLOW: {
				noupflow noupflow = (noupflow)theEObject;
				T result = casenoupflow(noupflow);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.GUARDEDACCESS: {
				guardedaccess guardedaccess = (guardedaccess)theEObject;
				T result = caseguardedaccess(guardedaccess);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.GUARDED: {
				guarded guarded = (guarded)theEObject;
				T result = caseguarded(guarded);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.AUTHORIZEDSTATUS: {
				authorizedstatus authorizedstatus = (authorizedstatus)theEObject;
				T result = caseauthorizedstatus(authorizedstatus);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.LOCKEDSTATUS: {
				lockedstatus lockedstatus = (lockedstatus)theEObject;
				T result = caselockedstatus(lockedstatus);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.REQUIRES: {
				requires requires = (requires)theEObject;
				T result = caserequires(requires);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case UmlsecPackage.PRIVACY: {
				privacy privacy = (privacy)theEObject;
				T result = caseprivacy(privacy);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			default: return defaultCase(theEObject);
		}
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>protectedaction</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>protectedaction</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseprotectedaction(protectedaction object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>rbac</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>rbac</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caserbac(rbac object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>SAP Transaction</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>SAP Transaction</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSAPTransaction(SAPTransaction object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>allowedusers</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>allowedusers</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseallowedusers(allowedusers object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>seperationofduty</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>seperationofduty</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseseperationofduty(seperationofduty object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>usedby</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>usedby</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseusedby(usedby object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>identifiable</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>identifiable</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseidentifiable(identifiable object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>securedependency</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>securedependency</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casesecuredependency(securedependency object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>call</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>call</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casecall(call object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>send</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>send</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casesend(send object) {
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
	 * Returns the result of interpreting the object as an instance of '<em>integrity</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>integrity</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseintegrity(integrity object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>high</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>high</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casehigh(high object) {
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
	 * Returns the result of interpreting the object as an instance of '<em>wire</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>wire</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casewire(wire object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Internet</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Internet</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseInternet(Internet object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>LAN</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>LAN</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseLAN(LAN object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>smartcard</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>smartcard</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casesmartcard(smartcard object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>PO Sdevice</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>PO Sdevice</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casePOSdevice(POSdevice object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>issuernode</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>issuernode</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseissuernode(issuernode object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>critical</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>critical</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casecritical(critical object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>datasecurity</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>datasecurity</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casedatasecurity(datasecurity object) {
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
	 * Returns the result of interpreting the object as an instance of '<em>fairexchange</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>fairexchange</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casefairexchange(fairexchange object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>provable</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>provable</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseprovable(provable object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>nodownflow</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>nodownflow</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casenodownflow(nodownflow object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>noupflow</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>noupflow</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casenoupflow(noupflow object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>guardedaccess</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>guardedaccess</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseguardedaccess(guardedaccess object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>guarded</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>guarded</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseguarded(guarded object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>authorizedstatus</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>authorizedstatus</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseauthorizedstatus(authorizedstatus object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>lockedstatus</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>lockedstatus</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caselockedstatus(lockedstatus object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>requires</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>requires</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caserequires(requires object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>privacy</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>privacy</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseprivacy(privacy object) {
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

} //UmlsecSwitch
