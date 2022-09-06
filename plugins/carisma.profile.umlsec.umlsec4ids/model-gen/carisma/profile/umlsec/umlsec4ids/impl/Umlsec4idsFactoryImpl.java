/**
 */
package carisma.profile.umlsec.umlsec4ids.impl;

import carisma.profile.umlsec.umlsec4ids.*;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class Umlsec4idsFactoryImpl extends EFactoryImpl implements Umlsec4idsFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static Umlsec4idsFactory init() {
		try {
			Umlsec4idsFactory theUmlsec4idsFactory = (Umlsec4idsFactory)EPackage.Registry.INSTANCE.getEFactory(Umlsec4idsPackage.eNS_URI);
			if (theUmlsec4idsFactory != null) {
				return theUmlsec4idsFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new Umlsec4idsFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Umlsec4idsFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
			case Umlsec4idsPackage.BASEFREE: return createbasefree();
			case Umlsec4idsPackage.BASE: return createbase();
			case Umlsec4idsPackage.TRUST: return createtrust();
			case Umlsec4idsPackage.TRUSTPLUS: return createtrustplus();
			case Umlsec4idsPackage.DATAPROVENANCETRACKING: return createdataprovenancetracking();
			case Umlsec4idsPackage.OWNER: return createOwner();
			case Umlsec4idsPackage.CONSUMER: return createConsumer();
			case Umlsec4idsPackage.X509: return createX509();
			case Umlsec4idsPackage.X509TLS: return createX509TLS();
			case Umlsec4idsPackage.IDSCP: return createIDSCP();
			case Umlsec4idsPackage.DATAUSAGECONTROL: return createdatausagecontrol();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public basefree createbasefree() {
		basefreeImpl basefree = new basefreeImpl();
		return basefree;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public base createbase() {
		baseImpl base = new baseImpl();
		return base;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public trust createtrust() {
		trustImpl trust = new trustImpl();
		return trust;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public trustplus createtrustplus() {
		trustplusImpl trustplus = new trustplusImpl();
		return trustplus;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public dataprovenancetracking createdataprovenancetracking() {
		dataprovenancetrackingImpl dataprovenancetracking = new dataprovenancetrackingImpl();
		return dataprovenancetracking;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Owner createOwner() {
		OwnerImpl owner = new OwnerImpl();
		return owner;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Consumer createConsumer() {
		ConsumerImpl consumer = new ConsumerImpl();
		return consumer;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public X509 createX509() {
		X509Impl x509 = new X509Impl();
		return x509;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public X509TLS createX509TLS() {
		X509TLSImpl x509TLS = new X509TLSImpl();
		return x509TLS;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public IDSCP createIDSCP() {
		IDSCPImpl idscp = new IDSCPImpl();
		return idscp;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public datausagecontrol createdatausagecontrol() {
		datausagecontrolImpl datausagecontrol = new datausagecontrolImpl();
		return datausagecontrol;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Umlsec4idsPackage getUmlsec4idsPackage() {
		return (Umlsec4idsPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static Umlsec4idsPackage getPackage() {
		return Umlsec4idsPackage.eINSTANCE;
	}

} //Umlsec4idsFactoryImpl
