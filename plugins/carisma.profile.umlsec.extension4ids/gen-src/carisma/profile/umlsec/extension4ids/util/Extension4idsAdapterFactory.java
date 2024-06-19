/**
 */
package carisma.profile.umlsec.extension4ids.util;

import carisma.profile.umlsec.extension4ids.*;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage
 * @generated
 */
public class Extension4idsAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static Extension4idsPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Extension4idsAdapterFactory() {
		if (modelPackage == null) {
			modelPackage = Extension4idsPackage.eINSTANCE;
		}
	}

	/**
	 * Returns whether this factory is applicable for the type of the object.
	 * <!-- begin-user-doc -->
	 * This implementation returns <code>true</code> if the object is either the model's package or is an instance object of the model.
	 * <!-- end-user-doc -->
	 * @return whether this factory is applicable for the type of the object.
	 * @generated
	 */
	@Override
	public boolean isFactoryForType(Object object) {
		if (object == modelPackage) {
			return true;
		}
		if (object instanceof EObject) {
			return ((EObject)object).eClass().getEPackage() == modelPackage;
		}
		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected Extension4idsSwitch<Adapter> modelSwitch =
		new Extension4idsSwitch<Adapter>() {
			@Override
			public Adapter caseIDSconnector(IDSconnector object) {
				return createIDSconnectorAdapter();
			}
			@Override
			public Adapter caseusagecontrol(usagecontrol object) {
				return createusagecontrolAdapter();
			}
			@Override
			public Adapter casedatatransfer(datatransfer object) {
				return createdatatransferAdapter();
			}
			@Override
			public Adapter caseprovider(provider object) {
				return createproviderAdapter();
			}
			@Override
			public Adapter caseconsumer(consumer object) {
				return createconsumerAdapter();
			}
			@Override
			public Adapter defaultCase(EObject object) {
				return createEObjectAdapter();
			}
		};

	/**
	 * Creates an adapter for the <code>target</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param target the object to adapt.
	 * @return the adapter for the <code>target</code>.
	 * @generated
	 */
	@Override
	public Adapter createAdapter(Notifier target) {
		return modelSwitch.doSwitch((EObject)target);
	}


	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.extension4ids.IDSconnector <em>ID Sconnector</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.extension4ids.IDSconnector
	 * @generated
	 */
	public Adapter createIDSconnectorAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.extension4ids.usagecontrol <em>usagecontrol</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.extension4ids.usagecontrol
	 * @generated
	 */
	public Adapter createusagecontrolAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.extension4ids.datatransfer <em>datatransfer</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.extension4ids.datatransfer
	 * @generated
	 */
	public Adapter createdatatransferAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.extension4ids.provider <em>provider</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.extension4ids.provider
	 * @generated
	 */
	public Adapter createproviderAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.extension4ids.consumer <em>consumer</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.extension4ids.consumer
	 * @generated
	 */
	public Adapter createconsumerAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for the default case.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @generated
	 */
	public Adapter createEObjectAdapter() {
		return null;
	}

} //Extension4idsAdapterFactory
