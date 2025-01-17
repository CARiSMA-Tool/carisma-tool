/**
 */
package carisma.profile.umlsec.extension4ids.util;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

import carisma.profile.umlsec.extension4ids.*;

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
			public Adapter caseUsageControl(UsageControl object) {
				return createUsageControlAdapter();
			}
			@Override
			public Adapter caseTransferProcessProtocol(TransferProcessProtocol object) {
				return createTransferProcessProtocolAdapter();
			}
			@Override
			public Adapter caseProviderConnector(ProviderConnector object) {
				return createProviderConnectorAdapter();
			}
			@Override
			public Adapter caseConsumerConnector(ConsumerConnector object) {
				return createConsumerConnectorAdapter();
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
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.extension4ids.UsageControl <em>Usage Control</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.extension4ids.UsageControl
	 * @generated
	 */
	public Adapter createUsageControlAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.extension4ids.TransferProcessProtocol <em>Transfer Process Protocol</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.extension4ids.TransferProcessProtocol
	 * @generated
	 */
	public Adapter createTransferProcessProtocolAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.extension4ids.ProviderConnector <em>Provider Connector</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.extension4ids.ProviderConnector
	 * @generated
	 */
	public Adapter createProviderConnectorAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.extension4ids.ConsumerConnector <em>Consumer Connector</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.extension4ids.ConsumerConnector
	 * @generated
	 */
	public Adapter createConsumerConnectorAdapter() {
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
