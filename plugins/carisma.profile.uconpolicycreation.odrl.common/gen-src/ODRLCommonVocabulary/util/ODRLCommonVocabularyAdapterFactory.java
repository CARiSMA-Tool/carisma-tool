/**
 */
package ODRLCommonVocabulary.util;

import ODRLCommonVocabulary.*;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage
 * @generated
 */
public class ODRLCommonVocabularyAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static ODRLCommonVocabularyPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ODRLCommonVocabularyAdapterFactory() {
		if (modelPackage == null) {
			modelPackage = ODRLCommonVocabularyPackage.eINSTANCE;
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
	protected ODRLCommonVocabularySwitch<Adapter> modelSwitch =
		new ODRLCommonVocabularySwitch<Adapter>() {
			@Override
			public Adapter caseODRLPolicy(ODRLPolicy object) {
				return createODRLPolicyAdapter();
			}
			@Override
			public Adapter caseRule(Rule object) {
				return createRuleAdapter();
			}
			@Override
			public Adapter caseRefinableElement(RefinableElement object) {
				return createRefinableElementAdapter();
			}
			@Override
			public Adapter caseLogicalConstraint(LogicalConstraint object) {
				return createLogicalConstraintAdapter();
			}
			@Override
			public Adapter caseConstraint(Constraint object) {
				return createConstraintAdapter();
			}
			@Override
			public Adapter caseConstrainableElement(ConstrainableElement object) {
				return createConstrainableElementAdapter();
			}
			@Override
			public Adapter casePartyFunction(PartyFunction object) {
				return createPartyFunctionAdapter();
			}
			@Override
			public Adapter caseParty(Party object) {
				return createPartyAdapter();
			}
			@Override
			public Adapter casePermission(Permission object) {
				return createPermissionAdapter();
			}
			@Override
			public Adapter caseDuty(Duty object) {
				return createDutyAdapter();
			}
			@Override
			public Adapter caseProhibition(Prohibition object) {
				return createProhibitionAdapter();
			}
			@Override
			public Adapter caseAsset(Asset object) {
				return createAssetAdapter();
			}
			@Override
			public Adapter caseAssetCollection(AssetCollection object) {
				return createAssetCollectionAdapter();
			}
			@Override
			public Adapter casePartyCollection(PartyCollection object) {
				return createPartyCollectionAdapter();
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
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.ODRLPolicy <em>ODRL Policy</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.ODRLPolicy
	 * @generated
	 */
	public Adapter createODRLPolicyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.Rule <em>Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.Rule
	 * @generated
	 */
	public Adapter createRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.RefinableElement <em>Refinable Element</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.RefinableElement
	 * @generated
	 */
	public Adapter createRefinableElementAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.LogicalConstraint <em>Logical Constraint</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.LogicalConstraint
	 * @generated
	 */
	public Adapter createLogicalConstraintAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.Constraint <em>Constraint</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.Constraint
	 * @generated
	 */
	public Adapter createConstraintAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.ConstrainableElement <em>Constrainable Element</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.ConstrainableElement
	 * @generated
	 */
	public Adapter createConstrainableElementAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.PartyFunction <em>Party Function</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.PartyFunction
	 * @generated
	 */
	public Adapter createPartyFunctionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.Party <em>Party</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.Party
	 * @generated
	 */
	public Adapter createPartyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.Permission <em>Permission</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.Permission
	 * @generated
	 */
	public Adapter createPermissionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.Duty <em>Duty</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.Duty
	 * @generated
	 */
	public Adapter createDutyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.Prohibition <em>Prohibition</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.Prohibition
	 * @generated
	 */
	public Adapter createProhibitionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.Asset <em>Asset</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.Asset
	 * @generated
	 */
	public Adapter createAssetAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.AssetCollection <em>Asset Collection</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.AssetCollection
	 * @generated
	 */
	public Adapter createAssetCollectionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link ODRLCommonVocabulary.PartyCollection <em>Party Collection</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see ODRLCommonVocabulary.PartyCollection
	 * @generated
	 */
	public Adapter createPartyCollectionAdapter() {
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

} //ODRLCommonVocabularyAdapterFactory
