/**
 */
package mltop10.impl;

import java.util.Collection;

import mltop10.Mltop10Package;
import mltop10.ThreatComments;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EDataTypeUniqueEList;

import org.eclipse.uml2.uml.Model;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Threat Comments</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link mltop10.impl.ThreatCommentsImpl#getInputManipulation <em>Input Manipulation</em>}</li>
 *   <li>{@link mltop10.impl.ThreatCommentsImpl#getBase_Package <em>Base Package</em>}</li>
 *   <li>{@link mltop10.impl.ThreatCommentsImpl#getBase_Model <em>Base Model</em>}</li>
 *   <li>{@link mltop10.impl.ThreatCommentsImpl#getAISupplyChain <em>AI Supply Chain</em>}</li>
 *   <li>{@link mltop10.impl.ThreatCommentsImpl#getDataPoisoning <em>Data Poisoning</em>}</li>
 *   <li>{@link mltop10.impl.ThreatCommentsImpl#getMembershipInference <em>Membership Inference</em>}</li>
 *   <li>{@link mltop10.impl.ThreatCommentsImpl#getModelInversion <em>Model Inversion</em>}</li>
 *   <li>{@link mltop10.impl.ThreatCommentsImpl#getModelPoisoning <em>Model Poisoning</em>}</li>
 *   <li>{@link mltop10.impl.ThreatCommentsImpl#getModelSkewing <em>Model Skewing</em>}</li>
 *   <li>{@link mltop10.impl.ThreatCommentsImpl#getModelTheft <em>Model Theft</em>}</li>
 *   <li>{@link mltop10.impl.ThreatCommentsImpl#getOutputIntegrity <em>Output Integrity</em>}</li>
 *   <li>{@link mltop10.impl.ThreatCommentsImpl#getTransferLearning <em>Transfer Learning</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ThreatCommentsImpl extends MinimalEObjectImpl.Container implements ThreatComments {
	/**
	 * The cached value of the '{@link #getInputManipulation() <em>Input Manipulation</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getInputManipulation()
	 * @generated
	 * @ordered
	 */
	protected EList<String> inputManipulation;

	/**
	 * The cached value of the '{@link #getBase_Package() <em>Base Package</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Package()
	 * @generated
	 * @ordered
	 */
	protected org.eclipse.uml2.uml.Package base_Package;

	/**
	 * The cached value of the '{@link #getBase_Model() <em>Base Model</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Model()
	 * @generated
	 * @ordered
	 */
	protected Model base_Model;

	/**
	 * The cached value of the '{@link #getAISupplyChain() <em>AI Supply Chain</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAISupplyChain()
	 * @generated
	 * @ordered
	 */
	protected EList<String> aiSupplyChain;

	/**
	 * The cached value of the '{@link #getDataPoisoning() <em>Data Poisoning</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDataPoisoning()
	 * @generated
	 * @ordered
	 */
	protected EList<String> dataPoisoning;

	/**
	 * The cached value of the '{@link #getMembershipInference() <em>Membership Inference</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMembershipInference()
	 * @generated
	 * @ordered
	 */
	protected EList<String> membershipInference;

	/**
	 * The cached value of the '{@link #getModelInversion() <em>Model Inversion</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getModelInversion()
	 * @generated
	 * @ordered
	 */
	protected EList<String> modelInversion;

	/**
	 * The cached value of the '{@link #getModelPoisoning() <em>Model Poisoning</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getModelPoisoning()
	 * @generated
	 * @ordered
	 */
	protected EList<String> modelPoisoning;

	/**
	 * The cached value of the '{@link #getModelSkewing() <em>Model Skewing</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getModelSkewing()
	 * @generated
	 * @ordered
	 */
	protected EList<String> modelSkewing;

	/**
	 * The cached value of the '{@link #getModelTheft() <em>Model Theft</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getModelTheft()
	 * @generated
	 * @ordered
	 */
	protected EList<String> modelTheft;

	/**
	 * The cached value of the '{@link #getOutputIntegrity() <em>Output Integrity</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOutputIntegrity()
	 * @generated
	 * @ordered
	 */
	protected EList<String> outputIntegrity;

	/**
	 * The cached value of the '{@link #getTransferLearning() <em>Transfer Learning</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTransferLearning()
	 * @generated
	 * @ordered
	 */
	protected EList<String> transferLearning;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ThreatCommentsImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Mltop10Package.Literals.THREAT_COMMENTS;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getInputManipulation() {
		if (inputManipulation == null) {
			inputManipulation = new EDataTypeUniqueEList<String>(String.class, this, Mltop10Package.THREAT_COMMENTS__INPUT_MANIPULATION);
		}
		return inputManipulation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public org.eclipse.uml2.uml.Package getBase_Package() {
		if (base_Package != null && base_Package.eIsProxy()) {
			InternalEObject oldBase_Package = (InternalEObject)base_Package;
			base_Package = (org.eclipse.uml2.uml.Package)eResolveProxy(oldBase_Package);
			if (base_Package != oldBase_Package) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Mltop10Package.THREAT_COMMENTS__BASE_PACKAGE, oldBase_Package, base_Package));
			}
		}
		return base_Package;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public org.eclipse.uml2.uml.Package basicGetBase_Package() {
		return base_Package;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setBase_Package(org.eclipse.uml2.uml.Package newBase_Package) {
		org.eclipse.uml2.uml.Package oldBase_Package = base_Package;
		base_Package = newBase_Package;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.THREAT_COMMENTS__BASE_PACKAGE, oldBase_Package, base_Package));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Model getBase_Model() {
		if (base_Model != null && base_Model.eIsProxy()) {
			InternalEObject oldBase_Model = (InternalEObject)base_Model;
			base_Model = (Model)eResolveProxy(oldBase_Model);
			if (base_Model != oldBase_Model) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Mltop10Package.THREAT_COMMENTS__BASE_MODEL, oldBase_Model, base_Model));
			}
		}
		return base_Model;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Model basicGetBase_Model() {
		return base_Model;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setBase_Model(Model newBase_Model) {
		Model oldBase_Model = base_Model;
		base_Model = newBase_Model;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.THREAT_COMMENTS__BASE_MODEL, oldBase_Model, base_Model));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getAISupplyChain() {
		if (aiSupplyChain == null) {
			aiSupplyChain = new EDataTypeUniqueEList<String>(String.class, this, Mltop10Package.THREAT_COMMENTS__AI_SUPPLY_CHAIN);
		}
		return aiSupplyChain;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getDataPoisoning() {
		if (dataPoisoning == null) {
			dataPoisoning = new EDataTypeUniqueEList<String>(String.class, this, Mltop10Package.THREAT_COMMENTS__DATA_POISONING);
		}
		return dataPoisoning;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getMembershipInference() {
		if (membershipInference == null) {
			membershipInference = new EDataTypeUniqueEList<String>(String.class, this, Mltop10Package.THREAT_COMMENTS__MEMBERSHIP_INFERENCE);
		}
		return membershipInference;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getModelInversion() {
		if (modelInversion == null) {
			modelInversion = new EDataTypeUniqueEList<String>(String.class, this, Mltop10Package.THREAT_COMMENTS__MODEL_INVERSION);
		}
		return modelInversion;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getModelPoisoning() {
		if (modelPoisoning == null) {
			modelPoisoning = new EDataTypeUniqueEList<String>(String.class, this, Mltop10Package.THREAT_COMMENTS__MODEL_POISONING);
		}
		return modelPoisoning;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getModelSkewing() {
		if (modelSkewing == null) {
			modelSkewing = new EDataTypeUniqueEList<String>(String.class, this, Mltop10Package.THREAT_COMMENTS__MODEL_SKEWING);
		}
		return modelSkewing;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getModelTheft() {
		if (modelTheft == null) {
			modelTheft = new EDataTypeUniqueEList<String>(String.class, this, Mltop10Package.THREAT_COMMENTS__MODEL_THEFT);
		}
		return modelTheft;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getOutputIntegrity() {
		if (outputIntegrity == null) {
			outputIntegrity = new EDataTypeUniqueEList<String>(String.class, this, Mltop10Package.THREAT_COMMENTS__OUTPUT_INTEGRITY);
		}
		return outputIntegrity;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getTransferLearning() {
		if (transferLearning == null) {
			transferLearning = new EDataTypeUniqueEList<String>(String.class, this, Mltop10Package.THREAT_COMMENTS__TRANSFER_LEARNING);
		}
		return transferLearning;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Mltop10Package.THREAT_COMMENTS__INPUT_MANIPULATION:
				return getInputManipulation();
			case Mltop10Package.THREAT_COMMENTS__BASE_PACKAGE:
				if (resolve) return getBase_Package();
				return basicGetBase_Package();
			case Mltop10Package.THREAT_COMMENTS__BASE_MODEL:
				if (resolve) return getBase_Model();
				return basicGetBase_Model();
			case Mltop10Package.THREAT_COMMENTS__AI_SUPPLY_CHAIN:
				return getAISupplyChain();
			case Mltop10Package.THREAT_COMMENTS__DATA_POISONING:
				return getDataPoisoning();
			case Mltop10Package.THREAT_COMMENTS__MEMBERSHIP_INFERENCE:
				return getMembershipInference();
			case Mltop10Package.THREAT_COMMENTS__MODEL_INVERSION:
				return getModelInversion();
			case Mltop10Package.THREAT_COMMENTS__MODEL_POISONING:
				return getModelPoisoning();
			case Mltop10Package.THREAT_COMMENTS__MODEL_SKEWING:
				return getModelSkewing();
			case Mltop10Package.THREAT_COMMENTS__MODEL_THEFT:
				return getModelTheft();
			case Mltop10Package.THREAT_COMMENTS__OUTPUT_INTEGRITY:
				return getOutputIntegrity();
			case Mltop10Package.THREAT_COMMENTS__TRANSFER_LEARNING:
				return getTransferLearning();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case Mltop10Package.THREAT_COMMENTS__INPUT_MANIPULATION:
				getInputManipulation().clear();
				getInputManipulation().addAll((Collection<? extends String>)newValue);
				return;
			case Mltop10Package.THREAT_COMMENTS__BASE_PACKAGE:
				setBase_Package((org.eclipse.uml2.uml.Package)newValue);
				return;
			case Mltop10Package.THREAT_COMMENTS__BASE_MODEL:
				setBase_Model((Model)newValue);
				return;
			case Mltop10Package.THREAT_COMMENTS__AI_SUPPLY_CHAIN:
				getAISupplyChain().clear();
				getAISupplyChain().addAll((Collection<? extends String>)newValue);
				return;
			case Mltop10Package.THREAT_COMMENTS__DATA_POISONING:
				getDataPoisoning().clear();
				getDataPoisoning().addAll((Collection<? extends String>)newValue);
				return;
			case Mltop10Package.THREAT_COMMENTS__MEMBERSHIP_INFERENCE:
				getMembershipInference().clear();
				getMembershipInference().addAll((Collection<? extends String>)newValue);
				return;
			case Mltop10Package.THREAT_COMMENTS__MODEL_INVERSION:
				getModelInversion().clear();
				getModelInversion().addAll((Collection<? extends String>)newValue);
				return;
			case Mltop10Package.THREAT_COMMENTS__MODEL_POISONING:
				getModelPoisoning().clear();
				getModelPoisoning().addAll((Collection<? extends String>)newValue);
				return;
			case Mltop10Package.THREAT_COMMENTS__MODEL_SKEWING:
				getModelSkewing().clear();
				getModelSkewing().addAll((Collection<? extends String>)newValue);
				return;
			case Mltop10Package.THREAT_COMMENTS__MODEL_THEFT:
				getModelTheft().clear();
				getModelTheft().addAll((Collection<? extends String>)newValue);
				return;
			case Mltop10Package.THREAT_COMMENTS__OUTPUT_INTEGRITY:
				getOutputIntegrity().clear();
				getOutputIntegrity().addAll((Collection<? extends String>)newValue);
				return;
			case Mltop10Package.THREAT_COMMENTS__TRANSFER_LEARNING:
				getTransferLearning().clear();
				getTransferLearning().addAll((Collection<? extends String>)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case Mltop10Package.THREAT_COMMENTS__INPUT_MANIPULATION:
				getInputManipulation().clear();
				return;
			case Mltop10Package.THREAT_COMMENTS__BASE_PACKAGE:
				setBase_Package((org.eclipse.uml2.uml.Package)null);
				return;
			case Mltop10Package.THREAT_COMMENTS__BASE_MODEL:
				setBase_Model((Model)null);
				return;
			case Mltop10Package.THREAT_COMMENTS__AI_SUPPLY_CHAIN:
				getAISupplyChain().clear();
				return;
			case Mltop10Package.THREAT_COMMENTS__DATA_POISONING:
				getDataPoisoning().clear();
				return;
			case Mltop10Package.THREAT_COMMENTS__MEMBERSHIP_INFERENCE:
				getMembershipInference().clear();
				return;
			case Mltop10Package.THREAT_COMMENTS__MODEL_INVERSION:
				getModelInversion().clear();
				return;
			case Mltop10Package.THREAT_COMMENTS__MODEL_POISONING:
				getModelPoisoning().clear();
				return;
			case Mltop10Package.THREAT_COMMENTS__MODEL_SKEWING:
				getModelSkewing().clear();
				return;
			case Mltop10Package.THREAT_COMMENTS__MODEL_THEFT:
				getModelTheft().clear();
				return;
			case Mltop10Package.THREAT_COMMENTS__OUTPUT_INTEGRITY:
				getOutputIntegrity().clear();
				return;
			case Mltop10Package.THREAT_COMMENTS__TRANSFER_LEARNING:
				getTransferLearning().clear();
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case Mltop10Package.THREAT_COMMENTS__INPUT_MANIPULATION:
				return inputManipulation != null && !inputManipulation.isEmpty();
			case Mltop10Package.THREAT_COMMENTS__BASE_PACKAGE:
				return base_Package != null;
			case Mltop10Package.THREAT_COMMENTS__BASE_MODEL:
				return base_Model != null;
			case Mltop10Package.THREAT_COMMENTS__AI_SUPPLY_CHAIN:
				return aiSupplyChain != null && !aiSupplyChain.isEmpty();
			case Mltop10Package.THREAT_COMMENTS__DATA_POISONING:
				return dataPoisoning != null && !dataPoisoning.isEmpty();
			case Mltop10Package.THREAT_COMMENTS__MEMBERSHIP_INFERENCE:
				return membershipInference != null && !membershipInference.isEmpty();
			case Mltop10Package.THREAT_COMMENTS__MODEL_INVERSION:
				return modelInversion != null && !modelInversion.isEmpty();
			case Mltop10Package.THREAT_COMMENTS__MODEL_POISONING:
				return modelPoisoning != null && !modelPoisoning.isEmpty();
			case Mltop10Package.THREAT_COMMENTS__MODEL_SKEWING:
				return modelSkewing != null && !modelSkewing.isEmpty();
			case Mltop10Package.THREAT_COMMENTS__MODEL_THEFT:
				return modelTheft != null && !modelTheft.isEmpty();
			case Mltop10Package.THREAT_COMMENTS__OUTPUT_INTEGRITY:
				return outputIntegrity != null && !outputIntegrity.isEmpty();
			case Mltop10Package.THREAT_COMMENTS__TRANSFER_LEARNING:
				return transferLearning != null && !transferLearning.isEmpty();
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuilder result = new StringBuilder(super.toString());
		result.append(" (InputManipulation: ");
		result.append(inputManipulation);
		result.append(", AISupplyChain: ");
		result.append(aiSupplyChain);
		result.append(", DataPoisoning: ");
		result.append(dataPoisoning);
		result.append(", MembershipInference: ");
		result.append(membershipInference);
		result.append(", ModelInversion: ");
		result.append(modelInversion);
		result.append(", ModelPoisoning: ");
		result.append(modelPoisoning);
		result.append(", ModelSkewing: ");
		result.append(modelSkewing);
		result.append(", ModelTheft: ");
		result.append(modelTheft);
		result.append(", OutputIntegrity: ");
		result.append(outputIntegrity);
		result.append(", TransferLearning: ");
		result.append(transferLearning);
		result.append(')');
		return result.toString();
	}

} //ThreatCommentsImpl
