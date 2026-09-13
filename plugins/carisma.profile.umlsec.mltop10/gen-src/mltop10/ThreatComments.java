/**
 */
package mltop10;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Model;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Threat Comments</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link mltop10.ThreatComments#getInputManipulation <em>Input Manipulation</em>}</li>
 *   <li>{@link mltop10.ThreatComments#getBase_Package <em>Base Package</em>}</li>
 *   <li>{@link mltop10.ThreatComments#getBase_Model <em>Base Model</em>}</li>
 *   <li>{@link mltop10.ThreatComments#getAISupplyChain <em>AI Supply Chain</em>}</li>
 *   <li>{@link mltop10.ThreatComments#getDataPoisoning <em>Data Poisoning</em>}</li>
 *   <li>{@link mltop10.ThreatComments#getMembershipInference <em>Membership Inference</em>}</li>
 *   <li>{@link mltop10.ThreatComments#getModelInversion <em>Model Inversion</em>}</li>
 *   <li>{@link mltop10.ThreatComments#getModelPoisoning <em>Model Poisoning</em>}</li>
 *   <li>{@link mltop10.ThreatComments#getModelSkewing <em>Model Skewing</em>}</li>
 *   <li>{@link mltop10.ThreatComments#getModelTheft <em>Model Theft</em>}</li>
 *   <li>{@link mltop10.ThreatComments#getOutputIntegrity <em>Output Integrity</em>}</li>
 *   <li>{@link mltop10.ThreatComments#getTransferLearning <em>Transfer Learning</em>}</li>
 * </ul>
 *
 * @see mltop10.Mltop10Package#getThreatComments()
 * @model
 * @generated
 */
public interface ThreatComments extends EObject {
	/**
	 * Returns the value of the '<em><b>Input Manipulation</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Input Manipulation</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Input Manipulation</em>' attribute list.
	 * @see mltop10.Mltop10Package#getThreatComments_InputManipulation()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getInputManipulation();

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
	 * @see mltop10.Mltop10Package#getThreatComments_Base_Package()
	 * @model ordered="false"
	 * @generated
	 */
	org.eclipse.uml2.uml.Package getBase_Package();

	/**
	 * Sets the value of the '{@link mltop10.ThreatComments#getBase_Package <em>Base Package</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Package</em>' reference.
	 * @see #getBase_Package()
	 * @generated
	 */
	void setBase_Package(org.eclipse.uml2.uml.Package value);

	/**
	 * Returns the value of the '<em><b>Base Model</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Model</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Model</em>' reference.
	 * @see #setBase_Model(Model)
	 * @see mltop10.Mltop10Package#getThreatComments_Base_Model()
	 * @model ordered="false"
	 * @generated
	 */
	Model getBase_Model();

	/**
	 * Sets the value of the '{@link mltop10.ThreatComments#getBase_Model <em>Base Model</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Model</em>' reference.
	 * @see #getBase_Model()
	 * @generated
	 */
	void setBase_Model(Model value);

	/**
	 * Returns the value of the '<em><b>AI Supply Chain</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>AI Supply Chain</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>AI Supply Chain</em>' attribute list.
	 * @see mltop10.Mltop10Package#getThreatComments_AISupplyChain()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getAISupplyChain();

	/**
	 * Returns the value of the '<em><b>Data Poisoning</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Data Poisoning</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Data Poisoning</em>' attribute list.
	 * @see mltop10.Mltop10Package#getThreatComments_DataPoisoning()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getDataPoisoning();

	/**
	 * Returns the value of the '<em><b>Membership Inference</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Membership Inference</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Membership Inference</em>' attribute list.
	 * @see mltop10.Mltop10Package#getThreatComments_MembershipInference()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getMembershipInference();

	/**
	 * Returns the value of the '<em><b>Model Inversion</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Model Inversion</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Model Inversion</em>' attribute list.
	 * @see mltop10.Mltop10Package#getThreatComments_ModelInversion()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getModelInversion();

	/**
	 * Returns the value of the '<em><b>Model Poisoning</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Model Poisoning</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Model Poisoning</em>' attribute list.
	 * @see mltop10.Mltop10Package#getThreatComments_ModelPoisoning()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getModelPoisoning();

	/**
	 * Returns the value of the '<em><b>Model Skewing</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Model Skewing</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Model Skewing</em>' attribute list.
	 * @see mltop10.Mltop10Package#getThreatComments_ModelSkewing()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getModelSkewing();

	/**
	 * Returns the value of the '<em><b>Model Theft</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Model Theft</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Model Theft</em>' attribute list.
	 * @see mltop10.Mltop10Package#getThreatComments_ModelTheft()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getModelTheft();

	/**
	 * Returns the value of the '<em><b>Output Integrity</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Output Integrity</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Output Integrity</em>' attribute list.
	 * @see mltop10.Mltop10Package#getThreatComments_OutputIntegrity()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getOutputIntegrity();

	/**
	 * Returns the value of the '<em><b>Transfer Learning</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Transfer Learning</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Transfer Learning</em>' attribute list.
	 * @see mltop10.Mltop10Package#getThreatComments_TransferLearning()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getTransferLearning();

} // ThreatComments
