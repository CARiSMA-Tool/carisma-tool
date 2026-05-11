/**
 */
package mltop10;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Artifact;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>AI Algorithm</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link mltop10.AIAlgorithm#getBase_Artifact <em>Base Artifact</em>}</li>
 *   <li>{@link mltop10.AIAlgorithm#isPublic <em>Public</em>}</li>
 *   <li>{@link mltop10.AIAlgorithm#isAccessControl <em>Access Control</em>}</li>
 *   <li>{@link mltop10.AIAlgorithm#isRandomize <em>Randomize</em>}</li>
 *   <li>{@link mltop10.AIAlgorithm#isRegularisation <em>Regularisation</em>}</li>
 * </ul>
 *
 * @see mltop10.Mltop10Package#getAIAlgorithm()
 * @model
 * @generated
 */
public interface AIAlgorithm extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Artifact</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Artifact</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Artifact</em>' reference.
	 * @see #setBase_Artifact(Artifact)
	 * @see mltop10.Mltop10Package#getAIAlgorithm_Base_Artifact()
	 * @model ordered="false"
	 * @generated
	 */
	Artifact getBase_Artifact();

	/**
	 * Sets the value of the '{@link mltop10.AIAlgorithm#getBase_Artifact <em>Base Artifact</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Artifact</em>' reference.
	 * @see #getBase_Artifact()
	 * @generated
	 */
	void setBase_Artifact(Artifact value);

	/**
	 * Returns the value of the '<em><b>Public</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Public</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Public</em>' attribute.
	 * @see #setPublic(boolean)
	 * @see mltop10.Mltop10Package#getAIAlgorithm_Public()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isPublic();

	/**
	 * Sets the value of the '{@link mltop10.AIAlgorithm#isPublic <em>Public</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Public</em>' attribute.
	 * @see #isPublic()
	 * @generated
	 */
	void setPublic(boolean value);

	/**
	 * Returns the value of the '<em><b>Access Control</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Access Control</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Access Control</em>' attribute.
	 * @see #setAccessControl(boolean)
	 * @see mltop10.Mltop10Package#getAIAlgorithm_AccessControl()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isAccessControl();

	/**
	 * Sets the value of the '{@link mltop10.AIAlgorithm#isAccessControl <em>Access Control</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Access Control</em>' attribute.
	 * @see #isAccessControl()
	 * @generated
	 */
	void setAccessControl(boolean value);

	/**
	 * Returns the value of the '<em><b>Randomize</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Randomize</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Randomize</em>' attribute.
	 * @see #setRandomize(boolean)
	 * @see mltop10.Mltop10Package#getAIAlgorithm_Randomize()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRandomize();

	/**
	 * Sets the value of the '{@link mltop10.AIAlgorithm#isRandomize <em>Randomize</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Randomize</em>' attribute.
	 * @see #isRandomize()
	 * @generated
	 */
	void setRandomize(boolean value);

	/**
	 * Returns the value of the '<em><b>Regularisation</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Regularisation</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Regularisation</em>' attribute.
	 * @see #setRegularisation(boolean)
	 * @see mltop10.Mltop10Package#getAIAlgorithm_Regularisation()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRegularisation();

	/**
	 * Sets the value of the '{@link mltop10.AIAlgorithm#isRegularisation <em>Regularisation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Regularisation</em>' attribute.
	 * @see #isRegularisation()
	 * @generated
	 */
	void setRegularisation(boolean value);

} // AIAlgorithm
