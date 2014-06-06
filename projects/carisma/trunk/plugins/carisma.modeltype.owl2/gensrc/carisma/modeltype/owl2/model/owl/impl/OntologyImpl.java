/**
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     {SecSE group} - initial API and implementation and/or initial documentation
 */
package carisma.modeltype.owl2.model.owl.impl;

import java.util.Collection;
import java.util.Map;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.BasicDiagnostic;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EObjectValidator;
import org.eclipse.emf.ecore.util.InternalEList;

import carisma.modeltype.owl2.model.owl.Annotation;
import carisma.modeltype.owl2.model.owl.Axiom;
import carisma.modeltype.owl2.model.owl.Ontology;
import carisma.modeltype.owl2.model.owl.OwlPackage;
import carisma.modeltype.owl2.model.owl.URI;
import carisma.modeltype.owl2.model.owl.util.OwlValidator;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Ontology</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.OntologyImpl#getOntologyAnnotations <em>Ontology Annotations</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.OntologyImpl#getAxioms <em>Axioms</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.OntologyImpl#getImportedOntologies <em>Imported Ontologies</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.OntologyImpl#getOntologyURI <em>Ontology URI</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.OntologyImpl#getVersionURI <em>Version URI</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.OntologyImpl#getContainer <em>Container</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.OntologyImpl#getOntologyId <em>Ontology Id</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class OntologyImpl extends EObjectImpl implements Ontology {
	/**
	 * The cached value of the '{@link #getOntologyAnnotations() <em>Ontology Annotations</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOntologyAnnotations()
	 * @generated
	 * @ordered
	 */
	protected EList<Annotation> ontologyAnnotations;

	/**
	 * The cached value of the '{@link #getAxioms() <em>Axioms</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAxioms()
	 * @generated
	 * @ordered
	 */
	protected EList<Axiom> axioms;

	/**
	 * The cached value of the '{@link #getImportedOntologies() <em>Imported Ontologies</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getImportedOntologies()
	 * @generated
	 * @ordered
	 */
	protected EList<Ontology> importedOntologies;

	/**
	 * The cached value of the '{@link #getOntologyURI() <em>Ontology URI</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOntologyURI()
	 * @generated
	 * @ordered
	 */
	protected URI ontologyURI;

	/**
	 * The cached value of the '{@link #getVersionURI() <em>Version URI</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVersionURI()
	 * @generated
	 * @ordered
	 */
	protected URI versionURI;

	/**
	 * The cached value of the '{@link #getContainer() <em>Container</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getContainer()
	 * @generated
	 * @ordered
	 */
	protected EList<EObject> container;

	/**
	 * The default value of the '{@link #getOntologyId() <em>Ontology Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOntologyId()
	 * @generated
	 * @ordered
	 */
	protected static final String ONTOLOGY_ID_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getOntologyId() <em>Ontology Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOntologyId()
	 * @generated
	 * @ordered
	 */
	protected String ontologyId = ONTOLOGY_ID_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected OntologyImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.ONTOLOGY;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Annotation> getOntologyAnnotations() {
		if (ontologyAnnotations == null) {
			ontologyAnnotations = new EObjectContainmentEList<Annotation>(Annotation.class, this, OwlPackage.ONTOLOGY__ONTOLOGY_ANNOTATIONS);
		}
		return ontologyAnnotations;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Axiom> getAxioms() {
		if (axioms == null) {
			axioms = new EObjectContainmentEList<Axiom>(Axiom.class, this, OwlPackage.ONTOLOGY__AXIOMS);
		}
		return axioms;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Ontology> getImportedOntologies() {
		if (importedOntologies == null) {
			importedOntologies = new EObjectContainmentEList<Ontology>(Ontology.class, this, OwlPackage.ONTOLOGY__IMPORTED_ONTOLOGIES);
		}
		return importedOntologies;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public URI getOntologyURI() {
		return ontologyURI;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetOntologyURI(URI newOntologyURI, NotificationChain msgs) {
		URI oldOntologyURI = ontologyURI;
		ontologyURI = newOntologyURI;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, OwlPackage.ONTOLOGY__ONTOLOGY_URI, oldOntologyURI, newOntologyURI);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setOntologyURI(URI newOntologyURI) {
		if (newOntologyURI != ontologyURI) {
			NotificationChain msgs = null;
			if (ontologyURI != null)
				msgs = ((InternalEObject)ontologyURI).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - OwlPackage.ONTOLOGY__ONTOLOGY_URI, null, msgs);
			if (newOntologyURI != null)
				msgs = ((InternalEObject)newOntologyURI).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - OwlPackage.ONTOLOGY__ONTOLOGY_URI, null, msgs);
			msgs = basicSetOntologyURI(newOntologyURI, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.ONTOLOGY__ONTOLOGY_URI, newOntologyURI, newOntologyURI));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public URI getVersionURI() {
		return versionURI;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetVersionURI(URI newVersionURI, NotificationChain msgs) {
		URI oldVersionURI = versionURI;
		versionURI = newVersionURI;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, OwlPackage.ONTOLOGY__VERSION_URI, oldVersionURI, newVersionURI);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setVersionURI(URI newVersionURI) {
		if (newVersionURI != versionURI) {
			NotificationChain msgs = null;
			if (versionURI != null)
				msgs = ((InternalEObject)versionURI).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - OwlPackage.ONTOLOGY__VERSION_URI, null, msgs);
			if (newVersionURI != null)
				msgs = ((InternalEObject)newVersionURI).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - OwlPackage.ONTOLOGY__VERSION_URI, null, msgs);
			msgs = basicSetVersionURI(newVersionURI, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.ONTOLOGY__VERSION_URI, newVersionURI, newVersionURI));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<EObject> getContainer() {
		if (container == null) {
			container = new EObjectContainmentEList<EObject>(EObject.class, this, OwlPackage.ONTOLOGY__CONTAINER);
		}
		return container;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getOntologyId() {
		return ontologyId;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setOntologyId(String newOntologyId) {
		String oldOntologyId = ontologyId;
		ontologyId = newOntologyId;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.ONTOLOGY__ONTOLOGY_ID, oldOntologyId, ontologyId));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean versionURIrequiresontologyURItobespecified(DiagnosticChain diagnostics, Map context) {
		// TODO: implement this method
		// -> specify the condition that violates the invariant
		// -> verify the details of the diagnostic, including severity and message
		// Ensure that you remove @generated or mark it @generated NOT
		if (false) {
			if (diagnostics != null) {
				diagnostics.add
					(new BasicDiagnostic
						(Diagnostic.ERROR,
						 OwlValidator.DIAGNOSTIC_SOURCE,
						 OwlValidator.ONTOLOGY__VERSION_UR_IREQUIRESONTOLOGY_UR_ITOBESPECIFIED,
						 EcorePlugin.INSTANCE.getString("_UI_GenericInvariant_diagnostic", new Object[] { "versionURIrequiresontologyURItobespecified", EObjectValidator.getObjectLabel(this, context) }),
						 new Object [] { this }));
			}
			return false;
		}
		return true;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case OwlPackage.ONTOLOGY__ONTOLOGY_ANNOTATIONS:
				return ((InternalEList<?>)getOntologyAnnotations()).basicRemove(otherEnd, msgs);
			case OwlPackage.ONTOLOGY__AXIOMS:
				return ((InternalEList<?>)getAxioms()).basicRemove(otherEnd, msgs);
			case OwlPackage.ONTOLOGY__IMPORTED_ONTOLOGIES:
				return ((InternalEList<?>)getImportedOntologies()).basicRemove(otherEnd, msgs);
			case OwlPackage.ONTOLOGY__ONTOLOGY_URI:
				return basicSetOntologyURI(null, msgs);
			case OwlPackage.ONTOLOGY__VERSION_URI:
				return basicSetVersionURI(null, msgs);
			case OwlPackage.ONTOLOGY__CONTAINER:
				return ((InternalEList<?>)getContainer()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.ONTOLOGY__ONTOLOGY_ANNOTATIONS:
				return getOntologyAnnotations();
			case OwlPackage.ONTOLOGY__AXIOMS:
				return getAxioms();
			case OwlPackage.ONTOLOGY__IMPORTED_ONTOLOGIES:
				return getImportedOntologies();
			case OwlPackage.ONTOLOGY__ONTOLOGY_URI:
				return getOntologyURI();
			case OwlPackage.ONTOLOGY__VERSION_URI:
				return getVersionURI();
			case OwlPackage.ONTOLOGY__CONTAINER:
				return getContainer();
			case OwlPackage.ONTOLOGY__ONTOLOGY_ID:
				return getOntologyId();
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
			case OwlPackage.ONTOLOGY__ONTOLOGY_ANNOTATIONS:
				getOntologyAnnotations().clear();
				getOntologyAnnotations().addAll((Collection<? extends Annotation>)newValue);
				return;
			case OwlPackage.ONTOLOGY__AXIOMS:
				getAxioms().clear();
				getAxioms().addAll((Collection<? extends Axiom>)newValue);
				return;
			case OwlPackage.ONTOLOGY__IMPORTED_ONTOLOGIES:
				getImportedOntologies().clear();
				getImportedOntologies().addAll((Collection<? extends Ontology>)newValue);
				return;
			case OwlPackage.ONTOLOGY__ONTOLOGY_URI:
				setOntologyURI((URI)newValue);
				return;
			case OwlPackage.ONTOLOGY__VERSION_URI:
				setVersionURI((URI)newValue);
				return;
			case OwlPackage.ONTOLOGY__CONTAINER:
				getContainer().clear();
				getContainer().addAll((Collection<? extends EObject>)newValue);
				return;
			case OwlPackage.ONTOLOGY__ONTOLOGY_ID:
				setOntologyId((String)newValue);
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
			case OwlPackage.ONTOLOGY__ONTOLOGY_ANNOTATIONS:
				getOntologyAnnotations().clear();
				return;
			case OwlPackage.ONTOLOGY__AXIOMS:
				getAxioms().clear();
				return;
			case OwlPackage.ONTOLOGY__IMPORTED_ONTOLOGIES:
				getImportedOntologies().clear();
				return;
			case OwlPackage.ONTOLOGY__ONTOLOGY_URI:
				setOntologyURI((URI)null);
				return;
			case OwlPackage.ONTOLOGY__VERSION_URI:
				setVersionURI((URI)null);
				return;
			case OwlPackage.ONTOLOGY__CONTAINER:
				getContainer().clear();
				return;
			case OwlPackage.ONTOLOGY__ONTOLOGY_ID:
				setOntologyId(ONTOLOGY_ID_EDEFAULT);
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
			case OwlPackage.ONTOLOGY__ONTOLOGY_ANNOTATIONS:
				return ontologyAnnotations != null && !ontologyAnnotations.isEmpty();
			case OwlPackage.ONTOLOGY__AXIOMS:
				return axioms != null && !axioms.isEmpty();
			case OwlPackage.ONTOLOGY__IMPORTED_ONTOLOGIES:
				return importedOntologies != null && !importedOntologies.isEmpty();
			case OwlPackage.ONTOLOGY__ONTOLOGY_URI:
				return ontologyURI != null;
			case OwlPackage.ONTOLOGY__VERSION_URI:
				return versionURI != null;
			case OwlPackage.ONTOLOGY__CONTAINER:
				return container != null && !container.isEmpty();
			case OwlPackage.ONTOLOGY__ONTOLOGY_ID:
				return ONTOLOGY_ID_EDEFAULT == null ? ontologyId != null : !ONTOLOGY_ID_EDEFAULT.equals(ontologyId);
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

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (ontologyId: ");
		result.append(ontologyId);
		result.append(')');
		return result.toString();
	}

} //OntologyImpl
