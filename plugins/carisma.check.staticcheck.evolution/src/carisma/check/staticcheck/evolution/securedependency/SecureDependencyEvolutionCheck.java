/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.check.staticcheck.evolution.securedependency;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Classifier;
import org.eclipse.uml2.uml.DirectedRelationship;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.UMLPackage;
import org.eclipse.uml2.uml.Usage;

import carisma.check.staticcheck.securedependency.SecureDependencyChecks;
import carisma.check.staticcheck.securedependency.SecureDependencyViolation;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.AddElement;
import carisma.evolution.DelElement;
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.DeltaList;
import carisma.evolution.SubstElement;
import carisma.evolution.uml2.ModifierMap;
import carisma.evolution.uml2.UMLModifier;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;


public class SecureDependencyEvolutionCheck implements CarismaCheckWithID {
    
    private static final String CHECK_ID = "carisma.check.staticcheck.evolution.securedependency";

    public static final String PRECONDITION_DELTAS_REGISTER_KEY = "carisma.data.evolution.deltas";
    public static final String PRECONDITIONS_MODIFIERS_REGISTRY_KEY = "carisma.data.evolution.modifiers";

    private static final String CHECK_NAME = "Evolution-aware Secure Dependency Check";

	/**
     * Constant String for the name of the Stereotype 'critical'.
     */
    private static final String CRITICAL = "UMLsec::critical";
    
    /**
     * Constant String for the name of the key 'name'.
     */
    private static final String NAME = "name";

    private final class UsageDescription {
        Usage usageDependency;
        Classifier client;
        Classifier supplier;

        UsageDescription(Usage usageDependency, Classifier client, Classifier supplier) {
            super();
            this.usageDependency = usageDependency;
            this.client = client;
            this.supplier = supplier;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + SecureDependencyEvolutionCheck.this.hashCode();
            result = prime * result + ((this.client == null) ? 0 : this.client.hashCode());
            result = prime * result + ((this.usageDependency == null) ? 0 : this.usageDependency.hashCode());
            result = prime * result + ((this.supplier == null) ? 0 : this.supplier.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            UsageDescription other = (UsageDescription) obj;
            if (this.client == null) {
                if (other.client != null) {
                    return false;
                }
            } else if (!this.client.equals(other.client)) {
                return false;
            }
            if (this.usageDependency == null) {
                if (other.usageDependency != null) {
                    return false;
                }
            } else if (!this.usageDependency.equals(other.usageDependency)) {
                return false;
            }
            if (this.supplier == null) {
                if (other.supplier != null) {
                    return false;
                }
            } else if (!this.supplier.equals(other.supplier)) {
                return false;
            }
            return true;
        }
    }

	private AnalysisHost host = null;

    private DeltaList deltaList = null;
    private ModifierMap deltaModifiers = null;

    private UMLModifier deltaModifier = null;

    private List<SecureDependencyViolation> secureDependencyViolations = null;
    private Map<UsageDescription, DeltaElement> processedUsageDependencies = null;

    /**
     * Private constructor. UMLsec will never be initialized.
     */
    public SecureDependencyEvolutionCheck() {
        this.secureDependencyViolations = new ArrayList<>();
        this.processedUsageDependencies = new HashMap<>();
    }

    public List<SecureDependencyViolation> getViolations() {
        return Collections.unmodifiableList(this.secureDependencyViolations);
    }

    @Override
    public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost newHost) {
        if (newHost != null) {
            this.host = newHost;
        } else {
            this.host = new DummyHost(true);
        }
        Resource currentModel = this.host.getAnalyzedModel();
        if (currentModel.getContents().isEmpty()) {
            this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
            return false;
        }
        if (!(currentModel.getContents().get(0) instanceof Model)) {
            this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
            return false;
        }
        try {
            this.deltaList = (DeltaList) this.host.getFromRegister(PRECONDITION_DELTAS_REGISTER_KEY);
            this.deltaModifiers = (ModifierMap) this.host.getFromRegister(PRECONDITIONS_MODIFIERS_REGISTRY_KEY);
        } catch (RegisterNotInUseException e) {
            Logger.log(LogLevel.ERROR, e.getMessage(), e);
            return false;
        }
        if (this.deltaList == null || this.deltaModifiers == null) {
            return false;
        }
        if (this.deltaList.isEmpty()) {
            this.host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No deltaList left to analyze."));
            return true;
        }
        int beforeMaxChanges = this.deltaList.getHighestChangeCountNow();
        boolean isSuccessful = checkDeltas();
        if (isSuccessful) {
            this.host.addResultMessage(
                    new AnalysisResultMessage(
                            StatusType.INFO,
                            "A successful maximum Delta (using " + this.deltaList.getHighestChangeCountNow() + " changes) exists."));
        } else {
            this.host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No successful maximum Delta (" + beforeMaxChanges + " changes) found."));
            if (this.deltaList.getHighestChangeCountNow() == 0) {
                this.host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "All Deltas violate <<secure dependency>>."));
            } else {
                this.host.addResultMessage(
                        new AnalysisResultMessage(
                                StatusType.ERROR,
                                "Maximum successful Delta has " + this.deltaList.getHighestChangeCountNow() + " changes."));
            }
        }
        return isSuccessful;
    }

    private boolean checkDeltas() {
        boolean hasMaxSuccessfulDelta = false;
        List<Delta> violatingEvolutions = new ArrayList<>();
        for (Delta d : this.deltaList.getRemainingDeltas()) {
            boolean deltaSuccessful = true;
            checkDelta(d);
            if (!this.secureDependencyViolations.isEmpty()) {
                violatingEvolutions.add(d);
                deltaSuccessful = false;
                for (SecureDependencyViolation v : this.secureDependencyViolations) {
                    this.host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, v.getDescription()));
                }
            }
            if (deltaSuccessful
                    && d.getNumberOfUsedChanges() == this.deltaList.getHighestChangeCountNow()) {
                hasMaxSuccessfulDelta = true;
            }
        }
        this.deltaList.removeAll(violatingEvolutions);
        return hasMaxSuccessfulDelta;
    }

    private void checkDelta(final Delta d) {
        init(d);
        for (DelElement del : d.getAllDeletions()) {
            checkDeletion(del);
        }
        for (AddElement add : d.getAllAdditions()) {
            checkAddition(add);
        }
        for (SubstElement sub : d.getAllSubstitutions()) {
            checkSubstitution(sub);
        }
    }

    private void init(final Delta d) {
        this.deltaModifier = this.deltaModifiers.get(d);
        if (this.secureDependencyViolations == null) {
            this.secureDependencyViolations = new ArrayList<>();
        }
        this.secureDependencyViolations.clear();
        if (this.processedUsageDependencies == null) {
            this.processedUsageDependencies = new HashMap<>();
        }
        this.processedUsageDependencies.clear();
    }

    private void checkAddition(AddElement add) {
        this.host.appendLineToReport("Checking addition '" + add + "'...");
        if (add.getMetaClass().equals(UMLPackage.eINSTANCE.getUsage())) {
            processAddedUsageDependency(add);
        } else if (add.getMetaClass().equals(UMLPackage.eINSTANCE.getClassifier())) {
            processAddedClassifier(add);
        } else if (add.getMetaClass().equals(UMLPackage.eINSTANCE.getStereotype())) {
            processAddedStereotype(add);
        } else if (add.getMetaClass().equals(UMLPackage.eINSTANCE.getProperty()) && add.getTarget() instanceof StereotypeApplication) {
            processAddedTaggedValue(add);
        }
    }

    private void checkDeletion(DelElement del) {
        this.host.appendLineToReport("Checking deletion '" + del + "'...");
        if (del.getTarget() instanceof Classifier) {
            processDeletedClassifier(del);
        } else if (del.getTarget() instanceof StereotypeApplication) {
            processDeletedStereotypeApplication(del);
        } else if (del.getTarget() instanceof TaggedValue) {
            processDeletedTaggedValue(del);
        }
    }

    private void checkSubstitution(SubstElement sub) {
        this.host.appendLineToReport("Checking substitution '" + sub + "'...");
        if (sub.getTarget() instanceof Classifier) {
            processDeletedClassifier(sub);
        } else if (sub.getTarget() instanceof StereotypeApplication) {
            processDeletedStereotypeApplication(sub);
        } else if (sub.getTarget() instanceof TaggedValue) {
            processDeletedTaggedValue(sub);
        }
        for (AddElement add : sub.getComponents()) {
            checkAddition(add);
        }
    }

    private void processAddedTaggedValue(AddElement add) {
        TaggedValue taggedValue = (TaggedValue) this.deltaModifier.getAddedElement(add);
        if (taggedValue == null) {
            Logger.log(LogLevel.ERROR, "added tagged value is null?!");
            return;
        }
        // tagged values of stereotype <<critical>>
        if (add.getTarget() instanceof StereotypeApplication) {
            StereotypeApplication app = taggedValue.getCorrespondingApplication();
            if (CRITICAL.equals(app.getAppliedStereotype().getQualifiedName())
                    && app.getExtendedElement() instanceof Classifier) {
                Classifier classifier = (Classifier) app.getExtendedElement();
                List<UsageDescription> deps = getTuplesOfIncomingDependencies(classifier);
                deps.addAll(getTuplesOfOutgoingDependencies(classifier));
                check(deps, add, taggedValue);
            }
        }
        // tagged values of other stereotypes than <<critical>> do not matter!
    }

    private void processAddedStereotype(AddElement add) {
        StereotypeApplication application = (StereotypeApplication) this.deltaModifier.getAddedElement(add);
        if (application == null) {
            Logger.log(LogLevel.ERROR, "added stereotype is null?!");
            return;
        }
        // stereotype <<critical>>
        if (add.getValues() != null && CRITICAL.equals(add.getValues().get(NAME))) {
            Element e = application.getExtendedElement();
            if (e instanceof Classifier) {
                Classifier classifier = (Classifier) e;
                List<UsageDescription> deps = getTuplesOfIncomingDependencies(classifier);
                deps.addAll(getTuplesOfOutgoingDependencies(classifier));
                check(deps, add, application);
            }
        } else if (add.getValues() != null && ("UMLsec::secrecy".equals(add.getValues().get(NAME)))
                || "UMLsec::integrity".equals(add.getValues().get(NAME)) || "UMLsec::high".equals(add.getValues().get(NAME))) {
            Element e = application.getExtendedElement();
            if (e instanceof Usage) {
                Usage dependency = (Usage) e;
                List<UsageDescription> deps = getTuples(dependency);
                check(deps, add, application);
            }
        }
        // other stereotypes do not matter!
    }

    private void processAddedClassifier(AddElement add) {
        Classifier classifier = (Classifier) this.deltaModifier.getAddedElement(add);
        if (classifier == null) {
            Logger.log(LogLevel.ERROR, "added classifier is null?!");
            return;
        }
        List<UsageDescription> deps = getTuplesOfIncomingDependencies(classifier);
        deps.addAll(getTuplesOfOutgoingDependencies(classifier));
        check(deps, add, classifier);
    }

    private void processAddedUsageDependency(AddElement add) {
        this.host.appendLineToReport("Checking added Usage Dependency...");
        Usage dependency = (Usage) this.deltaModifier.getAddedElement(add);
        if (dependency == null) {
            Logger.log(LogLevel.ERROR, "added usageDependency is null?!");
            return;
        }
        List<UsageDescription> deps = getTuples(dependency);
        check(deps, add, dependency);
    }

    private void processDeletedTaggedValue(DeltaElement del) {
        TaggedValue oldTaggedValue = (TaggedValue) del.getTarget();
        StereotypeApplication oldStereotypeApplication = oldTaggedValue.getCorrespondingApplication();
        // tagged values of stereotype <<critical>>
        if (CRITICAL.equals(oldStereotypeApplication.getAppliedStereotype().getQualifiedName())) {
            StereotypeApplication newStereotypeApplication = this.deltaModifier.getCorrespondingStereotypeApplication(oldStereotypeApplication);
            if (newStereotypeApplication != null && newStereotypeApplication.getExtendedElement() instanceof Classifier) {
                Classifier classifier = (Classifier) newStereotypeApplication.getExtendedElement();
                List<UsageDescription> deps = getTuplesOfIncomingDependencies(classifier);
                deps.addAll(getTuplesOfOutgoingDependencies(classifier));
                check(deps, del, oldTaggedValue);
            }
        }
        // tagged values of other stereotypes than <<critical>> do not matter!
    }

    private void processDeletedStereotypeApplication(DeltaElement del) {
        StereotypeApplication oldStereotypeApplication = (StereotypeApplication) del.getTarget();
        // stereotype <<critical>>
        if (CRITICAL.equals(oldStereotypeApplication.getAppliedStereotype().getQualifiedName())) {
            if (oldStereotypeApplication.getExtendedElement() instanceof Classifier) {
                Classifier oldClassifier = (Classifier) oldStereotypeApplication.getExtendedElement();
                Classifier classifier = (Classifier) this.deltaModifier.getMapping().get(oldClassifier);
                List<UsageDescription> deps = getTuplesOfIncomingDependencies(classifier);
                deps.addAll(getTuplesOfOutgoingDependencies(classifier));
                check(deps, del, oldStereotypeApplication);
            }
            // stereotypes <<secrecy>>,<<integrity>>,<<high>> on Dependencies
        } else if (("UMLsec::secrecy".equals(oldStereotypeApplication.getAppliedStereotype().getQualifiedName())
                || "UMLsec::integrity".equals(oldStereotypeApplication.getAppliedStereotype().getQualifiedName())
                || "UMLsec::high".equals(oldStereotypeApplication.getAppliedStereotype().getQualifiedName()))
                && oldStereotypeApplication.getExtendedElement() instanceof Usage) {
            Usage oldUsage = (Usage) oldStereotypeApplication.getExtendedElement();
            Usage usage = (Usage) this.deltaModifier.getMapping().get(oldUsage);
            List<UsageDescription> deps = getTuples(usage);
            check(deps, del, oldStereotypeApplication);
        }
        // other stereotypes do not matter!
    }

    private void processDeletedClassifier(DeltaElement del) {
        Classifier oldClassifier = (Classifier) del.getTarget();
        List<UsageDescription> deps = getTuplesOfIncomingDependencies(oldClassifier);
        deps.addAll(getTuplesOfOutgoingDependencies(oldClassifier));
        List<UsageDescription> newDeps = translateIfPossible(deps);
        check(newDeps, del, oldClassifier);
    }

    private List<UsageDescription> translateIfPossible(List<UsageDescription> oldDependencies) {
        ArrayList<UsageDescription> newDependencies = new ArrayList<>();
        for (UsageDescription oldDependency : oldDependencies) {
            Classifier oldClient = oldDependency.client;
            Classifier oldSupplier = oldDependency.supplier;
            Usage oldDependency2 = oldDependency.usageDependency;
            Classifier newClient = (Classifier) this.deltaModifier.getMapping().get(oldClient);
            Classifier newSupplier = (Classifier) this.deltaModifier.getMapping().get(oldSupplier);
            Usage newDependency2 = (Usage) this.deltaModifier.getMapping().get(oldDependency2);
            if (newDependency2 != null && newClient != null && newSupplier != null) {
                newDependencies.add(new UsageDescription(newDependency2, newClient, newSupplier));
            }
        }
        return newDependencies;
    }

    private boolean isNotYetProcessed(UsageDescription dd, DeltaElement deltaElement) {
        DeltaElement processedBy = this.processedUsageDependencies.get(dd);
        if (processedBy != null) {
            this.host.appendLineToReport("  ... has already been checked with '" + processedBy + "'.");
            return false;
        }
        this.processedUsageDependencies.put(dd, deltaElement);
        return true;
    }

    private void check(List<UsageDescription> deps, DeltaElement deltaElement, EObject element) {
        String name = "?";
        String type = "?";
        String evo = "?";
        if (element instanceof NamedElement) {
            name = ((NamedElement) element).getName();
            type = element.eClass().getName();
        } else if (element instanceof StereotypeApplication) {
            name = ((StereotypeApplication) element).getAppliedStereotype().getQualifiedName();
            type = "Stereotype";
        } else if (element instanceof TaggedValue) {
            name = ((TaggedValue) element).getCorrespondingApplication().getAppliedStereotype().getName() + "." + ((TaggedValue) element).getName();
            type = "TaggedValue";
        }
        if (deltaElement instanceof AddElement) {
            evo = "Addition";
        } else if (deltaElement instanceof DelElement) {
            evo = "Deletion";
        } else if (deltaElement instanceof SubstElement) {
            evo = "Substition";
        }
        for (UsageDescription dd : deps) {
            if (isNotYetProcessed(dd, deltaElement)) {
                SecureDependencyChecks sdc = new SecureDependencyChecks(this.host);
                List<SecureDependencyViolation> violations = sdc.checkDependency(dd.usageDependency, dd.client, dd.supplier);
                if (!violations.isEmpty()) {
                    this.secureDependencyViolations.addAll(violations);
                    this.host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
                            evo + " of " + type + " '" + name + "' violates the property <<secure usageDependency>> (see report for details)"));
                    this.host.appendLineToReport(evo + " of " + type + " '" + name + "' violates the property <<secure usageDependency>>");
                    for (SecureDependencyViolation v : violations) {
                        this.host.appendLineToReport("  " + v.getDescription());
                    }
                }
            }
        }
    }

    private List<UsageDescription> getTuples(Usage dependency) {
        ArrayList<UsageDescription> tuples = new ArrayList<>();
        List<NamedElement> clients = dependency.getClients();
        List<NamedElement> suppliers = dependency.getSuppliers();
        for (NamedElement c : clients) {
            for (NamedElement s : suppliers) {
                if (c instanceof Classifier && s instanceof Classifier) {
                    tuples.add(new UsageDescription(dependency, (Classifier) c, (Classifier) s));
                }
            }
        }
        return tuples;
    }

    private List<UsageDescription> getTuplesOfOutgoingDependencies(Classifier classifier) {
        ArrayList<UsageDescription> opposites = new ArrayList<>();
        for (DirectedRelationship rel : classifier.getSourceDirectedRelationships(UMLPackage.eINSTANCE.getUsage())) {
            Usage dep = (Usage) rel;
            if (SecureDependencyChecks.isRelevantDependency(dep)) {
                for (NamedElement sup : dep.getSuppliers()) {
                    if (sup instanceof Classifier) {
                        opposites.add(new UsageDescription(dep, classifier, (Classifier) sup));
                    }
                }
            }
        }
        return opposites;
    }

    private List<UsageDescription> getTuplesOfIncomingDependencies(Classifier classifier) {
        ArrayList<UsageDescription> opposites = new ArrayList<>();
        List<Classifier> supers = SecureDependencyChecks.getSuperClassifiers(classifier);
        for (Classifier s : supers) {
            for (DirectedRelationship rel : s.getTargetDirectedRelationships(UMLPackage.eINSTANCE.getUsage())) {
                Usage dep = (Usage) rel;
                if (SecureDependencyChecks.isRelevantDependency(dep)) {
                    for (NamedElement client : dep.getClients()) {
                        if (client instanceof Classifier) {
                            opposites.add(new UsageDescription(dep, (Classifier) client, s));
                        }
                    }
                }
            }
        }
        return opposites;
    }

	@Override
	public String getCheckID() {
		return CHECK_ID;
	}

	@Override
	public String getName() {
		return CHECK_NAME;
	}

}
