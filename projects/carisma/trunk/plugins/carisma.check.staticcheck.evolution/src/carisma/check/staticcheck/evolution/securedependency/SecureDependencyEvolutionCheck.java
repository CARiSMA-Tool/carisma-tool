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
import carisma.core.checks.CarismaCheck;
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


public class SecureDependencyEvolutionCheck implements CarismaCheck {
    
    /**
     * Constant String for the name of the Stereotype 'critical'.
     */
    private static final String CRITICAL = "UMLsec::critical";
    
    /**
     * Constant String for the name of the key 'name'.
     */
    private static final String NAME = "name";

    private final class UsageDescription {
        private Usage usageDependency;
        private Classifier client;
        private Classifier supplier;

        private UsageDescription(Usage usageDependency, Classifier client, Classifier supplier) {
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
            result = prime * result + ((client == null) ? 0 : client.hashCode());
            result = prime * result + ((usageDependency == null) ? 0 : usageDependency.hashCode());
            result = prime * result + ((supplier == null) ? 0 : supplier.hashCode());
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
            if (client == null) {
                if (other.client != null) {
                    return false;
                }
            } else if (!client.equals(other.client)) {
                return false;
            }
            if (usageDependency == null) {
                if (other.usageDependency != null) {
                    return false;
                }
            } else if (!usageDependency.equals(other.usageDependency)) {
                return false;
            }
            if (supplier == null) {
                if (other.supplier != null) {
                    return false;
                }
            } else if (!supplier.equals(other.supplier)) {
                return false;
            }
            return true;
        }
    }

    public static final String DELTAS_REGISTER_KEY = "carisma.data.evolution.deltas";
    public static final String MODIFIERS_REGISTRY_KEY = "carisma.data.evolution.modifiers";

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
        secureDependencyViolations = new ArrayList<SecureDependencyViolation>();
        processedUsageDependencies = new HashMap<UsageDescription, DeltaElement>();
    }

    public List<SecureDependencyViolation> getViolations() {
        return Collections.unmodifiableList(secureDependencyViolations);
    }

    @Override
    public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost newHost) {
        if (newHost != null) {
            host = newHost;
        } else {
            host = new DummyHost(true);
        }
        Resource currentModel = host.getAnalyzedModel();
        if (currentModel.getContents().isEmpty()) {
            host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
            return false;
        }
        if (!(currentModel.getContents().get(0) instanceof Model)) {
            host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
            return false;
        }
        try {
            deltaList = (DeltaList) host.getFromRegister(DELTAS_REGISTER_KEY);
            deltaModifiers = (ModifierMap) host.getFromRegister(MODIFIERS_REGISTRY_KEY);
        } catch (RegisterNotInUseException e) {
            Logger.log(LogLevel.ERROR, e.getMessage(), e);
            return false;
        }
        if (deltaList == null || deltaModifiers == null) {
            return false;
        }
        if (deltaList.isEmpty()) {
            host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No deltaList left to analyze."));
            return true;
        }
        int beforeMaxChanges = deltaList.getHighestChangeCountNow();
        boolean isSuccessful = checkDeltas();
        if (isSuccessful) {
            host.addResultMessage(
                    new AnalysisResultMessage(
                            StatusType.INFO,
                            "A successful maximum Delta (using " + deltaList.getHighestChangeCountNow() + " changes) exists."));
        } else {
            host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No successful maximum Delta (" + beforeMaxChanges + " changes) found."));
            if (deltaList.getHighestChangeCountNow() == 0) {
                host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "All Deltas violate <<secure dependency>>."));
            } else {
                host.addResultMessage(
                        new AnalysisResultMessage(
                                StatusType.ERROR,
                                "Maximum successful Delta has " + deltaList.getHighestChangeCountNow() + " changes."));
            }
        }
        return isSuccessful;
    }

    private boolean checkDeltas() {
        boolean hasMaxSuccessfulDelta = false;
        List<Delta> violatingEvolutions = new ArrayList<Delta>();
        for (Delta d : deltaList.getRemainingDeltas()) {
            boolean deltaSuccessful = true;
            checkDelta(d);
            if (!secureDependencyViolations.isEmpty()) {
                violatingEvolutions.add(d);
                deltaSuccessful = false;
                for (SecureDependencyViolation v : secureDependencyViolations) {
                    host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, v.getDescription()));
                }
            }
            if (deltaSuccessful
                    && d.getNumberOfUsedChanges() == deltaList.getHighestChangeCountNow()) {
                hasMaxSuccessfulDelta = true;
            }
        }
        deltaList.removeAll(violatingEvolutions);
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
        deltaModifier = deltaModifiers.get(d);
        if (secureDependencyViolations == null) {
            secureDependencyViolations = new ArrayList<SecureDependencyViolation>();
        }
        secureDependencyViolations.clear();
        if (processedUsageDependencies == null) {
            processedUsageDependencies = new HashMap<UsageDescription, DeltaElement>();
        }
        processedUsageDependencies.clear();
    }

    private void checkAddition(AddElement add) {
        host.appendLineToReport("Checking addition '" + add + "'...");
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
        host.appendLineToReport("Checking deletion '" + del + "'...");
        if (del.getTarget() instanceof Classifier) {
            processDeletedClassifier(del);
        } else if (del.getTarget() instanceof StereotypeApplication) {
            processDeletedStereotypeApplication(del);
        } else if (del.getTarget() instanceof TaggedValue) {
            processDeletedTaggedValue(del);
        }
    }

    private void checkSubstitution(SubstElement sub) {
        host.appendLineToReport("Checking substitution '" + sub + "'...");
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
        TaggedValue taggedValue = (TaggedValue) deltaModifier.getAddedElement(add);
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
        StereotypeApplication application = (StereotypeApplication) deltaModifier.getAddedElement(add);
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
        Classifier classifier = (Classifier) deltaModifier.getAddedElement(add);
        if (classifier == null) {
            Logger.log(LogLevel.ERROR, "added classifier is null?!");
            return;
        }
        List<UsageDescription> deps = getTuplesOfIncomingDependencies(classifier);
        deps.addAll(getTuplesOfOutgoingDependencies(classifier));
        check(deps, add, classifier);
    }

    private void processAddedUsageDependency(AddElement add) {
        host.appendLineToReport("Checking added Usage Dependency...");
        Usage dependency = (Usage) deltaModifier.getAddedElement(add);
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
            StereotypeApplication newStereotypeApplication = deltaModifier.getCorrespondingStereotypeApplication(oldStereotypeApplication);
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
                Classifier classifier = (Classifier) deltaModifier.getMapping().get(oldClassifier);
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
            Usage usage = (Usage) deltaModifier.getMapping().get(oldUsage);
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
        ArrayList<UsageDescription> newDependencies = new ArrayList<SecureDependencyEvolutionCheck.UsageDescription>();
        for (UsageDescription oldDependency : oldDependencies) {
            Classifier oldClient = oldDependency.client;
            Classifier oldSupplier = oldDependency.supplier;
            Usage oldDependency2 = oldDependency.usageDependency;
            Classifier newClient = (Classifier) deltaModifier.getMapping().get(oldClient);
            Classifier newSupplier = (Classifier) deltaModifier.getMapping().get(oldSupplier);
            Usage newDependency2 = (Usage) deltaModifier.getMapping().get(oldDependency2);
            if (newDependency2 != null && newClient != null && newSupplier != null) {
                newDependencies.add(new UsageDescription(newDependency2, newClient, newSupplier));
            }
        }
        return newDependencies;
    }

    private boolean isNotYetProcessed(UsageDescription dd, DeltaElement deltaElement) {
        DeltaElement processedBy = processedUsageDependencies.get(dd);
        if (processedBy != null) {
            host.appendLineToReport("  ... has already been checked with '" + processedBy + "'.");
            return false;
        }
        processedUsageDependencies.put(dd, deltaElement);
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
                SecureDependencyChecks sdc = new SecureDependencyChecks(host);
                List<SecureDependencyViolation> violations = sdc.checkDependency(dd.usageDependency, dd.client, dd.supplier);
                if (!violations.isEmpty()) {
                    secureDependencyViolations.addAll(violations);
                    host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
                            evo + " of " + type + " '" + name + "' violates the property <<secure usageDependency>> (see report for details)"));
                    host.appendLineToReport(evo + " of " + type + " '" + name + "' violates the property <<secure usageDependency>>");
                    for (SecureDependencyViolation v : violations) {
                        host.appendLineToReport("  " + v.getDescription());
                    }
                }
            }
        }
    }

    private List<UsageDescription> getTuples(Usage dependency) {
        ArrayList<UsageDescription> tuples = new ArrayList<UsageDescription>();
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
        ArrayList<UsageDescription> opposites = new ArrayList<UsageDescription>();
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
        ArrayList<UsageDescription> opposites = new ArrayList<UsageDescription>();
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

}
