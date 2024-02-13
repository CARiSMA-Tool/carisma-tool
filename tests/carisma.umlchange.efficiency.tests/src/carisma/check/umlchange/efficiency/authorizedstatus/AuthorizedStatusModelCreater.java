package carisma.check.umlchange.efficiency.authorizedstatus;



import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Constraint;
import org.eclipse.uml2.uml.FinalState;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.OpaqueExpression;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Pseudostate;
import org.eclipse.uml2.uml.PseudostateKind;
import org.eclipse.uml2.uml.Region;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.StateMachine;
import org.eclipse.uml2.uml.Transition;
import org.eclipse.uml2.uml.UMLFactory;

import carisma.evolution.uml2.UMLModifierElementFactory;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlchange.UMLChangeActivator;
import carisma.profile.umlsec.UMLsecActivator;

/**
 * this class creates a model with UMLsec authorized-status stereotypes and UMLChange stereotypes for performance tests.
 * @author Klaus Rudack
 *
 */
public class AuthorizedStatusModelCreater {
	
	/**
	 * UMLFactory Instance to create model-elements.
	 */
	private static UMLFactory uMLfac = UMLFactory.eINSTANCE;
	
	/**
	 * the first state in the StateMachine, every single path starts here.
	 */
	private State firstState = null;
	
	/**
	 * the last state in the StateMachine, every ingle path ends here.
	 */
	private State lastState = null;
	
	/**
	 * qualified string for UMLsec locked status.
	 */
	private String authorizedStatus = "UMLsec::authorized-status";
	
	/**
	 * qualified string for UMLchange edit.
	 */
	private static final String EDIT = "UMLchange::edit";
	
	/**
	 * qualified string for UMLchange add.
	 */
	private String add = "UMLchange::add";
	
	/**
	 * counter variable to create different ref ids.
	 */
	private int counter = 0;
	

	/**
	 * creates a new model.
	 * @param name the name of the model.
	 * @param paths amount of paths in the model
	 * @param percentage percentage of the paths that should contain an UMLChange stereotype
	 * @return resource of the new model
	 */
	public final Resource getNewModel(final String name, final int paths, final float percentage) {
		int change = (int) ((paths * percentage) / 100);
		int nonChange = paths - change;
		Model m = uMLfac.createModel();
		m.setName("ModelName");
		Package pkg = m.createNestedPackage("pkg");
		Region region = uMLfac.createRegion();
		region.setName("region");
		ResourceSet rs  = new ResourceSetImpl();
		StateMachine sm = uMLfac.createStateMachine();
		sm.setName(name);
		region.setStateMachine(sm);
		sm.setPackage(pkg);
		URI uri = URI.createURI("testmodel.uml");
		Resource model = rs.createResource(uri);
		model.getContents().add(m);
		UMLHelper.applyProfile(m, UMLsecActivator.UML_FILE);
		UMLHelper.applyProfile(m, UMLChangeActivator.UML_FILE);
		init(region);
		for (int i = 0; i < change; i++) {
			createPath(region, true);
		}
		for (int i = 0; i < nonChange; i++) {
			createPath(region, false);
		}
		createAdds(region, change);
		return model;
		
	}
	
	/**
	 * Initializes the {@link StateMachine}.
	 * Adds an {@link Pseudostate} as Initalsate with an outgoing {@link Transition} to a {@link State},
	 * and a {@link State} with an outgoing {@link Transition} to a {@link FinalState}.
	 * @param region the {@link Region} that should contain the Elements
	 */
	private void init(final Region region) {
		Pseudostate init = (Pseudostate) region.createSubvertex("Init", uMLfac.createPseudostate().eClass());
		init.setKind(PseudostateKind.INITIAL_LITERAL);
		FinalState finalState = (FinalState) region.createSubvertex("Final", uMLfac.createFinalState().eClass());
		State first = (State) region.createSubvertex("First", uMLfac.createState().eClass());
		this.firstState = first;
		State last = (State) region.createSubvertex("Last", uMLfac.createState().eClass());
		this.lastState = last;
		Transition t1 = region.createTransition("t1");
		t1.setSource(init);
		t1.setTarget(first);
		Transition t2  = region.createTransition("t2");
		t2.setSource(finalState);
		t2.setTarget(last);
	}
	
	/**
	 * creates a path from the "First" state to the "Last" state in the StateMachine.
	 * @param region {@link Region} that is Owner of the Elements
	 * @param change true if in this path should occur an UMLChange Stereotype, false otherwise
	 */
	private void createPath(final Region region, final boolean change) {
		State s1 = (State) region.createSubvertex("s1" + this.counter, uMLfac.createState().eClass());
		State s2 = (State) region.createSubvertex("s2" + this.counter, uMLfac.createState().eClass());
		State s3 = (State) region.createSubvertex("s3" + this.counter, uMLfac.createState().eClass());
		State s4 = (State) region.createSubvertex("s4" + this.counter, uMLfac.createState().eClass());
		State s5 = (State) region.createSubvertex("s5" + this.counter, uMLfac.createState().eClass());
		State s6 = (State) region.createSubvertex("s6ChangeState" + this.counter, uMLfac.createState().eClass());
		State s7 = (State) region.createSubvertex("s7" + this.counter, uMLfac.createState().eClass());
		Transition t1 = region.createTransition("t1" + this.counter);
		Transition t2 = region.createTransition("t2" + this.counter);
		Transition t3 = region.createTransition("t3" + this.counter);
		Transition t4 = region.createTransition("t4" + this.counter);
		Transition t5 = region.createTransition("t5" + this.counter);
		Transition t6 = region.createTransition("t6" + this.counter);
		Transition t7 = region.createTransition("t7" + this.counter);
		Transition t8 = region.createTransition("t8" + this.counter);
		Transition t9 = region.createTransition("t9" + this.counter);
		Transition t10 = region.createTransition("ti1" + this.counter);
		t1.setSource(this.firstState);
		t1.setTarget(s1);
		t2.setSource(s1);
		t2.setTarget(s2);
		t3.setSource(s2);
		t3.setTarget(s3);
		t4.setSource(s3);
		t4.setTarget(s4);
		t5.setSource(s4);
		t5.setTarget(s5);
		t6.setSource(s5);
		t6.setTarget(this.lastState);
		t7.setSource(s2);
		t7.setTarget(s6);
		t8.setSource(s4);
		t8.setTarget(s7);
		t9.setSource(s7);
		t9.setTarget(s6);
		t10.setSource(s6);
		t10.setTarget(this.lastState);

		Constraint t3Constraint = createConstraint("GuardT3", "right");
		Constraint t7Constraint = createConstraint("GuardT7", "right");
		Constraint t9Constraint = createConstraint("GuardT9", "right");

		t3.setGuard(t3Constraint);
		t7.setGuard(t7Constraint);
		t9.setGuard(t9Constraint);
		
		UMLModifierElementFactory.insertContainmentRelationship(t3, t3Constraint);
		UMLModifierElementFactory.insertContainmentRelationship(t7, t7Constraint);
		UMLModifierElementFactory.insertContainmentRelationship(t9, t9Constraint);

		StereotypeApplication stereoAppAuthorizedStatuss3 = UMLHelper.applyStereotype(s3, this.authorizedStatus);
		StereotypeApplication stereoAppAuthorizedStatuss6 = UMLHelper.applyStereotype(s6, this.authorizedStatus);
		
		
		if (stereoAppAuthorizedStatuss3 == null || stereoAppAuthorizedStatuss6 == null) {
			System.out.println("Failed to create Stereotype 'authorizedStatus'");
//			TODO: Ausgabe etc wegen Fehler
			return;
		}
		
		TaggedValue permissionT3 = stereoAppAuthorizedStatuss3.getTaggedValue("permission");
		permissionT3.setValue("right");
		TaggedValue permissionT6 = stereoAppAuthorizedStatuss6.getTaggedValue("permission");
		permissionT6.setValue("right");
		if (change) {
			this.counter++;
		}
	}
	
	/** Helper method to create guard Constraint for a Transition.
	 *  
	 * @param constraintName Name of the Constraint. 
	 * @param specificationContent Content of the 'specification' Field.
	 * @return Constraint with Name=constraintName, specification=specificationContent and Language="En". null if one of the parameter is null!
	 */
	private static Constraint createConstraint(final String constraintName, final String specificationContent) {
		String languageName = "En";
		if (constraintName != null && specificationContent != null) {
			Constraint newConstraint = UMLFactory.eINSTANCE.createConstraint();
			newConstraint.setName(constraintName);
			OpaqueExpression guard = UMLFactory.eINSTANCE.createOpaqueExpression();
			newConstraint.setSpecification(guard);
			guard.getLanguages().add(languageName);
			guard.getBodies().add(specificationContent);
			return newConstraint;
		}
		return null;
	}
	
	/**
	 * creates an add stereotype at the given region to create some {@link Transition}s.
	 * @param region region to add transitions in
	 * @param amount amount of transitions
	 */
	private void createAdds(final Region region, final int amount) {
		if (amount > 0) {
			StereotypeApplication stereoAppAdd =  UMLHelper.applyStereotype(region, this.add);
			if (stereoAppAdd != null) {
				TaggedValue refTag = stereoAppAdd.getTaggedValue("ref");
				TaggedValue newTag = stereoAppAdd.getTaggedValue("new");
				TaggedValue consTag = stereoAppAdd.getTaggedValue("constraint");
				if (refTag == null) {
					return;
				}
				if (newTag == null) {
					return;
				}
				for (int i = 0; i < amount; i++) {
					int consIndex = 0;
					if (i == 0) {
						consIndex = amount - 1;
					} else {
						consIndex = i - 1;
					}
					consTag.setValue("addRef" + i + "={AND(addRef" + consIndex + ")}");
					newTag.setValue("addRef" + i + "={Transition(name=addTransition" + i + ",source=s1" + i + ",target=s3" + i + ")}");
				}
			}
		} else {
			//TODO eventuell ausgabe etc sollte es nicht funktioniert haben
			System.out.println("add funzt nicht");
		}	
	}
}
