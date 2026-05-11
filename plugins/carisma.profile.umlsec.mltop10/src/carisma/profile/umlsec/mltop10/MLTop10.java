package carisma.profile.umlsec.mltop10;

import org.eclipse.uml2.uml.Stereotype;

import carisma.modeltype.uml2.CarismaProfileDescriptor;

public enum MLTop10 {

	MLModel("MLModel"), AIApplication("AIApplication"), TrainingData("TrainingData"), FeedbackData("FeedbackData"),
	AIAlgorithm("AIAlgorithm"), SecureAIScenario("SecureAIScenario"), TrainingDataServer("TrainingDataServer"),
	Integrity("integrity"), Secrecy("secrecy");

	private static final String PROFILE_NAME = "mltop10";
	private static final String PROFILE_VERSION = "1";
	private static final String PROFILE_URI = "platform:/carisma.profile.umlsec.extension4ids/profile/mltop10.profile.uml";

	public static final CarismaProfileDescriptor DESCRIPTOR = new CarismaProfileDescriptor(PROFILE_NAME,
			PROFILE_VERSION, PROFILE_URI);

	private final String readableName;

	private MLTop10(final String newName) {
		readableName = newName;
	}

	public String toString() {
		return readableName;
	}

	public static MLTop10 getValue(final String name) {
		for (MLTop10 type : MLTop10.values()) {
			if (type.toString().equalsIgnoreCase(name)) {
				return type;
			}
		}
		return null;
	}

	/**
	 * Checks whether the given stereotype corresponds to the one represented by
	 * this enum literal.
	 * 
	 * @param stereotype
	 * @return
	 */
	public boolean isEqual(Stereotype stereotype) {
		return contains(stereotype) && stereotype.getName().equalsIgnoreCase(this.readableName);
	}

	/**
	 * Checks whether the given Stereotype is a UMLsec stereotype.
	 * 
	 * @param stereotype
	 * @return
	 */
	public static boolean contains(Stereotype stereotype) {
		return (stereotype.getProfile().getDefinition().getNsURI().contains(DESCRIPTOR.getProfileName()));
	}

}
