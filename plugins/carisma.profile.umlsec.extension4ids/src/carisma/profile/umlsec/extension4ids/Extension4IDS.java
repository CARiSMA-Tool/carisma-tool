package carisma.profile.umlsec.extension4ids;

import org.eclipse.uml2.uml.Stereotype;

import carisma.modeltype.uml2.CarismaProfileDescriptor;

public enum Extension4IDS {
	
	IDSCONNECTOR ("IDSconnector"),
	USAGECONTROL ("UsageControl"),
	DATATRANSFER ("TransferProcessProtocol"),
	PROVIDER ("ProviderConnector"),
	CONSUMER ("ConsumerConnector");
	private static final String PROFILE_NAME = "extension4ids";
	private static final String PROFILE_VERSION = "1";
	private static final String PROFILE_URI = "platform:/carisma.profile.umlsec.extension4ids/profile/extension4ids.profile.uml";

	public static final CarismaProfileDescriptor DESCRIPTOR = new CarismaProfileDescriptor(PROFILE_NAME, PROFILE_VERSION, PROFILE_URI);
	
	private final String readableName;
	
	private Extension4IDS (final String newName) {
		readableName = newName;
	}
	
	public String toString() {
		return readableName;
	}
	
	public static Extension4IDS getValue(final String name) {
		for (Extension4IDS type : Extension4IDS.values()) {
			if (type.toString().equalsIgnoreCase(name)) {
				return type;
			}
		}
		return null;
	}

	/**
	 * Checks whether the given stereotype corresponds to the one represented by this enum literal.
	 * @param stereotype
	 * @return
	 */
	public boolean isEqual(Stereotype stereotype) {
		return contains(stereotype) && stereotype.getName().equalsIgnoreCase(this.readableName);
	}
	
	/**
	 * Checks whether the given Stereotype is a UMLsec stereotype.
	 * @param stereotype
	 * @return
	 */
	public static boolean contains(Stereotype stereotype) {
		return (stereotype.getProfile().getDefinition().getNsURI().contains(DESCRIPTOR.getProfileName()));
	}

}
