package carisma.modeltype.uml2;

public class CarismaProfileDescriptor {

	private String profileName;
	private String profileVersion;
	private String profileURI;
	
	public CarismaProfileDescriptor(String profileName, String profileVersion, String profileURI) {
		this.profileName = profileName;
		this.profileVersion = profileVersion;
		this.profileURI = profileURI;
	}
	
	public String getProfileName() {
		return this.profileName;
	}
	public String getProfileVersion() {
		return this.profileVersion;
	}
	public String getProfileURI() {
		return this.profileURI;
	}
	
}
