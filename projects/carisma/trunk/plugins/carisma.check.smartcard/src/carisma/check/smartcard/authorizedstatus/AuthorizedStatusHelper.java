package carisma.check.smartcard.authorizedstatus;

import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Transition;

import carisma.modeltype.uml2.StereotypeApplication;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


public final class AuthorizedStatusHelper {
	
	private AuthorizedStatusHelper() {
		
	}
	
	public static String getPermission(final Transition incomingTransition) {
		State targetState = (State) incomingTransition.getTarget();
		StereotypeApplication authApp = UMLsecUtil.getStereotypeApplication(targetState, UMLsec.AUTHORIZED_STATUS);
		if (authApp != null) {
			return (String) authApp.getTaggedValue("permission").getValue();
		}
		return "";
	}
}
