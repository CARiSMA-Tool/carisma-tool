package carisma.check.idscheck.transferprocessprotocol;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Interaction;
import org.eclipse.uml2.uml.Lifeline;
import org.eclipse.uml2.uml.Message;

import carisma.check.idscheck.dto.RelevantMessagesDto;
import carisma.profile.umlsec.extension4ids.Extension4IDS;
import carisma.profile.umlsec.extension4ids.Extension4IDSUtil;
import carisma.profile.umlsec.extension4ids.TransferType;

/**
 * Helper class for the Transfer Process Protocol Check
 * @author Sanjeev Sun Shakya
 */
public class TransferProcessProtocolHelper {
	
	private static final String TRANSFER_REQUEST = "transfer_req_step";
	private static final String TRANSFER_START = "transfer_start_step";
	private static final String TRANSFER_COMPLETE = "transfer_complete_step";
	private static final String PUSH_PULL_STEP = "push_pull_step";
	private static final String TRANSFER_SUSPENDED = "transfer_suspend_step";
	private static final String TRANSFER_TERMINATED = "transfer_terminate_step";
	private static final String TRANSFER_TYPE = "type";
	
	/**
	 * Checks if any of the interactions have the Data Transfer stereotype.
	 *
	 * @param interactions The list of interactions to check.
	 * @return true if any interaction has the Data Transfer stereotype; false otherwise.
	 */
	public static boolean hasDataTransferStereotype(final List<Interaction> interactions) {
		return interactions.stream()
        .anyMatch(element -> Extension4IDSUtil.hasStereotype(element, Extension4IDS.DATATRANSFER));
	}
	
	/**
	 * Retrieves the set of lifelines annotated with a specific stereotype.
	 *
	 * @param lifelines The set of lifelines to filter.
	 * @param stereotype The stereotype to check for.
	 * @return A set of lifelines annotated with the specified stereotype.
	 */
	public static Set<Lifeline> getAnnotatedLifeline(final Set<Lifeline> lifelines, final Extension4IDS stereotype){
		return lifelines.stream()
		        .filter(element -> Extension4IDSUtil.hasStereotype(element, stereotype))
		        .map(element -> (Lifeline) element)
		        .collect(Collectors.toSet());
	}
	
	/**
	 * Retrieves the tagged values for relevant messages from an interaction.
	 *
	 * @param interaction The interaction from which to retrieve tagged values.
	 * @return A DTO containing the relevant tagged values for the messages.
	 */
	public static RelevantMessagesDto getTaggedValues(Interaction interaction) {
		
		TransferType taggedValueType = getTransferTypeTaggedValue(TRANSFER_TYPE, Extension4IDS.DATATRANSFER, interaction);

		Message taggedValueTransferRequest = (Message) getTaggedValue(TRANSFER_REQUEST, Extension4IDS.DATATRANSFER, interaction);

		Message taggedValueTransferStart = (Message) getTaggedValue(TRANSFER_START, Extension4IDS.DATATRANSFER, interaction);

		Message taggedValueTransferComplete = (Message) getTaggedValue(TRANSFER_COMPLETE, Extension4IDS.DATATRANSFER, interaction);

		Message taggedValueTransferSuspend = (Message) getTaggedValue(TRANSFER_SUSPENDED, Extension4IDS.DATATRANSFER, interaction);

		Message taggedValueTransferTerminate = (Message) getTaggedValue(TRANSFER_TERMINATED, Extension4IDS.DATATRANSFER, interaction);

		Message taggedValuePushPull = (Message) getTaggedValue(PUSH_PULL_STEP, Extension4IDS.DATATRANSFER, interaction);
		
		return new RelevantMessagesDto(taggedValueType, 
				taggedValueTransferRequest, 
				taggedValueTransferStart, 
				taggedValueTransferComplete, 
				taggedValueTransferTerminate,
				taggedValueTransferSuspend, 
				taggedValuePushPull);
	}
	
	
	/**
	 * Retrieves the first tagged value for a given tag name, stereotype, and parent element.
	 *
	 * @param tagName The name of the tag.
	 * @param stereo The stereotype to check for the tag.
	 * @param stereoParent The parent element that contains the stereotype.
	 * @return The first tagged value if present, or null if no tagged value is found.
	 */
	private static Object getTaggedValue(final String tagName, final Extension4IDS stereo, final Element stereoParent) {
		List<Object> taggedValue = Extension4IDSUtil.getTaggedValues(tagName, stereo, stereoParent);
		return taggedValue.isEmpty() ? null : taggedValue.get(0);
	}
	
	/**
	 * Retrieves the TransferType tagged value for a given tag name, stereotype, and parent element.
	 *
	 * @param tagName The name of the tag.
	 * @param stereo The stereotype to check for the tag.
	 * @param stereoParent The parent element that contains the stereotype.
	 * @return The TransferType tagged value if present, or null if no valid tagged value is found.
	 */
	private static TransferType getTransferTypeTaggedValue(final String tagName, final Extension4IDS stereo, final Element stereoParent) {
	    Object taggedValue = getTaggedValue(tagName, stereo, stereoParent);

	    if (taggedValue instanceof String) {
	        try {
	            return TransferType.valueOf((String) taggedValue);
	        } catch (IllegalArgumentException e) {
	            System.err.println("Invalid TransferType value: " + taggedValue);
	        }
	    } else {
	        System.err.println("Tagged value is not a String: " + taggedValue);
	    }
	    return null;
	}
	
	

}
