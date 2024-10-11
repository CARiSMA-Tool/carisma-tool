package carisma.check.extension4ids.transferprocessprotocol;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.InteractionFragment;
import org.eclipse.uml2.uml.Lifeline;
import org.eclipse.uml2.uml.Message;
import org.eclipse.uml2.uml.MessageOccurrenceSpecification;

import carisma.modeltype.uml2.UMLHelper;

import org.eclipse.uml2.uml.Package;

/**
 * Helper class for Sequence Diagram
 * @author Sanjeev Sun Shakya
 */

public class UMLSequenceHelper {
	
	private UMLSequenceHelper() {
		
	}
	
	/**
	 * Returns all the Lifelines in the sequence diagram
	 * @param pkg - the package to check
	 * @return -  set of all the lifelines
	 */
	public static Set<Lifeline>  getAllLifeLines(final Package pkg){

		return UMLHelper.getAllElementsOfType(pkg, Lifeline.class)
				.stream()
				.filter(element -> element instanceof Lifeline)
				.map(element -> (Lifeline) element)
				.collect(Collectors.toSet());
	}
	
	/**
	 * Returns all the Messages in the sequence diagram
	 * @param pkg - the package to check
	 * @return - set of all the messages
	 */
	public static Set<Message> getAllMessages(final Package pkg){
		
		return UMLHelper.getAllElementsOfType(pkg, Message.class)
				.stream()
				.filter(element -> element instanceof Message)
				.map(element -> (Message) element)
				.collect(Collectors.toSet());
		
	}
	
	/**
	 * Returns all the MessageOccurrenceSpecification in the sequence diagram
	 * @param pkg
	 * @return - set of all the MessageOccurrenceSpecification
	 */
	public static Set<MessageOccurrenceSpecification> getAllMessageOccurrenceSpecification(final Package pkg){
		
		return UMLHelper.getAllElementsOfType(pkg, MessageOccurrenceSpecification.class)
			.stream()
			.filter(element -> element instanceof MessageOccurrenceSpecification)
			.map(element -> (MessageOccurrenceSpecification) element)
			.collect(Collectors.toSet());
		
	}
	
	/**
	 * Returns Send and Receive Lifeline of a message in the sequence diagram
	 * @param message - the message whose Send and Receive Lifeline is needed
	 * @return - map of Send and Receive Lifeline
	 */
	public static Map<String, Lifeline> getSendAndRecieveLifeline(final Message message) {
		Map<String, Lifeline> sendReceiveLifeline = new HashMap<>();
		Optional<MessageOccurrenceSpecification> sendEvent = getSendEvent(message);
		Optional<MessageOccurrenceSpecification> receiveEvent = getReceiveEvent(message);
		
		Optional<Lifeline> sendLifeline = sendEvent
				.map(MessageOccurrenceSpecification::getCovered)
				.map(element -> (Lifeline) element);
		
		Optional<Lifeline> receiveLifeline = receiveEvent
				.map(MessageOccurrenceSpecification::getCovered)
				.map(element -> (Lifeline) element);
		
		sendLifeline.ifPresent(element -> sendReceiveLifeline.put("sendLifeline", element));
		receiveLifeline.ifPresent(element -> sendReceiveLifeline.put("receiveLifeline", element));
		
		return sendReceiveLifeline;
	}
	
	/**
	 * Returns the send event of a message
	 * @param message - message of whose message occurrence specification is needed
	 * @return - message occurrence specification
	 */
	public static Optional<MessageOccurrenceSpecification> getSendEvent(final Message message) {
		return Optional.ofNullable((MessageOccurrenceSpecification) message.getSendEvent());
	
	}
	
	/**
	 * Returns the receive event of a message
	 * @param message - message of whose message occurrence specification is needed
	 * @return - message occurrence specification
	 */
	public static Optional<MessageOccurrenceSpecification> getReceiveEvent(final Message message) {
		return Optional.ofNullable((MessageOccurrenceSpecification) message.getReceiveEvent());
	}
	
	/**
	 * Returns all the send events of a pacakge
	 * @param pkg - the package to check
	 * @return - Set of send events
	 */
	public static Set<MessageOccurrenceSpecification> getAllSendEvents(final Package pkg) {
	    Set<MessageOccurrenceSpecification> sendEvents = new LinkedHashSet<>();
	    Iterator<EObject> iterator = pkg.eAllContents();

	    while (iterator.hasNext()) {
	        EObject eObject = iterator.next();
	        if (eObject instanceof MessageOccurrenceSpecification) {
	            MessageOccurrenceSpecification occurrence = (MessageOccurrenceSpecification) eObject;
	            if (occurrence.getName().contains("Send")) {
	                sendEvents.add(occurrence);
	            }
	        }
	    }

	    return sendEvents;
	}
	
	/**
	 * Retrieves all messages sent from a given lifeline.
	 *
	 * @param lifeline - The lifeline from which to retrieve sent messages.
	 * @return A set of messages sent from the specified lifeline.
	 */
	public static Set<Message> getAllMessagesFromLifeline(final Lifeline lifeline){
		Set<Message> sentMessages = lifeline.getCoveredBys().stream()
				.filter(element -> element instanceof MessageOccurrenceSpecification && element.getName().contains("Send"))
				.map(element -> (MessageOccurrenceSpecification) element)
				.map(element -> element.getMessage())
				.collect(Collectors.toSet());
		
		return sentMessages;
	}
	
	/**
	 * Retrieves all messages sent to a given lifeline.
	 *
	 * @param lifeline - The lifeline from which to retrieve recevied messages.
	 * @return A set of messages sent to the specified lifeline.
	 */
	public static Set<Message> getAllMessagesToLifeline(final Lifeline lifeline){
		Set<Message> receivedMessages = lifeline.getCoveredBys().stream()
				.filter(element -> element instanceof MessageOccurrenceSpecification && element.getName().contains("Receive"))
				.map(element -> (MessageOccurrenceSpecification) element)
				.map(element -> element.getMessage())
				.collect(Collectors.toSet());
		
		return receivedMessages;
	}
	
	/**
	 * Retrieves all messages exchanged between two given lifelines.
	 *
	 * @param lifelineA - The first lifeline.
	 * @param lifelineB - The second lifeline.
	 * @return A set of messages exchanged between the specified lifelines.
	 */
	public static Set<Message> getAllMessagesBetweenLifelines(final Lifeline lifelineA, final Lifeline lifelineB){
		Set<Message> allMessagesBetweenLifelines = new HashSet<>();
		
		Set<Message> lifelineASentMessages = getAllMessagesFromLifeline(lifelineA);
		Set<Message> lifelineAReceivedMessages = getAllMessagesToLifeline(lifelineA);
		
		Set<Message> lifelineBSentMessages = getAllMessagesFromLifeline(lifelineB);
		Set<Message> lifelineBReceivedMessages = getAllMessagesToLifeline(lifelineB);
		
		allMessagesBetweenLifelines.addAll(
		        lifelineASentMessages.stream()
		            .filter(lifelineBReceivedMessages::contains)
		            .collect(Collectors.toSet())
		    );

		    allMessagesBetweenLifelines.addAll(
		        lifelineBSentMessages.stream()
		            .filter(lifelineAReceivedMessages::contains)
		            .collect(Collectors.toSet())
		    );
		
		return allMessagesBetweenLifelines;
	}
	
	/**
	 * Retrieves all messages sent from lifelineA that are not received by lifelineB
	 * and messages sent from lifelineB that are not received by lifelineA.
	 *
	 * @param lifelineA - The first lifeline.
	 * @param lifelineB - The second lifeline.
	 * @return A set of messages not exchanged between the specified lifelines.
	 */
	public static Set<Message> getAllMessagesNotBetweenLifelines(final Lifeline lifelineA, final Lifeline lifelineB){
		Set<Message> allMessagesNotBetweenLifelines = new HashSet<>();
		
		Set<Message> lifelineASentMessages = getAllMessagesFromLifeline(lifelineA);
		Set<Message> lifelineAReceivedMessages = getAllMessagesToLifeline(lifelineA);
		
		Set<Message> lifelineBSentMessages = getAllMessagesFromLifeline(lifelineB);
		Set<Message> lifelineBReceivedMessages = getAllMessagesToLifeline(lifelineB);
		
		allMessagesNotBetweenLifelines.addAll(
		        lifelineASentMessages.stream()
		            .filter(message -> !lifelineBReceivedMessages.contains(message))
		            .collect(Collectors.toSet())
		    );

	    allMessagesNotBetweenLifelines.addAll(
		        lifelineBSentMessages.stream()
		            .filter(message -> !lifelineAReceivedMessages.contains(message))
		            .collect(Collectors.toSet())
		    );
		
		return allMessagesNotBetweenLifelines;
	}
	

}
