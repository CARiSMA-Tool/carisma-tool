package carisma.check.extension4ids.datatransfer;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import carisma.check.extension4ids.dto.DataTransferProtocolDto;
import carisma.check.extension4ids.dto.RelevantMessagesDto;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.extension4ids.Extension4IDS;
import carisma.profile.umlsec.extension4ids.Extension4IDSUtil;

import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Interaction;
import org.eclipse.uml2.uml.Lifeline;
import org.eclipse.uml2.uml.Message;
import org.eclipse.uml2.uml.MessageOccurrenceSpecification;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;

public class DataTransferProtocol {
	
	private static final String PUSH = "push";
	private static final String PULL = "pull";
	
	private AnalysisHost analysisHost;
	
	public DataTransferProtocol(AnalysisHost host) {
		if(host!= null) {
			this.analysisHost = host;
		} else {
			this.analysisHost = new DummyHost(true);
		}
	}
	
	public boolean checkDataTransferProtocol(final Package pkg) {
		boolean checkSuccessful = true;
		 List<Interaction> interactions = UMLHelper.getAllElementsOfType(pkg, Interaction.class);
		 Set<Lifeline> lifelines = UMLSequenceHelper.getAllLifeLines(pkg);
		 Set<Message> allMessages = UMLSequenceHelper.getAllMessages(pkg);
		 Set<Lifeline> providers = DataTransferProtocolHelper.getAnnotatedLifeline(lifelines, Extension4IDS.PROVIDER);
		 Set<Lifeline> consumers = DataTransferProtocolHelper.getAnnotatedLifeline(lifelines,Extension4IDS.CONSUMER);
		
		 //Checks if datatransfer stereotype is present
		if(!DataTransferProtocolHelper.hasDataTransferStereotype(interactions)) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The model is not annotated with Data Transfer Stereotype!"));
			return false;
		}
		
		//checks if atmost one provider and consumer is present
		if(!hasValidConsumerAndProvider(providers, consumers)) {
			return false;
		}
		
		if(allMessages.isEmpty()) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The model doesnt have any messages"));
			return false;
		}
		
		Lifeline provider = providers.stream().findFirst().get();
		Lifeline consumer = consumers.stream().findFirst().get();
		Interaction interaction = interactions.stream().findFirst().get();
		DataTransferProtocolDto dataTransferProtocolDto = new DataTransferProtocolDto(provider, consumer);
		RelevantMessagesDto relevantMessagesDto = DataTransferProtocolHelper.getTaggedValues(interaction);	
		
		//checks if minimum combination of steps to make a data transfer is present
		if(!hasMinimumSteps(relevantMessagesDto)) {
			return false;
		}
		
		//checks if order of the requests is correct
		if(!hasCorrectOrderOfSteps(relevantMessagesDto, pkg)) {
			return false;
		}
		
		//checks if requests are to or from either provider or consumer
		if(!hasValidProtocolSteps(dataTransferProtocolDto, relevantMessagesDto)) {
			return false;
		}
		
		return checkSuccessful;
		
	}
	
	
	/**
	 * Validates the presence and counts of consumers and providers.
	 *
	 * @param provider The set of lifelines annotated as providers.
	 * @param consumer The set of lifelines annotated as consumers.
	 * @return true if there is at least one consumer and one provider, and no more than one of each; false otherwise.
	 */
	public boolean hasValidConsumerAndProvider(final Set<Lifeline> provider, final Set<Lifeline> consumer) {
		int consumerCount = consumer.size();
		int providerCount = provider.size();
		
		
		if (consumerCount == 0 && providerCount == 0) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Consumer and Provider not present!"));
	        return false;
	    }
	    if (consumerCount == 0) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Consumer not present!"));
	        return false;
	    }
	    if (providerCount == 0) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Provider not present!"));
	        return false;
	    }
	    if (consumerCount > 1 && providerCount > 1) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Too many consumers and providers!"));
	        return false;
	    }
	    if (consumerCount > 1) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Too many consumers!"));
	        return false;
	    }
	    if (providerCount > 1) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Too many providers!"));
	        return false;
	    }
	    return true;
		
	}
	
	/**
	 * Validates the presence of minimum required steps in the data transfer protocol.
	 *
	 * @param relevantMessagesDto The DTO containing relevant messages for the protocol.
	 * @return true if the minimum required steps are present and valid; false otherwise.
	 */
	public boolean hasMinimumSteps(final RelevantMessagesDto relevantMessagesDto) {    
	    boolean transferRequestPresent = relevantMessagesDto.getTransferRequest() != null;
	    boolean transferStartPresent = relevantMessagesDto.getTransferStart() != null;
	    boolean transferTerminatePresent = relevantMessagesDto.getTransferTerminate() != null;
	    boolean transferSuspendPresent = relevantMessagesDto.getTransferSuspend() != null;
	    boolean transferCompletePresent = relevantMessagesDto.getTransferComplete() != null;
	    boolean pushPullPresent = relevantMessagesDto.getPushPull() != null;
	    
	    if (!transferRequestPresent) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer request step not present!"));
	        return false;
	    }
	    
	    if (!transferStartPresent && !transferTerminatePresent) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Once transfer request is sent, there should be transfer start or terminate!"));
	        return false;
	    }

	    if (transferStartPresent) {
	        if (!transferSuspendPresent && !transferTerminatePresent && !pushPullPresent) {
	            this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "If transfer start is present, there should be either suspend, terminate, or push/pull!"));
	            return false;
	        }
	    }

	    if (pushPullPresent) {
	        if (!transferSuspendPresent && !transferTerminatePresent && !transferCompletePresent) {
	            this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "If push/pull is present, there should be either suspend, terminate, or complete!"));
	            return false;
	        }
	    }

	    if (transferSuspendPresent && !transferStartPresent) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "If transfer suspend is present, transfer start should be present!"));
	        return false;
	    }
	    
	    if(transferCompletePresent) {
	    	if(transferTerminatePresent) {
	    		this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "If transfer complete is present, transfer terminate should not be present!"));
		        return false;
	    	} else if(transferSuspendPresent) {
	    		this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "If transfer complete is present, transfer suspend should not be present!"));
		        return false;
	    	}
	    }

	    return true;
	}
	
	/**
	 * Validates the order of message steps in the data transfer protocol.
	 *
	 * @param relevantMessagesDto The DTO containing relevant messages for the protocol.
	 * @param pkg The UML package containing the message occurrences.
	 * @return true if the message steps are in the correct order; false otherwise.
	 */
	public boolean hasCorrectOrderOfSteps(final RelevantMessagesDto relevantMessagesDto, final Package pkg) {
		Set<MessageOccurrenceSpecification> sendOccurance = UMLSequenceHelper.getAllSendEvents(pkg);
		
		return checkOrder(relevantMessagesDto, sendOccurance);
	}
	
	/**
	 * Validates the steps in the data transfer protocol for correctness.
	 *
	 * @param dataTransferProtocolDto The DTO containing the data transfer protocol details.
	 * @param relevantMessagesDto The DTO containing relevant messages for the protocol.
	 * @return true if all protocol steps are valid; false otherwise.
	 */
	public boolean hasValidProtocolSteps(DataTransferProtocolDto dataTransferProtocolDto, RelevantMessagesDto relevantMessagesDto) {
		
		if (relevantMessagesDto.getTransferRequest()!= null && !isValidTransferRequestStep(dataTransferProtocolDto, relevantMessagesDto.getTransferRequest())) {
	        return false;
	    }

	    if (relevantMessagesDto.getTransferStart()!= null && !isValidTransferStartStep(dataTransferProtocolDto, relevantMessagesDto.getTransferStart())) {
	        return false;
	    }

	    if (relevantMessagesDto.getPushPull()!= null && !isValidPushPullStep(dataTransferProtocolDto, relevantMessagesDto.getPushPull(), relevantMessagesDto.getType())) {
	        return false;
	    }

	    if (relevantMessagesDto.getTransferComplete()!= null && !isValidTransferCompleteStep(dataTransferProtocolDto, relevantMessagesDto.getTransferComplete(), relevantMessagesDto.getType())) {
	        return false;
	    }

	    if (relevantMessagesDto.getTransferSuspend()!= null && !isValidSuspendOrTerminateStep(dataTransferProtocolDto, relevantMessagesDto.getTransferSuspend(), "Transfer Suspend")) {
	        return false;
	    }

	    if (relevantMessagesDto.getTransferTerminate()!= null && !isValidSuspendOrTerminateStep(dataTransferProtocolDto, relevantMessagesDto.getTransferTerminate(), "Transfer Terminate")) {
	        return false;
	    }

	    return true;
	}

	
	/**
	 * Validates the order of the relevant message steps in the data transfer protocol.
	 *
	 * @param dto The DTO containing relevant messages for the protocol.
	 * @param sendOccurrences The set of all send event occurrences in the package.
	 * @return true if the message steps are in the correct order; false otherwise.
	 */
	private boolean checkOrder(RelevantMessagesDto dto, Set<MessageOccurrenceSpecification> sendOccurrences) {

	    Optional<MessageOccurrenceSpecification> requestSend = dto.getTransferRequest() != null ? UMLSequenceHelper.getSendEvent(dto.getTransferRequest()) : Optional.empty();
	    Optional<MessageOccurrenceSpecification> startSend = dto.getTransferStart() != null ? UMLSequenceHelper.getSendEvent(dto.getTransferStart()) : Optional.empty();
	    Optional<MessageOccurrenceSpecification> completeSend = dto.getTransferComplete() != null ? UMLSequenceHelper.getSendEvent(dto.getTransferComplete()) : Optional.empty();
	    Optional<MessageOccurrenceSpecification> terminateSend = dto.getTransferTerminate() != null ? UMLSequenceHelper.getSendEvent(dto.getTransferTerminate()) : Optional.empty();
	    Optional<MessageOccurrenceSpecification> suspendSend = dto.getTransferSuspend() != null ? UMLSequenceHelper.getSendEvent(dto.getTransferSuspend()) : Optional.empty();
	    Optional<MessageOccurrenceSpecification> pushPullSend = dto.getPushPull() != null ? UMLSequenceHelper.getSendEvent(dto.getPushPull()) : Optional.empty();

	    // Check order: request -> start -> pushPull -> complete
	    if (!isOrdered(requestSend, startSend, sendOccurrences) || !isOrdered(startSend, pushPullSend, sendOccurrences) || !isOrdered(pushPullSend, completeSend, sendOccurrences)) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Order violation: Transfer Start should be after Transfer Request, Push/Pull should be after Transfer Start, and Transfer Complete should be after Push/Pull"));
	        return false;
	    }

	    // Terminate can be after request, start, or suspend
	    if (terminateSend.isPresent() && !(isOrdered(requestSend, terminateSend, sendOccurrences) || isOrdered(startSend, terminateSend, sendOccurrences) || isOrdered(suspendSend, terminateSend, sendOccurrences))) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer Terminate should be after  Transfer Request, Transfer Start, or Transfer Suspend"));
	        return false;
	    }

	    // Suspend and start must have correct order between them
	    if (suspendSend.isPresent() && startSend.isPresent() && !(isOrdered(startSend, suspendSend, sendOccurrences) || isOrdered(suspendSend, startSend, sendOccurrences))) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer Suspend and Transfer Start should have correct order between them"));
	        return false;
	    }

	    return true;
	}
	
	/**
	 * Checks if the first message occurrence is ordered before the second in the given set of occurrences.
	 *
	 * @param first The first message occurrence.
	 * @param second The second message occurrence.
	 * @param occurrences The set of all message occurrences.
	 * @return true if the first message occurrence is ordered before the second; false otherwise.
	 */
	private boolean isOrdered(Optional<MessageOccurrenceSpecification> first, Optional<MessageOccurrenceSpecification> second, Set<MessageOccurrenceSpecification> occurrences) {
	    if (!first.isPresent() || !second.isPresent()) {
	    	this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, first.get().getName() + " or " + second.get().getName() + "step missing"));
	        return false; // If any of the events are missing, we assume no violation
	    }
	    System.out.println("First: "+ first.get().getName() + "index: " + occurrences.stream().toList().indexOf(first.get()));
	    System.out.println("Second: "+ second.get().getName() + "index: " + occurrences.stream().toList().indexOf(second.get()));
	    return occurrences.stream().toList().indexOf(first.get()) < occurrences.stream().toList().indexOf(second.get());
	}
	
	public boolean isValidTransferRequestStep(final DataTransferProtocolDto dataTransferProtocolDto, final Message message) {
	    Lifeline provider = dataTransferProtocolDto.getProvider();
	    Lifeline consumer = dataTransferProtocolDto.getConsumer();

	    if (message == null) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer Request Step is empty"));
	        return false;
	    }

	    Map<String, Lifeline> sendReceiveLifelines = UMLSequenceHelper.getSendAndRecieveLifeline(message);

	    if (!consumer.equals(sendReceiveLifelines.get("sendLifeline"))) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer Request must be sent by consumer"));
	        return false;
	    }

	    if (!provider.equals(sendReceiveLifelines.get("receiveLifeline"))) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer Request must be received by provider"));
	        return false;
	    }

	    return true;
	}

	public boolean isValidTransferStartStep(final DataTransferProtocolDto dataTransferProtocolDto, final Message message) {
	    Lifeline provider = dataTransferProtocolDto.getProvider();
	    Lifeline consumer = dataTransferProtocolDto.getConsumer();

	    if (message == null) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer Start Step is empty"));
	        return false;
	    }

	    Map<String, Lifeline> sendReceiveLifelines = UMLSequenceHelper.getSendAndRecieveLifeline(message);

	    if (!provider.equals(sendReceiveLifelines.get("sendLifeline"))) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer Start must be sent by provider"));
	        return false;
	    }

	    if (!consumer.equals(sendReceiveLifelines.get("receiveLifeline"))) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer Start must be received by consumer"));
	        return false;
	    }

	    return true;
	}

	public boolean isValidPushPullStep(final DataTransferProtocolDto dataTransferProtocolDto, final Message message, final String type) {
	    Lifeline provider = dataTransferProtocolDto.getProvider();
	    Lifeline consumer = dataTransferProtocolDto.getConsumer();

	    if (message == null) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Push/Pull Step is empty"));
	        return false;
	    }

	    Map<String, Lifeline> sendReceiveLifelines = UMLSequenceHelper.getSendAndRecieveLifeline(message);

	    if (PUSH.equalsIgnoreCase(type)) {
	        if (!provider.equals(sendReceiveLifelines.get("sendLifeline"))) {
	            this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Push should be sent by provider"));
	            return false;
	        }
	        if (!consumer.equals(sendReceiveLifelines.get("receiveLifeline"))) {
	            this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Push should be received by consumer"));
	            return false;
	        }
	    } else if (PULL.equalsIgnoreCase(type)) {
	        if (!consumer.equals(sendReceiveLifelines.get("sendLifeline"))) {
	            this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Pull should be sent by consumer"));
	            return false;
	        }
	        if (!provider.equals(sendReceiveLifelines.get("receiveLifeline"))) {
	            this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Pull should be received by provider"));
	            return false;
	        }
	    } else {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Invalid type for Push/Pull"));
	        return false;
	    }

	    return true;
	}

	public boolean isValidTransferCompleteStep(final DataTransferProtocolDto dataTransferProtocolDto, final Message message, final String type) {
		Lifeline provider = dataTransferProtocolDto.getProvider();
	    Lifeline consumer = dataTransferProtocolDto.getConsumer();
		
		if (message == null) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer Complete Step is empty"));
	        return false;
	    }
	    
	    Map<String, Lifeline> sendReceiveLifelines = UMLSequenceHelper.getSendAndRecieveLifeline(message);

	    if (PUSH.equalsIgnoreCase(type)) {
	        if (!provider.equals(sendReceiveLifelines.get("sendLifeline"))) {
	            this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer complete should be sent by provider"));
	            return false;
	        }
	        if (!consumer.equals(sendReceiveLifelines.get("receiveLifeline"))) {
	            this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer complete should be received by consumer"));
	            return false;
	        }
	    } else if (PULL.equalsIgnoreCase(type)) {
	        if (!(provider.equals(sendReceiveLifelines.get("sendLifeline")) || consumer.equals(sendReceiveLifelines.get("sendLifeline")))) {
	            this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer complete should be sent by either consumer or provider"));
	            return false;
	        }
	        if (!(provider.equals(sendReceiveLifelines.get("receiveLifeline")) || provider.equals(sendReceiveLifelines.get("receiveLifeline")))) {
	            this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Transfer complete should be sent by either consumer or provider"));
	            return false;
	        }
	    } else {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Invalid type for Push/Pull"));
	        return false;
	    }

	    return true;
	}

	public boolean isValidSuspendOrTerminateStep(final DataTransferProtocolDto dataTransferProtocolDto, final Message message, final String stepName) {
		Lifeline provider = dataTransferProtocolDto.getProvider();
	    Lifeline consumer = dataTransferProtocolDto.getConsumer();
	    
	    if (message == null) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Push/Pull Step is empty"));
	        return false;
	    }


	    Map<String, Lifeline> sendReceiveLifelines = UMLSequenceHelper.getSendAndRecieveLifeline(message);

	    if (!provider.equals(sendReceiveLifelines.get("sendLifeline")) || !provider.equals(sendReceiveLifelines.get("receiveLifeline"))) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, stepName + "must be sent either by provider or consumer"));
	        return false;
	    }

	    if (!consumer.equals(sendReceiveLifelines.get("receiveLifeline")) || !consumer.equals(sendReceiveLifelines.get("sendLifeline"))) {
	        this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, stepName + "must be sent either by provider or consumer"));
	        return false;
	    }

	    return true;
	}

}
