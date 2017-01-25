package carisma.check.sequencediagramcrypto;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Interaction;
import org.eclipse.uml2.uml.Lifeline;
import org.eclipse.uml2.uml.MessageOccurrenceSpecification;
import org.eclipse.uml2.uml.MessageSort;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.ValueSpecification;

import carisma.check.sequencediagramcrypto.data.KnowledgeBase;
import carisma.check.sequencediagramcrypto.exception.InvalidSyntaxException;
import carisma.check.sequencediagramcrypto.exception.UnexpectedStringEndException;
import carisma.check.sequencediagramcrypto.filter.And;
import carisma.check.sequencediagramcrypto.filter.Equal;
import carisma.check.sequencediagramcrypto.filter.Filter;
import carisma.check.sequencediagramcrypto.filter.message.MessageAfter;
import carisma.check.sequencediagramcrypto.filter.message.MessageBefore;
import carisma.check.sequencediagramcrypto.filter.message.SelectSource;
import carisma.check.sequencediagramcrypto.filter.message.SelectTarget;
import carisma.check.sequencediagramcrypto.fol.FOLTerm;
import carisma.check.sequencediagramcrypto.fol.MessageTerm;
import carisma.check.sequencediagramcrypto.fol.TermGenerator;
import carisma.check.sequencediagramcrypto.knowledge.Knowledge;
import carisma.check.sequencediagramcrypto.knowledge.MessageKnowledge;
import carisma.check.sequencediagramcrypto.message.Argument;
import carisma.check.sequencediagramcrypto.message.ArgumentImp;
import carisma.check.sequencediagramcrypto.message.AsymmetricMessage;
import carisma.check.sequencediagramcrypto.message.Guard;
import carisma.check.sequencediagramcrypto.message.GuardImp;
import carisma.check.sequencediagramcrypto.message.Message;
import carisma.check.sequencediagramcrypto.message.Messages;
import carisma.check.sequencediagramcrypto.message.Peer;
import carisma.check.sequencediagramcrypto.message.PeerImp;
import carisma.check.sequencediagramcrypto.message.SymmetricMessage;
import carisma.check.sequencediagramcrypto.parser.InitialKnowledgeParser;
import carisma.check.sequencediagramcrypto.parser.KnowledgeToCheckParser;
import carisma.check.sequencediagramcrypto.parser.SyntaxParser;
import carisma.check.sequencediagramcrypto.replacewithjava8.Optional;
import carisma.check.sequencediagramcrypto.report.AttackerDoesNotKnowReport;
import carisma.check.sequencediagramcrypto.report.AttackerKnowReport;
import carisma.check.sequencediagramcrypto.report.AttackerTermReport;
import carisma.check.sequencediagramcrypto.report.CheckReporter;
import carisma.check.sequencediagramcrypto.report.ExceptionReport;
import carisma.check.sequencediagramcrypto.report.KnowledgeReport;
import carisma.check.sequencediagramcrypto.report.PeerTermReport;
import carisma.check.sequencediagramcrypto.report.Reporter;
import carisma.check.sequencediagramcrypto.report.StringReport;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.StringParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
//import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;

/**
 * Contains a Simple CARiSMA Check which returns all elements of a given Model.
 *
 */

public class Check implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.sequencediagramcrypto";
	public static final String CHECK_NAME = "Sequence Diagram Crypto Analyzer";
	
	public static String	PARAMETER_INITIAL_KNOWLEDGE			= "carisma.check.sequencediagramcrypto.initialknowledge";
	public static String	PARAMETER_REPORT_MITMA_KNOWLEDGE	= "carisma.check.sequencediagramcrypto.reportkbase";
	public static String	PARAMETER_KNOWLEDGE_TO_CHECK		= "carisma.check.sequencediagramcrypto.possibletoknow";
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Package) {
			Package model = (Package) currentModel.getContents().get(0);
			// --- Start Here Own Check
			
			Reporter reporter = new CheckReporter(host);
			KnowledgeBase attackerKnowledgeBase = new KnowledgeBase();
			
			Messages messages;
			try {
				messages = this.readAllMessagesFromModel(model, host, parameters);
			}
			catch (InvalidSyntaxException e) {
				reporter.report(new ExceptionReport(e));
				return false;
			}
			catch (UnexpectedStringEndException e) {
				reporter.report(new ExceptionReport(e));
				return false;
			}
			
			final Set<Peer> peers = this.readAllPeersFromModel(model, host, parameters);
			
			reporter.report(new StringReport("%----------PHASE 1----------%"));
			boolean phase1Result;
			try {
				phase1Result = this.phase1(model, host, parameters, peers, messages, reporter, attackerKnowledgeBase);
			}
			catch (InvalidSyntaxException e) {
				reporter.report(new ExceptionReport(e));
				phase1Result = false;
			}
			catch (UnexpectedStringEndException e) {
				reporter.report(new ExceptionReport(e));
				phase1Result = false;
			}
			
			reporter.report(new StringReport("%----------PHASE 2----------%"));
			boolean phase2Result;
			try {
				phase2Result = this.phase2(model, host, parameters, peers, messages, reporter, attackerKnowledgeBase);
			}
			catch (InvalidSyntaxException e) {
				reporter.report(new ExceptionReport(e));					
				phase2Result = false;
			}
			catch (UnexpectedStringEndException e) {
				reporter.report(new ExceptionReport(e));
				phase2Result = false;
			}
				
			return phase1Result && phase2Result;
			
			// --- End Here Own Check
		}
		host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING,	"Content is not a model!"));
		return false;
	}
	
	private Messages readAllMessagesFromModel(Package model, AnalysisHost host,
			Map<String, CheckParameter> parameters) throws InvalidSyntaxException,
			UnexpectedStringEndException {
		Messages messages = new Messages();
		{
			int msgNo = 0;
			for (Element element : model.allOwnedElements()) {
				
				// Filter all non MCS elements and null MCS elements
				if (element != null && element instanceof MessageOccurrenceSpecification) {
					MessageOccurrenceSpecification senderMsg = (MessageOccurrenceSpecification) element;
					
					// Filter all null message elements
					if (senderMsg.getMessage() != null) {
						
						MessageOccurrenceSpecification receiverMsg;
						receiverMsg = (MessageOccurrenceSpecification) senderMsg.getMessage()
								.getReceiveEvent();
						
						// receiverMsg must exists and cover at least one other
						// Element
						if (receiverMsg != null && receiverMsg.getCovereds().size() > 0) {
							
							String sourceName = senderMsg.getCovereds().get(0).getQualifiedName();
							Peer source = new PeerImp(sourceName);
							String targetName = receiverMsg.getCovereds().get(0).getQualifiedName();
							Peer target = new PeerImp(targetName);
							Guard guard = new GuardImp();
							// TODO: Read Guard Information!
							
							MessageSort msgSort = senderMsg.getMessage().getMessageSort();
							String name = senderMsg.getMessage().getQualifiedName();
							
							List<Argument> args = new ArrayList<Argument>();
							for (ValueSpecification arg : senderMsg.getMessage().getArguments()) {
								Argument sdmArg = new ArgumentImp(arg.getName().trim());
								args.add(sdmArg);
							}
							
							Message newMsg;
							if (msgSort.getValue() == MessageSort.SYNCH_CALL) {
								newMsg = new SymmetricMessage(name, args, source, target, msgNo,
										guard);
							}
							else {
								newMsg = new AsymmetricMessage(name, args, source, target, msgNo,
										guard);
							}
							msgNo += 1;
							
							// Only add Messages between two peers and don't add
							// messages which are already in messages
							if (!source.equals(target) && !messages.contains(newMsg)) {
								messages.add(newMsg);
							}
						}
					}
				}
			}
			
		}
		return messages;
	}
	
	private Set<Peer> readAllPeersFromModel(Package model, AnalysisHost host,
			Map<String, CheckParameter> parameters) {
		Set<Peer> peers = new HashSet<Peer>();
		{
			for (Element element : model.allOwnedElements()) {
				if (element instanceof Interaction) {
					for (Lifeline lifeline : ((Interaction) element).getLifelines()) {
						Peer peer;
						{
							String peerName = lifeline.getQualifiedName();
							peer = new PeerImp(peerName);
						}
						peers.add(peer);
					}
				}
				
			}
		}
		return Collections.unmodifiableSet(peers);
	}
	
	private boolean phase1(Package model, AnalysisHost host,
			Map<String, CheckParameter> parameters, Set<Peer> peers, Messages messages,
			Reporter reporter, KnowledgeBase attackerKnowledgeBase) throws InvalidSyntaxException, UnexpectedStringEndException {
		
		// Build MITM Attackers Knowledge
		{
			// Initial knowledge
			Optional<String> initialKnowledge = this.getStringParameter(parameters,
					Check.PARAMETER_INITIAL_KNOWLEDGE);
			if (initialKnowledge.isPresent()) {
				List<Knowledge> initialKnowledgeList = null;
				initialKnowledgeList = InitialKnowledgeParser.Parse(initialKnowledge.get());
				for (Knowledge knowledge : initialKnowledgeList) {
					attackerKnowledgeBase.add(knowledge);
				}
			}
			
			for (Message message : messages) {
				String messageName = message.name();
				
				for (int i = 0; i < message.arguments().size(); i++) {
					// Knowledge the attacker gains though observing the
					// communication
					Knowledge k1 = new MessageKnowledge(messageName, i);
					attackerKnowledgeBase.add(k1);
					// Knowledge the attacker might guess while attacking
					String arg = message.arguments().get(i).value();
					Knowledge k2 = SyntaxParser.parseVal(new StringBuilder(arg));
					attackerKnowledgeBase.add(k2);
					// Attacker guess both are equal
					attackerKnowledgeBase.setSame(k1, k2);
				}
			}
		}
		
		// Conclude the current attackers Knowledge
		int lastSize;
		do {
			lastSize = attackerKnowledgeBase.size();
			attackerKnowledgeBase.conclusion();
		} while (lastSize < attackerKnowledgeBase.size());
		
		if (this.getBooleanParameter(parameters, Check.PARAMETER_REPORT_MITMA_KNOWLEDGE)
				.isPresent()) {
			if (this.getBooleanParameter(parameters, Check.PARAMETER_REPORT_MITMA_KNOWLEDGE).get()) {
				reporter.report(new KnowledgeReport(attackerKnowledgeBase));
			}
		}
		
		boolean result = true;
		for (Peer peer : peers) {
			
			// Receives(peer)
			Filter<Message> filterTargetMessages = new SelectTarget(new Equal<Peer>(peer));
			Messages receivedMessages = messages.filter(filterTargetMessages);
			
			// Sends(peer)
			Filter<Message> filterSourceMessages = new SelectSource(new Equal<Peer>(peer));
			Messages sendMessages = messages.filter(filterSourceMessages);
			
			// Messages(peer)
			Messages peerMessages = new Messages();
			{
				peerMessages.addAll(receivedMessages);
				peerMessages.addAll(sendMessages);
			}
			
			List<MessageTerm> messageTerms = new ArrayList<MessageTerm>(sendMessages.size());
			for (Message message : sendMessages) {
				
				Messages recievedInBetweenMessages = new Messages();
				Optional<Message> prevSendMessage = sendMessages.messageBefor(message);
				// in case a message was send previously, get all received
				// messages between those two messages
				if (prevSendMessage.isPresent()) {
					Filter<Message> inBetweenFilter = new And<Message>(new MessageAfter(
							prevSendMessage.get()), new MessageBefore(message));
					recievedInBetweenMessages = receivedMessages.filter(inBetweenFilter);
				}
				
				// Generate Message Term
				// (ReceivedMessagesTerm,GuardTerm,SendMessagesTerm)
				MessageTerm messageTerm = TermGenerator.MessageFOLTerm(recievedInBetweenMessages,
						message);
				messageTerms.add(messageTerm);
				
			}// for(Message message : sendMessages) END
			
			// Generate from list of MessageTerms the peerTerm
			FOLTerm peerTerm;
			if (peerMessages.anyAsymmetric()) {
				peerTerm = TermGenerator.AsymmetricPeerTerm(messageTerms);
			}
			else {
				peerTerm = TermGenerator.SymmetricPeerTerm(messageTerms);
			}
			
			// Check whether or not a MITM attacker is able to impersonate peer
			boolean termEvaluation = peerTerm.evaluate(attackerKnowledgeBase);
			
			// Report the result
			reporter.report(new PeerTermReport(peer, peerTerm, termEvaluation));
			if (termEvaluation) {
				String reportMsg = "MITMA might be able to impersonate Object " + peer.name();
				reporter.report(new StringReport(reportMsg, StatusType.WARNING));
			}
			result = result && termEvaluation;
		}// for(Peer peer : peers) END
		
		return result;
		
	}
	
	private boolean phase2(Package model, AnalysisHost host,
			Map<String, CheckParameter> parameters, Set<Peer> peers, Messages messages,
			Reporter reporter, KnowledgeBase attackerKnowledgeBase) throws InvalidSyntaxException, UnexpectedStringEndException {
		boolean result = true;
		Optional<String> knowledgeToCheckString = this.getStringParameter(parameters,
				Check.PARAMETER_KNOWLEDGE_TO_CHECK);
		if (knowledgeToCheckString.isPresent()) {
			
			List<Object> knowledgeToCheck = KnowledgeToCheckParser.Parse(knowledgeToCheckString.get());
			
			for (Object o : knowledgeToCheck) {
				if (o instanceof Knowledge) {
					Knowledge know = (Knowledge) o;
					if (attackerKnowledgeBase.knows(know)) {
						reporter.report(new AttackerKnowReport(know));
						result = false;
					}
					else {
						reporter.report(new AttackerDoesNotKnowReport(know));
					}
				}
				if (o instanceof FOLTerm) {
					FOLTerm term = (FOLTerm) o;
					boolean eval = term.evaluate(attackerKnowledgeBase);
					reporter.report(new AttackerTermReport(term, eval));
				}
			}
		}
		return result;
	}
	
	private Optional<Boolean> getBooleanParameter(Map<String, CheckParameter> parameters,
			String parameterName) {
		CheckParameter paramter = parameters.get(parameterName);
		if (paramter == null || !(paramter instanceof BooleanParameter)) {
			return Optional.empty();
		}
		Boolean value = ((BooleanParameter) parameters.get(parameterName)).getValue();
		return Optional.of(value);
	}
	
	private Optional<String> getStringParameter(Map<String, CheckParameter> parameters,
			String parameterName) {
		CheckParameter paramter = parameters.get(parameterName);
		if (paramter == null || !(paramter instanceof StringParameter)) {
			return Optional.empty();
		}
		String value = ((StringParameter) parameters.get(parameterName)).getValue();
		return Optional.of(value);
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
