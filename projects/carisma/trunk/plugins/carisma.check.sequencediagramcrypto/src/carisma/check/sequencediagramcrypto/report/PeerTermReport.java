package carisma.check.sequencediagramcrypto.report;

import java.util.ArrayList;
import java.util.List;

import carisma.check.sequencediagramcrypto.fol.FOLTerm;
import carisma.check.sequencediagramcrypto.message.Peer;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;

public class PeerTermReport implements Report {
	
	// TODO : Better Formating?
	final private static String	MSG_TEXT_1	= "Objekt : %s";
	final private static String	MSG_TEXT_2	= "Term : %s";
	final private static String	MSG_TEXT_3	= "Term Evaluation: %b";
	
	private final Peer			peer;
	private final FOLTerm		peerTerm;
	private boolean				termEvaluation;
	
	public PeerTermReport(Peer peer, FOLTerm peerTerm, boolean termEvaluation) {
		this.peer = peer;
		this.peerTerm = peerTerm;
		this.termEvaluation = termEvaluation;
	}
	
	@Override
	public List<AnalysisResultMessage> getAnalysisResultMessages() {
		List<AnalysisResultMessage> list = new ArrayList<AnalysisResultMessage>();
		{
			String message_1 = String.format(PeerTermReport.MSG_TEXT_1, this.peer.name());
			String message_2 = String.format(PeerTermReport.MSG_TEXT_2, this.peerTerm.toString());
			String message_3 = String.format(PeerTermReport.MSG_TEXT_3, this.termEvaluation);
			
			list.add(new AnalysisResultMessage(StatusType.INFO, message_1));
			list.add(new AnalysisResultMessage(StatusType.INFO, message_2));
			list.add(new AnalysisResultMessage(StatusType.INFO, message_3));
		}
		return list;
	}
	
}
