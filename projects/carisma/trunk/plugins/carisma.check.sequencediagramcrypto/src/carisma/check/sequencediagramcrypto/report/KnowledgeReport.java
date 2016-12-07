package carisma.check.sequencediagramcrypto.report;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import carisma.check.sequencediagramcrypto.knowledge.Knowledge;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;

public class KnowledgeReport implements Report {
	
	final private static int LINE_WRAP = 80;
	final private static String LINE_SEPARATOR = System.getProperty("line.separator");
	final private Set<Knowledge>	knows;
	
	public KnowledgeReport(Set<Knowledge> knows) {
		this.knows = knows;
	}
	
	@Override
	public List<AnalysisResultMessage> getAnalysisResultMessages() {
		List<AnalysisResultMessage> list = new ArrayList<AnalysisResultMessage>();
		{
			StringBuilder strBuilder = new StringBuilder(this.knows.size()*4 + 2);
			{
				int charCounter = 0;
				strBuilder.append("[MITM Knowledge] {");
				for (Iterator<Knowledge> iter = this.knows.iterator(); iter.hasNext();) {
					Knowledge know = iter.next();
					String appendString = know.toString();
					strBuilder.append(appendString);
					if (iter.hasNext()) {
						strBuilder.append(',');
						charCounter += appendString.length();
						if(charCounter > KnowledgeReport.LINE_WRAP){
							strBuilder.append(KnowledgeReport.LINE_SEPARATOR);
							strBuilder.append("                        ");
							charCounter = 0 ;
						}
					}
					
				}
				strBuilder.append('}');
			}
			String message = strBuilder.toString();
			list.add(new AnalysisResultMessage(StatusType.INFO, message));
		}
		return list;
	}
	
}
