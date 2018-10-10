package carisma.check.sequencediagrammcrypto.test;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;

public class DummyHostSequence extends DummyHost{

	private Resource analyzedModel;
	
	private List<AnalysisResultMessage> resultMessage;
	
	public DummyHostSequence(boolean printToSystemOut) {
		super(printToSystemOut);
		this.resultMessage = new ArrayList<AnalysisResultMessage>();
	}
	
	@Override
	public Resource getAnalyzedModel() {
		return this.analyzedModel;
	}
	
	public void setAnalyzedModel(Resource model){
		this.analyzedModel = model;
	}
	
	public void unloadAnalyzedModel(){
		this.analyzedModel.unload();
	}

	public List<AnalysisResultMessage> getResultMessage() {
		return resultMessage;
	}

	public void setResultMessage(List<AnalysisResultMessage> resultMessage) {
		this.resultMessage = resultMessage;
	}
	
	@Override
	public void addResultMessage(AnalysisResultMessage resultMessage) {
		this.resultMessage.add(resultMessage);
	}
	
	

}
