package carisma.check.rabac;

import java.io.File;

import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.UserAbortedAnalysisException;
import carisma.core.analysis.result.AnalysisResultMessage;

public class TestHost implements AnalysisHost {
	
	private final RABACTest rabacTest;

	public TestHost(RABACTest rabacTest) {
		this.rabacTest = rabacTest;
	}

	@Override
	public void addResultMessage(AnalysisResultMessage detail) {
		System.out.println(detail.getText());
	}

	@Override
	public void appendToReport(String text) {
	}

	@Override
	public void appendLineToReport(String text) {
		this.rabacTest.getReport().append(text);
	}

	@Override
	public Resource getAnalyzedModel() {
		return this.rabacTest.getModel();
	}

	@Override
	public String getCurrentModelFilename() {
		return "";
	}

	@Override
	public void putToRegister(String registerName, Object data) throws RegisterInUseException {
	}

	@Override
	public boolean isRegisterInUse(String registerName) {
		return false;
	}

	@Override
	public Object getFromRegister(String registerName) throws RegisterNotInUseException {
		return null;
	}

	@Override
	public Object removeFromRegister(String registerName) throws RegisterNotInUseException {
		return null;
	}

	@Override
	public void displayError(String message) {
	}

	@Override
	public File getFileToBeWritten(File file) throws UserAbortedAnalysisException {
		return file;
	}
}