package carisma.check.ids.tests;

import java.io.File;

import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.UserAbortedAnalysisException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

public class TestHost implements AnalysisHost {

    public Resource modelres;

    public TestHost(Resource modelres) {
        this.modelres = modelres;
    }
 

	@Override
    public void addResultMessage(final AnalysisResultMessage detail) {
        Logger.log(LogLevel.INFO, detail.getText());
    }

    @Override
    public void appendToReport(final String text) {
        Logger.log(LogLevel.INFO, text);			
    }

    @Override
    public void appendLineToReport(final String text) {
        Logger.log(LogLevel.INFO, text);			
    }

    @Override
    public Resource getAnalyzedModel() {
        return this.modelres;
    }

    @Override
    public String getCurrentModelFilename() {
        return this.modelres.getURI().toFileString();
    }

    @Override
    public void putToRegister(final String registerName, final Object data)
            throws RegisterInUseException {
        // TODO Auto-generated method stub
        
    }

    @Override
    public boolean isRegisterInUse(final String registerName) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Object getFromRegister(final String registerName)
            throws RegisterNotInUseException {
        return null;
    }

    @Override
    public Object removeFromRegister(final String registerName)
            throws RegisterNotInUseException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void displayError(final String message) {
        // TODO Auto-generated method stub
        Logger.log(LogLevel.INFO, message);
    }

    @Override
    public File getFileToBeWritten(final File file)
            throws UserAbortedAnalysisException {
        // TODO Auto-generated method stub
        return file;
    }
}

