package carisma.check.sequencediagrammcrypto.test;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import carisma.check.sequencediagramcrypto.Check;
import carisma.core.analysis.StringParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CheckParameterDescriptor;

import java.util.logging.Logger;


public class SequencediagrammCryptoCheckTest {
	
	private static final Logger logger = Logger.getLogger(SequencediagrammCryptoCheckTest.class.getName());
	
    private String filepath = "resources/models";
	
	DummyHostSequence host = new DummyHostSequence(true);
	
	HashMap<String, CheckParameter> parameters = new HashMap<>();
	
	
	private void loadModel(String filepath, String name) {
		File file = new File(new File(filepath), name);
		if(file.exists()){
			Resource r = new ResourceSetImpl().createResource(URI.createURI(file.getAbsolutePath()));
			try(FileInputStream in = new FileInputStream(file)){
				r.load(in, Collections.EMPTY_MAP);
				EList<EObject> contents = r.getContents();
				assertTrue(1 <= contents.size());
				EObject obj = contents.get(0);
				assertTrue(obj instanceof Model);
			} catch (IOException e) {
				logger.warning("Error message: " + e.getMessage());
				}
			host.setAnalyzedModel(r);
		}
	}
	
	@Before
	public final void buildParameters(){
		this.parameters.put(Check.PARAMETER_REPORT_MITMA_KNOWLEDGE, new StringParameter(new CheckParameterDescriptor(null, null,
				null, null, false, null), "true"));
		this.parameters.put(Check.PARAMETER_INITIAL_KNOWLEDGE, new StringParameter(new CheckParameterDescriptor(null, null,
				null, null, false, null), "symenc(s,key), wissen, conc(geheim,wissen)"));
		this.parameters.put(Check.PARAMETER_KNOWLEDGE_TO_CHECK, new StringParameter(new CheckParameterDescriptor(null, null,
				null, null, false, null), "s"));
	}
	
	
	@After
	public final void cleanup() {
		this.host.unloadAnalyzedModel();
	}
	
	/*
	 * tests EXAMPLE
	 */
	@Test
	public final void testSequencediagrammCrypto1() throws IOException {
		loadModel(filepath, "EXAMPLE.uml");
		Check sequenceCheck = new Check();
		assertTrue(sequenceCheck.perform(parameters, host));
		List<AnalysisResultMessage> am = new ArrayList<AnalysisResultMessage>();
		AnalysisResultMessage am0 = new AnalysisResultMessage(StatusType.INFO, "%----------PHASE 1----------%");
		AnalysisResultMessage am1 = new AnalysisResultMessage(StatusType.INFO, "Objekt : model::Interaction1::Client");
		AnalysisResultMessage am2 = new AnalysisResultMessage(StatusType.INFO, "Term : (Knows(enc(conc(password,username),pub_server_key)) & Knows(symenc(data_uri,session_key)))");
		AnalysisResultMessage am3 = new AnalysisResultMessage(StatusType.INFO, "Term Evaluation: true");
		AnalysisResultMessage am4 = new AnalysisResultMessage(StatusType.WARNING, "MITMA might be able to impersonate Object model::Interaction1::Client");
		AnalysisResultMessage am5 = new AnalysisResultMessage(StatusType.INFO, "Objekt : model::Interaction1::Server");
		AnalysisResultMessage am6 = new AnalysisResultMessage(StatusType.INFO, "Term : (Knows(symenc(session_key,pub_client_key)) & Knows(symenc(data,session_key)))");
		AnalysisResultMessage am7 = new AnalysisResultMessage(StatusType.INFO, "Term Evaluation: true");
		AnalysisResultMessage am8 = new AnalysisResultMessage(StatusType.WARNING, "MITMA might be able to impersonate Object model::Interaction1::Server");
		AnalysisResultMessage am9 = new AnalysisResultMessage(StatusType.INFO, "%----------PHASE 2----------%");
		AnalysisResultMessage am10 = new AnalysisResultMessage(StatusType.INFO, "MITMA does not know s");
		am.add(am0);
		am.add(am1);
		am.add(am2);
		am.add(am3);
		am.add(am4);
		am.add(am5);
		am.add(am6);
		am.add(am7);
		am.add(am8);
		am.add(am9);
		am.add(am10);
		for(int i = 0; i<am.size(); i++){
			assertEquals(am.get(i).getText(), host.getResultMessage().get(i).getText());
			assertEquals(am.get(i).getStatus(), host.getResultMessage().get(i).getStatus());
		}
	}
	
	/*
	 * tests paper_example
	 */
	@Test
	public final void testSequencediagrammCrypto2() throws IOException {
		loadModel(filepath, "paper_example.uml");
		Check sequenceCheck = new Check();
		assertTrue(sequenceCheck.perform(parameters, host));
		List<AnalysisResultMessage> am = new ArrayList<AnalysisResultMessage>();
		AnalysisResultMessage am0 = new AnalysisResultMessage(StatusType.INFO, "%----------PHASE 1----------%");
		AnalysisResultMessage am1 = new AnalysisResultMessage(StatusType.INFO, "Objekt : model::Interaction1::A");
		AnalysisResultMessage am2 = new AnalysisResultMessage(StatusType.INFO, "Term : (knows(a) & knows(a) & knows(d))");
		AnalysisResultMessage am3 = new AnalysisResultMessage(StatusType.INFO, "Term Evaluation: true");
		AnalysisResultMessage am4 = new AnalysisResultMessage(StatusType.WARNING, "MITMA might be able to impersonate Object model::Interaction1::A");
		AnalysisResultMessage am5 = new AnalysisResultMessage(StatusType.INFO, "Objekt : model::Interaction1::B");
		AnalysisResultMessage am6 = new AnalysisResultMessage(StatusType.INFO, "Term : (knows(b) & knows(c))");
		AnalysisResultMessage am7 = new AnalysisResultMessage(StatusType.INFO, "Term Evaluation: true");
		AnalysisResultMessage am8 = new AnalysisResultMessage(StatusType.WARNING, "MITMA might be able to impersonate Object model::Interaction1::B");
		AnalysisResultMessage am9 = new AnalysisResultMessage(StatusType.INFO, "%----------PHASE 2----------%");
		AnalysisResultMessage am10 = new AnalysisResultMessage(StatusType.INFO, "MITMA does not know s");
		am.add(am0);
		am.add(am1);
		am.add(am2);
		am.add(am3);
		am.add(am4);
		am.add(am5);
		am.add(am6);
		am.add(am7);
		am.add(am8);
		am.add(am9);
		am.add(am10);
		for(int i = 0; i<am.size(); i++){
			assertEquals(am.get(i).getText(), host.getResultMessage().get(i).getText());
			assertEquals(am.get(i).getStatus(), host.getResultMessage().get(i).getStatus());
		}
	}
	
	
	/*
	 * tests SDExampleCitizenRegistry
	 */
	@Test
	public final void testSequencediagrammCrypto3() throws IOException {
		loadModel(filepath, "SDExampleCitizensRegistry.uml");
		Check sequenceCheck = new Check();
		assertTrue(sequenceCheck.perform(parameters, host));
		List<AnalysisResultMessage> am = new ArrayList<AnalysisResultMessage>();
		AnalysisResultMessage am0 = new AnalysisResultMessage(StatusType.INFO, "%----------PHASE 1----------%");
		AnalysisResultMessage am1 = new AnalysisResultMessage(StatusType.INFO, "Objekt : RootElement::Interaction1::CitizensRegistry");
		AnalysisResultMessage am2 = new AnalysisResultMessage(StatusType.INFO, "Term : (Knows(symenc(session_key,pub_client_key)) & Knows(symenc(data,session_key)))");
		AnalysisResultMessage am3 = new AnalysisResultMessage(StatusType.INFO, "Term Evaluation: true");
		AnalysisResultMessage am4 = new AnalysisResultMessage(StatusType.WARNING, "MITMA might be able to impersonate Object RootElement::Interaction1::CitizensRegistry");
		AnalysisResultMessage am5 = new AnalysisResultMessage(StatusType.INFO, "Objekt : RootElement::Interaction1::RegiteredCitizen");
		AnalysisResultMessage am6 = new AnalysisResultMessage(StatusType.INFO, "Term : (Knows(symenc(conc(password,username),pub_server_key)) & Knows(symenc(data_uri,session_key)))");
		AnalysisResultMessage am7 = new AnalysisResultMessage(StatusType.INFO, "Term Evaluation: true");
		AnalysisResultMessage am8 = new AnalysisResultMessage(StatusType.WARNING, "MITMA might be able to impersonate Object RootElement::Interaction1::RegiteredCitizen");
		AnalysisResultMessage am9 = new AnalysisResultMessage(StatusType.INFO, "%----------PHASE 2----------%");
		AnalysisResultMessage am10 = new AnalysisResultMessage(StatusType.INFO, "MITMA does not know s");
		am.add(am0);
		am.add(am1);
		am.add(am2);
		am.add(am3);
		am.add(am4);
		am.add(am5);
		am.add(am6);
		am.add(am7);
		am.add(am8);
		am.add(am9);
		am.add(am10);
		for(int i = 0; i<am.size(); i++){
			assertEquals(am.get(i).getText(), host.getResultMessage().get(i).getText());
			assertEquals(am.get(i).getStatus(), host.getResultMessage().get(i).getStatus());
		}
	}
	
	/*
	 * tests model
	 */
	@Test
	public final void testSequencediagrammCrypto4() throws IOException {
		loadModel(filepath, "model.uml");
		Check sequenceCheck = new Check();
		assertTrue(sequenceCheck.perform(parameters, host));
		List<AnalysisResultMessage> am = new ArrayList<AnalysisResultMessage>();
		AnalysisResultMessage am0 = new AnalysisResultMessage(StatusType.INFO, "%----------PHASE 1----------%");
		AnalysisResultMessage am1 = new AnalysisResultMessage(StatusType.INFO, "%----------PHASE 2----------%");
		am.add(am0);
		am.add(am1);
		for(int i = 0; i<am.size(); i++){
			assertEquals(am.get(i).getText(), host.getResultMessage().get(i).getText());
			assertEquals(am.get(i).getStatus(), host.getResultMessage().get(i).getStatus());
		}
	}
	
	/*
	 * tests null
	 */
	@Test
	public final void testSequencediagrammCrypto5() throws IOException {
		loadModel(filepath, "null.uml");
		Check sequenceCheck = new Check();
		assertFalse(sequenceCheck.perform(parameters, host));
		List<AnalysisResultMessage> am = new ArrayList<AnalysisResultMessage>();
		AnalysisResultMessage am0 = new AnalysisResultMessage(StatusType.WARNING, "Empty model");
		am.add(am0);
		for(int i = 0; i<am.size(); i++){
			assertEquals(am.get(i).getText(), host.getResultMessage().get(i).getText());
			assertEquals(am.get(i).getStatus(), host.getResultMessage().get(i).getStatus());
		}
		
	}

}
