package carisma.check.extension4ids.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Interaction;
import org.eclipse.uml2.uml.Lifeline;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import carisma.check.extension4ids.dto.DataTransferProtocolDto;
import carisma.check.extension4ids.dto.RelevantMessagesDto;
import carisma.check.extension4ids.transferprocessprotocol.TransferProcessProtocol;
import carisma.check.extension4ids.transferprocessprotocol.TransferProcessProtocolCheck;
import carisma.check.extension4ids.transferprocessprotocol.TransferProcessProtocolHelper;
import carisma.check.extension4ids.transferprocessprotocol.UMLSequenceHelper;
import carisma.check.extension4ids.usagecontrol.UsageControlCheck;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.extension4ids.Extension4IDS;
import carisma.profile.umlsec.extension4ids.Extension4IDSUtil;

public class TransferProcessProtocolTests {
    private String filepath = "resources/transfer_process_protocol";
    
    private ResourceSet rs = new ResourceSetImpl();
    
    private Resource modelres = null;
    
    private Model model = null;
    
    private AnalysisHost analysisHost;
    private TransferProcessProtocol transferProcessProtocol;

    public final void loadModel(final String testmodelname) throws IOException {
        File testmodelfile = new File(this.filepath + File.separator + testmodelname);
        assertTrue(testmodelfile.exists());
        this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
        this.modelres.load(Collections.EMPTY_MAP);
        this.model = (Model) this.modelres.getContents().get(0);
    }
    
 
    @Test
    public final void testNoDataTransferStereotype() throws IOException {
        loadModel("transfer_process_protocol_no_stereotype.uml");
        TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();

		List<Element> interactions = Extension4IDSUtil.getStereotypedElements(model, Extension4IDS.DATATRANSFER);
        assertEquals(0,interactions.size());

        TestHost analysisHost = new TestHost(this.modelres);
        assertFalse(check.perform(null, analysisHost));

        this.modelres.unload();
    }

    @Test
    public final void testNoProvider() throws IOException {
        loadModel("transfer_process_protocol_no_provider.uml");
        TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();
        List<Element> providers = Extension4IDSUtil.getStereotypedElements(this.model, Extension4IDS.PROVIDER);
        List<Element> consumers = Extension4IDSUtil.getStereotypedElements(this.model, Extension4IDS.CONSUMER);
        assertEquals(0, providers.size());
        assertEquals(1, consumers.size());

        TestHost analysisHost = new TestHost(this.modelres);
        assertFalse(check.perform(null, analysisHost)); 

        this.modelres.unload();
    }

    @Test
    public final void testNoConsumer() throws IOException {
        loadModel("transfer_process_protocol_no_consumer.uml");
        TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();
        List<Element> providers = Extension4IDSUtil.getStereotypedElements(this.model, Extension4IDS.PROVIDER);
        List<Element> consumers = Extension4IDSUtil.getStereotypedElements(this.model, Extension4IDS.CONSUMER);
        assertEquals(1, providers.size());
        assertEquals(0, consumers.size());

        TestHost analysisHost = new TestHost(this.modelres);
        assertFalse(check.perform(null, analysisHost));

        this.modelres.unload();
    }

    @Test
    public final void testTooManyProviders() throws IOException {
        loadModel("transfer_process_protocol_too_many_providers.uml");
        TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();
        List<Element> providers = Extension4IDSUtil.getStereotypedElements(this.model, Extension4IDS.PROVIDER);
        List<Element> consumers = Extension4IDSUtil.getStereotypedElements(this.model, Extension4IDS.CONSUMER);
        assertTrue(providers.size() > 1);
        assertEquals(1, consumers.size());

        TestHost analysisHost = new TestHost(this.modelres);
        assertFalse(check.perform(null, analysisHost));

        this.modelres.unload();
    }

    @Test
    public final void testTooManyConsumers() throws IOException {
        loadModel("transfer_process_protocol_too_many_consumers.uml");
        TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();
        List<Element> providers = Extension4IDSUtil.getStereotypedElements(this.model, Extension4IDS.PROVIDER);
        List<Element> consumers = Extension4IDSUtil.getStereotypedElements(this.model, Extension4IDS.CONSUMER);
        assertEquals(1, providers.size());
        assertTrue(consumers.size() > 1);

        TestHost analysisHost = new TestHost(this.modelres);
        assertFalse(check.perform(null, analysisHost)); // Ensure the perform method returns false

        this.modelres.unload();
    }

    @Test
    public final void testNoTransferRequest() throws IOException {
        loadModel("transfer_process_protocol_no_transfer_request.uml");
        TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();
        List<Interaction> interactions = UMLHelper.getAllElementsOfType(this.model, Interaction.class);
        Interaction interaction = interactions.stream().findFirst().get();
        RelevantMessagesDto relevantMessagesDto = TransferProcessProtocolHelper.getTaggedValues(interaction);
        assertNull(relevantMessagesDto.getTransferRequest());

        TestHost analysisHost = new TestHost(this.modelres);

        assertFalse(check.perform(null, analysisHost));

        this.modelres.unload();
    }

    @Test
    public final void testNoTransferStartOrTerminate() throws IOException {
        loadModel("transfer_process_protocol_no_transfer_start_or_terminate.uml");
        TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();
        List<Interaction> interactions = UMLHelper.getAllElementsOfType(this.model, Interaction.class);
        Interaction interaction = interactions.stream().findFirst().get();
        RelevantMessagesDto relevantMessagesDto = TransferProcessProtocolHelper.getTaggedValues(interaction);
        assertNotNull(relevantMessagesDto.getTransferRequest());
        assertNull(relevantMessagesDto.getTransferStart());
        assertNull(relevantMessagesDto.getTransferTerminate());

        TestHost analysisHost = new TestHost(this.modelres);

        assertFalse(check.perform(null, analysisHost));

        this.modelres.unload();
    }

    @Test
    public final void testTransferStartNoSuspendOrTerminateOrPushPull() throws IOException {
        loadModel("transfer_process_protocol_transfer_start_no_suspend_terminate_pushpull.uml");
        TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();
        List<Interaction> interactions = UMLHelper.getAllElementsOfType(this.model, Interaction.class);
        Interaction interaction = interactions.stream().findFirst().get();
        RelevantMessagesDto relevantMessagesDto = TransferProcessProtocolHelper.getTaggedValues(interaction);
        assertNotNull(relevantMessagesDto.getTransferStart());
        assertNull(relevantMessagesDto.getTransferSuspend());
        assertNull(relevantMessagesDto.getTransferTerminate());
        assertNull(relevantMessagesDto.getPushPull());

        TestHost analysisHost = new TestHost(this.modelres);

        assertFalse(check.perform(null, analysisHost));

        this.modelres.unload();
    }

    @Test
    public final void testPushPullNoSuspendOrTerminateOrComplete() throws IOException {
        loadModel("transfer_process_protocol_pushpull_no_suspend_terminate_complete.uml");
        TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();
        List<Interaction> interactions = UMLHelper.getAllElementsOfType(this.model, Interaction.class);
        Interaction interaction = interactions.stream().findFirst().get();
        RelevantMessagesDto relevantMessagesDto = TransferProcessProtocolHelper.getTaggedValues(interaction);
        assertNotNull(relevantMessagesDto.getPushPull());
        assertNull(relevantMessagesDto.getTransferTerminate());
        assertNull(relevantMessagesDto.getTransferSuspend());
        assertNull(relevantMessagesDto.getTransferComplete());

        TestHost analysisHost = new TestHost(this.modelres);

        assertFalse(check.perform(null, analysisHost));

        this.modelres.unload();
    }

    @Test
    public final void testSuspendWithoutStart() throws IOException {
        loadModel("transfer_process_protocol_suspend_without_start.uml");
        TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();
        List<Interaction> interactions = UMLHelper.getAllElementsOfType(this.model, Interaction.class);
        Interaction interaction = interactions.stream().findFirst().get();
        RelevantMessagesDto relevantMessagesDto = TransferProcessProtocolHelper.getTaggedValues(interaction);
        assertNotNull(relevantMessagesDto.getTransferSuspend());
        assertNull(relevantMessagesDto.getTransferStart());

        TestHost analysisHost = new TestHost(this.modelres);

        assertFalse(check.perform(null, analysisHost)); 

        this.modelres.unload();
    }

    @Test
    public final void testCompleteWithTerminateOrSuspend() throws IOException {
    	loadModel("transfer_process_protocol_complete_with_terminate.uml");
    	TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();
        List<Interaction> interactions = UMLHelper.getAllElementsOfType(this.model, Interaction.class);
        Interaction interaction = interactions.stream().findFirst().get();
        RelevantMessagesDto relevantMessagesDto = TransferProcessProtocolHelper.getTaggedValues(interaction);
        assertNotNull(relevantMessagesDto.getTransferTerminate());
        assertNotNull(relevantMessagesDto.getTransferComplete());

        TestHost analysisHost = new TestHost(this.modelres); 

        assertFalse(check.perform(null, analysisHost)); 

        this.modelres.unload();
    }
    
    @Test
    public final void testIncorrectOrderOfSteps() throws IOException {
    	loadModel("transfer_process_protocol_incorrrect_order.uml");
    	TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();
        List<Interaction> interactions = UMLHelper.getAllElementsOfType(this.model, Interaction.class);
        Interaction interaction = interactions.stream().findFirst().get();
        RelevantMessagesDto relevantMessagesDto = TransferProcessProtocolHelper.getTaggedValues(interaction);
        TestHost analysisHost = new TestHost(this.modelres);
        TransferProcessProtocol tpp = new TransferProcessProtocol(analysisHost);
        assertFalse(tpp.hasCorrectOrderOfSteps(relevantMessagesDto, this.model));
        assertFalse(check.perform(null, analysisHost)); 

        this.modelres.unload();
    }
    
    @Test
    public final void testInvalidProtocolSteps() throws IOException{
    	loadModel("transfer_process_protocol_invalid_protocol_steps.uml");
    	TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();
        List<Interaction> interactions = UMLHelper.getAllElementsOfType(this.model, Interaction.class);
        Interaction interaction = interactions.stream().findFirst().get();
        RelevantMessagesDto relevantMessagesDto = TransferProcessProtocolHelper.getTaggedValues(interaction);
        TestHost analysisHost = new TestHost(this.modelres);
        TransferProcessProtocol tpp = new TransferProcessProtocol(analysisHost);

		Set<Lifeline> lifelines = UMLSequenceHelper.getAllLifeLines(this.model);
        Set<Lifeline> providers = TransferProcessProtocolHelper.getAnnotatedLifeline(lifelines, Extension4IDS.PROVIDER);
		Set<Lifeline> consumers = TransferProcessProtocolHelper.getAnnotatedLifeline(lifelines,Extension4IDS.CONSUMER);
        Lifeline provider = providers.stream().findFirst().get();
		Lifeline consumer = consumers.stream().findFirst().get();
		DataTransferProtocolDto dataTransferProtocolDto = new DataTransferProtocolDto(provider, consumer);
		
		assertFalse(tpp.hasValidProtocolSteps(dataTransferProtocolDto, relevantMessagesDto));
		assertFalse(check.perform(null, analysisHost)); 

        this.modelres.unload();
    }
    
    @Test
    public final void testCheckSuccess() throws IOException{
    	loadModel("transfer_process_protocol_check_success.uml");
    	TransferProcessProtocolCheck check = new TransferProcessProtocolCheck();
        List<Interaction> interactions = UMLHelper.getAllElementsOfType(this.model, Interaction.class);
        Interaction interaction = interactions.stream().findFirst().get();
        RelevantMessagesDto relevantMessagesDto = TransferProcessProtocolHelper.getTaggedValues(interaction);
        TestHost analysisHost = new TestHost(this.modelres);
        TransferProcessProtocol tpp = new TransferProcessProtocol(analysisHost);

		Set<Lifeline> lifelines = UMLSequenceHelper.getAllLifeLines(this.model);
        Set<Lifeline> providers = TransferProcessProtocolHelper.getAnnotatedLifeline(lifelines, Extension4IDS.PROVIDER);
		Set<Lifeline> consumers = TransferProcessProtocolHelper.getAnnotatedLifeline(lifelines,Extension4IDS.CONSUMER);
		Lifeline provider = providers.stream().findFirst().get();
		Lifeline consumer = consumers.stream().findFirst().get();
		DataTransferProtocolDto dataTransferProtocolDto = new DataTransferProtocolDto(provider, consumer);
		
		assertTrue(TransferProcessProtocolHelper.hasDataTransferStereotype(interactions));
		assertTrue(tpp.hasMinimumSteps(relevantMessagesDto));
		assertTrue(tpp.hasValidConsumerAndProvider(providers, consumers));
		assertTrue(tpp.hasCorrectOrderOfSteps(relevantMessagesDto, this.model));
		assertTrue(tpp.hasValidProtocolSteps(dataTransferProtocolDto, relevantMessagesDto));
		
		assertTrue(check.perform(null, analysisHost));
		
		this.modelres.unload();
    }

    @After
    public void unloadModel() {
        for (Resource r : this.rs.getResources()) {
            r.unload();
        }
    }
    
   
}
