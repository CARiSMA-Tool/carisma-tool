package carisma.core.reports;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import carisma.core.analysis.result.AnalysisResult;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;

/**
 * An XML report created from an AnalysisResult.
 * 
 */
public class XMLReport {

	private String xml;

	public XMLReport(AnalysisResult result) {
		try (var out = new ByteArrayOutputStream()) {
			final var context = JAXBContext.newInstance(carisma.core.analysis.result.AnalysisResult.class);
			final var m = context.createMarshaller();
			m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			m.setProperty(Marshaller.JAXB_ENCODING, "UTF-8");
			m.marshal(result, out);
			xml = new String(out.toByteArray(), StandardCharsets.UTF_8);
		} catch (JAXBException | IOException e) {
			System.err.println(e.getMessage());
		}
	}

	public String getXml() {
		return this.xml;
	}

	@Override
	public String toString() {
		return this.xml;
	}

}
