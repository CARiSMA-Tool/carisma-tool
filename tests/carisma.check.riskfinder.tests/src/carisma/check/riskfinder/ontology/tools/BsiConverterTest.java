package carisma.check.riskfinder.ontology.tools;

//import java.io.FileInputStream;
//import org.junit.Test;
//
//
//import com.thoughtworks.xstream.XStream;
//import com.thoughtworks.xstream.io.xml.DomDriver;
//
//
//public class BsiConverterTest {
//FIXME: Test unnoetig, da deprecated?
//	@Test
//	public void test() throws Exception {
//		// einlesen:
//		XStream xstream = new XStream(new DomDriver());
//		DataRoot.setAliases(xstream);
//
//		DataRoot complianceData = (DataRoot) xstream
//				.fromXML(new FileInputStream(
//						"resources/allrules.xml"));
//		System.out.println(complianceData);
//
//		BsiConverter converter = new BsiConverter();
//		converter.open();
//		converter.init();
//		converter.addBsiCatalogue(complianceData.getBsiCatalogue());
//
//		converter.saveToFile("resources/ontology1.owl");
//
//	}
//
//}
