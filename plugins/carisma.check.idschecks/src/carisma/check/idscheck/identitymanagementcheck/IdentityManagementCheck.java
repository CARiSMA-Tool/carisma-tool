package carisma.check.idscheck.identitymanagementcheck;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Node;
import org.eclipse.uml2.uml.Package;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.umlsec4ids.UMLsec;
import carisma.profile.umlsec.umlsec4ids.UMLsecUtil;
import java.util.logging.Logger;

/**
 * analyzes a deployment diagram with respect to identity management rules.
 * @author Alexander Peikert
 *
 */

public class IdentityManagementCheck implements CarismaCheckWithID {

	private static final Logger logger = Logger.getLogger(IdentityManagementCheck.class.getName());
	public static final String CHECK_ID = "carisma.check.idscheck.identitymanagementcheck";
	public static final String CHECK_NAME = "UMLsec4ids Identity Management Check";

	/**
	 * the model to check.
	 */
	private Package model = null;
	
	/**
	 * AnalysisHost for report.
	 */
    private AnalysisHost analysisHost;

	public IdentityManagementCheck() {
		// TODO Auto-generated constructor stub
	}

	
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost newHost) {
	    if (newHost != null) {
	        this.analysisHost = newHost;
	    } else {
	        this.analysisHost = new DummyHost(true);
	    }
		Resource currentModel = this.analysisHost.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			this.analysisHost.appendLineToReport("Empty model");
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Package) {
			this.model = (Package) currentModel.getContents().get(0);
			return startCheck();
		}
		this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
		this.analysisHost.appendLineToReport("Content is not a model!");
		return false;
	}	

	/**
	 * main function that starts the check.
	 * @return true if the model is correct according to identity management rules, false otherwise
	 */
	
	public boolean startCheck() {
		boolean checkSuccessful = true;
		//Get all nodes and nodes with stereotypes
		ArrayList<Node> nodeList = (ArrayList<Node>) UMLHelper.getAllElementsOfType(model, Node.class);
		ArrayList<Element> x509List = (ArrayList<Element>) UMLsecUtil.getStereotypedElements(this.model, UMLsec.X509);
		ArrayList<Element> x509TLSList = (ArrayList<Element>) UMLsecUtil.getStereotypedElements(this.model, UMLsec.X509TLS);
		//-----------------------------------------------------------------
				
		//Check if any node has the required stereotypes
		if ((x509List.size() < nodeList.size()) && x509TLSList.size() == nodeList.size()) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes miss the <<X509>> Stereotype"));
			this.analysisHost.appendLineToReport("Nodes miss the <<X509>> Stereotype");
			checkSuccessful = false;
		}
		
		if ((x509List.size() == nodeList.size()) && x509TLSList.size() < nodeList.size()) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes miss the <<X509TLS>> Stereotype"));
			this.analysisHost.appendLineToReport("Nodes miss the <<X509TLS>> Stereotype");
			checkSuccessful = false;
		}
		
		if ((x509List.size() < nodeList.size()) && x509TLSList.size() < nodeList.size()) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes miss the <<X509TLS>> and <<X509>> Stereotype"));
			this.analysisHost.appendLineToReport("Nodes miss the <<X509TLS>> and <<X509>> Stereotypes");
			checkSuccessful = false;
		}
		//get the current date
		
		LocalDate today = LocalDate.now();

		String formattedDate = today.format(DateTimeFormatter.ofPattern("yyyyMMdd"));

		

		
		int intDay = 0;
        try{
        	intDay = Integer.parseInt(formattedDate);
        } catch (NumberFormatException ex) {
            logger.warning("Failed to parse formatted date: " + formattedDate);
            logger.warning("Error message: " + ex.getMessage());
        }
        
        //------------------------------------------------------
        // check if the date of the X.509 certificate is a valid date and check if the certificate is already expired
        int intX509 = 0;
        for(int i = 0; i < nodeList.size(); i++) {
			List<Object> dayX509 = UMLsecUtil.getTaggedValues("expiration_date_yyyy_mm_dd", UMLsec.X509, nodeList.get(i));
				for(int z = 0; z < dayX509.size(); z++) {
					String strX509 = dayX509.get(z).toString();
					if(strX509.length() != 8) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes <<X509>> certificate has an invalid date"));
						this.analysisHost.appendLineToReport("Invalid date in X.509 certificate of " + nodeList.get(i).getName() + ", not of format YYYY/MM/DD");
						checkSuccessful = false;	
					 }
					if(strX509.length() == 8) {
						String monthChar = strX509.substring(4, 6);
						String dayChar = strX509.substring(6, 8);
						int intMonthChar = Integer.parseInt(monthChar);
						int intDayChar = Integer.parseInt(dayChar);
						if( (intMonthChar < 1 || intMonthChar > 12) || (intDayChar < 1 || intDayChar > 31) ) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes <<X509>> certificate has an invalid date"));
							this.analysisHost.appendLineToReport("Invalid date in X.509 certificate of " + nodeList.get(i).getName());
							checkSuccessful = false;
						}
						if((intMonthChar == 4 || intMonthChar == 6 || intMonthChar == 9 || intMonthChar == 11) && intDayChar > 30) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes <<X509>> certificate has an invalid date"));
							this.analysisHost.appendLineToReport("Invalid date in X.509 certificate of " + nodeList.get(i).getName());
							checkSuccessful = false;
						}
						if(intMonthChar == 2 && intDayChar > 29) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes <<X509>> certificate has an invalid date"));
							this.analysisHost.appendLineToReport("Invalid date in X.509 certificate of " + nodeList.get(i).getName());
							checkSuccessful = false;
						}
					
						try{
				        		intX509 = Integer.parseInt(dayX509.get(z).toString());
				        	} catch (NumberFormatException ex) {
				            logger.warning("Failed to parse formatted date: " + formattedDate);
				            logger.warning("Error message: " + ex.getMessage());
							}
						if(intX509 < intDay) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes <<X509>> certificate is expired"));
							this.analysisHost.appendLineToReport("X.509 certificate of " + nodeList.get(i).getName() + " is expired");
							checkSuccessful = false;
						}
				     }
				}
		}
        //------------------------------------------------------
        // check if the date of the X.509 certificate is a valid date and check if the certificate is already expired
        
        int intX509TLS = 0;
        for(int i = 0; i < nodeList.size(); i++) {
			List<Object> dayX509TLS = UMLsecUtil.getTaggedValues("expiration_date_yyyy_mm_dd", UMLsec.X509TLS, nodeList.get(i));
				for(int z = 0; z < dayX509TLS.size(); z++) {
					String strX509TLS = dayX509TLS.get(z).toString();
					if(strX509TLS.length() != 8) {
					    this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes <<X509TLS>> certificate has an invalid date"));
						this.analysisHost.appendLineToReport("Invalid date in X.509TLS certificate of " + nodeList.get(i).getName() + ", not of format YYYY/MM/DD");
						checkSuccessful = false;
					 }
					if(strX509TLS.length() == 8) {
						String monthCharTLS = strX509TLS.substring(4, 6);
						String dayCharTLS = strX509TLS.substring(6, 8);


						int intMonthCharTLS = Integer.parseInt(monthCharTLS);
						int intDayCharTLS = Integer.parseInt(dayCharTLS);
						if( (intMonthCharTLS < 1 || intMonthCharTLS > 12) || (intDayCharTLS < 1 || intDayCharTLS > 31) ) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes <<X509TLS>> certificate has an invalid date"));
							this.analysisHost.appendLineToReport("Invalid date in X.509TLS certificate of " + nodeList.get(i).getName());
							checkSuccessful = false;
						}
						if((intMonthCharTLS == 4 || intMonthCharTLS == 6 || intMonthCharTLS == 9 || intMonthCharTLS == 11) && intDayCharTLS > 30) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes <<X509TLS>> certificate has an invalid date"));
							this.analysisHost.appendLineToReport("Invalid date in X.509TLS certificate of " + nodeList.get(i).getName());
							checkSuccessful = false;
						}
						if(intMonthCharTLS == 2 && intDayCharTLS > 29) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes <<X509TLS>> certificate has an invalid date"));
							this.analysisHost.appendLineToReport("Invalid date in X.509TLS certificate of " + nodeList.get(i).getName());
							checkSuccessful = false;
						}
					
					 	try{
					 			intX509TLS = Integer.parseInt(dayX509TLS.get(z).toString());
					 		} catch (NumberFormatException ex) {
				            logger.warning("Failed to parse formatted date: " + formattedDate);
				            logger.warning("Error message: " + ex.getMessage());
					 		}	
					 	if(intX509TLS < intDay) {
					 		this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes <<X509TLS>> certificate is expired"));
					 		this.analysisHost.appendLineToReport("X.509TLS certificate of " + nodeList.get(i).getName() + " is expired");
					 		checkSuccessful = false;
					 	}
					}
				}
		}
        // return the result of the check
		return checkSuccessful;
	}
		
	
	@Override
	public String getCheckID() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getName() {
		// TODO Auto-generated method stub
		return null;
	}

}
