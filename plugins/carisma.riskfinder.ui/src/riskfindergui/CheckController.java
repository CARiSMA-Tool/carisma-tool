package riskfindergui;

import java.io.File;
import java.util.ArrayList;

import org.eclipse.emf.ecore.resource.Resource;

import carisma.check.riskfinder.AnalyserResult;
import carisma.check.riskfinder.Check;
import carisma.processanalysis.textmodel.ProcessDescription;

/**
 * Class for interacting with the class Check
 *
 */
public class CheckController {
	
	public static ArrayList<AnalyserResult> getResultAr() {
		if (Check.getInstance() == null) {
			return null;
		}
		return Check.getInstance().getResultAr(); 
	}

	public static Resource getCurrentModel() {
		if (Check.getInstance() == null) {
			return null;
		}
		return Check.getInstance().getCurrentModel();
	}

	public static ArrayList<String> getActivityNames() {
		if (Check.getInstance() == null) {
			return null;
		}
		return Check.getInstance().getActivityNames();
	}

	public static ProcessDescription getDescription() {
		if (Check.getInstance() == null) {
			return null;
		}
		return Check.getInstance().getDescription();
	}
	public static Boolean getAppend(){
		if (Check.getInstance() == null) {
			return null;
		}
		return Check.getInstance().getAppend();
	}
	public static Boolean getSynonyms(){
		if (Check.getInstance() == null) {
			return null;
		}
		return Check.getInstance().getSynonyms();
	}
	public static File getOntology(){
		if (Check.getInstance() == null) {
			return null;
		}
		return Check.getInstance().getOntology();
	}
	public static File getStopwords(){
		if (Check.getInstance() == null) {
			return null;
		}
		return Check.getInstance().getStopwords();
	}

	
	
}
