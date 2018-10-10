package carisma.check.requirescheck;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.activity.ActivityDiagramManager;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


/**
 * this class evaluates a given model according to the UMLchange Stereotype <<requires>>.
 * @author Klaus Rudack
 *
 */
public class RequiresCheck implements CarismaCheck {
	
	/**
	 * string name of the stereotype <<requires>>.
	 */
	private final String requiresName = "UMLsec::requires";
	
	/**
	 * boolean to save if errors occurred.
	 */
	private boolean errors = false;
	
	/**
	 * AnalysisHost for report.
	 */
	private AnalysisHost host = new DummyHost(true);
	
	/**
	 * constructor.
	 * @param h - AnalysisHost for report
	 */
	public RequiresCheck(final AnalysisHost h) {
		if (h != null) {
			host = h;
		}
	}
	
	
	/**
	 * this method starts the check.
	 * @param model - model to check
	 * @return - true if the model is correct according to <<requires>>, false otherwise
	 */
	private boolean startCheck(final Package model) {
		if (model == null) {
			AnalysisResultMessage resultMessage = new AnalysisResultMessage(StatusType.ERROR, "The given model is null!");
			host.addResultMessage(resultMessage);
			host.appendLineToReport("The given model is null");
			return false;
		}
		ActivityDiagramManager adm = new ActivityDiagramManager((Package) model.getOwnedElements().get(0));
		List<List<Element>> pathsList = adm.getAllPaths();
		if (pathsList.size() == 0) {
			AnalysisResultMessage resultMessage = new AnalysisResultMessage(StatusType.INFO, "No paths could be found in the model.");
			host.addResultMessage(resultMessage);
			host.appendLineToReport("No paths could be found in the model.");
			return true;
		}
		List<Action> requiringActions = new ArrayList<Action>();
		for (Element element : UMLsecUtil.getStereotypedElements(model, UMLsec.REQUIRES)) {
			requiringActions.add((Action) element);
		}
		if (requiringActions.size() == 0) {
			AnalysisResultMessage resultMessage = new AnalysisResultMessage(StatusType.INFO, "No actions with stereotype <<requires>> has been found.");
			host.addResultMessage(resultMessage);
			host.appendLineToReport("No actions with stereotype <<requires>> has been found.");
			return true;
		}
		for (Action requiringElement : requiringActions) {
			for (List<Element> path : pathsList) {
				singleCheck(requiringElement, path);
			}
		}
		if (!errors) {
			AnalysisResultMessage resultMessage = new AnalysisResultMessage(StatusType.INFO, "Check successful with respect to <<requires>>.");
			host.addResultMessage(resultMessage);
			host.appendLineToReport("Check successful with respect to <<requires>>.");
			return true;
		} else {
			return false;
		}
	}
	
	/**
	 * tests if the given <<requires>> stereotyped element requires is in the path and if so, checks if it violates the model.
	 * @param requiringElement - element with stereotype <<requires>>
	 * @param path - a single path in die model
	 */
	private void singleCheck(final Element requiringElement, final List<Element> path) {
		if (!path.contains(requiringElement)) {
			return;
		}
		Stereotype requiresStereotype = requiringElement.getAppliedStereotype(requiresName);
		if (requiresStereotype == null) {
			return;
		}
		@SuppressWarnings("unchecked")
		List<Action> requiresActions = (List<Action>) requiringElement.getValue(requiresStereotype, "actions");
		if (requiresActions.size() == 0) {
			return;
		}
		List<Element> subpath = path.subList(0, path.indexOf(requiringElement));
		for (Element action : requiresActions) {
			if (!subpath.contains(action)) {
				errors = true;
				AnalysisResultMessage resultMessage = new AnalysisResultMessage(StatusType.ERROR, "Check failed with respect to <<requires>>!");
				host.addResultMessage(resultMessage);
				host.appendLineToReport("Check failed with respect to <<requires>>!");
				writeDetailResult(requiringElement, action, path);
				return;
			}
		}
	}
	
	/**
	 * writes a path that violates the model according to <<requires>> to the console.
	 * @param requires - element that got <<requires>> applies
	 * @param action - element that should be in the path in front of the <<requires>> element, but is not
	 * @param path - the path that violates the model
	 */
	private void writeDetailResult(final Element requires, final Element action, final List<Element> path) {
		String result = "";
		final String arrow = " --> ";
		host.appendLineToReport("The required action " + ((NamedElement) action).getName() + " for the action " + ((NamedElement) requires).getName()
				+ " is not in the following path:");
		for (Element element : path) {
			result += ((NamedElement) element).getName() + arrow;
		}
		result = result.substring(0, result.lastIndexOf(arrow));
		host.appendLineToReport(result);
	}
	
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost analysisHost) {
		if (analysisHost != null) {
			host = analysisHost;
		}
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel == null) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Resource is null"));
			return false;
		}
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Package) {
			Package model = (Package) currentModel.getContents().get(0);
			return startCheck(model);
		} else {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
		return false;
		}
	}
	
}
