package carisma.check.requirescheck;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.activity.ActivityDiagramManager;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;

/**
 * this class evaluates a given model according to the UMLchange Stereotype
 * <<requires>>.
 *
 * @author Klaus Rudack
 *
 */
public class RequiresCheck implements CarismaCheck {

	/**
	 * string name of the stereotype <<requires>>.
	 */
	private static final String requiresName = "UMLsec::requires";

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
	 *
	 * @param h - AnalysisHost for report
	 */
	public RequiresCheck(final AnalysisHost h) {
		if (h != null) {
			this.host = h;
		}
	}

	/**
	 * this method starts the check.
	 *
	 * @param model - model to check
	 * @return - true if the model is correct according to <<requires>>, false
	 *         otherwise
	 */
	private boolean startCheck(final Package model) {
		if (model == null) {
			final var resultMessage = new AnalysisResultMessage(StatusType.ERROR, "The given model is null!");
			this.host.addResultMessage(resultMessage);
			this.host.appendLineToReport("The given model is null");
			return false;
		}
		ArrayList<Package> packageList = (ArrayList<Package>) UMLHelper.getAllElementsOfType(model, Package.class);
		if (packageList.size() > 0) {
			final var adm = new ActivityDiagramManager((Package) model.getOwnedElements().get(0));
			final var pathsList = adm.getAllPaths();
			if (pathsList.isEmpty()) {
				final var resultMessage = new AnalysisResultMessage(StatusType.INFO,
						"No paths could be found in the model.");
				this.host.addResultMessage(resultMessage);
				this.host.appendLineToReport("No paths could be found in the model.");
				return true;
			}
			final List<Action> requiringActions = new ArrayList<>();
			for (final Element element : UMLsecUtil.getStereotypedElements(model, UMLsec.REQUIRES)) {
				requiringActions.add((Action) element);
			}
			if (requiringActions.isEmpty()) {
				final var resultMessage = new AnalysisResultMessage(StatusType.INFO,
						"No actions with stereotype <<requires>> has been found.");
				this.host.addResultMessage(resultMessage);
				this.host.appendLineToReport("No actions with stereotype <<requires>> has been found.");
				return true;
			}
			for (final Action requiringElement : requiringActions) {
				for (final List<Element> path : pathsList) {
					singleCheck(requiringElement, path);
				}
			}
		} else {
			final var adm = new ActivityDiagramManager(model, host); //gehen richtig rein
			final var pathsList = adm.getAllPaths();
			if (pathsList.isEmpty()) {
				final var resultMessage = new AnalysisResultMessage(StatusType.INFO,
						"No paths could be found in the model.");
				this.host.addResultMessage(resultMessage);
				this.host.appendLineToReport("No paths could be found in the model.");
				return true;
			}
			final List<Action> requiringActions = new ArrayList<>();
			for (final Element element : UMLsecUtil.getStereotypedElements(model, UMLsec.REQUIRES)) {
				requiringActions.add((Action) element);
			}
			if (requiringActions.isEmpty()) {
				final var resultMessage = new AnalysisResultMessage(StatusType.INFO,
						"No actions with stereotype <<requires>> has been found.");
				this.host.addResultMessage(resultMessage);
				this.host.appendLineToReport("No actions with stereotype <<requires>> has been found.");
				return true;
			}
			for (final Action requiringElement : requiringActions) {
				for (final List<Element> path : pathsList) {
					singleCheck(requiringElement, path);
				}
			}
		}
		/*
		final var adm = new ActivityDiagramManager((Package) model.getOwnedElements().get(0));
		final var pathsList = adm.getAllPaths();
		if (pathsList.isEmpty()) {
			final var resultMessage = new AnalysisResultMessage(StatusType.INFO,
					"No paths could be found in the model.");
			this.host.addResultMessage(resultMessage);
			this.host.appendLineToReport("No paths could be found in the model.");
			return true;
		}
		
		final List<Action> requiringActions = new ArrayList<>();
		for (final Element element : UMLsecUtil.getStereotypedElements(model, UMLsec.REQUIRES)) {
			requiringActions.add((Action) element);
		}
		if (requiringActions.isEmpty()) {
			final var resultMessage = new AnalysisResultMessage(StatusType.INFO,
					"No actions with stereotype <<requires>> has been found.");
			this.host.addResultMessage(resultMessage);
			this.host.appendLineToReport("No actions with stereotype <<requires>> has been found.");
			return true;
		}
		for (final Action requiringElement : requiringActions) {
			for (final List<Element> path : pathsList) {
				singleCheck(requiringElement, path);
			}
		}
		*/
		if (!this.errors) {
			final var resultMessage = new AnalysisResultMessage(StatusType.INFO,
					"Check successful with respect to <<requires>>.");
			this.host.addResultMessage(resultMessage);
			this.host.appendLineToReport("Check successful with respect to <<requires>>.");
			return true;
		} else {
			return false;
		}
	}

	/**
	 * tests if the given <<requires>> stereotyped element requires is in the path
	 * and if so, checks if it violates the model.
	 *
	 * @param requiringElement - element with stereotype <<requires>>
	 * @param path             - a single path in die model
	 */
	private void singleCheck(final Element requiringElement, final List<Element> path) {
		if (!path.contains(requiringElement)) {
			return;
		}
		final var requiresStereotype = requiringElement.getAppliedStereotype(RequiresCheck.requiresName);
		if (requiresStereotype == null) {
			return;
		}
		@SuppressWarnings("unchecked")
		final var requiresActions = (List<Action>) requiringElement.getValue(requiresStereotype, "actions");
		if (requiresActions.isEmpty()) {
			return;
		}
		final var subpath = path.subList(0, path.indexOf(requiringElement));
		for (final Element action : requiresActions) {
			if (!subpath.contains(action)) {
				this.errors = true;
				final var resultMessage = new AnalysisResultMessage(StatusType.ERROR,
						"Check failed with respect to <<requires>>!");
				this.host.addResultMessage(resultMessage);
				this.host.appendLineToReport("Check failed with respect to <<requires>>!");
				writeDetailResult(requiringElement, action, path);
				return;
			}
		}
	}

	/**
	 * writes a path that violates the model according to <<requires>> to the
	 * console.
	 *
	 * @param requires - element that got <<requires>> applies
	 * @param action   - element that should be in the path in front of the
	 *                 <<requires>> element, but is not
	 * @param path     - the path that violates the model
	 */
	private void writeDetailResult(final Element requires, final Element action, final List<Element> path) {
		this.host.appendLineToReport("The required action " + ((NamedElement) action).getName() + " for the action "
				+ ((NamedElement) requires).getName() + " is not in the following path: ");
		if (path.isEmpty()) {
			this.host.appendLineToReport("No path found!");
		}
		else {
			final var result = new StringBuilder(((NamedElement) path.get(0)).getName());
			for (var i = 1; i < path.size(); i++) {
				result.append(" --> ");
				result.append(((NamedElement) path.get(i)).getName());
			}
			this.host.appendLineToReport(result.toString());
		}
	}

	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost analysisHost) {
		if (analysisHost != null) {
			this.host = analysisHost;
		}
		final var currentModel = this.host.getAnalyzedModel();
		if (currentModel == null) {
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Resource is null"));
			return false;
		}
		if (currentModel.getContents().isEmpty()) {
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Package) {
			final var model = (Package) currentModel.getContents().get(0);
			return startCheck(model);
		} else {
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
			return false;
		}
	}

}
