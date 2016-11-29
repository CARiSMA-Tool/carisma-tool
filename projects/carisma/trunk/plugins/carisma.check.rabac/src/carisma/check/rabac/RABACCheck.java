package carisma.check.rabac;

import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBContext;

import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.StateMachine;
import org.eclipse.uml2.uml.Transition;
import org.eclipse.uml2.uml.Vertex;
import org.eclipse.ocl.ParserException;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CarismaCheckWithID;
import carisma.profile.umlsec.rabac.UMLsec;
import carisma.profile.umlsec.rabac.UMLsecUtil;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.statemachine.StateMachinePaths;
import carisma.ocl.OclEvaluator;

/**
 * CARiSMA Check for analyzing CHECK_ID models. Requires a valid configuration file
 * containing at least the sessions to make decisions.
 */

public class RABACCheck implements CarismaCheckWithID {

	// Check IDs
	public static final String CHECK_ID = "carisma.check.rabac"; //$NON-NLS-1$
	public static final String PARAM_CONFIGURATION = "carisma.check.rabac.configuration"; //$NON-NLS-1$
	public static final String CHECK_NAME = "RABACsec: Use transformation input"; //$NON-NLS-1$

	AnalysisHost analysisHost;

	Map<String, SetWrapper> sessions;
	Set<Attribute> attributes;

	/**
	 * Run the check
	 * 
	 * @param parameters
	 *            parameters for this check
	 * @param host
	 *            deliver analysis results to this host
	 * @return success of the check
	 */
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		this.analysisHost = host;
		Resource model = host.getAnalyzedModel();

		if (model.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Empty model"));
			return false;
		}

		if (model.getContents().get(0) instanceof Package) {
			Package content = (Package) model.getContents().get(0);

			List<Element> abac = UMLsecUtil.getStereotypedElements(content, UMLsec.ABAC);
			int abacNum = abac.size();
			if (abacNum == 0) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
						"Could not find the main CHECK_ID stereotype!"));
				return false;
			}
			if (abacNum > 1) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Found " + abacNum
						+ " main CHECK_ID stereotypes, model must only contain one!"));
				return false;
			}
			Element abacClass = abac.get(0);

			List<String> usersTag = UMLsecUtil.getStringValues("roles", UMLsec.ABAC, abacClass);
			if (usersTag.size() == 0) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Roles missing!"));
				return false;
			}
			Set<String> users = parseTag(usersTag.get(0), null, 0);
			if (users.size() == 0) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Roles missing!"));
				return false;
			}

			List<String> rightsTag = UMLsecUtil.getStringValues("rights", UMLsec.ABAC, abacClass);
			if (rightsTag.size() == 0) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Rights missing!"));
				return false;
			}

			// read saved configuration
			try {
				RABACConfig config = (RABACConfig) JAXBContext
						.newInstance(RABACConfig.class)
						.createUnmarshaller()
						.unmarshal(
								((InputFileParameter) parameters.get(PARAM_CONFIGURATION)).getValue());
				this.sessions = config.getSessions();
				this.attributes = config.getAttributes();
			} catch (Exception e) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Error reading configuration file!"));
				return false;
			}

			if (!this.sessions.keySet().isEmpty()) {
				for (String u : this.sessions.keySet()) {
					if (!users.contains(u)) {
						host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Loaded sessions, but user "
								+ u + " is not valid for this model!"));
						return false;
					}
				}
				host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Loaded sessions"));

				List<String> dsdTag = UMLsecUtil.getStringValues("dsd", UMLsec.ABAC, abacClass);
				if (dsdTag.size() > 0) {
					String[] dsdEntries = dsdTag.get(0).split("\\),\\(");
					for (String d : dsdEntries) {
						String[] dsdElements = d.split(",");
						int limit;
						try {
							limit = Integer.parseInt(dsdElements[dsdElements.length - 1].replace(")", ""));
						} catch (NumberFormatException e) {
							host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
									"Dynamic seperation of duty contains invalid syntax!"));
							return false;
						}
						for (String u : this.sessions.keySet()) {
							int numRoles = 0;
							for (String r : this.sessions.get(u).getSet()) {
								if (Arrays.asList(dsdElements).toString().contains(r)) {
									numRoles++;
								}
							}
							if (numRoles > limit) {
								host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING,
										"Dynamic seperation of duty is violated for user " + u + " !"));
							}
						}
					}
				}

				List<StateMachine> stateMachines = UMLHelper.getAllElementsOfType(content, StateMachine.class);
				if (stateMachines.size() > 0) {
					host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, stateMachines.size()
							+ " StateMachine(s) found"));
				}

				List<Element> abacRequire = UMLsecUtil.getStereotypedElements(content, UMLsec.ABACREQUIRE);
				if (abacRequire.isEmpty()) {
					host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "No CHECK_ID constraints found!"));
					return true;
				}

				// save paths of all StateMachines
				StateMachinePaths pathFinder = new StateMachinePaths();
				List<List<List<Element>>> paths = new ArrayList<>();
				for (StateMachine m : stateMachines) {
					paths.add(pathFinder.getPaths(m, host, true));
				}

				String globalFilter = null;
				List<String> attributeFiltersTag = UMLsecUtil.getStringValues("attributeFilters", UMLsec.ABAC,
						abacClass);
				if (attributeFiltersTag.size() > 0) {
					globalFilter = attributeFiltersTag.get(0);
				}

				for (String u : this.sessions.keySet()) {
					host.appendLineToReport("User " + u + " has access to the following protected items:");

					Set<Transition> validTransitions = new HashSet<>();
					// verify constraints based on all elements which contain a
					// CHECK_ID stereotype
					for (Element e : abacRequire) {
						List<String> right = UMLsecUtil.getStringValues("right", UMLsec.ABACREQUIRE, e);
						if (right.isEmpty()) {
							host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, ((NamedElement) e)
									.getName() + " has no right!"));
							return false;
						}

						Set<String> roles = this.sessions.get(u).getSet();
						List<String> rhTag = UMLsecUtil.getStringValues("rh", UMLsec.ABAC, abacClass);
						if (rhTag.size() > 0) {
							roles = getElementsFromHierarchy(roles, rhTag.get(0));
						}

						for (String r : roles) {
							if (rightsTag.get(0).contains("(" + r + "," + right.get(0) + ")")) {
								EObject transformedModel = transformModelBasic(u,
										e instanceof Transition ? ((Transition) e).containingStateMachine().getName()
												: ((Operation) e).getClass_().getName());

								List<String> filter = UMLsecUtil.getStringValues("filters", UMLsec.ABACREQUIRE, e);
								if ((filter.isEmpty() || filter.get(0).equals("") || attributeFilter(filter.get(0),
										transformedModel))
										&& (globalFilter == null || attributeFilter(globalFilter, transformedModel))) {
									if (e instanceof Transition) {
										// transitions are printed after all
										// operations
										validTransitions.add((Transition) e);
									} else {
										host.appendLineToReport("\t" + ((Operation) e).getName());
									}
								}
								break;
							}
						}
					}

					// choose state machine paths with transitions that have
					// passed CHECK_ID
					for (List<List<Element>> m : paths) {
						Set<Element> states = new HashSet<>();
						boolean printMachine = true;
						for (List<Element> p : m) {
							boolean printPath = false;
							for (Element e : p) {
								states.add(e);
								// make sure access control happens on path
								if (e instanceof Transition
										&& !UMLsecUtil.getStringValues("right", UMLsec.ABACREQUIRE, e).isEmpty()) {
									if (validTransitions.contains(e)) {
										printPath = true;
									} else {
										// loops are valid traces too
										if (!states.contains(e)) {
											printPath = false;
										}
										break;
									}
								}
							}
							if (printPath == true) {
								if (printMachine == true) {
									host.appendLineToReport("\n\tIn state machine "
											+ ((Vertex) p.get(0)).containingStateMachine().getName()
											+ " these traces can be performed:");
									printMachine = false;
								}

								host.appendToReport("\t\t");
								states = new HashSet<>();
								// print the path by its state names
								for (Element e : p) {
									if (validTransitions.contains(e)) {
										host.appendToReport(" (via right "
												+ UMLsecUtil.getStringValues("right", UMLsec.ABACREQUIRE, e).get(0)
												+ ")");
									}
									if (e instanceof Vertex) {
										host.appendToReport(" -> " + ((Vertex) e).getName());
										// stop after a loop
										if (states.contains(e)) {
											break;
										}
										states.add(e);
									}
								}
								host.appendLineToReport("");
							}
						}
					}

					host.appendLineToReport("");
				}
			}

			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO,
					"Verified CHECK_ID constraints, view report for details"));
			return true;
		}

		host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Content is not a model!"));
		return false;
	}

	/**
	 * Get elements from strings in set notation
	 * 
	 * @param tag
	 *            represents the tuples of a binary relation
	 * @param match
	 *            used to match against elements of tuples, can be null
	 * @param side
	 *            Decides which elements from the index of the tuples will be
	 *            collected. If {@code match} is passed, this will lead to the
	 *            opposite side being chosen after matching.
	 * @return the parsed elements
	 */
	public static Set<String> parseTag(String tag, String match, int side) {
		String validNames = "[a-zA-Z0-9\\s]+";
		Matcher matcher = Pattern.compile(
				"\\(" + (side == 0 && match != null ? match : validNames) + ","
						+ (side == 1 && match != null ? match : validNames) + "+\\)").matcher(tag);
		if (match != null) {
			side = side ^ 1; //TODO: Was passiert hier?
		}
		Set<String> elements = new HashSet<>();
		while (matcher.find()) {
			String[] e = matcher.group().split(",");
			if (side == 0) {
				elements.add(e[0].substring(1));
			} else {
				elements.add(e[1].substring(0, e[1].length() - 1));
			}
		}
		return elements;
	}

	/**
	 * Get all elements extended by a hierarchy
	 * 
	 * @param elements
	 *            initial elements
	 * @param hierarchy
	 *            represents the tuples of a binary inheritance relation
	 * @return all elements including their heirs
	 */
	public static Set<String> getElementsFromHierarchy(Set<String> elements, String hierarchy) {
		Set<String> newElements = new HashSet<>(elements);
		for (String e : elements) {
			Set<String> inheritedElements = parseTag(hierarchy, e, 1);
			if (!inheritedElements.isEmpty() && !elements.containsAll(inheritedElements)) {
				newElements.addAll(inheritedElements);
				newElements.addAll(getElementsFromHierarchy(newElements, hierarchy));
			}
		}
		return newElements;
	}

	/**
	 * Converts a string to a class that is compatible with a specified ECore
	 * type
	 * 
	 * @param attribute
	 *            target type
	 * @param value
	 *            string to convert
	 * @return compatible class
	 */
	public static Object convertToECore(EAttribute attribute, String value) {
		if (value != null) {
			if (attribute.getUpperBound() == -1) {
				String elements[] = value.substring(1, value.length() - 1).split(",");
				if (attribute.getEType().getName().equals("EDouble")) {
					EList<Double> list = new BasicEList<>();
					for (String s : elements) {
						list.add(Double.valueOf(s));
					}
					return list;
				}
				if (attribute.getEType().getName().equals("EString")) {
					EList<String> list = new BasicEList<>();
					for (String s : elements) {
						list.add(s);
					}
					return list;
				}
			} else if (attribute.getEType().getName().equals("EDouble")) {
				return Double.valueOf(value);
			}
		}

		return value;
	}

	private EObject transformModelBasic(String user, String object) {
		EcoreFactory ef = EcoreFactory.eINSTANCE;
		EcorePackage ep = EcorePackage.eINSTANCE;

		// create a helper model to hold all attributes defined in the model
		// chosen for analysis
		EClass filter = ef.createEClass();
		EPackage model = ef.createEPackage();
		model.getEClassifiers().add(filter);

		for (Attribute a : this.attributes) {
			EAttribute attribute = ef.createEAttribute();
			attribute.setName(a.getName());
			attribute.setEType(ep.getEString());
			if (a.getType() != null) {
				String value = a.getValues().get(a.getType().equals("User") ? user : object);
				// change attribute types
				if (value != null) {
					if (!value.matches(".*[a-zA-Z].*")) {
						attribute.setEType(ep.getEDouble());
					}
					if (value.matches("\\{(.*)\\}")) {
						attribute.setUpperBound(-1);
					}
				}
			}
			filter.getEStructuralFeatures().add(attribute);
		}

		List<EAttribute> attributeInstances = filter.getEAllAttributes();
		EObject filterInstance = model.getEFactoryInstance().create(filter);
		// set values of attributes according to configuration
		for (Attribute a : this.attributes) {
			if (a.getType() != null) {
				for (EAttribute e : attributeInstances) {
					if (e.getName().equals(a.getName())) {
						filterInstance.eSet(e,
								convertToECore(e, a.getValues().get(a.getType().equals("User") ? user : object)));
						break;

					}
				}
			}
		}

		return filterInstance;
	}

	private boolean attributeFilter(String filter, EObject model) {
		try {
			// evaluate filter on the helper model
			return !OclEvaluator.query(model, model.eClass(), filter).isViolated();
		} catch (ParserException e) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Parsing of filter failed: "
					+ e.getMessage()));
			return false;
		}
	}

	@Override
	public String getCheckID() {
		return CHECK_ID;
	}

	@Override
	public String getName() {
		return CHECK_NAME;
	}

}