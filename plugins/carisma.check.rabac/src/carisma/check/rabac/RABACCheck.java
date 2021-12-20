package carisma.check.rabac;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.ocl.ParserException;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.StateMachine;
import org.eclipse.uml2.uml.Transition;
import org.eclipse.uml2.uml.Vertex;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.statemachine.StateMachinePaths;
import carisma.ocl.OclEvaluator;
import carisma.profile.umlsec.rabac.UMLsec;
import carisma.profile.umlsec.rabac.UMLsecUtil;
import jakarta.xml.bind.JAXBContext;

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
	public boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		this.analysisHost = host;
		final var model = host.getAnalyzedModel();

		if (model.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Empty model"));
			this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
			this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
			return false;
		}

		if (model.getContents().get(0) instanceof Package) {
			final var content = (Package) model.getContents().get(0);

			final var abac = UMLsecUtil.getStereotypedElements(content, UMLsec.ABAC);
			final var abacNum = abac.size();
			if (abacNum == 0) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
						"Could not find the main CHECK_ID stereotype!"));
				this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
				this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
				return false;
			}
			if (abacNum > 1) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Found " + abacNum
						+ " main CHECK_ID stereotypes, model must only contain one!"));
				this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
				this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
				return false;
			}
			final var abacClass = abac.get(0);

			final var usersTag = UMLsecUtil.getStringValues("roles", UMLsec.ABAC, abacClass);
			if (usersTag.isEmpty()) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Roles missing!"));
				this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
				this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
				return false;
			}
			final var users = parseTag(usersTag.get(0), null, 0);
			if (users.isEmpty()) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Roles missing!"));
				this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
				this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
				return false;
			}

			final var rightsTag = UMLsecUtil.getStringValues("rights", UMLsec.ABAC, abacClass);
			if (rightsTag.isEmpty()) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Rights missing!"));
				this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
				this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
				return false;
			}

			// read saved configuration
			try {
				final var config = (RABACConfig) JAXBContext
						.newInstance(RABACConfig.class)
						.createUnmarshaller()
						.unmarshal(
								((InputFileParameter) parameters.get(PARAM_CONFIGURATION)).getValue());
				this.sessions = config.getSessions();
				this.attributes = config.getAttributes();
			} catch (final Exception e) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Error reading configuration file!"));
				this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
				this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
				return false;
			}

			if (!this.sessions.isEmpty()) {
				for (final String u : this.sessions.keySet()) {
					if (!users.contains(u)) {
						host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Loaded sessions, but user "
								+ u + " is not valid for this model!"));
						this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
						this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
						return false;
					}
				}
				host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Loaded sessions"));

				final var dsdTag = UMLsecUtil.getStringValues("dsd", UMLsec.ABAC, abacClass);
				if (dsdTag.size() > 0) {
					final var dsdEntries = dsdTag.get(0).split("\\),\\(");
					for (final String d : dsdEntries) {
						final var dsdElements = d.split(",");
						int limit;
						try {
							limit = Integer.parseInt(dsdElements[dsdElements.length - 1].replace(")", ""));
						} catch (final NumberFormatException e) {
							host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
									"Dynamic seperation of duty contains invalid syntax!"));
							this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
							this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
							return false;
						}
						for (final String u : this.sessions.keySet()) {
							var numRoles = 0;
							for (final String r : this.sessions.get(u).getSet()) {
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

				final List<StateMachine> stateMachines = UMLHelper.getAllElementsOfType(content, StateMachine.class);
				if (stateMachines.size() > 0) {
					host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, stateMachines.size()
							+ " StateMachine(s) found"));
				}

				final var abacRequire = UMLsecUtil.getStereotypedElements(content, UMLsec.ABACREQUIRE);
				if (abacRequire.isEmpty()) {
					host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "No CHECK_ID constraints found!"));
					this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
					this.analysisHost.appendLineToReport("No errors have been detected.");
					this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
					this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
					return true;
				}

				// save paths of all StateMachines
				final var pathFinder = new StateMachinePaths();
				final List<List<List<Element>>> paths = new ArrayList<>();
				for (final StateMachine m : stateMachines) {
					paths.add(pathFinder.getPaths(m, host, true));
				}

				String globalFilter = null;
				final var attributeFiltersTag = UMLsecUtil.getStringValues("attributeFilters", UMLsec.ABAC,
						abacClass);
				if (attributeFiltersTag.size() > 0) {
					if("".equals(globalFilter) || "null".equals(globalFilter)){
						attributeFiltersTag.remove(0);
					}
					else{
						globalFilter = attributeFiltersTag.get(0);
					}
				}

				for (final String u : this.sessions.keySet()) {
					host.appendLineToReport("User " + u + " has access to the following protected items:");

					final Set<Transition> validTransitions = new HashSet<>();
					// verify constraints based on all elements which contain a
					// CHECK_ID stereotype
					for (final Element e : abacRequire) {
						final var right = UMLsecUtil.getStringValues("right", UMLsec.ABACREQUIRE, e);
						if (right.isEmpty()) {
							host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, ((NamedElement) e)
									.getName() + " has no right!"));
							this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
							this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
							return false;
						}

						Set<String> roles = this.sessions.get(u).getSet();
						final var rhTag = UMLsecUtil.getStringValues("rh", UMLsec.ABAC, abacClass);
						if (rhTag.size() > 0) {
							roles = getElementsFromHierarchy(roles, rhTag.get(0));
						}

						for (final String r : roles) {
							if (rightsTag.get(0).contains("(" + r + "," + right.get(0) + ")")) {
								final var transformedModel = transformModelBasic(u,
										e instanceof Transition ? ((Transition) e).containingStateMachine().getName()
												: ((Operation) e).getClass_().getName());

								final var filter = UMLsecUtil.getStringValues("filters", UMLsec.ABACREQUIRE, e);
								if ((filter.isEmpty() || filter.get(0).equals("") || attributeFilter(filter.get(0),
										transformedModel))
										&& ((globalFilter == null) || attributeFilter(globalFilter, transformedModel))) {
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
					for (final List<List<Element>> m : paths) {
						Set<Element> states = new HashSet<>();
						var printMachine = true;
						for (final List<Element> p : m) {
							var printPath = false;
							for (final Element e : p) {
								states.add(e);
								// make sure access control happens on path
								if ((e instanceof Transition)
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
								for (final Element e : p) {
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
			this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
			this.analysisHost.appendLineToReport("No errors have been detected.");
			this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
			this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
			return true;
		}

		host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Content is not a model!"));
		this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
		this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
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
	public static Set<String> parseTag(final String tag, final String match, int side) {
		final var validNames = "[a-zA-Z0-9\\s]+";
		final var matcher = Pattern.compile(
				"\\(" + ((side == 0) && (match != null) ? match : validNames) + ","
						+ ((side == 1) && (match != null) ? match : validNames) + "+\\)").matcher(tag);
		if (match != null) {
			side = side ^ 1; //TODO: Was passiert hier?
		}
		final Set<String> elements = new HashSet<>();
		while (matcher.find()) {
			final var e = matcher.group().split(",");
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
	public static Set<String> getElementsFromHierarchy(final Set<String> elements, final String hierarchy) {
		final Set<String> newElements = new HashSet<>(elements);
		for (final String e : elements) {
			final var inheritedElements = parseTag(hierarchy, e, 1);
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
	public static Object convertToECore(final EAttribute attribute, final String value) {
		if (value != null) {
			if (attribute.getUpperBound() == -1) {
				final String elements[] = value.substring(1, value.length() - 1).split(",");
				if (attribute.getEType().getName().equals("EDouble")) {
					final EList<Double> list = new BasicEList<>();
					for (final String s : elements) {
						list.add(Double.valueOf(s));
					}
					return list;
				}
				if (attribute.getEType().getName().equals("EString")) {
					final EList<String> list = new BasicEList<>();
					Collections.addAll(list, elements);
					return list;
				}
			} else if (attribute.getEType().getName().equals("EDouble")) {
				return Double.valueOf(value);
			}
		}

		return value;
	}

	private EObject transformModelBasic(final String user, final String object) {
		final var ef = EcoreFactory.eINSTANCE;
		final var ep = EcorePackage.eINSTANCE;

		// create a helper model to hold all attributes defined in the model
		// chosen for analysis
		final var filter = ef.createEClass();
		final var model = ef.createEPackage();
		model.getEClassifiers().add(filter);

		for (final Attribute a : this.attributes) {
			final var attribute = ef.createEAttribute();
			attribute.setName(a.getName());
			attribute.setEType(ep.getEString());
			if (a.getType() != null) {
				final var value = a.getValues().get(a.getType().equals("User") ? user : object);
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

		final List<EAttribute> attributeInstances = filter.getEAllAttributes();
		final var filterInstance = model.getEFactoryInstance().create(filter);
		// set values of attributes according to configuration
		for (final Attribute a : this.attributes) {
			if (a.getType() != null) {
				for (final EAttribute e : attributeInstances) {
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

	private boolean attributeFilter(final String filter, final EObject model) {
		try {
			// evaluate filter on the helper model
			return !OclEvaluator.query(model, model.eClass(), filter).isViolated();
		} catch (final ParserException e) {
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