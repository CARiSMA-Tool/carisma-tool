/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.ui.eclipse.editors;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.fieldassist.ControlDecoration;
import org.eclipse.jface.fieldassist.FieldDecorationRegistry;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.IDetailsPage;
import org.eclipse.ui.forms.IFormPart;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;

import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.CheckReference;
import carisma.core.analysis.FloatParameter;
import carisma.core.analysis.FolderParameter;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.IntegerParameter;
import carisma.core.analysis.OutputFileParameter;
import carisma.core.analysis.StringParameter;
import carisma.core.checks.CheckDescriptor;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.ParameterType;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.ui.eclipse.CarismaGUI;

/**
 * Details page of the adf editor master/detail block.
 */
public class AdfEditorCheckDetailsPage implements IDetailsPage {

	/**
	 * The ManagedForm instance.
	 */
	private IManagedForm managedForm;

	/**
	 * The CheckReference which provides the informations for this page.
	 */
	private CheckReference input;

	/**
	 * The parent composite.
	 */
	private Composite parentComposite = null;

	/**
	 * Instance of FormToolkit to create GUI elements.
	 */
	private FormToolkit toolkit = null;

	/**
	 * Description of the Check.
	 */
	private Section descriptionSection;

	/**
	 * Parameters of the Check.
	 */
	private Section parameterSection;

	/**
	 * Sub composite of description section.
	 */
	private Composite descriptionComposite;

	/**
	 * Sub composite of parameter section.
	 */
	private Composite parameterComposite;

	/**
	 * The controller between model (analysis) and view (adf editor).
	 */
	private AdfEditorController controller;

	/**
	 * A String which contains 'Browse...'.
	 */
	private static final String BROWSE = "Browse...";

	/**
	 * Map to get the query on demand check box for a parameter.
	 */
	private HashMap<CheckParameter, Button> qodButtonMap;

	/**
	 * TODO .
	 */
	private Listener masterListener;

	// ########################################################################################
	/**
	 * Constructor.
	 * 
	 * @param controller
	 *            the corresponding AdfEditorController instance
	 * @param modifyListener
	 *            TODO
	 */
	public AdfEditorCheckDetailsPage(final AdfEditorController controller,
			final Listener listener) {
		if (controller == null) {
			throw new IllegalArgumentException(
					"Parameter 'AdfEditorController' must not be null.");
		}
		this.controller = controller;
		this.masterListener = listener;
	}

	/**
	 * Initialize method sets the managed form.
	 * 
	 * @param mform
	 *            the managedForm
	 */
	public final void initialize(final IManagedForm mform) {
		this.managedForm = mform;
	}

	/**
	 * Sets and configures the composite for the detail part.
	 * 
	 * @param parent
	 *            the parent composite
	 */
	public final void createContents(final Composite parent) {
		if (parentComposite == null) {
			parentComposite = parent;
		}
		toolkit = managedForm.getToolkit();
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 1;
		gridLayout.marginHeight = toolkit.getBorderStyle() == SWT.BORDER ? 0
				: 2;
		gridLayout.marginWidth = toolkit.getBorderStyle() == SWT.BORDER ? 0 : 2;
		parentComposite.setLayout(gridLayout);
	}

	@Override
	public void commit(final boolean arg0) {
	}

	@Override
	public void dispose() {
	}

	@Override
	public final boolean isDirty() {
		return false;
	}

	@Override
	public final boolean isStale() {
		return false;
	}

	@Override
	public final void refresh() {
	}

	@Override
	public void setFocus() {

	}

	@Override
	public final boolean setFormInput(final Object arg0) {
		return false;
	}

	/**
	 * Method builds the structure of the detail part and calls a handle method,
	 * depending on the parameter type.
	 * 
	 * @param input
	 *            the check reference, that is currently selected in the list of
	 *            checks of the MasterDetailBlock
	 */
	private void createDescriptionSection(final CheckReference input) {

		if (input != null) {
			CheckDescriptor checkDescriptor = CarismaGUI.INSTANCE
					.getCheckRegistry().getCheckDescriptor(input.getCheckID());
			if (checkDescriptor != null) {
				descriptionSection = toolkit.createSection(parentComposite,
						Section.DESCRIPTION);
				descriptionSection.marginWidth = 10;
				descriptionSection.setText(checkDescriptor.getName());
				descriptionSection.setLayoutData(new GridData(
						GridData.FILL_HORIZONTAL));
				toolkit.createCompositeSeparator(descriptionSection);

				if (descriptionComposite == null) {
					descriptionComposite = toolkit
							.createComposite(parentComposite);
				} else if (!descriptionComposite.isDisposed()) {
					descriptionComposite.dispose();
					descriptionComposite = toolkit.createComposite(
							parentComposite, SWT.NONE);
				}
				GridLayout gridLayout = new GridLayout(1, false);
				gridLayout.marginTop = 2;
				gridLayout.marginRight = 2;
				gridLayout.marginBottom = 10;
				gridLayout.marginLeft = 10;
				descriptionComposite.setLayout(gridLayout);
				descriptionComposite.setLayoutData(new GridData(
						GridData.FILL_HORIZONTAL));

				// The ID
				Label labelId = toolkit
						.createLabel(descriptionComposite, "ID:");
				setBoldFont(labelId);
				toolkit.createLabel(descriptionComposite,
						checkDescriptor.getCheckDescriptorId());

				// The description text
				Label labelDescription = toolkit.createLabel(
						descriptionComposite, "Description:");
				labelDescription.setLayoutData(new GridData(SWT.FILL, SWT.FILL,
						true, true));
				setBoldFont(labelDescription);
				Label labelDescriptionValue = toolkit.createLabel(
						descriptionComposite, checkDescriptor.getDescription(),
						SWT.WRAP);
				labelDescriptionValue.setLayoutData(new GridData(SWT.FILL,
						SWT.FILL, true, true));

				// List of preconditions
				List<String> preconditions = checkDescriptor.getRequiredKeys();
				if (preconditions.size() > 0) {
					Label labelPrecon = toolkit.createLabel(
							descriptionComposite, "Preconditions:");
					labelPrecon.setLayoutData(new GridData(SWT.FILL, SWT.FILL,
							true, true));
					setBoldFont(labelPrecon);
					for (String str : preconditions) {
						Label labelPreconValue = toolkit.createLabel(
								descriptionComposite, str);
						labelPreconValue.setLayoutData(new GridData(SWT.FILL,
								SWT.FILL, true, true));
					}
				}

				// List of postconditions
				List<String> postconditions = checkDescriptor.getProvidedKeys();
				if (postconditions.size() > 0) {
					Label labelPostcon = toolkit.createLabel(
							descriptionComposite, "Postconditions:");
					labelPostcon.setLayoutData(new GridData(SWT.FILL, SWT.FILL,
							true, true));
					setBoldFont(labelPostcon);
					for (String str : postconditions) {
						Label labelPostconValue = toolkit.createLabel(
								descriptionComposite, str);
						labelPostconValue.setLayoutData(new GridData(SWT.FILL,
								SWT.FILL, true, true));
					}
				}
			} else {
				toolkit.createLabel(parentComposite,
						"ERROR: CheckDescriptor was not found!");
			}
		} else {
			toolkit.createLabel(parentComposite,
					"ERROR: CheckDescriptor is null");
		}
	}

	/**
	 * Method builds the structure of the detail part and calls a handle method,
	 * depending on the parameter type.
	 * 
	 * @param checkReference
	 *            is a reference to the check, that is currently selected in
	 *            MasterDetailBlock
	 */
	private void createParameterSection(final CheckReference checkReference) {
		parameterSection = toolkit.createSection(parentComposite,
				Section.DESCRIPTION);

		// Create a new composite if not a existent or the page has been
		// switched
		if (parameterComposite == null) {
			parameterComposite = toolkit.createComposite(parentComposite);
		} else if (!parameterComposite.isDisposed()) {
			parameterComposite.dispose();
			parameterComposite = toolkit.createComposite(parentComposite,
					SWT.NONE);
		}

		// If there are no parameters, the parameter section will be empty
		if (checkReference.getParameters().size() > 0) {
			qodButtonMap = new HashMap<CheckParameter, Button>();

			parameterSection.marginWidth = 10;
			parameterSection.setText("Parameters");
			parameterSection.setLayoutData(new GridData(
					GridData.FILL_HORIZONTAL));
			toolkit.createCompositeSeparator(parameterSection);

			GridLayout singleParameterGridLayout = new GridLayout(3, false);
			singleParameterGridLayout.marginHeight = 2;
			singleParameterGridLayout.marginRight = 10;
			singleParameterGridLayout.marginLeft = 10;
			parameterComposite.setLayout(singleParameterGridLayout);
			parameterComposite.setLayoutData(new GridData(
					GridData.FILL_HORIZONTAL));

			// Create the header labels
			Label labelHeaderName = toolkit.createLabel(parameterComposite,
					"Name");
			setBoldFont(labelHeaderName);
			labelHeaderName.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER,
					false, false));

			Label labelHeaderValue = toolkit.createLabel(parameterComposite,
					"Value");
			setBoldFont(labelHeaderValue);
			labelHeaderValue.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER,
					true, false));

			Label labelHeaderQod = toolkit.createLabel(parameterComposite,
					"Ask?");
			setBoldFont(labelHeaderQod);
			labelHeaderQod.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER,
					false, false));

			boolean requiredParametersExistent = false;

			for (final CheckParameter checkParameter : checkReference
					.getParameters()) {
				// Parameter label
				String parameterName = checkParameter.getDescriptor().getName();
				parameterName = parameterName.substring(0, 1).toUpperCase(
						Locale.ENGLISH)
						+ parameterName.substring(1);
				if (!checkParameter.getDescriptor().isOptional()) {
					requiredParametersExistent = true;
					parameterName += "*";
				}
				parameterName += ":";
				final Label labelName = toolkit.createLabel(parameterComposite,
						parameterName);
				//Bug #1518: Wie kann man die Position des Tooltips verändern?
				//http://stackoverflow.com/questions/11375250/set-tooltip-text-at-a-particular-location
				labelName.setToolTipText(checkParameter.getDescriptor()
						.getDescription());
				labelName.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER,
						false, false));

				// Composite to place the parameter input elements
				final Composite singleParameterComposite = toolkit
						.createComposite(parameterComposite);
				GridLayout valueGridLayout = new GridLayout(2, false);
				valueGridLayout.marginLeft = 2;
				singleParameterComposite.setLayout(valueGridLayout);
				singleParameterComposite.setLayoutData(new GridData(SWT.FILL,
						SWT.CENTER, true, false));

				// Query on demand check box
				final Button buttonQod = toolkit.createButton(
						parameterComposite, "", SWT.CHECK);
				buttonQod.setToolTipText("Ask for parameter at runtime?");
				buttonQod.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER,
						false, false));
				buttonQod.setSelection(checkParameter.isQueryOnDemand());
				buttonQod.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(final SelectionEvent e) {
						controller.setParameterQod(checkParameter,
								buttonQod.getSelection());
					}
				});
				buttonQod.addListener(SWT.Selection, this.masterListener);
				qodButtonMap.put(checkParameter, buttonQod);

				// Handle different check parameter
				if (checkParameter.getDescriptor().getType()
						.equals(ParameterType.STRING)) {
					handleTextParameter(singleParameterComposite,
							checkParameter);
				} else if (checkParameter.getDescriptor().getType()
						.equals(ParameterType.INTEGER)) {
					handleIntParameter(singleParameterComposite, checkParameter);
				} else if (checkParameter.getDescriptor().getType()
						.equals(ParameterType.FLOAT)) {
					handleFloatParameter(singleParameterComposite,
							checkParameter);
				} else if (checkParameter.getDescriptor().getType()
						.equals(ParameterType.BOOLEAN)) {
					handleBooleanParameter(singleParameterComposite,
							checkParameter);
				} else if (checkParameter.getDescriptor().getType()
						.equals(ParameterType.INPUTFILE)) {
					handleInputFileParameter(singleParameterComposite,
							checkParameter);
				} else if (checkParameter.getDescriptor().getType()
						.equals(ParameterType.FOLDER)) {
					handleFolderParameter(singleParameterComposite,
							checkParameter);
				} else if (checkParameter.getDescriptor().getType()
						.equals(ParameterType.OUTPUTFILE)) {
					handleOutputParameter(singleParameterComposite,
							checkParameter);
				}
			}

			// Create label to explain the "*"
			if (requiredParametersExistent) {
				GridData horSpanGridData = new GridData(
						GridData.FILL_HORIZONTAL);
				horSpanGridData.horizontalSpan = 4;
				Label labelOptional = toolkit.createLabel(parameterComposite,
						"* Parameter is required");
				setItalicFont(labelOptional);
				labelOptional.setAlignment(SWT.RIGHT);
				labelOptional.setLayoutData(horSpanGridData);
			}

			parentComposite.layout();
		}
	}

	@Override
	public final void selectionChanged(final IFormPart part,
			final ISelection selection) {
		if (selection != null && !selection.isEmpty()) {
			IStructuredSelection ssel = (IStructuredSelection) selection;
			if (ssel.size() == 1) {
				input = (CheckReference) ssel.getFirstElement();
				if (descriptionSection != null) {
					descriptionSection.dispose();
				}
				if (parameterSection != null) {
					parameterSection.dispose();
				}
				createDescriptionSection(input);
				createParameterSection(input);
				parentComposite.layout(); // refresh
			}
		} else {
			createDescriptionSection(null);
		}
	}

	/**
	 * Handles Text parameter.
	 * 
	 * @param comp
	 *            the composite for the parameter input elements
	 * @param checkParameter
	 *            the checkParameter to handle
	 */
	private void handleTextParameter(final Composite comp,
			final CheckParameter checkParameter) {
		final Text text = toolkit.createText(comp, "", SWT.SINGLE);
		GridData layoutData = new GridData(SWT.FILL, SWT.TOP, true, false);
		layoutData.horizontalSpan = 2;
		text.setLayoutData(layoutData);

		String value = ((StringParameter) checkParameter).getValue();
		if (value != null && !value.isEmpty()) {
			text.setText(((StringParameter) checkParameter).getValue());
		}

		text.setToolTipText(checkParameter.getDescriptor().getDescription());
		text.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				controller.setParameter(checkParameter, text.getText());
				if (text.getText().isEmpty()) {
					if (!checkParameter.getDescriptor().isOptional()) {
						controller.setParameterQod(checkParameter, true);
					}
				} else {
					controller.setParameterQod(checkParameter, false);
				}
				qodButtonMap.get(checkParameter).setSelection(
						checkParameter.isQueryOnDemand());
			}
		});
		text.addListener(SWT.Modify, this.masterListener);
	}

	/**
	 * Handles Integer parameter.
	 * 
	 * @param comp
	 *            the composite for the parameter input elements
	 * @param checkParameter
	 *            the checkParameter to handle
	 */
	private void handleIntParameter(final Composite comp,
			final CheckParameter checkParameter) {
		final String errorText = "Please insert an integer value!";

		final Text text = toolkit.createText(comp, "", SWT.SINGLE);
		GridData layoutData = new GridData(SWT.FILL, SWT.TOP, true, false);
		layoutData.horizontalSpan = 2;
		text.setLayoutData(layoutData);

		int value = ((IntegerParameter) checkParameter).getValue();
		text.setText(String.valueOf(value));
		text.setToolTipText(checkParameter.getDescriptor().getDescription());

		final ControlDecoration decoration = new ControlDecoration(text,
				SWT.LEFT | SWT.TOP);
		Image contpro = FieldDecorationRegistry.getDefault()
				.getFieldDecoration(FieldDecorationRegistry.DEC_ERROR)
				.getImage();
		decoration.setImage(contpro);
		decoration.hide();

		text.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				try {
					controller.setParameter(checkParameter,
							Integer.parseInt(text.getText()));
					decoration.hide();
				} catch (Exception exc) {
					controller.setParameter(checkParameter,
							Integer.parseInt("0"));
					decoration.show();
					decoration.showHoverText(errorText);
				}
				if (text.getText().isEmpty()) {
					if (!checkParameter.getDescriptor().isOptional()) {
						controller.setParameterQod(checkParameter, true);
					}
				} else {
					controller.setParameterQod(checkParameter, false);
				}
				qodButtonMap.get(checkParameter).setSelection(
						checkParameter.isQueryOnDemand());
			}
		});

		text.addListener(SWT.Verify, new Listener() {
			@Override
			public void handleEvent(final Event e) {
				String newCharacter = e.text;
				String newText = applyEventToText(text, e);
				try {
					if (!newCharacter.equals("") && !newCharacter.equals("-")) {
						Integer.parseInt(newCharacter);
					}
					if (!newText.equals("")) {
						Integer.parseInt(newText);
					}
					decoration.hide();
				} catch (Exception exc) {
					e.doit = false;
					decoration.show();
					decoration.showHoverText(errorText);
				}
			}
		});

		text.addFocusListener(new FocusListener() {
			@Override
			public void focusGained(final FocusEvent e) {
			}

			@Override
			public void focusLost(final FocusEvent e) {
				decoration.hide();
			}
		});
		
		text.addListener(SWT.Modify, masterListener);
	}

	/**
	 * Handles Float parameter.
	 * 
	 * @param comp
	 *            the composite for the parameter input elements
	 * @param checkParameter
	 *            the checkParamter to handle
	 */
	private void handleFloatParameter(final Composite comp,
			final CheckParameter checkParameter) {
		final String errorText = "Please insert a float value!";

		final Text text = toolkit.createText(comp, "", SWT.SINGLE);
		GridData layoutData = new GridData(SWT.FILL, SWT.TOP, true, false);
		layoutData.horizontalSpan = 2;
		text.setLayoutData(layoutData);

		float value = ((FloatParameter) checkParameter).getValue();
		text.setText(String.valueOf(value));

		text.setToolTipText(checkParameter.getDescriptor().getDescription());
		final ControlDecoration decoration = new ControlDecoration(text,
				SWT.LEFT | SWT.TOP);
		Image contpro = FieldDecorationRegistry.getDefault()
				.getFieldDecoration(FieldDecorationRegistry.DEC_ERROR)
				.getImage();
		decoration.setImage(contpro);
		decoration.hide();

		text.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				try {
					controller.setParameter(checkParameter,
							Float.parseFloat(text.getText()));
					controller.setParameterQod(checkParameter, false);
					decoration.hide();
				} catch (Exception exc) { // e.g. field is empty
					controller.setParameter(checkParameter,
							Float.parseFloat("0"));
					if (!checkParameter.getDescriptor().isOptional()) {
						controller.setParameterQod(checkParameter, true);
					}
					decoration.show();
					decoration.showHoverText(errorText);
				}
				qodButtonMap.get(checkParameter).setSelection(
						checkParameter.isQueryOnDemand());
			}
		});

		text.addListener(SWT.Verify, new Listener() {
			@Override
			public void handleEvent(final Event e) {
				e.text = e.text.replace(",", ".");
				String newCharacter = e.text;
				String newText = applyEventToText(text, e);
				try {
					if (!newCharacter.equals("") && !newCharacter.equals("-")
							&& !newCharacter.equals(".")) {
						Float.parseFloat(newCharacter);
					}
					if (!newText.equals("")) {
						float newFloat = Float.parseFloat(newText);
						if (newFloat != 0) {
							if ((newFloat > Float.MAX_VALUE || newFloat < Float.MIN_VALUE)
									&& ((newFloat * -1) > Float.MAX_VALUE || (newFloat * -1) < Float.MIN_VALUE)) {
								throw new NumberFormatException(
										"Float out of range");
							}
						}
					}
					decoration.hide();
				} catch (Exception exc) {
					e.doit = false;
					decoration.show();
					decoration.showHoverText(errorText);
				}
			}
		});

		text.addFocusListener(new FocusListener() {
			@Override
			public void focusGained(final FocusEvent e) {
			}

			@Override
			public void focusLost(final FocusEvent e) {
				decoration.hide();
			}
		});
		
		text.addListener(SWT.Modify, masterListener);
	}

	/**
	 * Handles Boolean parameter.
	 * 
	 * @param comp
	 *            the composite for the parameter input elements
	 * @param checkParameter
	 *            the checkParamter to handle
	 */
	private void handleBooleanParameter(final Composite comp,
			final CheckParameter checkParameter) {
		Button buttonTrue = toolkit.createButton(comp, "true", SWT.RADIO);
		buttonTrue.setToolTipText(checkParameter.getDescriptor()
				.getDescription());
		Button buttonFalse = toolkit.createButton(comp, "false", SWT.RADIO);
		buttonFalse.setToolTipText(checkParameter.getDescriptor()
				.getDescription());
		if (((BooleanParameter) checkParameter).getValue()) {
			buttonTrue.setSelection(true);
			buttonFalse.setSelection(false);
		} else {
			buttonTrue.setSelection(false);
			buttonFalse.setSelection(true);
		}
		buttonTrue.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				controller.setParameter(checkParameter, true);
				controller.setParameterQod(checkParameter, false);
				qodButtonMap.get(checkParameter).setSelection(false);
			}
		});
		buttonFalse.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				controller.setParameter(checkParameter, false);
				controller.setParameterQod(checkParameter, false);
				qodButtonMap.get(checkParameter).setSelection(false);
			}
		});
	}

	/**
	 * Handles InputFile parameter.
	 * 
	 * @param comp
	 *            the composite for the parameter input elements
	 * @param checkParameter
	 *            the checkParamter to handle
	 */
	private void handleInputFileParameter(final Composite comp,
			final CheckParameter checkParameter) {
		final Text text = toolkit.createText(comp, "");
		text.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
		text.setEditable(false);
		text.setToolTipText(checkParameter.getDescriptor().getDescription());

		Button browse = toolkit.createButton(comp,
				AdfEditorCheckDetailsPage.BROWSE, SWT.PUSH);
		browse.setToolTipText("Browse for an\ninput file");
		browse.setLayoutData(new GridData(SWT.NONE, SWT.TOP, false, false));

		final ControlDecoration decoration = new ControlDecoration(text,
				SWT.LEFT | SWT.TOP);
		Image contpro = FieldDecorationRegistry.getDefault()
				.getFieldDecoration(FieldDecorationRegistry.DEC_ERROR)
				.getImage();
		decoration.setImage(contpro);
		decoration.hide();

		// Initially check if the file can be read
		final String parameterValue;
		if (((InputFileParameter) checkParameter).getValue() != null) {
			if (((InputFileParameter) checkParameter).getValue().canRead()) {
				parameterValue = ((InputFileParameter) checkParameter)
						.getValue().toString();
			} else {
				parameterValue = "";
				decoration.show();
				decoration.setShowHover(true);
				decoration
						.showHoverText("Invalid input file.\nFully qualified path is needed and has to exist.");
			}
		} else {
			parameterValue = "";
		}
		text.setText(parameterValue);

		// Hide decoration if focus has been lost
		text.addFocusListener(new FocusListener() {
			@Override
			public void focusGained(final FocusEvent e) {
			}

			@Override
			public void focusLost(final FocusEvent e) {
				decoration.hide();
			}
		});
		
		text.addListener(SWT.Modify, masterListener);

		// Open file dialog
		browse.addSelectionListener(new SelectionAdapter() {
			private FileDialog fileDialog = null;

			public void widgetSelected(final SelectionEvent e) {
				fileDialog = new FileDialog(comp.getShell());
				fileDialog.setText("Input File Selection");
				if (!text.getText().isEmpty()) {
					fileDialog.setFilterPath(text.getText());
				} else {
					fileDialog.setFilterPath(ResourcesPlugin.getWorkspace()
							.getRoot().getLocation().toOSString());
				}
				String path = fileDialog.open();
				if (path != null) {
					File inputFile = new File(path);
					if (inputFile.canRead()) {
						controller.setParameter(checkParameter, inputFile);
						decoration.hide();
						text.setText(inputFile.getPath());
						text.update();
						controller.setParameterQod(checkParameter, false);
						qodButtonMap.get(checkParameter).setSelection(false);
					} else {
						controller.setParameter(checkParameter, null);
						decoration.show();
						decoration.setShowHover(true);
						decoration
								.showHoverText("Invalid input file.\nFully qualified path is needed and has to exist.");
						text.setText("");
						text.update();
						if (!checkParameter.getDescriptor().isOptional()) {
							controller.setParameterQod(checkParameter, true);
							qodButtonMap.get(checkParameter).setSelection(true);
						}
					}
				}
			}
		});
	}

	/**
	 * Handles Folder parameter.
	 * 
	 * @param comp
	 *            the composite for the parameter input elements
	 * @param checkParameter
	 *            the checkParamter to handle
	 */
	private void handleFolderParameter(final Composite comp,
			final CheckParameter checkParameter) {
		final String folderValue;
		if (((FolderParameter) checkParameter).getValue() != null) {
			folderValue = ((FolderParameter) checkParameter).getValue()
					.getPath();
		} else {
			folderValue = "";
		}

		final Text text = toolkit.createText(comp, folderValue);
		text.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
		text.setEditable(false);
		text.setToolTipText(checkParameter.getDescriptor().getDescription());

		final ControlDecoration decoration = new ControlDecoration(text,
				SWT.LEFT | SWT.TOP);
		Image contpro = FieldDecorationRegistry.getDefault()
				.getFieldDecoration(FieldDecorationRegistry.DEC_ERROR)
				.getImage();
		decoration.setImage(contpro);
		decoration.hide();

		text.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				File inputFile = new File(text.getText());
				if (!inputFile.canRead() || !inputFile.isDirectory()) {
					controller.setParameter(checkParameter, null);
					decoration.show();
					decoration.setShowHover(true);
					decoration
							.showHoverText("Invalid input folder.\nFully qualified path is needed and has to exist.");
				} else {
					controller.setParameter(checkParameter, inputFile);
					decoration.hide();
				}
			}
		});

		text.addFocusListener(new FocusListener() {
			@Override
			public void focusGained(final FocusEvent e) {
			}

			@Override
			public void focusLost(final FocusEvent e) {
				decoration.hide();
			}
		});
		
		text.addListener(SWT.Modify, masterListener);

		Button folderOpen = toolkit.createButton(comp,
				AdfEditorCheckDetailsPage.BROWSE, SWT.PUSH);
		folderOpen.setLayoutData(new GridData(SWT.NONE, SWT.TOP, false, false));
		folderOpen.setToolTipText("Browse for a folder");

		folderOpen.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				DirectoryDialog dirDialog = new DirectoryDialog(comp.getShell());
				dirDialog.setText("Folder Selection");
				if (!text.getText().isEmpty()) {
					dirDialog.setFilterPath(text.getText());
				} else {
					dirDialog.setFilterPath(ResourcesPlugin.getWorkspace()
							.getRoot().getLocation().toOSString());
				}
				String result = dirDialog.open();
				if (result != null && !result.isEmpty()) {
					File outputFile = new File(result);
					controller.setParameter(checkParameter, outputFile);
					text.setText(outputFile.getPath());
					text.update();
					controller.setParameterQod(checkParameter, false);
					qodButtonMap.get(checkParameter).setSelection(false);
				}
			}
		});
	}

	/**
	 * Handles Output parameter.
	 * 
	 * @param comp
	 *            the composite for the parameter input elements
	 * @param checkParameter
	 *            the checkParamter to handle
	 */
	private void handleOutputParameter(final Composite comp,
			final CheckParameter checkParameter) {

		final String outputFileValue;
		if (((OutputFileParameter) checkParameter).getValue() != null) {
			outputFileValue = (((OutputFileParameter) checkParameter)
					.getValue()).getPath();
		} else {
			outputFileValue = "";
		}

		final Text text = toolkit.createText(comp, outputFileValue);
		text.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
		text.setEditable(false);
		text.setToolTipText(checkParameter.getDescriptor().getDescription());

		Button outputFileOpen = toolkit.createButton(comp,
				AdfEditorCheckDetailsPage.BROWSE, SWT.PUSH);
		outputFileOpen.setLayoutData(new GridData(SWT.NONE, SWT.TOP, false,
				false));
		outputFileOpen.setToolTipText("Browse for an\noutput file");

		final ControlDecoration decoration = new ControlDecoration(text,
				SWT.LEFT | SWT.TOP);
		final Image errorImage = FieldDecorationRegistry.getDefault()
				.getFieldDecoration(FieldDecorationRegistry.DEC_ERROR)
				.getImage();
		decoration.setImage(errorImage);
		decoration.setShowHover(true);
		decoration.hide();

		text.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				try {
					if (!text.getText().isEmpty()) {
						controller.setParameter(checkParameter,
								new File(text.getText()));
					} else {
						controller.setParameter(checkParameter, null);
					}
					if (((OutputFileParameter) checkParameter)
							.isInsertedValueValid()) {
						decoration.hide();
					} else {
						decoration.show();
						decoration
								.showHoverText("Invalid output file.\nFully qualified path is needed and has to exist.");
					}
				} catch (SWTException exc) {
					Logger.log(LogLevel.ERROR, "", exc);
				}
			}
		});

		text.addFocusListener(new FocusListener() {
			@Override
			public void focusGained(final FocusEvent e) {
			}

			@Override
			public void focusLost(final FocusEvent e) {
				decoration.hide();
			}
		});
		
		text.addListener(SWT.Modify, masterListener);

		outputFileOpen.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				FileDialog fDialog = new FileDialog(comp.getShell(), SWT.SAVE);
				fDialog.setText("Output File Selection");
				if (!text.getText().isEmpty()) {
					fDialog.setFilterPath(text.getText());
				} else {
					fDialog.setFilterPath(ResourcesPlugin.getWorkspace()
							.getRoot().getLocation().toOSString());
				}
				String selection = fDialog.open();
				if (selection != null && !selection.isEmpty()) {
					controller
							.setParameter(checkParameter, new File(selection));
					text.setText(selection);
					text.update();
					controller.setParameterQod(checkParameter, false);
					qodButtonMap.get(checkParameter).setSelection(false);
				}
			}
		});
	}

	/**
	 * Applies a modify event to a text and returns the changed text.
	 * 
	 * @param text
	 *            The text element whose content is used to apply the event
	 * @param event
	 *            The event to apply
	 * @return The possible new content of the text element
	 */
	private String applyEventToText(final Text text, final Event event) {
		String oldTextBefore = text.getText(0, event.start - 1);
		String oldTextAfter = text.getText(event.end, text.getText().length());
		String returnText = oldTextBefore + event.text + oldTextAfter;
		return returnText;
	}

	/**
	 * Sets the font of a label to a bold font.
	 * 
	 * @param label
	 *            the label which gets the bold font
	 */
	private void setBoldFont(final Label label) {
		FontData[] boldFont = label.getFont().getFontData();
		for (FontData font : boldFont) {
			font.setStyle(SWT.BOLD);
		}
		label.setFont(new Font(label.getParent().getDisplay(), boldFont));
	}

	/**
	 * Sets the font of a label to a bold font.
	 * 
	 * @param label
	 *            the label which gets the bold font
	 */
	private void setItalicFont(final Label label) {
		FontData[] italicFont = label.getFont().getFontData();
		for (FontData font : italicFont) {
			font.setStyle(SWT.ITALIC);
		}
		label.setFont(new Font(label.getParent().getDisplay(), italicFont));
	}
}
