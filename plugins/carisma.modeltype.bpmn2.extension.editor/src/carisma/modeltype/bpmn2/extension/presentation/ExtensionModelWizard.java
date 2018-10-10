/**
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 */
package carisma.modeltype.bpmn2.extension.presentation;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.StringTokenizer;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.common.CommonPlugin;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.XMLResource;
import org.eclipse.emf.edit.ui.provider.ExtendedImageRegistry;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;
import org.eclipse.ui.dialogs.ISelectionStatusValidator;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;
import org.eclipse.ui.model.BaseWorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.part.ISetSelectionTarget;

import carisma.modeltype.bpmn2.extension.ExtensionFactory;
import carisma.modeltype.bpmn2.extension.ExtensionPackage;
import carisma.modeltype.bpmn2.extension.presentation.util.ExtensionModelWizardHelper;
import carisma.modeltype.bpmn2.extension.provider.ExtensionEditPlugin;



/**
 * This is a simple wizard for creating a new model file.
 * <!-- begin-user-doc -->
 * <!-- end-user-doc -->
 * @generated
 */
public class ExtensionModelWizard extends Wizard implements INewWizard {
	
	/**
	 * The ID of the related help side.
	 */
	public static final String RELATEDHELP = "carisma.modeltype.bpmn2.extension.BPMN2ExtensionWizard";
	
	/**
	 * The supported extensions for created files.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List<String> FILE_EXTENSIONS =
		Collections.unmodifiableList(Arrays.asList(ExtensionEditorPlugin.INSTANCE.getString("_UI_ExtensionEditorFilenameExtensions").split("\\s*,\\s*")));

	/**
	 * A formatted list of supported file extensions, suitable for display.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final String FORMATTED_FILE_EXTENSIONS =
		ExtensionEditorPlugin.INSTANCE.getString("_UI_ExtensionEditorFilenameExtensions").replaceAll("\\s*,\\s*", ", ");

	/**
	 * This caches an instance of the model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ExtensionPackage extensionPackage = ExtensionPackage.eINSTANCE;

	/**
	 * This caches an instance of the model factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ExtensionFactory extensionFactory = this.extensionPackage.getExtensionFactory();

	/**
	 * This is the file creation page.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ExtensionModelWizardNewFileCreationPage newFileCreationPage;

	/**
	 * This is the details page.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated not
	 */
	protected ExtensioModelWizardDetailsPage detailsPage;
	
	/**
	 * This is the initial object creation page.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ExtensionModelWizardInitialObjectCreationPage initialObjectCreationPage;

	/**
	 * Remember the selection during initialization for populating the default container.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected IStructuredSelection selection;

	/**
	 * Remember the workbench during initialization.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected IWorkbench workbench;

	/**
	 * Caches the names of the types that can be created as the root object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected List<String> initialObjectNames;

	/**
	 * This just records the information.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.workbench = workbench;
		this.selection = selection;
		setWindowTitle(ExtensionEditorPlugin.INSTANCE.getString("_UI_Wizard_label"));
		setDefaultPageImageDescriptor(ExtendedImageRegistry.INSTANCE.getImageDescriptor(ExtensionEditorPlugin.INSTANCE.getImage("full/wizban/NewExtension")));
	}

	/**
	 * Returns the names of the types that can be created as the root object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected Collection<String> getInitialObjectNames() {
		if (this.initialObjectNames == null) {
			this.initialObjectNames = new ArrayList<>();
			for (EClassifier eClassifier : this.extensionPackage.getEClassifiers()) {
				if (eClassifier instanceof EClass) {
					EClass eClass = (EClass)eClassifier;
					if (!eClass.isAbstract()) {
						this.initialObjectNames.add(eClass.getName());
					}
				}
			}
			Collections.sort(this.initialObjectNames, CommonPlugin.INSTANCE.getComparator());
		}
		return this.initialObjectNames;
	}

	/**
	 * Create a new model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EObject createInitialModel() {
		
		/** Define Root Object manually */
		//EClass eClass = (EClass)extensionPackage.getEClassifier(initialObjectCreationPage.getInitialObjectName());
		EClass eClass = (EClass)this.extensionPackage.getEClassifier("ExtensionRoot");
		
		EObject rootObject = this.extensionFactory.create(eClass);
		
		return rootObject;
	}

	/**
	 * Do the work after everything is specified.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean performFinish() {
		try {
			// Remember the file.
			//
			final IFile modelFile = getModelFile();

			// Do the work within an operation.
			//
			WorkspaceModifyOperation operation =
				new WorkspaceModifyOperation() {
					@Override
					protected void execute(IProgressMonitor progressMonitor) {
						try {
							// Create a resource set
							//
							ResourceSet resourceSet = new ResourceSetImpl();

							// Get the URI of the model file.
							//
							URI fileURI = URI.createPlatformResourceURI(modelFile.getFullPath().toString(), true);

							// Create a resource for this file.
							//
							Resource resource = resourceSet.createResource(fileURI);

							// Add the initial model object to the contents.
							//
							EObject rootObject = createInitialModel();
							if (rootObject != null) {
								resource.getContents().add(rootObject);
								
								if (ExtensionModelWizard.this.detailsPage.sourceFile != null) {
									ExtensionModelWizardHelper.addExtensibleObjectsToModel(rootObject, ExtensionModelWizard.this.detailsPage.sourceFile.getLocation().toFile());
								}
							}

							// Save the contents of the resource to the file system.
							//
							Map<Object, Object> options = new HashMap<>();
							
							/** Define Encoding manually */
							//options.put(XMLResource.OPTION_ENCODING, initialObjectCreationPage.getEncoding());
							options.put(XMLResource.OPTION_ENCODING, "UTF-8");
							
							resource.save(options);
						}
						catch (Exception exception) {
							ExtensionEditorPlugin.INSTANCE.log(exception);
						}
						finally {
							progressMonitor.done();
						}
					}
				};

			getContainer().run(false, false, operation);

			// Select the new file resource in the current view.
			//
			IWorkbenchWindow workbenchWindow = this.workbench.getActiveWorkbenchWindow();
			IWorkbenchPage page = workbenchWindow.getActivePage();
			final IWorkbenchPart activePart = page.getActivePart();
			if (activePart instanceof ISetSelectionTarget) {
				final ISelection targetSelection = new StructuredSelection(modelFile);
				getShell().getDisplay().asyncExec
					(new Runnable() {
						 @Override
						public void run() {
							 ((ISetSelectionTarget)activePart).selectReveal(targetSelection);
						 }
					 });
			}

			// Open an editor on the new file.
			//
			try {
				page.openEditor
					(new FileEditorInput(modelFile),
					 this.workbench.getEditorRegistry().getDefaultEditor(modelFile.getFullPath().toString()).getId());					 	 
			}
			catch (PartInitException exception) {
				MessageDialog.openError(workbenchWindow.getShell(), ExtensionEditorPlugin.INSTANCE.getString("_UI_OpenEditorError_label"), exception.getMessage());
				return false;
			}

			return true;
		}
		catch (Exception exception) {
			ExtensionEditorPlugin.INSTANCE.log(exception);
			return false;
		}
	}
	
	@Override
	public final void setContainer(final IWizardContainer wizardContainer) {
	    if (null != wizardContainer && wizardContainer instanceof WizardDialog) {
	        WizardDialog dialog = (WizardDialog) wizardContainer;
	        dialog.setHelpAvailable(true);
	    }
	    super.setContainer(wizardContainer);
	}

	/**
	 * This is the one page of the wizard.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public class ExtensionModelWizardNewFileCreationPage extends WizardNewFileCreationPage {
		/**
		 * Pass in the selection.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		public ExtensionModelWizardNewFileCreationPage(String pageId, IStructuredSelection selection) {
			super(pageId, selection);
		}

		/**
		 * The framework calls this to see if the file is correct.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		@Override
		protected boolean validatePage() {
			if (super.validatePage()) {
				String extension = new Path(getFileName()).getFileExtension();
				if (extension == null || !FILE_EXTENSIONS.contains(extension)) {
					String key = FILE_EXTENSIONS.size() > 1 ? "_WARN_FilenameExtensions" : "_WARN_FilenameExtension";
					setErrorMessage(ExtensionEditorPlugin.INSTANCE.getString(key, new Object [] { FORMATTED_FILE_EXTENSIONS }));
					return false;
				}
				return true;
			}
			return false;
		}

		/**
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		public IFile getModelFile() {
			return ResourcesPlugin.getWorkspace().getRoot().getFile(getContainerFullPath().append(getFileName()));
		}
		
		@Override
		public final void createControl(final Composite parent) {
			setHelpAvailable(true);
			super.createControl(parent);
			PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(), RELATEDHELP);
			
		}
		
		@Override
		public final void performHelp() {
			PlatformUI.getWorkbench().getHelpSystem().displayHelp(RELATEDHELP);
		}
	}

	/**
	 * This is the page where the type of object to create is selected.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public class ExtensionModelWizardInitialObjectCreationPage extends WizardPage {
		/**
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		protected Combo initialObjectField;

		/**
		 * @generated
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 */
		protected List<String> encodings;

		/**
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		protected Combo encodingField;

		/**
		 * Pass in the selection.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		public ExtensionModelWizardInitialObjectCreationPage(String pageId) {
			super(pageId);
		}

		/**
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		@Override
		public void createControl(Composite parent) {
			Composite composite = new Composite(parent, SWT.NONE);
			{
				GridLayout layout = new GridLayout();
				layout.numColumns = 1;
				layout.verticalSpacing = 12;
				composite.setLayout(layout);

				GridData data = new GridData();
				data.verticalAlignment = GridData.FILL;
				data.grabExcessVerticalSpace = true;
				data.horizontalAlignment = GridData.FILL;
				composite.setLayoutData(data);
			}
			
			Label containerLabel = new Label(composite, SWT.LEFT);
			{
				containerLabel.setText(ExtensionEditorPlugin.INSTANCE.getString("_UI_ModelObject"));

				GridData data = new GridData();
				data.horizontalAlignment = GridData.FILL;
				containerLabel.setLayoutData(data);
			}

			this.initialObjectField = new Combo(composite, SWT.BORDER);
			{
				GridData data = new GridData();
				data.horizontalAlignment = GridData.FILL;
				data.grabExcessHorizontalSpace = true;
				this.initialObjectField.setLayoutData(data);
			}

			for (String objectName : getInitialObjectNames()) {
				this.initialObjectField.add(getLabel(objectName));
			}

			if (this.initialObjectField.getItemCount() == 1) {
				this.initialObjectField.select(0);
			}
			this.initialObjectField.addModifyListener(this.validator);

			Label encodingLabel = new Label(composite, SWT.LEFT);
			{
				encodingLabel.setText(ExtensionEditorPlugin.INSTANCE.getString("_UI_XMLEncoding"));

				GridData data = new GridData();
				data.horizontalAlignment = GridData.FILL;
				encodingLabel.setLayoutData(data);
			}
			this.encodingField = new Combo(composite, SWT.BORDER);
			{
				GridData data = new GridData();
				data.horizontalAlignment = GridData.FILL;
				data.grabExcessHorizontalSpace = true;
				this.encodingField.setLayoutData(data);
			}

			for (String encoding : getEncodings()) {
				this.encodingField.add(encoding);
			}

			this.encodingField.select(0);
			this.encodingField.addModifyListener(this.validator);

			setPageComplete(validatePage());
			setControl(composite);
		}

		/**
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		protected ModifyListener validator =
			new ModifyListener() {
				@Override
				public void modifyText(ModifyEvent e) {
					setPageComplete(validatePage());
				}
			};

		/**
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		protected boolean validatePage() {
			return getInitialObjectName() != null && getEncodings().contains(this.encodingField.getText());
		}

		/**
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		@Override
		public void setVisible(boolean visible) {
			super.setVisible(visible);
			if (visible) {
				if (this.initialObjectField.getItemCount() == 1) {
					this.initialObjectField.clearSelection();
					this.encodingField.setFocus();
				}
				else {
					this.encodingField.clearSelection();
					this.initialObjectField.setFocus();
				}
			}
		}

		/**
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		public String getInitialObjectName() {
			String label = this.initialObjectField.getText();

			for (String name : getInitialObjectNames()) {
				if (getLabel(name).equals(label)) {
					return name;
				}
			}
			return null;
		}

		/**
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		public String getEncoding() {
			return this.encodingField.getText();
		}

		/**
		 * Returns the label for the specified type name.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		protected String getLabel(String typeName) {
			try {
				return ExtensionEditPlugin.INSTANCE.getString("_UI_" + typeName + "_type");
			}
			catch(MissingResourceException mre) {
				ExtensionEditorPlugin.INSTANCE.log(mre);
			}
			return typeName;
		}

		/**
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		protected Collection<String> getEncodings() {
			if (this.encodings == null) {
				this.encodings = new ArrayList<>();
				for (StringTokenizer stringTokenizer = new StringTokenizer(ExtensionEditorPlugin.INSTANCE.getString("_UI_XMLEncodingChoices")); stringTokenizer.hasMoreTokens(); ) {
					this.encodings.add(stringTokenizer.nextToken());
				}
			}
			return this.encodings;
		}
	}

	/**
	 * The framework calls this to create the contents of the wizard.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
		@Override
	public void addPages() {
		// Create a page, set the title, and the initial model file name.
		//
		this.newFileCreationPage = new ExtensionModelWizardNewFileCreationPage("Whatever", this.selection);
		this.newFileCreationPage.setTitle(ExtensionEditorPlugin.INSTANCE.getString("_UI_ExtensionModelWizard_label"));
		this.newFileCreationPage.setDescription(ExtensionEditorPlugin.INSTANCE.getString("_UI_ExtensionModelWizard_description"));
		this.newFileCreationPage.setFileName(ExtensionEditorPlugin.INSTANCE.getString("_UI_ExtensionEditorFilenameDefaultBase") + "." + FILE_EXTENSIONS.get(0));
		addPage(this.newFileCreationPage);

		// Try and get the resource selection to determine a current directory for the file dialog.
		//
		if (this.selection != null && !this.selection.isEmpty()) {
			// Get the resource...
			//
			Object selectedElement = this.selection.iterator().next();
			if (selectedElement instanceof IResource) {
				// Get the resource parent, if its a file.
				//
				IResource selectedResource = (IResource)selectedElement;
				if (selectedResource.getType() == IResource.FILE) {
					selectedResource = selectedResource.getParent();
				}

				// This gives us a directory...
				//
				if (selectedResource instanceof IFolder || selectedResource instanceof IProject) {
					// Set this for the container.
					//
					this.newFileCreationPage.setContainerFullPath(selectedResource.getFullPath());

					// Make up a unique new name here.
					//
					String defaultModelBaseFilename = ExtensionEditorPlugin.INSTANCE.getString("_UI_ExtensionEditorFilenameDefaultBase");
					String defaultModelFilenameExtension = FILE_EXTENSIONS.get(0);
					String modelFilename = defaultModelBaseFilename + "." + defaultModelFilenameExtension;
					for (int i = 1; ((IContainer)selectedResource).findMember(modelFilename) != null; ++i) {
						modelFilename = defaultModelBaseFilename + i + "." + defaultModelFilenameExtension;
					}
					this.newFileCreationPage.setFileName(modelFilename);
				}
			}
		}
		/** Disabled 'Select root element' page 
		 * Root element will be created automatically in the createInitialModel method
		 * Encoding will be set automatically to 'UTF-8' in the performFinish method */
		//initialObjectCreationPage = new ExtensionModelWizardInitialObjectCreationPage("Whatever2");
		//initialObjectCreationPage.setTitle(ExtensionEditorPlugin.INSTANCE.getString("_UI_ExtensionModelWizard_label"));
		//initialObjectCreationPage.setDescription(ExtensionEditorPlugin.INSTANCE.getString("_UI_Wizard_initial_object_description"));
		//addPage(initialObjectCreationPage);
		
		this.detailsPage = new ExtensioModelWizardDetailsPage("Whatever2");
		this.detailsPage.setTitle(ExtensionEditorPlugin.INSTANCE.getString("_UI_ExtensionModelWizard_label"));
		this.detailsPage.setDescription(ExtensionEditorPlugin.INSTANCE.getString("_UI_ExtensionModelWizard_description"));
		addPage(this.detailsPage);
	}

	/**
	 * Get the file from the page.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public IFile getModelFile() {
		return this.newFileCreationPage.getModelFile();
	}
	
	/**
	 * Class ExtensioModelWizardDetailsPage
	 */
	protected class ExtensioModelWizardDetailsPage extends WizardPage {

		private IFile sourceFile;
		final static String extensionFilter = "bpmn2";
		
		/**
		 * @param pageName
		 */
		protected ExtensioModelWizardDetailsPage(String pageName) {
			super(pageName);
			setDescription("Choose the model file and its type");
		}

		/**
		 * 
		 */
		@Override
		public void createControl(Composite parent) {
			final Composite comp = new Composite(parent, SWT.NONE);
			
			setHelpAvailable(true);
			PlatformUI.getWorkbench().getHelpSystem().setHelp(comp, RELATEDHELP);
			
			GridLayout gl = new GridLayout(3, false);
			gl.marginWidth = gl.marginHeight = parent.getStyle() == SWT.BORDER ? 0
					: 2;
			comp.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			comp.setLayout(gl);


			GridData gd = new GridData(GridData.FILL_HORIZONTAL);
			gd.horizontalSpan = 1;
			
			new Label(comp, SWT.NONE).setText("Select the model file");

			final Text mf = new Text(comp, SWT.SINGLE | SWT.BORDER);
			gd = new GridData(GridData.FILL_HORIZONTAL);
			mf.setLayoutData(gd);
			mf.setText("");
//			mf.setEditable(false);
			
			

			Button browse = new Button(comp, SWT.PUSH);
			browse.setText("Browse...");

			browse.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
										
					final ElementTreeSelectionDialog selectionDialog = new ElementTreeSelectionDialog(comp.getShell(), new WorkbenchLabelProvider(), new BaseWorkbenchContentProvider());
					selectionDialog.setTitle("Model file Selection");
					selectionDialog.setMessage("Select the element from the tree:");
					selectionDialog.setInput(ResourcesPlugin.getWorkspace().getRoot());
					selectionDialog.setAllowMultiple(false);
				
					selectionDialog.setValidator(new ISelectionStatusValidator() {
						@Override
						public IStatus validate(Object[] selection) {
							if ( (selection != null) && (selection.length > 0)) {							
								if (selection[0] instanceof IFile ) {
									if ( !"".equals(ExtensioModelWizardDetailsPage.extensionFilter)) { 
										// "modeltype" known, but wrong fileExtension
										if ( ((IFile) selection[0]).getFullPath().toString().endsWith("." + ExtensioModelWizardDetailsPage.extensionFilter) ){
											return Status.OK_STATUS;								
										} 
										// "modeltype" known and fileExtension is right
										return new Status(IStatus.ERROR, PlatformUI.PLUGIN_ID, IStatus.ERROR, "Model file must ends with \"." + ExtensioModelWizardDetailsPage.extensionFilter +  "\"", null);
									}
									// "modeltype" not known, each file can be choosen
									return Status.OK_STATUS;
								}
							}
							return new Status(IStatus.ERROR, PlatformUI.PLUGIN_ID,
				                    IStatus.ERROR, "Select model file" , null);
						}
					});
					selectionDialog.open();
					Object[] result = selectionDialog.getResult();

					if (result != null) {
						mf.setText(""); 	// clear 
						IResource mfile = (IResource) result[0];
							try {
								setSourceFile((IFile) mfile);
								mf.setText(mfile.getFullPath().toString());
								setPageComplete(validatePage());
							} catch (ClassCastException cce) {
								ExtensionModelWizard.this.detailsPage.setErrorMessage("Must select file");
							}
					}
				}
			}); 

			setControl(comp);
		}
		
		/**
		 * @param sourceFile, set the file that has to be analyzed, e.g. model.uml
		 */
		public void setSourceFile(IFile sourceFile) {
			this.sourceFile = sourceFile;
		}

		/**
		 * @return the source file for the analysis, e.g. "model.uml"
		 */
		public IFile getSourceFile() {
			return this.sourceFile;
		}

		protected boolean validatePage() {
			if (getSourceFile() != null) {
				if (getSourceFile().toString().endsWith("." + ExtensioModelWizardDetailsPage.extensionFilter)) {
					ExtensionModelWizard.this.detailsPage.setErrorMessage(null); 
					ExtensionModelWizard.this.detailsPage.setDescription(null);
					setPageComplete(true);
					return true;					
				}
				ExtensionModelWizard.this.detailsPage.setErrorMessage("Model file is not of type "+ ExtensioModelWizardDetailsPage.extensionFilter.toUpperCase(Locale.ENGLISH));
				return false;					
			}
			setDescription("Choose the model file and its type");
			setPageComplete(false);
			return false;
		}
		
		@Override
		public final void performHelp() {
			PlatformUI.getWorkbench().getHelpSystem().displayHelp(RELATEDHELP);
		}
	}

}