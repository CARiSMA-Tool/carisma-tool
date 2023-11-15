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
package carisma.template.core;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.pde.core.plugin.IPluginBase;
import org.eclipse.pde.core.plugin.IPluginElement;
import org.eclipse.pde.core.plugin.IPluginExtension;
import org.eclipse.pde.core.plugin.IPluginModelFactory;
import org.eclipse.pde.core.plugin.IPluginReference;
import org.eclipse.pde.ui.templates.OptionTemplateSection;
import org.eclipse.pde.ui.templates.PluginReference;
import org.eclipse.pde.ui.templates.TemplateOption;

import carisma.core.Carisma;
import carisma.modeltype.uml2.ModeltypeUML2Activator;
import carisma.template.main.Activator;
import carisma.template.main.ShareWizardData;


/**
 * 
 * @author Berghoff, Benjamin
 * 
 * This class handles the creation of the CARiSMA core check wizard and the check itself. 
 *
 */

public class DummyCheckSection extends OptionTemplateSection {
	
	
	/*
	 * Add variables which are used to setup the check and as variable names in the Template files
	 */
	/** Unique name for publisher option.*/
	private static final String KEY_PUBLISHER_NAME = "publisherName";
	/** Unique name for check description option.*/
	private static final String KEY_CHECK_DESCRIPTION = "checkDescription";
	/** Unique name for publisher option.*/
	private static final String KEY_CHECK_NAME = "checkName";
	/** Unique name for model type option.*/
	private static final String KEY_TARGET_MODEL_TYPE = "targetModelType";
	/** Unique name for class name option.*/
	private static final String KEY_CLASS_NAME = "className";
	
	/** Constant String for the Attribute 'name'. */
	private static final String NAME = "name";
	
// 	private BooleanOption menuActions;
	
	/** Public constructor.
	 * Sets the page count to one and creates all options.
	 */
	public DummyCheckSection() {
		setPageCount(1);
		createOptions();
	}

	
	@Override
	public final void addPages(final Wizard wizard) {
		WizardPage page = createPage(0, "");
		page.setTitle("Check Wizard");
		page.setDescription("This Wizard allows you to setup your Check for the CARiSMA Tool properly.");
		wizard.addPage(page);
		validatePage();
		markPagesAdded();
	}
	
	
	/**
	 * This method adds options to a given wizard page.
	 */
	private void createOptions() {
		addOption(KEY_PLUGIN_ID, "Check ID", "carisma.check.samplepackage", 0);
		addOption(KEY_PACKAGE_NAME, "Package Name", "carisma.check.samplepackage", 0);
		addOption(KEY_CLASS_NAME, "Class Name", "Check", 0);
		addBlankField(0);
		TemplateOption checkName = addOption(KEY_CHECK_NAME, "Check Name", "generated-check", 0);
		checkName.setRequired(true);
		addOption(KEY_CHECK_DESCRIPTION, "Check Description", "Prints the content of a UML2 model to the console", 0);
		addOption(KEY_PUBLISHER_NAME, "Publisher", "[Enter publisher]", 0);
		addOption(KEY_TARGET_MODEL_TYPE, "Target Model Type", "UML2", 0);
		ShareWizardData.setPackageName(getStringOption(KEY_PACKAGE_NAME));
		
		// menuActions = (BooleanOption)addOption("MENU_ACTIONS", "Do you wish to create an entry into the popup menu? ", true, 0);
	}
	
	/** If any required option is empty the pageComplete flag of page 0 will be set to false.
	 * 
	 */
	private void validatePage() {
		for (TemplateOption option : getOptions(0)) {
			if (option.isRequired() && option.isEmpty()) {
				this.getPage(0).setPageComplete(false);
				return;
			}
		}
	}
	
	
	@Override
	public final void validateOptions(final TemplateOption source) {
		if (source.getName().equals(KEY_PACKAGE_NAME) && !source.getValue().toString().startsWith("carisma.check.")) {
			this.getPage(0).setErrorMessage("The package name should be something like \"carisma.check.CHECK_NAME\".");
			this.getPage(0).setPageComplete(false);
		} else {
			ShareWizardData.setPackageName(getStringOption(KEY_PACKAGE_NAME));
			super.validateOptions(source);
		}
	}

	@Override
	public final String getUsedExtensionPoint() {
		return Carisma.EXTENSION_POINT_CARISMA_CARISMACHECK;
	}
	
	@Override
	public final IPluginReference[] getDependencies(final String schemaVersion) {
		return new IPluginReference[] {new PluginReference("org.eclipse.ui",  //$NON-NLS-1$
				null, 0), new PluginReference("org.eclipse.core.runtime", null, 0),
				new PluginReference(ModeltypeUML2Activator.PLUGIN_ID, null, 0),
				new PluginReference("org.eclipse.core.resources", null, 0),
				new PluginReference("org.eclipse.uml2.uml.resources", null, 0)
				};
	}

	@Override
	public final String[] getNewFiles() {
		
		return new String[0];
	}

	@Override
	public final String getSectionId() {
		
		return "checktemplate";
	}

	@Override
	protected final boolean isOkToCreateFolder(final File sourceFolder) {
		if (sourceFolder.getName().equals(".svn")) {
			return false;
		}
		return true;
	}

	@Override
	protected final URL getInstallURL() {
		return Activator.getiNSTANCE().getBundle().getEntry("/");
	}

	@Override
	protected final ResourceBundle getPluginResourceBundle() {
		return Platform.getResourceBundle(Activator.getiNSTANCE().getBundle());
	}

	@Override
	protected final void updateModel(final IProgressMonitor monitor) throws CoreException {
		IPluginModelFactory modelFactory = this.model.getPluginFactory();
		IPluginBase plugin = this.model.getPluginBase();
		//------------ \begin{IPluginExtension} -------------------

		IPluginExtension analysisExtension = createExtension(Carisma.EXTENSION_POINT_CARISMA_CARISMACHECK, true);
		IPluginElement element = modelFactory.createElement(plugin);
		IPluginElement parameter1 = modelFactory.createElement(plugin); 
		IPluginElement parameter2 = modelFactory.createElement(plugin); 


		element.setName("check");
		element.setAttribute("id", getStringOption(KEY_PLUGIN_ID));
		if ("".equals(getStringOption(KEY_CHECK_NAME))) {
			element.setAttribute(NAME, getStringOption(KEY_CLASS_NAME));
		} else {
			element.setAttribute(NAME, getStringOption(KEY_CHECK_NAME));
		}
		element.setAttribute("description", getStringOption(KEY_CHECK_DESCRIPTION));
		element.setAttribute("publisher", getStringOption(KEY_PUBLISHER_NAME));
		element.setAttribute("targetModelType", getStringOption(KEY_TARGET_MODEL_TYPE));
		element.setAttribute("implementingClass", getStringOption(KEY_PACKAGE_NAME) + "." + getStringOption(KEY_CLASS_NAME));


		parameter1.setName("parameter");
		parameter1.setAttribute("id", "carisma.check.dummy.parameter1");
		parameter1.setAttribute("optional", "false");
		parameter1.setAttribute("type", "String");
		parameter1.setAttribute(NAME, "bb a test string");
		parameter1.setAttribute("description", "e.g. your name");

		parameter2.setName("parameter");
		parameter2.setAttribute("id", "carisma.check.dummy.parameter2");
		parameter2.setAttribute("optional", "true");
		parameter2.setAttribute("type", "int");
		parameter2.setAttribute("defaultValue", "17");
		parameter2.setAttribute(NAME, "bb a number");
		parameter2.setAttribute("description", "possible values: 0, 1, or 2");
		
		
		element.add(parameter1);
		element.add(parameter2);
	
		
		analysisExtension.add(element);
		
		plugin.add(analysisExtension);
		
		// ----------- \end{IPluginExtension} ---------------------------------
		
/*		if(menuActions.isSelected()){
			
			// ---------------------  \begin{IPluginExtension} --------------------
			IPluginExtension actionMenuExtension = createExtension("org.eclipse.ui.popupMenus", true);
			IPluginElement popupElement = modelFactory.createElement(plugin);
			IPluginElement actionElement = modelFactory.createElement(plugin);
			
			popupElement.setName("objectContribution");
			popupElement.setAttribute("id", "carisma.check.dummy.contribution1");
			popupElement.setAttribute("objectClass", "org.eclipse.core.resources.IFile");
			
			actionElement.setName("action");
			actionElement.setAttribute("id", "carisma.check.dummy.RunDummy");
			actionElement.setAttribute("label", "Run Dummy Check");
			actionElement.setAttribute("class", getStringOption(KEY_PACKAGE_NAME)+ ".RunDummyCheck");
			actionElement.setAttribute("menubarPath", "carisma.core.menu1/group1");
			actionElement.setAttribute("enablesFor", "1");
			
			popupElement.add(actionElement);
			actionMenuExtension.add(popupElement);
			
			plugin.add(actionMenuExtension);
			
			// ------------------ \end{IPluginExtension} -----------------
			
			
			
			// ---------------------  \begin{IPluginExtension} --------------------
			IPluginExtension helpExtension = createExtension("org.eclipse.help.toc", true);
			IPluginElement help = modelFactory.createElement(plugin);
			
			help.setName("toc");
			help.setAttribute("file", "help/toc.xml");
			help.setAttribute("primary", "true");
			
			helpExtension.add(help);
			plugin.add(helpExtension);
			// ------------------ \end{IPluginExtension} -----------------
			
		}
		*/
	}
	@Override
	public final URL getTemplateLocation() {
		try {
			List<String> candidates = getDirectoryCandidates();
			for (String candidate : candidates) {
				if (Activator.getiNSTANCE().getBundle().getEntry(candidate) != null) {
					return new URL(getInstallURL(), candidate);
				} 
				super.getTemplateLocation();
			}
		} catch (MalformedURLException e) {
		    Logger.getLogger(DummyCheckSection.class.getName()).log(Level.WARNING, "Malformed URL", e);
		}
		return null;
	}
	
	private List<String> getDirectoryCandidates() {
		double version = getTargetVersion();
		List<String> result = new ArrayList<>();
		if (version >= 3.6) { 
			result.add("templates_3.6" + "/" + getSectionId() + "/"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		if (version >= 3.5) {
			result.add("templates_3.5" + "/" + getSectionId() + "/"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		if (version >= 3.4) {
			result.add("templates_3.4" + "/" + getSectionId() + "/"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		if (version >= 3.3) {
			result.add("templates_3.3" + "/" + getSectionId() + "/"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		if (version >= 3.2) {
			result.add("templates_3.2" + "/" + getSectionId() + "/"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		if (version >= 3.1) {
			result.add("templates_3.1" + "/" + getSectionId() + "/"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		if (version >= 3.0) {
			result.add("templates_3.0" + "/" + getSectionId() + "/"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		return result;
	}
}
