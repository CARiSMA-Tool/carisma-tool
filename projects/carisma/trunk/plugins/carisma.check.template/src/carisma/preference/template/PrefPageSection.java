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
package carisma.preference.template;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Locale;
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
import org.eclipse.pde.ui.IFieldData;
import org.eclipse.pde.ui.templates.BooleanOption;
import org.eclipse.pde.ui.templates.OptionTemplateSection;
import org.eclipse.pde.ui.templates.PluginReference;
import org.eclipse.pde.ui.templates.TemplateOption;

import carisma.core.Carisma;
import carisma.template.main.Activator;
import carisma.template.main.ShareWizardData;

/**
 * Part of the Template providing the preferencePage.
 *
 * @author bberghoff
 *
 */
public class PrefPageSection extends OptionTemplateSection {

    /**
     * Logger.
     */
    private static Logger logger = Logger.getLogger("PrefPageSection");
	/** Static name for the main Class "MainClassPage. */
	private static final String CLASS_NAME = "MainClassPage";
	/**
	 * Static name for "pageName".
	 */
	private static final String PAGE_NAME = "pageName";
	/**
	 * Static name for the main package "mainPackageName".
	 */
	private static final String KEY_MAIN_PACKAGE_NAME = "mainPackageName";

	/**
	 * Indicator whether this Options have been created or not.
	 */
	private boolean firsttime = true;

	/**
	 * Local name for the name of the main package.
	 */
	private String mainPackageName = null;

	/**
	 * Option to decide whether the Preferences are created or not.
	 */
	private BooleanOption createPage;

	/**
	 * Create the default wizard.
	 */
	public PrefPageSection() {
		setPageCount(1);
		createOptions();
		// IWorkbench ab = PlatformUI.getWorkbench();
	}

	@Override
	public final void addPages(final Wizard wizard) {
		WizardPage page = createPage(0, "");
		page.setTitle("Preference Page Wizard");
		page.setDescription("Configure your preference page.");
		wizard.addPage(page);
		markPagesAdded();
		validateOptions(this.createPage);
	}

	/**
	 * Adds Options to a WizardPage.
	 */
	public final void createOptions() {
		this.createPage = (BooleanOption) addOption("Create_a_PreferencePage",
				"Create a Preference Page.", false, 0);
		addOption(CLASS_NAME, "Class Name", "MyPrefPage", 0);
		addOption(PAGE_NAME, "Page Name", "My Preference Page", 0);
		addOption(KEY_PACKAGE_NAME, "Package Name", ShareWizardData.getPackageName() + ".preferencePage", 0);
	}

	@Override
	public final void validateOptions(final TemplateOption source) {
		if (this.firsttime) {

			TemplateOption[] allPageOptions2 = getOptions(0);
			for (TemplateOption tOption : allPageOptions2) {
				if (KEY_PACKAGE_NAME.equals(tOption.getName())) {
					tOption.setValue(ShareWizardData.getPackageName() + ".preferencepage");
					if (!"".equals(ShareWizardData.getPackageName())) {
						this.firsttime = false;
					}
				}
			}
		}

		if (this.createPage.isSelected()) {
			TemplateOption[] allPageOptions = getOptions(0);
			for (int i = 0; i < allPageOptions.length; i++) {
				TemplateOption nextOption = allPageOptions[i];

				nextOption.setEnabled(true);

				String helper = nextOption.getValue().toString();
				if (KEY_PACKAGE_NAME.equals(nextOption.getName())
						&& !nextOption.getValue().toString().startsWith("carisma.check.")) {
					this.getPage(0).setMessage(
							"The package name should start with \"carisma.check."
									+ "CHECK_NAME\".preferencepage");
					// this.getPage(0).setPageComplete(false);
				} else if (KEY_PACKAGE_NAME.equals(nextOption.getName())
						&& !nextOption.getValue().equals(helper.toLowerCase(Locale.ENGLISH).trim())) {
					nextOption.setValue(helper.toLowerCase(Locale.ENGLISH).trim().replaceAll("[-]", "_"));
					super.validateOptions(source);
				}
			}
		} else {
			TemplateOption[] allPageOptions = getOptions(0);
			for (int i = 0; i < allPageOptions.length; i++) {
				TemplateOption nextOption = allPageOptions[i];

				if (!"Create_a_PreferencePage".equals(nextOption.getName())) {
					nextOption.setEnabled(false);
				}
			}
			this.getPage(0).setPageComplete(true);
		}
	}

	@Override
	protected void initializeFields(IFieldData data) {
		String id = data.getId();
		initializeOption(KEY_MAIN_PACKAGE_NAME, id);
		this.mainPackageName = id.toLowerCase(Locale.ENGLISH);
	}

	@Override
	public String getLabel() {
		return "CARiSMA Pref Page";
	}

	@Override
	public String getDescription() {
		return "A simple Preference Page.";
	}

	@Override
	public String getStringOption(String name) {
	    if (name == null) {
	        throw new IllegalArgumentException("Parameter 'name' shall not be null");
	    } else if (this.mainPackageName == null) {
            logger.log(Level.SEVERE, "The import failed. Activator needs to be changed.");
            return "fail";
	    } else if (name.equals(KEY_MAIN_PACKAGE_NAME)) {
			return this.mainPackageName.toLowerCase(Locale.ENGLISH);
		} else {
		    return super.getStringOption(name);
		}
	}

	@Override
	public String getUsedExtensionPoint() {
		return "org.eclipse.ui.preferencePages";
	}

	@Override
	public String[] getNewFiles() {
		return new String[0];
	}

	@Override
	public String getSectionId() {
		return "preferencepagetemplate";
	}

	@Override
	protected URL getInstallURL() {
		return Activator.getiNSTANCE().getBundle().getEntry("/");
	}

	@Override
	protected ResourceBundle getPluginResourceBundle() {
		return Platform.getResourceBundle(Activator.getiNSTANCE().getBundle());
	}

	@Override
	protected void updateModel(IProgressMonitor monitor) throws CoreException {

		if (this.createPage.isSelected()) {

			IPluginExtension extensionPrefPage = createExtension("org.eclipse.ui.preferencePages", true);
			IPluginModelFactory factory = this.model.getPluginFactory();
			IPluginBase plugin = this.model.getPluginBase();
			IPluginElement element = factory.createElement(extensionPrefPage);

			element.setName("page");
			element.setAttribute("id", getStringOption(KEY_PACKAGE_NAME) + "." + getStringOption(PAGE_NAME));
			element.setAttribute("name", getStringOption(PAGE_NAME));
			element.setAttribute("category", "carisma.core.index");
			element.setAttribute("class", getStringOption(KEY_PACKAGE_NAME) + "." + getStringOption(CLASS_NAME));

			extensionPrefPage.add(element);

			plugin.add(extensionPrefPage);
		}
	}

	@Override
	public boolean isDependentOnParentWizard() {
		return true;
	}

	@Override
	public int getNumberOfWorkUnits() {
		return super.getNumberOfWorkUnits() + 1;
	}

	@Override
	public IPluginReference[] getDependencies(String schemaVersion) {
		ArrayList<IPluginReference> result = new ArrayList<>();

		result.add(new PluginReference("org.eclipse.core.runtime", null, 0));
		result.add(new PluginReference("org.eclipse.ui", null, 0));
		result.add(new PluginReference(Carisma.PLUGIN_ID, null, 0));

		return result.toArray(new IPluginReference[result.size()]);
	}

	@Override
	protected boolean isOkToCreateFolder(File sourceFolder) {
		if (sourceFolder.getName().equals(".svn")) {
			return false;
		} else if (!this.createPage.isSelected()) {
			return false;
		}
		return true;
	}

	@Override
	public URL getTemplateLocation() {
		try {
			String[] candidates = getDirectoryCandidates();
			for (int i = 0; i < candidates.length; i++) {
				if (Activator.getiNSTANCE().getBundle().getEntry(candidates[i]) != null) {
					return new URL(getInstallURL(), candidates[i]);
				}
				super.getTemplateLocation();
			}
		} catch (MalformedURLException e) { // do nothing
		}
		return null;
	}

	private String[] getDirectoryCandidates() {
		double version = getTargetVersion();
		ArrayList<String> result = new ArrayList<>();
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
		return result.toArray(new String[result.size()]);
	}
}
