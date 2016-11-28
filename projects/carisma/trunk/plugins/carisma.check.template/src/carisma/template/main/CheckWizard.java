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
package carisma.template.main;

import org.eclipse.pde.ui.IFieldData;
import org.eclipse.pde.ui.templates.ITemplateSection;
import org.eclipse.pde.ui.templates.NewPluginTemplateWizard;

import carisma.preference.template.PrefPageSection;
import carisma.template.core.DummyCheckSection;




public class CheckWizard extends NewPluginTemplateWizard  {
	
	@Override
	public void init(IFieldData data){
		super.init(data);
		setWindowTitle("Wizard for a CARiSMA check");
		
	}
 
	@Override
	public ITemplateSection[] createTemplateSections() {
		return new ITemplateSection[]{new DummyCheckSection(), new PrefPageSection()};
	}
}
