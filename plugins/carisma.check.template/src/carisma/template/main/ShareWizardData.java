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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/** Helper class to share Data between different Wizards.
 * 
 * @author bberghoff
 *
 */
public final class ShareWizardData implements ActionListener {
	
    /** Hide constructor.
     * 
     */
	private ShareWizardData() {
	}
	
	/** 
	 * Chosen Name for the Package.
	 */
	private static String packageName = "";
	
	/**
	 * Setter for packageName.
	 * @param name Name to set.
	 */
	public static void setPackageName(final String name) {
		packageName = name;		
	}
	
	/** Getter for packageName.
	 * 
	 * @return the package-name.
	 */
	public static String getPackageName() {
		return packageName;
	}

	@Override
    public void actionPerformed(final ActionEvent arg0) {
		notifyAll();
	}
}
