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
package carisma.core.util;

import java.io.InputStream;

public class StringInputStream extends InputStream {
	
	private int position = 0;
	private String data = null;
	
	StringInputStream(String data) {
		this.data = data;
	}
	
	public int read() throws java.io.IOException {
		if (position < data.length()) {
			return data.charAt(position++);
		} else {
			return -1;
		}
	}
}