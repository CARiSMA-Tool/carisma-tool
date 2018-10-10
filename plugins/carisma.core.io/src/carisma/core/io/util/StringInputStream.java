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
package carisma.core.io.util;

import java.io.InputStream;

public class StringInputStream extends InputStream {
	
	private int position = 0;
	private String data = null;
	
	StringInputStream(String data) {
		this.data = data;
	}
	
	@Override
	public int read() throws java.io.IOException {
		if (this.position < this.data.length()) {
			return this.data.charAt(this.position++);
		}
		return -1;
	}

	
	/**
	 * Creates an InputStream from a given String.
	 * @param string
	 * @return
	 */
	public static InputStream createInputStreamFromString(String string) {
		return new StringInputStream(string);
	}
}