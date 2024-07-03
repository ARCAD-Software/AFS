/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     ARCAD Software - initial API and implementation
 *******************************************************************************/
package com.arcadsoftware.afs.client.core.tools;

public class FormatUtils {

	public static String sizeToString(long bytes) {
<<<<<<< master
	    final int unit = 1024;
	    if (bytes < unit) {
	    	return bytes + " B"; //$NON-NLS-N$
	    }
	    final int exp = (int) (Math.log(bytes) / Math.log(unit));
	    final char pre = "KMGTPE".charAt(exp - 1); //$NON-NLS-1$
	    return String.format("%.1f %sB", bytes / Math.pow(unit, exp), pre); //$NON-NLS-1$					
=======
		final int unit = 1024;
		if (bytes < unit) {
			return bytes + " B";
		}
		final int exp = (int) (Math.log(bytes) / Math.log(unit));
		final char pre = "KMGTPE".charAt(exp - 1);
		return String.format("%.1f %sB", bytes / Math.pow(unit, exp), pre);
>>>>>>> 38f2e60 Clean-up AFS Client
	}

}
