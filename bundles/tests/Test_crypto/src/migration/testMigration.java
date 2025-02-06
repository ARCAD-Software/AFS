/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
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
package migration;

import org.junit.Test;

public class testMigration {

	@Test
	public void testBase64() throws Exception {
		byte[] b = "chaine complète".getBytes();
		System.out.println("Java = " + java.util.Base64.getEncoder().encodeToString(b));
		System.out.println("GCry = " + Base64.encode(b));
	}
	
}
