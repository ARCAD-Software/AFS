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
package com.arcadsoftware.email.internal;

import java.text.Normalizer;

public class CallNormalizer {

	public static String normalize(String filename) {
		// Only compatible with Java 1.6.
		return Normalizer.normalize(filename, Normalizer.Form.NFC);
		// TODO Remove this class if the Compatibility level of this plugin is raised up to Java 1.6.
	}

}
