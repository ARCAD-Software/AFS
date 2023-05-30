/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.rest;

import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver;

/**
 * This class implement a JSon serialiser with compact output.
 * 
 * <p>
 * This serializer is able to convert BeanMaps from and to JSon format.
 * But the converted JSON format is not assured to be compatible with
 * the result of the old <b>JsonStreamCompact</b> class. 
 * 
 * @see JsonStreamCompact
 * @author ARCAD Software
 * @since 2022.01
 */
public class JettisonSreamCompact extends XStreamCompact {

	public JettisonSreamCompact() {
		this((ClassLoader) null);
	}

	public JettisonSreamCompact(ClassLoader classLoader) {
		super(classLoader, new JettisonMappedXmlDriver());
	}
}
