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
package com.arcadsoftware.dbutils;

import org.apache.commons.dbutils.handlers.MapHandler;

/**
 * Create a basic map handler that convert aliases column names.
 */
public class AliasesMapHandler extends MapHandler {

	public final static AliasesMapHandler instance = new AliasesMapHandler();
	
	public AliasesMapHandler() {
		super(AliasesRowProcessor.instance);
	}
	
	
}
