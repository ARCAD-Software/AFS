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
package com.arcadsoftware.database.sql.postgresql.internal;

import org.osgi.framework.BundleContext;

import com.arcadsoftware.osgi.ILoggedPlugin;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

public class ExtendableHikariDataSource extends HikariDataSource {

	public ExtendableHikariDataSource(ILoggedPlugin activator, BundleContext context, HikariConfig configuration) {
		super(configuration);
	}

}
