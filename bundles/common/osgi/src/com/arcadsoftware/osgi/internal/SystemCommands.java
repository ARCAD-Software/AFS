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
package com.arcadsoftware.osgi.internal;

import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

public class SystemCommands implements CommandProvider {

	private final BundleContext context;
	
	public SystemCommands(BundleContext context) {
		super();
		this.context = context;
	}

	@Override
	public String getHelp() {
		return " ---Misc System Commands---\n" +
				"\tstartAll - start all bundles that are currently not started.";
	}

	public void _startAll(CommandInterpreter ci) throws Exception {
		final ArrayList<Bundle> bundles = new ArrayList<Bundle>();
		boolean work = false;
		for(Bundle bundle:context.getBundles()) {
			if (!context.getBundle().getSymbolicName().equals(bundle.getSymbolicName()) &&
					(bundle.getState() != Bundle.ACTIVE) &&
					(bundle.getState() != Bundle.STARTING)) {
				try {
					bundle.start();
					work = true;
				} catch (Exception e) {
					bundles.add(bundle);
				}
			}
		}
		// process the bunlde the do not start as long as at least one bundle has been started. 
		while (work && !bundles.isEmpty()) {
			work = false;
			Iterator<Bundle> i = bundles.iterator();
			while (i.hasNext()) {
				Bundle bundle = i.next();
				if (!context.getBundle().getSymbolicName().equals(bundle.getSymbolicName()) &&
						(bundle.getState() != Bundle.ACTIVE) &&
						(bundle.getState() != Bundle.STARTING)) {
					bundles.add(bundle);
					try {
						bundle.start();
						i.remove();
						work = true;
					} catch (Exception e) {}
				}
			}
		}
	}
	
}
