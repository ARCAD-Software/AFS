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
package com.arcadsoftware.osgi.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
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
		return " ---Misc AFS System Commands---\n" +
				"\tstartAll - start all bundles that are currently not started.\n" +
				"\tlistDuplicated - start all duplicated bundles (with same symbolic names and different versions).\n";
	}

	public void _startAll(CommandInterpreter ci) throws Exception {
		final ArrayList<Bundle> bundles = new ArrayList<Bundle>();
		boolean work = false;
		for (Bundle bundle:context.getBundles()) {
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
	
	public void _listDuplicated(CommandInterpreter ci) throws Exception {
		Bundle[] bundles = context.getBundles();
		if (bundles != null) {
			HashSet<Bundle> doubles = new HashSet<>();
			for (Bundle b: bundles) {
				for (Bundle d: bundles) {
					if (b.getSymbolicName().equals(d.getSymbolicName()) && !b.getVersion().equals(d.getVersion())) {
						doubles.add(b);
						break;
					}
				}
			}
			if (doubles.isEmpty()) {
				ci.println("There is no duplicated bundle in this platform.");
			} else {
				HashSet<String> sn = new HashSet<>();
				for (Bundle b: doubles) {
					sn.add(b.getSymbolicName());
				}
				ArrayList<String> list = new ArrayList<String>(sn);
				Collections.sort(list);
				for (String name: list) {
					ci.println(name + ":");
					for (Bundle b: doubles) {
						if (name.equals(b.getSymbolicName())) {
							ci.println("  " + b.getBundleId() + " version " + b.getVersion());
						}
					}
					ci.println();
				}
			}
		}
	}
}
