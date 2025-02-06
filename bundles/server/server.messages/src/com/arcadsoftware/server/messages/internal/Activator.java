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
package com.arcadsoftware.server.messages.internal;

import org.osgi.framework.BundleContext;

import com.arcadsoftware.osgi.AbstractActivator;

public class Activator extends AbstractActivator {

	private MessageBundleTracker messageTraker;

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		// Create the message Tracker;
		messageTraker = new MessageBundleTracker(this);
		messageTraker.open();		
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		if (messageTraker != null) {
			messageTraker.close();
			messageTraker = null;
		}			
		super.stop(context);
	}

}