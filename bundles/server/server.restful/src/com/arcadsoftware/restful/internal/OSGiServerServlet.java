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
package com.arcadsoftware.restful.internal;

import org.restlet.Application;
import org.restlet.Component;
import org.restlet.Context;
import org.restlet.data.Protocol;

import org.restlet.ext.servlet.ServerServlet;

/**
 * 
 */
public class OSGiServerServlet extends ServerServlet {

	private static final long serialVersionUID = 6882950963886061267L;

	private final Activator activator;
	
	public OSGiServerServlet(Activator activator) {
		super();
		this.activator = activator;
	}

	@Override
	protected Application createApplication(Context parentContext) {
		Application application = activator.createApplication(false, false);
        if (application != null) {
        	// Set the context based on the Servlet's context
            application.setContext(parentContext.createChildContext());
            application.setStatusService(new ServicesStatus(activator));
        }
        return application;
	}

	@Override
	protected Component createComponent() {
		Component component = super.createComponent();
		component.getClients().add(Protocol.HTTP);
		component.getClients().add(OSGiClientHelper.OSGI);
		component.getClients().add(Protocol.CLAP);
		component.getClients().add(Protocol.FILE);
		// FIXME n'est jamais vérifié !
		if (component.getApplication() != null) {
			component.getApplication().setStatusService(new ServicesStatus(activator));
		}
		return component;
	}

	@Override
	public void destroy() {
		activator.stopServer();
		super.destroy();
	}	
}