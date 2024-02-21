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

import java.io.IOException;
import java.net.URL;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.osgi.framework.Bundle;
import org.restlet.Client;
import org.restlet.data.MediaType;
import org.restlet.data.Protocol;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.representation.InputRepresentation;

import org.restlet.engine.connector.ClientHelper;

/**
 * 
 */
public class OSGiClientHelper extends ClientHelper {
	
	public static final Protocol OSGI = new Protocol("osgi", "OSGI", "OSGI Bundle Resource", Protocol.UNKNOWN_PORT); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

	static private Activator activator;

	static public void setActivator(Activator arg) {
		activator = arg;
	}

    public OSGiClientHelper(Client client) {
        super(client);
        getProtocols().add(Protocol.valueOf("OSGI")); //$NON-NLS-1$
    }

    public void handle(Request request, Response response) {
        if (activator == null) {
        	return;
        }
    	Matcher matcher = Pattern.compile("(osgi://)([^/]*)").matcher(request.getResourceRef().getHostIdentifier()); //$NON-NLS-1$
        if (matcher.matches()) {
            Bundle[] bundles = activator.getContext().getBundles();
            for (Bundle bundle : bundles) {
                if (matcher.group(2).equals(bundle.getSymbolicName())) {
                    URL url = bundle.getResource(request.getResourceRef().getPath());
                    if (url != null) {
                        try {
                        	response.setEntity(new InputRepresentation(url.openStream(), MediaType.ALL));
                        } catch (IOException e) {
                           	activator.info(Messages.getString("OSGiClientHelper.ErrorResNotReadable") + url.toString() + Messages.getString("OSGiClientHelper.ErrorResNotReadableEnd")); //$NON-NLS-1$ //$NON-NLS-2$
                        }
                    }
                }
            }
        }
    }
}
