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
package com.arcadsoftware.server.web.internal;

import com.arcadsoftware.rest.FileRestlet;
import com.arcadsoftware.rest.IBranch;
import com.arcadsoftware.rest.OSGiApplication;
import com.arcadsoftware.rest.RouteList;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;

import org.restlet.Context;
import org.restlet.routing.Router;

public class WebBranch implements IBranch {
	
	private final Activator activator;
	private final String webRoot;
	private OSGiApplication application;
	
	public WebBranch(Activator activator, String webRoot) {
		super();
		this.activator = activator;
		this.webRoot = webRoot;
	}

	@Override
	public void setApplication(OSGiApplication application) {
		this.application = application;
	}

	@Override
	public OSGiApplication getApplication() {
		return application;
	}

	@Override
	public Object attach(Context context, Router router) {
		RouteList rl = new RouteList();
		// List all sub folder under /web
		Enumeration<URL> urls = activator.getContext().getBundle().findEntries("web", null, false); //$NON-NLS-1$
		if (urls != null) {
			String defpath = webRoot;
			while (urls.hasMoreElements()) {
				try {
					File element = activator.toFile(urls.nextElement());
					if ((element != null) && element.isDirectory()) {
						String subdir = '/' + element.getName().replace('.', '/');
						String path = subdir + '/';
						if ((defpath == null) && (element.getName().indexOf('.') < 0)) {
							defpath = element.getName();
						}
						// Permanently deriect /dir to /dir/
						rl.add(router.attach(subdir, new Redirection(context, path)));
						rl.add(router.attach(path, new RedirectionDirectory(activator, context, path, element.getName(), element.toURI().toURL().toExternalForm())));
						activator.info("Attach a web container to local path: " + path);
					}
				} catch (IOException e) {
					activator.error("Unable to attach a web container to local path", e);
				}
			}
			if ((defpath != null) && !defpath.isEmpty()) {
				urls = activator.getContext().getBundle().findEntries("web/" + defpath, null, false); //$NON-NLS-1$
				if (urls != null) {
					while (urls.hasMoreElements()) {
						try {
							File element = activator.toFile(urls.nextElement());
							if (element != null) {
								if (element.isDirectory()) {
									String path = '/' + element.getName() + '/';
									rl.add(router.attach(path, new RedirectionDirectory(activator, context, path, defpath, element.toURI().toURL().toExternalForm())));
								} else if (isNotErrorFile(element)) {
									// FIXME this resource does not provide an error redirection mechanism...
									rl.add(router.attach('/' + element.getName(), new FileRestlet(context, element)));
								}
							}
						} catch (IOException e) {
							activator.error("Unable to attach a default web content to local path", e);
						}
					}
					activator.info(String.format("Default web container (%s) attached to root path.", defpath));
				}
			}
		}
		return rl;
	}

	private boolean isNotErrorFile(File element) {
		if (!element.isFile()) {
			return false;
		}
		String s = element.getName();
		if ((s.lastIndexOf('.') == 3) && (('3' == s.charAt(0)) || ('4' == s.charAt(0)) || ('5' == s.charAt(0))) && Character.isDigit(s.charAt(1)) && Character.isDigit(s.charAt(2))) {
			return false;
		}
		return true;
	}

	@Override
	public void detach(Context context, Router router, Object reference) {
		if (reference != null) {
			((RouteList) reference).detachAll(router);
		}
	}
}
