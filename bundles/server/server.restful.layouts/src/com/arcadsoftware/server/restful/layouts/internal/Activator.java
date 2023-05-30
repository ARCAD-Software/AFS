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
package com.arcadsoftware.server.restful.layouts.internal;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Dictionary;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.restlet.Context;
import org.restlet.routing.Router;

import com.arcadsoftware.osgi.AbstractFileRepositoryActivator;
import com.arcadsoftware.rest.RouteList;
import com.arcadsoftware.rest.SimpleBranch;
import com.arcadsoftware.rest.SimpleResourceBranch;

public class Activator extends AbstractFileRepositoryActivator {

	private static final String SECTIONSHEADER = "Arcad-LayoutsFolder"; //$NON-NLS-1$
	private static final String GROOVY = "groovy"; //$NON-NLS-1$
	private static final String ORG_CODEHAUS_GROOVY = "org.codehaus.groovy"; //$NON-NLS-1$
	private static final String DYNAMICIMPORT = "DynamicImport-Package"; //$NON-NLS-1$
	private static final String DEFAULTDIRNAME = "files/layouts"; //$NON-NLS-1$
	private static final String LAYOUTFOLDERNAME = '/' + DEFAULTDIRNAME + '/';

	private static Activator instance;

	public static Activator getInstance() {
		return instance;
	}
	
	public void start(BundleContext context) throws Exception {
		instance = this;
		super.start(context);
		// ReadOnly access to layout files :
		registerService(SimpleResourceBranch.clazz, //
				new SimpleResourceBranch("/layout/{type}",DynamicEditorLayoutResource.class), //$NON-NLS-1$ 
				SimpleResourceBranch.properties(SimpleResourceBranch.ROOTBRANCH));
		registerService(SimpleBranch.clazz, //
				new SimpleBranch(){
					@Override
					protected RouteList createAttachedResources(Context context, Router router) {
						RouteList routes = new RouteList();
						routes.add(router.attach("/admin/layout",AdminLayoutListResource.class)); //$NON-NLS-1$
						routes.add(router.attach("/admin/layout/",AdminLayoutListResource.class)); //$NON-NLS-1$
						routes.add(router.attach("/admin/layout/{type}",AdminLayoutResource.class)); //$NON-NLS-1$
						return routes;
					}}, 
				SimpleBranch.properties(SimpleBranch.SECUREDBRANCH));
	}

	@Override
	protected String getDefaultDirName() {
		return DEFAULTDIRNAME;
	}

	@Override
	protected String getFileExtension() {
		return ".xml"; //$NON-NLS-1$
	}
	
	
	private boolean isIgnored(Bundle bundle) {
		if (ORG_CODEHAUS_GROOVY.equals(bundle.getSymbolicName()) ||
			GROOVY.equals(bundle.getSymbolicName()) ||
			getContext().getBundle().getSymbolicName().equals(bundle.getSymbolicName())) {
			return true;
		}
		Dictionary<?, ?> headers = bundle.getHeaders();
		if (headers != null) {
			Object o = headers.get(DYNAMICIMPORT);
			if ((o != null) && (o.toString().length() > 0)) {
				return "*".equals(o.toString().trim()); //$NON-NLS-1$
			}
		}
		return false;
	}

	private String getBundleLayoutsFolder(Bundle bundle) {
		if (isIgnored(bundle)) {
			return null;
		}
		Dictionary<?, ?> headers = bundle.getHeaders();
		if (headers != null) {
			Object o = headers.get(SECTIONSHEADER);
			if ((o != null) && (o.toString().length() > 0)) {
				return o.toString(); //$NON-NLS-1$
			}
		}
		return LAYOUTFOLDERNAME;
	}

	@Override
	public String[] getFileList(String foldername, boolean useExtension, boolean recurse) {
		ArrayList<String> result = new ArrayList<String>();
		Collections.addAll(result, super.getFileList(foldername, useExtension, recurse));
		for(Bundle b:getContext().getBundles()) {
			final String folder = getBundleLayoutsFolder(b);
			if (folder != null) {
				addBundleFiles(b, foldername, useExtension, recurse, result);
			}
		}		
		return result.toArray(new String[result.size()]);
	}

	@Override
	public File getFile(String filekey, String langcode, Calendar fileLastModification) {
		File result = super.getFile(filekey, langcode, fileLastModification);
		if (result != null) {
			return result;
		}
		String filename = getFileName(filekey, langcode);
		for(Bundle b:getContext().getBundles()) {
			String folder = getBundleLayoutsFolder(b);
			if (folder != null) {
				if (folder.endsWith("/") && filename.startsWith("/")) { //$NON-NLS-1$ //$NON-NLS-2$
					if (folder.length() == 1) {
						folder = ""; //$NON-NLS-1$
					} else {
						folder = folder.substring(0, folder.length() - 1);
					}
				}
				result = getbundleFile(b, folder + filename, fileLastModification);
				if (result != null) {
					return result;
				}
			}
		}
		return null;
	}
	
	
}
