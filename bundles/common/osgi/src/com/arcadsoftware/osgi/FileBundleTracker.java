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
package com.arcadsoftware.osgi;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Dictionary;
import java.util.HashMap;

import org.eclipse.core.runtime.FileLocator;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;

import com.arcadsoftware.osgi.internal.Messages;

/**
 * <p>
 * This class implement a Bundle Listener used to load a resource file from other bundles.
 * 
 * <p>
 * This operation require that the bundle org.eclipse.equinox.common is
 * started before to access to bundles files. While this bundle is not started no files will 
 * be loaded. This selection is proceeded when bundles start (deselection when bundles stop). 
 * 
 * <p>
 * Any bundle that use a DynamicImport-Package of any package (*) is ignored. 
 * 
 * <p>
 * Do not forget to open and close this listener when the Activator is stopped. 
 * 
 * <p>
 * Usage :
 * <ul>
 * <li> Create the FileBundleTracker into the "start" method of your bundle.
 * <li> Set the activator or a class that implement "IloggedPlugin" to log errors during selection process.
 * <li> Set the "defaultFileName" property to define the path (into a bundle) to the selected file.
 * For instance "/META-INF/option.xml".
 * <li> Set the Manifest Header that can be use to define a specific file to be selected. For
 * instance "Arcad-OptionFile", this header should be then used into any Bundle manifest to declare
 * the path (into the bundle) of a file to select.
 * <li> Call "open" to start to select files. 
 * <li> When needed use the method "getFile" or "getFiles" to get the currently selected files.
 * <li> You can add listener that implement interface "IFileAddedListener" 
 * <li> When the selection is no more used, or into the "stop" method of your bundle, call the
 * "close method.
 * </ul>
 * 
 * <p>
 * Default implementation select only one file per bundle if more file are needed or different selection
 * process, override methods "addBundle" and "removeBundle". 
 * 
 */
public class FileBundleTracker implements BundleListener {

	private static final String GROOVY = "groovy"; //$NON-NLS-1$
	private static final String ORG_CODEHAUS_GROOVY = "org.codehaus.groovy"; //$NON-NLS-1$
	private static final String EQUINOX_COMMON = "org.eclipse.equinox.common"; //$NON-NLS-1$
	private static final String DYNAMICIMPORT = "DynamicImport-Package"; //$NON-NLS-1$

	private ILoggedPlugin activator;
	private BundleContext context;
	private final HashMap<Bundle, File> files = new HashMap<Bundle, File>();
	private String header;
	private String defaultFileName;
	private ArrayList<IFileAddedListener> listeners;

	public FileBundleTracker(BundleContext context) {
		super();
		this.context = context;
		listeners = new ArrayList<IFileAddedListener>();
	}

	public void open() {
		if (isCommonStarted()) {
			proceedDelayedBundleScan();
		}
		context.addBundleListener(this);
	}
	
	public void close() {
		context.removeBundleListener(this);
	}
	
	private void proceedDelayedBundleScan() {
		for(Bundle bundle: context.getBundles()) {
			if (bundle.getState() == Bundle.ACTIVE) {
				addBundle(bundle);
			}
		}
	}

	private boolean isCommonStarted() {
		for(Bundle b: context.getBundles()) {
			if (EQUINOX_COMMON.equals(b.getSymbolicName()) && (b.getState() == Bundle.ACTIVE)) {
				return true;
			}
		}
		return false;
	}

	public final void bundleChanged(BundleEvent event) {
		Bundle bundle = event.getBundle();
		switch (event.getType()) {
		case BundleEvent.STARTED:
			// Suppression du bundle de Groovy qui a accès aux resources des autres bundles...
			if (!isIgnored(bundle)) {
				if (EQUINOX_COMMON.equals(event.getBundle().getSymbolicName())) {
					proceedDelayedBundleScan();
				} else if (isCommonStarted()) {
					addBundle(bundle);
				}
			}
			break;
		case BundleEvent.STOPPED:
			removeBundle(bundle);
			break;
		}
	}
	
	protected boolean isIgnored(Bundle bundle) {
		if (ORG_CODEHAUS_GROOVY.equals(bundle.getSymbolicName()) ||
			GROOVY.equals(bundle.getSymbolicName())) {
			return true;
		}
		String di = getHeaderValue(bundle, DYNAMICIMPORT);
		return (di != null) && "*".equals(di.trim()); //$NON-NLS-1$
	}

	/**
	 * Return the Header value from the given Bundle.
	 * 
	 * @param bundle a installed Bundle.
	 * @param header a header name.
	 * @return null if this header is not declared into this bundle.
	 */
	protected String getHeaderValue(Bundle bundle, String header) {
		Dictionary<String,String> headers = bundle.getHeaders();
		if (headers != null) {
			String o = headers.get(header);
			if (o != null && o.length() > 0) {
				return o;
			}
		}
		return null;
	}
	
	/**
	 * This method is called when a bundle is removed.
	 * 
	 * <p>
	 * Override this method to change the default process.
	 * 
	 * @param bundle
	 */
	protected void removeBundle(Bundle bundle) {
		files.remove(bundle);
	}

	/**
	 * This method is called each type a Bundle is added.
	 * 
	 * <p>
	 * Override this method to change the default process.
	 * 
	 * @param bundle
	 */
	protected void addBundle(Bundle bundle) {
		if (header != null) {
			Dictionary<String,String> headers = bundle.getHeaders();
			if (headers != null) {
				String o = headers.get(header);
				if (o != null && o.length() > 0) {
					File file = getBundleFile(bundle, o);
					if (file != null) {
						addBundleFile(bundle, file, o);
						return;
					}
				}
			}
		}
		if (defaultFileName != null) {
			File file = getBundleFile(bundle, defaultFileName);
			if (file != null) {
				addBundleFile(bundle, file, defaultFileName);
			}
		}			
	}
		
	/**
	 * 
	 * @param bundle
	 * @param file
	 * @param filename
	 */
	protected void addBundleFile(Bundle bundle, File file, String filename) {
		files.put(bundle, file);
	}

	/**
	 * Load a file from the given Bundle.
	 * 
	 * @param bundle any installed bundle.
	 * @param filename relative to the bundle root. Must start with a slash (/).
	 * @return null if this file does not exist.
	 */
	public File getBundleFile(Bundle bundle, String filename) {
		URL url = bundle.getEntry(filename);
		if (url == null) {
			// Use bundle classpath.
			url = bundle.getResource(filename);
			if (url == null) {
				return null;
			}
		}
		try {
			url = FileLocator.toFileURL(url);
			return new File(new URI(url.getProtocol(), url.getPath(), null));
		} catch (URISyntaxException e) {
			if (activator != null) {
				activator.debug(e);
			}
			try {
				return new File(url.getPath());
			} catch (Throwable t) {
				if (activator != null) {
					activator.debug(t);
				}
			}
		} catch (Throwable e) {
			if (activator != null) {
				activator.error(String.format(Messages.getString("FileBundleTracker_Error_LoadingBundle"), filename, bundle.getBundleId()), e); //$NON-NLS-1$
			}
		}
		return null;
	}

	public ILoggedPlugin getActivator() {
		return activator;
	}

	public BundleContext getContext() {
		return context;
	}

	/**
	 * Set the activator to log messages and errors.
	 * 
	 * @param activator
	 */
	public void setActivator(ILoggedPlugin activator) {
		this.activator = activator;
	}

	/**
	 * Get the seleted files.
	 * @return
	 */
	public Collection<File> getFiles() {
		return files.values();
	}
	
	/**
	 * Get the selected file from the given bundle.
	 * @param bundle
	 * @return
	 */
	public File getFile(Bundle bundle) {
		return files.get(bundle);
	}

	public String getHeader() {
		return header;
	}

	public String getDefaultFileName() {
		return defaultFileName;
	}

	public void setHeader(String header) {
		this.header = header;
	}

	public void setDefaultFileName(String defaultFileName) {
		this.defaultFileName = defaultFileName;
	}

	public void addListener(IFileAddedListener listener) {
		listeners.add(listener);
	}
	
	public void removeListener(IFileAddedListener listener) {
		listeners.remove(listener);
	}
	
}
