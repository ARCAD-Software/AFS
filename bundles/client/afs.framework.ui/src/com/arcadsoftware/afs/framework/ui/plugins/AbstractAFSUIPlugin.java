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
package com.arcadsoftware.afs.framework.ui.plugins;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Hashtable;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.jface.resource.CompositeImageDescriptor;
import org.eclipse.jface.resource.FontRegistry;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import com.arcadsoftware.afs.framework.ui.internal.Activator;
import com.arcadsoftware.osgi.ILoggedPlugin;

public abstract class AbstractAFSUIPlugin extends AbstractUIPlugin implements ILoggedPlugin {

	protected static final String ICON_PATH = "icons/";

	private class PluginShellProvider implements IShellProvider {

		AbstractAFSUIPlugin plugin;

		public PluginShellProvider(AbstractAFSUIPlugin plugin) {
			this.plugin = plugin;
		}

		@Override
		public Shell getShell() {
			return plugin.getPluginShell();
		}
	}

	private class DecoratorImageDescriptor extends CompositeImageDescriptor {

		ImageData baseImage;
		ImageData overlay;

		/**
		 * Constructor for ArcadCompositeImageDescriptor.
		 */
		public DecoratorImageDescriptor(ImageData baseImage, ImageData overlay) {
			super();
			this.baseImage = baseImage;
			this.overlay = overlay;
		}

		@SuppressWarnings("deprecation")
		@Override
		protected void drawCompositeImage(int width, int height) {
			drawImage(baseImage, 0, 0);
			drawImage(overlay, 0, 8);
		}

		@Override
		protected Point getSize() {
			return new Point(16, 16);
		}
	}

	private final Hashtable<String, ImageDescriptor> imageDescriptorRegistry = new Hashtable<>();
	private ImageRegistry imageRegistry;
	protected FontRegistry fontRegistry;
	protected ColorRegistry colorRegistry;
	protected ResourceBundle resourceBundle;
	private final PluginShellProvider shellProvider = new PluginShellProvider(this);

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		try {
			resourceBundle = loadResourceBundle(getResourceBundleName(), Locale.getDefault());
		} catch (final MissingResourceException e) {
			// FIXME Error not logged !
			resourceBundle = null;
		}
	}

	/**
	 * Returns the plugin's resource bundle, can be null.
	 */
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}

	/**
	 * Return the key associated value into the downloaded properties file.
	 *
	 * @param key
	 *            the key value.
	 * @return key is the corresponding string is not found.
	 */

	public String getResourceString(String key) {
		try {
			final String value = resourceBundle.getString(key);
			if ((value != null) && !value.isEmpty() && (value.charAt(0) == '!')) {
				return getResourceString(value.substring(1));
			}
			return value;
		} catch (final MissingResourceException e) {
			return key;
		}
	}

	public static String getIconPath() {
		return ICON_PATH;
	}

	public String getPluginPath() throws IOException {
		return new Path(FileLocator.resolve(getBundle().getEntry("/")).getPath()).toOSString(); //$NON-NLS-1$
	}

	protected ImageDescriptor putImageInRegistry(String id, String fileName) {
		final ImageDescriptor fid = getPluginImage(fileName);
		imageRegistry.put(id, fid);
		imageDescriptorRegistry.put(id, fid);
		return fid;
	}

	public ImageDescriptor getPluginImage(String fileName) {
		try {
			return ImageDescriptor.createFromURL(new URL(getBundle().getEntry("/"), fileName)); //$NON-NLS-1$
		} catch (final MalformedURLException e) {
			// FIXME Error not logged !
			return null;
		}
	}

	/**
	 * Get an image from the JFace Image Registry.
	 * <p>
	 * You do not have to dispose this image.
	 *
	 * @param key
	 *            The image key in the registry.
	 * @return may return null if the image is not found.
	 * @see ImageRegistry
	 */
	public Image getImage(String key) {
		if ((key == null) || (key.length() == 0)) {
			return null;
		}
		if (imageRegistry == null) {
			imageRegistry = JFaceResources.getImageRegistry();
			fillImageRegistry();
		}
		try {
			return imageRegistry.get(key);
		} catch (final Exception e) {
			log(e);
			return null;
		}
	}

	public Font getFont(String key) {
		if ((key == null) || (key.length() == 0)) {
			return null;
		}
		if (fontRegistry == null) {
			fontRegistry = JFaceResources.getFontRegistry();
			initializeFontRegistry();
		}
		return fontRegistry.get(key);
	}

	public Color getColor(String key) {
		if ((key == null) || (key.length() == 0)) {
			return null;
		}
		if (colorRegistry == null) {
			colorRegistry = JFaceResources.getColorRegistry();
			initializeColorRegistry();
		}
		return colorRegistry.get(key);
	}

	public ImageDescriptor getImageDescriptor(String key) {
		if (imageRegistry == null) {
			imageRegistry = JFaceResources.getImageRegistry();
			fillImageRegistry();
		}
		return imageDescriptorRegistry.get(key);
	}

	@SuppressWarnings("deprecation")
	public Shell getPluginShell() {
		if ((getWorkbench() != null) && (getWorkbench().getActiveWorkbenchWindow() != null)) {
			return getWorkbench().getActiveWorkbenchWindow().getShell();
		}
		return null;
	}

	public Image getCompositeImage(String key, String decoKey) {
		getCompositeImageDescriptor(key, decoKey);
		return getImage(key + '_' + decoKey);
	}

	public Image getCompositeImage(Image image, String decoKey) {
		final ImageDescriptor imgd = getCompositeImageDescriptor(image, decoKey);
		if (imgd != null) {
			return imgd.createImage();
		}
		return null;
	}

	public String getVersion() {
		return getBundle().getHeaders().get(org.osgi.framework.Constants.BUNDLE_VERSION);
	}

	@SuppressWarnings("deprecation")
	public ImageDescriptor getCompositeImageDescriptor(String key, String decoKey) {
		final String id = key + '_' + decoKey;
		DecoratorImageDescriptor deco = (DecoratorImageDescriptor) imageDescriptorRegistry.get(id);
		if (deco == null) {
			final ImageDescriptor main = getImageDescriptor(key);
			final ImageDescriptor overlay = getImageDescriptor(decoKey);
			if ((main != null) && (overlay != null)) {
				deco = new DecoratorImageDescriptor(main.getImageData(), overlay.getImageData());
				imageDescriptorRegistry.put(id, deco);
				final Image imgd = deco.createImage();
				imageRegistry.put(id, imgd);
			}
		}
		return deco;
	}

	@SuppressWarnings("deprecation")
	public ImageDescriptor getCompositeImageDescriptor(Image base, String decoKey) {
		return new DecoratorImageDescriptor(base.getImageData(), getImageDescriptor(decoKey).getImageData());
	}

	public IShellProvider getShellProvider() {
		return shellProvider;
	}

	protected void initializeFontRegistry() {
		// Do nothing
	}

	protected void initializeColorRegistry() {
		// Do nothing
	}

	public boolean openConfirm(String message) {
		return MessageDialog.openConfirm(getPluginShell(), getApplicationTitle(), message);
	}

	public boolean openConfirm(String title, String message) {
		return MessageDialog.openConfirm(getPluginShell(), title, message);
	}

	public boolean openQuestion(String message) {
		return MessageDialog.openQuestion(getPluginShell(), getApplicationTitle(), message);
	}

	public void openInformation(String message) {
		MessageDialog.openInformation(getPluginShell(), getApplicationTitle(), message);
	}

	public void openInformation(String title, String message) {
		MessageDialog.openInformation(getPluginShell(), title, message);
	}

	public void openWarning(String message) {
		MessageDialog.openWarning(getPluginShell(), getApplicationTitle(), message);
	}

	public void openWarning(String title, String message) {
		MessageDialog.openWarning(getPluginShell(), title, message);
	}

	public void openError(String message) {
		MessageDialog.openError(getPluginShell(), getApplicationTitle(), message);
	}

	public void openError(String title, String message) {
		MessageDialog.openError(getPluginShell(), title, message);
	}

	@SuppressWarnings("deprecation")
	private IWorkbenchPage getPluginPage() {
		if (getWorkbench() == null) {
			return null;
		}
		if (getWorkbench().getActiveWorkbenchWindow() != null) {
			return getWorkbench().getActiveWorkbenchWindow().getActivePage();
		}
		if (getWorkbench().getWorkbenchWindows().length > 0) {
			return (getWorkbench().getWorkbenchWindows()[0]).getActivePage();
		}
		return null;
	}

	public IViewPart openView(String ID) {
		final IWorkbenchPage page = getPluginPage();
		if (page != null) {
			return page.findView(ID);
		}
		return null;
	}

	public IViewPart showView(String ID) {
		final IWorkbenchPage page = getPluginPage();
		if (page != null) {
			try {
				return page.showView(ID, null, IWorkbenchPage.VIEW_ACTIVATE);
			} catch (final PartInitException e) {
				error(e.getLocalizedMessage(), e);
			}
		}
		return null;
	}

	/**
	 * mode to show view
	 *
	 * @param ID
	 * @param mode
	 * @return
	 */
	public IViewPart showView(String ID, int mode) {
		final IWorkbenchPage page = getPluginPage();
		if (page != null) {
			try {
				return page.showView(ID, null, mode);
			} catch (final PartInitException e) {
				error(e.getLocalizedMessage(), e);
			}
		}
		return null;
	}

	public IViewPart findView(String ID) {
		final IWorkbenchPage page = getPluginPage();
		if (page != null) {
			return page.findView(ID);
		}
		return null;
	}

	public boolean isViewVisible(String ID) {
		final IWorkbenchPage page = getPluginPage();
		return (page != null) && page.isPartVisible(findView(ID));
	}

	@Override
	public void log(String message) {
		getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(), message));
	}

	@Override
	public void log(String message, Throwable e) {
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), message, e));
	}

	@Override
	public void log(Throwable e) {
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), e.getLocalizedMessage(), e));
	}

	@Override
	public void warn(String message) {
		getLog().log(new Status(IStatus.WARNING, getBundle().getSymbolicName(), message));
	}

	@Override
	public void warn(String message, Throwable e) {
		getLog().log(new Status(IStatus.WARNING, getBundle().getSymbolicName(), message, e));
	}

	@Override
	public void debug(String message) {
		if (isDebugging()) {
			getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(),
					Activator.resString("Log.debug") + message)); //$NON-NLS-1$
		}
	}

	@Override
	public void debug(String message, Throwable e) {
		if (isDebugging()) {
			getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(),
					Activator.resString("Log.debug") + message, e)); //$NON-NLS-1$
		}
	}

	@Override
	public void debug(Throwable e) {
		if (isDebugging()) {
			getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(),
					Activator.resString("Log.debug") + e.getLocalizedMessage(), e)); //$NON-NLS-1$
		}
	}

	@Override
	public void error(String message, Throwable e) {
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), message, e));

	}

	public void error(String message) {
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), message));

	}

	public abstract String getResourceBundleName();

	protected abstract void fillImageRegistry();

	protected abstract ResourceBundle loadResourceBundle(String bundleName, Locale local)
			throws MissingResourceException;

	protected abstract String getApplicationTitle();

}
