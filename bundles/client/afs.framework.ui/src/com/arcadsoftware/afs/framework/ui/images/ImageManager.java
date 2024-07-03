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
package com.arcadsoftware.afs.framework.ui.images;

import java.util.Hashtable;
import java.util.Set;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.viewers.DecorationOverlayIcon;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.arcadsoftware.afs.framework.ui.internal.Activator;

/**
 * @deprecated use com.arcadsoftware.documentation.icons.Icon and com.arcadsoftware.documentation.brands.Brand
 *             enumerations instead
 */
@Deprecated
public class ImageManager {

	private static ImageManager instance = new ImageManager();

	private final Hashtable<String, ImageDescriptor> imageDescriptorRegistry;
	private final ImageRegistry imageRegistry;

	private ImageManager() {
		imageDescriptorRegistry = new Hashtable<>();
		imageRegistry = new ImageRegistry();
	}

	public static ImageManager getInstance() {
		return instance;
	}

	private ImageDescriptor registerImage(String key) {
		final int pos = key.indexOf(":"); //$NON-NLS-1$
		ImageDescriptor result = null;
		if (pos > 0) {
			final String bundleId = key.substring(0, pos);
			final String imageKey = key.substring(pos + 1);
			result = AbstractUIPlugin.imageDescriptorFromPlugin(bundleId, imageKey);
		} else {
			result = Activator.getDefault().getImageDescriptor(key);
		}
		if (result != null) {
			imageDescriptorRegistry.put(key, result);
			imageRegistry.put(key, result);
		}
		return result;
	}

	public void registerImage(String key, ImageDescriptor descriptor) {
		imageDescriptorRegistry.put(key, descriptor);
		imageRegistry.put(key, descriptor);
	}

	public ImageDescriptor getImageDescriptor(String key) {
		if (key != null) {
			ImageDescriptor result = imageDescriptorRegistry.get(key);
			if (result == null) {
				result = registerImage(key);
			}
			return result;
		}
		return null;
	}

	public Image getImage(String key) {
		final Image result = imageRegistry.get(key);
		// We try to find registre the image
		if (result == null) {
			registerImage(key);
		}
		return imageRegistry.get(key);
	}

	public String getDescriptorKey(ImageDescriptor descriptor) {
		final Set<String> keys = imageDescriptorRegistry.keySet();
		for (final String key : keys) {
			final ImageDescriptor im = imageDescriptorRegistry.get(key);
			if (im == descriptor) {
				return key;
			}
		}
		return null;
	}

	public Image getImageFromDescriptor(ImageDescriptor descriptor) {
		final String key = getDescriptorKey(descriptor);
		if (key != null) {
			return imageRegistry.get(key);
		}
		return null;
	}

	/**
	 * add top right overlay Image on Base Image
	 *
	 * @param baseImage
	 * @param overlayImage
	 * @return
	 */
	public ImageDescriptor getImageTopRightDecoratedDescriptor(String baseImage, String overlayImage) {
		return getImageDecoratedDescriptor(baseImage, overlayImage, IDecoration.TOP_RIGHT);
	}

	/**
	 * add overlay Image on Base Image
	 *
	 * @param baseImage
	 * @param overlayImage
	 * @param position
	 *            IDecoration constant
	 * @return
	 */
	public ImageDescriptor getImageDecoratedDescriptor(String baseImage, String overlayImage, int position) {
		if ((baseImage != null) && (overlayImage != null)) {
			final String decoratedImageId = baseImage.concat(overlayImage);
			final Image bImage = imageRegistry.get(baseImage);
			final ImageDescriptor overlayImageDescriptor = getImageDescriptor(overlayImage);
			final DecorationOverlayIcon decoratedImage = new DecorationOverlayIcon(bImage, overlayImageDescriptor,
					position);
			final ImageDescriptor result = imageDescriptorRegistry.get(decoratedImageId);
			if (result == null) {
				imageDescriptorRegistry.put(decoratedImageId, decoratedImage);
				imageRegistry.put(decoratedImageId, decoratedImage);
				return imageDescriptorRegistry.get(decoratedImageId);
			}
			return result;
		}
		return null;
	}
}
