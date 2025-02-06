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
package com.arcadsoftware.afs.client.brands;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.DecorationOverlayIcon;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

/**
 * @author ARCAD Software
 */
public enum AFSIcon {

	ARCAD("icons/arcad.png"),
	ADD("icons/add.png"),
	CREATE("icons/create.png"),
	CHECKBOX_EMPTY("icons/checkbox_empty.png"),
	CHECKBOX_FILLED("icons/checkbox_filled.png"),
	DELETE("icons/delete.png"),
	EDIT("icons/edit.png"),
	ENTRY_COPY("icons/entry_copy.png"),
	ERROR("icons/error.png"),
	HELP("icons/help.png"),
	LIST("icons/list.png"),
	LOG("icons/log.png"),
	MACRO_LOG("icons/macro_log.png"),
	NEXT_ARROW("icons/next_arrow.png"),
	PREFERENCES("icons/preferences.png"),
	PREVIOUS_ARROW("icons/previous_arrow.png"),
	REMOVE("icons/remove.png"),
	REFRESH("icons/refresh.png"),
	REPORT("icons/report.png"),
	REPORTS("icons/reports.png"),
	REPORT_HTML("icons/reoprt_html.png"),
	REPORT_PDF("icons/reoprt_pdf.png"),
	SEARCH_EXTENDED("icons/search_extended.png"),
	SERVER("icons/server.png"),
	SERVERS("icons/servers.png"),
	SERVER_CONFIG("icons/server_config.png"),
	SERVER_CONNECT("icons/server_connect.png"),
	SERVER_DELETE("icons/server_delete.png"),
	SERVER_EDIT("icons/server_edit.png"),
	SERVER_INFO("icons/server_info.png"),
	SERVER_LOG("icons/server_log.png"),
	SERVER_PREFERENCES("icons/server_preferences.png"),
	SSHKEY("icons/sshkey.png"),
	SSHKEY_CREATE("icons/sshkey_create.png"),
	SSHKEY_DELETE("icons/sshkey_delete.png"),
	SSHKEY_EDIT("icons/sshkey_edit.png"),
	SSHKEY_IMPORT("icons/sshkey_import.png"),
	SSHKEY_PUBLIC("icons/sshkey_public.png"),
	TLS("icons/tls.png"),
	USER_GROUP_IMPORT("icons/user_group_import.png");

	private final String path;
	private Image image;
	private ImageDescriptor imageDescriptor;

	private AFSIcon(String path) {
		this.path = path;
	}

	/**
	 * @return
	 */
	public String path() {
		return path;
	}

	/**
	 * Get the Icon image.
	 *
	 * @return
	 */
	public Image image() {
		if (image == null) {
			load();
		}
		return image;
		// FIXME The contract with the caller is not clear who must dispose the Image ?
		// FIXME a resource leak is possible here.
	}

	/**
	 * Get the Image descriptor.
	 *
	 * @return
	 */
	public ImageDescriptor imageDescriptor() {
		if (imageDescriptor == null) {
			load();
		}
		return imageDescriptor;
	}

	private void load() {
		try {
			final Bundle bundle = FrameworkUtil.getBundle(getClass());
			imageDescriptor = ImageDescriptor.createFromURL(bundle.getEntry(path));
		} catch (final Exception var2) {
			imageDescriptor = ImageDescriptor.getMissingImageDescriptor();
		}
		image = imageDescriptor.createImage(true, Display.getDefault());
	}

	/**
	 * Get an image from registry.
	 *
	 * @param key
	 * @return
	 */
	public static Image getImage(String key) {
		for (final AFSIcon icon : values()) {
			if (icon.path.equalsIgnoreCase(key)) {
				return icon.image();
			}
		}
		return null;
	}

	/**
	 * Add top right overlay Image on Base Image.
	 *
	 * @param baseImage
	 * @param overlayImage
	 * @return
	 */
	public static ImageDescriptor getImageTopRightDecoratedDescriptor(Image bImage,
			ImageDescriptor overlayImageDescriptor) {
		return getImageDecoratedDescriptor(bImage, overlayImageDescriptor, IDecoration.TOP_RIGHT);
	}

	/**
	 * Add overlay Image on Base Image.
	 *
	 * @param baseImage
	 * @param overlayImage
	 * @param position
	 *            IDecoration constant
	 * @return
	 */
	public static ImageDescriptor getImageDecoratedDescriptor(Image bImage, ImageDescriptor overlayImageDescriptor,
			int position) {
		if ((bImage != null) && (overlayImageDescriptor != null)) {
			return new DecorationOverlayIcon(bImage, overlayImageDescriptor, position);
		}
		return null;
	}

}
