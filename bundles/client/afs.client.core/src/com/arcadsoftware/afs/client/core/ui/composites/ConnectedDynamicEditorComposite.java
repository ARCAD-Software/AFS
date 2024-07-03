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
package com.arcadsoftware.afs.client.core.ui.composites;

import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.loaders.CoreContentLoader;
import com.arcadsoftware.afs.client.core.ui.loaders.CoreEditorLoader;
import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;
import com.arcadsoftware.editor.swt.DynamicEditorComposite;

public class ConnectedDynamicEditorComposite extends DynamicEditorComposite {
	private final ServerConnection connexion;

	public ConnectedDynamicEditorComposite(ServerConnection connexion, Composite parent, int style,
			String type, String layoutName, boolean readOnly) {
		super(parent, style);
		this.connexion = connexion;
		super.initContent(parent, null, type, layoutName, readOnly, true);
	}

	public ConnectedDynamicEditorComposite(ServerConnection connexion, Composite parent, int style,
			String type, String layoutName, boolean readOnly, boolean createContent) {
		super(parent, style);
		this.connexion = connexion;
		super.initContent(parent, null, type, layoutName, readOnly, createContent);
	}

	public ConnectedDynamicEditorComposite(ServerConnection connexion, Composite parent, int style,
			String type, String layoutName) {
		this(connexion, parent, style, type, layoutName, false);
	}

	public ConnectedDynamicEditorComposite(ServerConnection connexion, Composite parent, int style,
			String type) {
		this(connexion, parent, style, type, type, false);
	}

	public ConnectedDynamicEditorComposite(ServerConnection connexion, Composite parent, int style,
			String type, boolean readOnly) {
		this(connexion, parent, style, type, type, readOnly);
	}

	@Override
	protected void initializeEditorLoader(SWTRenderer renderer) {
		final Object loader = renderer.getEditorLoader();
		if (loader instanceof CoreEditorLoader) {
			final CoreEditorLoader el = (CoreEditorLoader) loader;
			el.setServerConnection(getConnection());
		}
	}

	@Override
	protected void initializeDataLoader(SWTRenderer renderer) {
		final Object dataLoader = renderer.getDataLoader();
		if (dataLoader instanceof CoreContentLoader) {
			final CoreContentLoader l = (CoreContentLoader) dataLoader;
			l.setServerConnection(getConnection());
		}
	}

	protected ServerConnection getConnection() {
		return connexion;
	}

}
