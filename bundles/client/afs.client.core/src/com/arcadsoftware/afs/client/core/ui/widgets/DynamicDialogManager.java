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
package com.arcadsoftware.afs.client.core.ui.widgets;

import java.util.List;

import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.ElementParameter;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.ISWTRenderer;

public class DynamicDialogManager {
	private static final String DIALOG = "dialog";
	private ElementParameter dialogParameter = null;
	private final ISWTRenderer parentRenderer;

	public DynamicDialogManager(ISWTRenderer renderer, ILayoutParameters parameters) {
		parentRenderer = renderer;
		final List<ElementParameter> dialogElements = parameters.getListElementParameter(DIALOG);
		if (dialogElements.size() > 0) {
			dialogParameter = dialogElements.get(0);
		}
	}

	public DynamicDialog createDialog(Shell shell, ServerConnection connection, BeanMap beanMap, boolean readOnly) {
		final DynamicDialog dialog = new DynamicDialog(shell, connection, beanMap, parentRenderer, dialogParameter,
				readOnly, false);
		return dialog;
	}

	public DynamicDialog createDialogForAddtion(Shell shell, ServerConnection connection, final String type) {
		final DynamicDialog dialog = new DynamicDialog(shell, connection, null, parentRenderer, dialogParameter, false,
				true) {
			@Override
			public String getType() {
				return type;
			}
		};
		return dialog;
	}

	public ElementParameter getDialogParameter() {
		return dialogParameter;
	}

}
