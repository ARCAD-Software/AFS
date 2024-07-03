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
package com.arcadsoftware.afs.client.core.ui.propertypage;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.composites.ConnectedDynamicEditorComposite;
import com.arcadsoftware.beanmap.BeanMap;

public abstract class AbstractBeanMapPropertyPage extends AbstractConnectedPropertypage {

	DataAccessHelper helper;
	protected BeanMap initalBeanmap;
	protected BeanMap result;

	protected ConnectedDynamicEditorComposite editor;

	public AbstractBeanMapPropertyPage() {
		super();
		noDefaultAndApplyButton();
	}

	@Override
	protected Control createContents(Composite parent) {
		final Control c = super.createContents(parent);
		if (c != null) {
			return c;
		}

		helper = new DataAccessHelper(getServerConnection());
		initalBeanmap = getEditedBeanMap(helper);

		final Composite composite = new Composite(parent, SWT.NONE);

		final GridLayout gl = new GridLayout(3, false);
		gl.marginWidth = gl.marginHeight = gl.marginTop = gl.marginBottom = gl.marginLeft = gl.marginRight = 0;
		composite.setLayout(gl);
		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalSpan = 3;
		composite.setLayoutData(gridData);

		editor = new ConnectedDynamicEditorComposite(getServerConnection(), composite, 0, initalBeanmap.getType(),
				getLayoutName(), false) {
			@Override
			protected ServerConnection getConnection() {
				return getServerConnection();
			}
		};

		gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalSpan = 3;
		editor.setLayoutData(gridData);

		editor.load(initalBeanmap);
		editor.getRenderer().fireActivatedEvent();

		parent.setBackground(editor.getBackground());
		return composite;
	}

	@Override
	public boolean performOk() {
		if (editor != null) {
			editor.save();
		}
		return super.performOk();
	}

	public String getLayoutName() {
		return null;
	}

	public abstract BeanMap getEditedBeanMap(DataAccessHelper helper);

}
