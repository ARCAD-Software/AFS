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
package com.arcadsoftware.afs.client.core.ui.dialogs;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;

/**
 * Subclass this class to create a selection dialog on a a BeanMapList
 *
 * @author ARCAD Software
 */
public abstract class AbstractSimpleSearchBeanMapListWithBarDialog extends
		AbstractSimpleSearchBeanMapListDialog {

	public AbstractSimpleSearchBeanMapListWithBarDialog(Shell parentShell,
			ServerConnection connection, boolean showAll) {
		super(parentShell, connection, showAll);
	}

	protected Button createButton(Composite parent,
			String text,
			ImageDescriptor imageDescriptor,
			int anchor,
			int offset,
			int width,
			int height) {

		final Button b = new Button(parent, SWT.PUSH);
		b.setText(text);

		final FormData fData = new FormData();
		fData.top = new FormAttachment(0, 0);
		fData.height = height;
		fData.width = width;
		if (anchor == 0) {// left anchor
			fData.left = new FormAttachment(0, offset);
		} else {
			fData.left = new FormAttachment(100, offset);
		}
		b.setLayoutData(fData);
		b.setImage(imageDescriptor.createImage());
		return b;
	}

	@Override
	public void createAreaAfter(Composite parent) {
		final Composite bar = new Composite(parent, SWT.BORDER);
		final GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 3;
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.verticalIndent = 1;
		bar.setLayoutData(layoutData);
		final FormLayout fl = new FormLayout();
		fl.marginHeight = fl.marginWidth = 1;
		fl.marginLeft = fl.marginTop = fl.marginRight = fl.marginBottom = 0;

		bar.setLayout(fl);

		createAfterButtonBar(bar);
	}

	public abstract void createAfterButtonBar(Composite buttonBar);

}
