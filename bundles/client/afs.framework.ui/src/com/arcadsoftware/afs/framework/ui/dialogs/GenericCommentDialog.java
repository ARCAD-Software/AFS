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
/*
 * Created on 15 janv. 2007
 */
package com.arcadsoftware.afs.framework.ui.dialogs;

import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.afs.framework.ui.internal.Activator;

public class GenericCommentDialog extends AbstractAFSDialog {

	private final String valueLabel;
	private final boolean mandatory;
	private final String title;
	private String value;
	private Text valueText;

	protected GenericCommentDialog(Shell parentShell, String title, String label, boolean mandatory) {
		super(parentShell);
		this.title = title;
		valueLabel = label;
		this.mandatory = mandatory;
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		final GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 3;
		composite.setLayout(gridLayout);
		GuiFormatTools.createLabel(composite, valueLabel, false, 3);
		valueText = GuiFormatTools.createText(composite, "", true, 3);
		return composite;
	}

	public String getValue() {
		return value;
	}

	@Override
	protected void okPressed() {
		value = valueText.getText();
		if (mandatory) {
			if (value.trim().length() == 0) {
				Activator.getDefault().openError(
						String.format(Activator.resString("GenericCommentDialog.message.err.mandatoryvalue"),
								valueLabel));
			} else {
				super.okPressed();
			}
		} else {
			super.okPressed();
		}
	}

	@Override
	public int getHeight() {
		return 300;
	}

	@Override
	public int getWidth() {
		return 400;
	}

	@Override
	public String getTitle() {
		return title;
	}

	public static String getText(Shell parentShell, String title, String label, boolean mandatory) {
		final GenericCommentDialog dialog = new GenericCommentDialog(parentShell, title, label, mandatory);
		if (dialog.open() == 0) {
			return dialog.getValue();
		}
		return ""; //$NON-NLS-1$
	}

}
