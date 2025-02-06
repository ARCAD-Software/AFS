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
package com.arcadsoftware.afs.client.core.ui.dialogs;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;

/**
 * Dialogue permettant la saisie d'une valeur texte.
 *
 * @author ARCAD Software
 */
public class GenericTextValueDialog extends AbstractAFSDialog {

	private Text valueText;
	private final int limit;
	private String title = ""; //$NON-NLS-1$
	private String label = ""; //$NON-NLS-1$
	private String defaultValue = ""; //$NON-NLS-1$
	private String value = ""; //$NON-NLS-1$

	public GenericTextValueDialog(Shell parentShell, String title, String label, int limit, String defaultText) {
		super(parentShell, false, true);
		this.label = label;
		this.limit = limit;
		this.title = title;
		defaultValue = defaultText;
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		final GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 3;
		composite.setLayout(gridLayout);
		valueText = GuiFormatTools.createLabelledText(composite, label, defaultValue, limit);
		return composite;
	}

	public String getValue() {
		return value;
	}

	@Override
	protected void buttonPressed(int buttonId) {
		value = valueText.getText();
		super.buttonPressed(buttonId);
	}

	@Override
	public Point getSize() {
		return new Point(350, 150);
	}

	@Override
	public String getTitle() {
		return title;
	}

	public static String open(Shell parentShell, String title, String label, int limit, String defaultText) {
		final GenericTextValueDialog dialog = new GenericTextValueDialog(parentShell, title, label, limit, defaultText);
		if (dialog.open() == 0) {
			return dialog.getValue();
		}
		return ""; //$NON-NLS-1$
	}

	public static String open(Shell parentShell, String title, String label, String defaultText) {
		return open(parentShell, title, label, -1, defaultText);
	}

	public static String open(Shell parentShell, String title, String label) {
		return open(parentShell, title, label, -1, ""); //$NON-NLS-1$
	}

}
