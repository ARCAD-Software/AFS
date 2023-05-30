/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.afs.client.core.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

public class AFSFormatTools {

	public static Text createLabelledText(Composite parent, String label) {
		return createLabelledText(parent, label, "", -1); //$NON-NLS-1$
	}

	public static Text createLabelledText(Composite parent, String label, int limit) {
		return createLabelledText(parent, label, "", limit); //$NON-NLS-1$
	}

	public static Text createLabelledText(Composite parent, String label, String defaultText) {
		return createLabelledText(parent, label, defaultText, -1);
	}
	public static Text createLabelledText(Composite parent, String label, String defaultText, int limit) {
		return createLabelledText(parent, label, defaultText, limit, SWT.BORDER);
	}

	public static Text createLabelledText(Composite parent, String label, String defaultText, int limit, int style) {
		Label textLabel = new Label(parent, SWT.NONE);
		textLabel.setText(label);
		new Label(parent, SWT.NONE).setText(":"); //$NON-NLS-1$
		Text text = new Text(parent, style);
		GridData gridData = new GridData(GridData.BEGINNING);
		gridData.horizontalAlignment = GridData.FILL;
		gridData.grabExcessHorizontalSpace = true;
		text.setLayoutData(gridData);
		if (defaultText != null)
			text.setText(defaultText);
		text.setData(textLabel);
		if (limit > 0)
			text.setTextLimit(limit);
		return text;
	}	
	
	public static Composite createComposite(Composite parent) {
		return createComposite(parent, 3, false, 1);
	}

	public static Composite createComposite(Composite parent, int numberOfColumn, boolean equalWidth) {
		return createComposite(parent, numberOfColumn, equalWidth, 1);
	}

	public static Composite createComposite(Composite parent, int numberOfColumn, boolean equalWidth, int horizontalSpan) {

		return createComposite(parent, numberOfColumn, equalWidth, horizontalSpan, SWT.NONE);
	}

	public static Composite createComposite(Composite parent, int numberOfColumn, boolean equalWidth,
			int horizontalSpan, int style) {
		int newHorizontalSpan = horizontalSpan;
		int columnNumber = numberOfColumn;
		if (numberOfColumn < 1)
			columnNumber = 1;
		if (horizontalSpan < 1)
			newHorizontalSpan = 1;

		Composite composite = new Composite(parent, style);
		composite.setLayout(new GridLayout(columnNumber, equalWidth));
		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalSpan = newHorizontalSpan;
		composite.setLayoutData(gridData);
		return composite;
	}	
	
	public static Button createLabelledCheckbox(Composite parent, String label, boolean checked) {
		new Label(parent, SWT.NONE).setText(label);
		new Label(parent, SWT.NONE).setText(":"); //$NON-NLS-1$
		Button checkbox = new Button(parent, SWT.CHECK);
		GridData gridData = new GridData(GridData.BEGINNING);
		gridData.horizontalAlignment = GridData.FILL;
		gridData.grabExcessHorizontalSpace = true;
		checkbox.setLayoutData(gridData);
		checkbox.setSelection(checked);
		return checkbox;
	}	
	
	
	public static Text createLabelledTextWithButton(Composite parent, String label, String defaultText,
			boolean readonly, String buttonText) {
		new Label(parent, SWT.NONE).setText(label);
		new Label(parent, SWT.NONE).setText(":"); //$NON-NLS-1$
		// Création du composite de réception
		Composite p = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(2, false);
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		layout.horizontalSpacing = 0;
		p.setLayout(layout);
		GridData gridData = new GridData(GridData.BEGINNING);
		gridData.horizontalAlignment = GridData.FILL;
		gridData.grabExcessHorizontalSpace = true;

		p.setLayoutData(gridData);

		Text valueText;
		if (readonly) {
			valueText = new Text(p, SWT.BORDER | SWT.READ_ONLY);
		} else {
			valueText = new Text(p, SWT.BORDER);
		}
		gridData = new GridData(GridData.BEGINNING);
		gridData.horizontalAlignment = GridData.FILL;
		gridData.grabExcessHorizontalSpace = true;
		valueText.setLayoutData(gridData);

		Button bAdd = new Button(p, SWT.PUSH);
		bAdd.setText(buttonText);
		gridData = new GridData();
		gridData.heightHint = 21;
		bAdd.setLayoutData(gridData);

		valueText.setData(bAdd);
		valueText.setText(defaultText);
		return valueText;
	}
	
	public static Text createLabelledTextWithEllipsis(Composite parent, String label, boolean readonly) {
		return createLabelledTextWithButton(parent, label, "", readonly, "..."); //$NON-NLS-1$ //$NON-NLS-2$ 
	}
	
}
