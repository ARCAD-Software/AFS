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
package com.arcadsoftware.afs.client.core.ui.dialogs;

import java.util.Calendar;
import java.util.Date;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.arcadsoftware.afs.client.core.internal.Activator;

public class GenericDatetimeDialog extends AbstractAFSDialog {

	private DateTime dateInput;
	private Spinner hourChooser;
	private Spinner minutChooser;

	private Date edited;
	private Calendar selectedDate = null;

	public GenericDatetimeDialog(Shell parentShell) {
		super(parentShell, false, true);
	}

	@Override
	public Point getSize() {
		return new Point(345, 150);
	}

	@Override
	public String getTitle() {
		return "";
	}

	public void setEdited(Date edited) {
		this.edited = edited;
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		final GridLayout gl = (GridLayout) composite.getLayout();
		gl.numColumns = 9;
		gl.makeColumnsEqualWidth = false;
		composite.setLayout(gl);
		// gl.marginWidth=gl.marginHeight=gl.marginTop=gl.marginBottom=gl.marginLeft = gl.marginRight = 0;
		new Label(composite, SWT.NONE).setText(Activator.resString("label.date"));
		new Label(composite, SWT.NONE).setText(":");
		dateInput = new DateTime(composite, SWT.DATE);

		new Label(composite, SWT.NONE).setText(Activator.resString("label.hour"));
		new Label(composite, SWT.NONE).setText(":");
		hourChooser = new Spinner(composite, SWT.BORDER);
		hourChooser.setMaximum(23);
		new Label(composite, SWT.NONE).setText(Activator.resString("label.minute"));
		new Label(composite, SWT.NONE).setText(":");
		//
		minutChooser = new Spinner(composite, SWT.BORDER);
		minutChooser.setMaximum(59);

		final Calendar cal = Calendar.getInstance();
		if (edited != null) {
			cal.setTime(edited);
		}

		dateInput.setDate(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH));
		hourChooser.setSelection(cal.get(Calendar.HOUR_OF_DAY));
		minutChooser.setSelection(cal.get(Calendar.MINUTE));

		return composite;
	}

	@Override
	protected void okPressed() {
		selectedDate = Calendar.getInstance();
		selectedDate.set(Calendar.DATE, dateInput.getDay());
		selectedDate.set(Calendar.YEAR, dateInput.getYear());
		selectedDate.set(Calendar.MONTH, dateInput.getMonth());

		selectedDate.set(Calendar.HOUR_OF_DAY, hourChooser.getSelection());
		selectedDate.set(Calendar.MINUTE, minutChooser.getSelection());
		super.okPressed();
	}

	public Calendar getSelectedDate() {
		return selectedDate;
	}

	public Calendar getSelectedTime(Date d) {
		setEdited(d);
		if (open() == Window.OK) {
			return getSelectedDate();
		}
		return null;
	}

	public Calendar getSelectedTime() {
		return getSelectedTime(null);
	}

}
