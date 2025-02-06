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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.client.editors.swtwidgets.internal.Messages;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Date SWT Widget provider for the dynamic editors.
 */
public class DateInputSWTProvider implements IInputSWTProvider {

	private static final String OK = Messages.DateInputSWTProvider_okButton;
	private static final String BUTTON_LABEL = "..."; //$NON-NLS-1$
	private Element Element;
	private ISWTRenderer swtRenderer;
	DateTime dateTime;

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element,
			MetaDataEntity structure) {
		Element = element;
		swtRenderer = renderer;
		final String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		if (label.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
		}
		if (!element.isReadonly()) {
			final int horizontalSpan = (label.length() > 0) ? 1 : 3;
			final Composite composite = createDefaultComposite(renderer, horizontalSpan);
			dateTime = new DateTime(composite, SWT.DATE | SWT.BORDER);
			dateTime.setEnabled(!element.isReadonly());
			if (parameters.getParameterBoolean(DEFAULT)) {
				dateTime.setFocus();
			}
			if (renderer.getParent().getLayout() instanceof GridLayout) {
				// FIXME The parent is not the good one here...
				dateTime.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			}
			final Button button = renderer.getToolkit().createButton(composite, BUTTON_LABEL, SWT.PUSH);
			button.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent event) {
					showDateChoiceDialog(label);
				}
			});
		} else {
			dateTime = new DateTime(renderer.getParent(), SWT.DATE | SWT.BORDER);
			dateTime.setEnabled(!element.isReadonly());
			if (parameters.getParameterBoolean(DEFAULT)) {
				dateTime.setFocus();
			}
			if (renderer.getParent().getLayout() instanceof GridLayout) {
				if (label.length() > 0) {
					dateTime.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
				} else {
					dateTime.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 3, 1));
				}
			}
		}
		renderer.getRendererBinding().bindElement(element, dateTime);
	}

	@Override
	public void dispose() {
		// Do nothing
	}

	private Composite createDefaultComposite(ISWTRenderer renderer, int horizontalSpan) {
		final Composite composite = renderer.getToolkit().createComposite(renderer.getParent(), SWT.NONE);
		final GridLayout gridLayout = new GridLayout(2, false);
		gridLayout.marginBottom = gridLayout.marginHeight = gridLayout.marginLeft = gridLayout.marginRight = gridLayout.marginTop = gridLayout.marginWidth = 0;
		composite.setLayout(gridLayout);
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			composite.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, horizontalSpan, 1));
		}
		return composite;
	}

	void showDateChoiceDialog(String title) {
		final Shell dialog = new Shell(Display.getCurrent(), SWT.SHELL_TRIM | SWT.APPLICATION_MODAL);
		dialog.setText(title);
		final GridLayout gridLayout = new GridLayout();
		gridLayout.marginHeight = gridLayout.marginWidth = gridLayout.verticalSpacing = 0;
		dialog.setLayout(gridLayout);
		final DateTime calendar = new DateTime(dialog, SWT.CALENDAR);
		swtRenderer.getRendererBinding().bindElement(Element, calendar);
		final Button ok = new Button(dialog, SWT.PUSH | SWT.CENTER);
		ok.setText(OK);
		ok.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		ok.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				dialog.close();
			}
		});
		dialog.setDefaultButton(ok);
		dialog.pack();
		dialog.open();
	}

}
