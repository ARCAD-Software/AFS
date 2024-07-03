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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.EXPANDABLE;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.ui.forms.widgets.ExpandableComposite;

import com.arcadsoftware.client.editors.swtwidgets.ExpendableObservableTextValue;
import com.arcadsoftware.client.editors.swtwidgets.listeners.ScrolledCompositeExpansionListener;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Calendar SWT Widget provider for the dynamic editors.
 */
public class CalendarSWTProvider implements IInputSWTProvider {

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element,
			MetaDataEntity structure) {
		final Composite parent = renderer.getParent();
		final String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		if (label.length() > 0) {
			renderer.getToolkit().createLabel(parent, label);
			renderer.getToolkit().createLabel(parent, TWO_POINTS);
		}
		DateTime dateTime;
		if (!parameters.getParameterBoolean(EXPANDABLE)) {
			dateTime = new DateTime(parent, SWT.CALENDAR);
			if (parent.getLayout() instanceof GridLayout) {
				if (label.length() > 0) {
					dateTime.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_CENTER));
				} else {
					dateTime.setLayoutData(new GridData(GridData.CENTER, GridData.BEGINNING, false, false, 3, 1));
				}
			}
		} else {
			final ExpandableComposite group = renderer.getToolkit().createExpandableComposite(parent,
					ExpandableComposite.TREE_NODE | ExpandableComposite.TITLE_BAR);
			group.setLayout(new FillLayout());
			group.addExpansionListener(new ScrolledCompositeExpansionListener());
			if (parent.getLayout() instanceof GridLayout) {
				if (label.length() > 0) {
					group.setLayoutData(new GridData(GridData.FILL_BOTH));
				} else {
					group.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
				}
			}
			dateTime = new DateTime(group, SWT.CALENDAR);
			group.setClient(dateTime);
			renderer.getRendererBinding().getBinding().bindValue(new ExpendableObservableTextValue(group),
					renderer.getRendererBinding().getObservableAttribute((MetaDataAttribute) element), null, null);
		}
		renderer.getToolkit().adapt(dateTime);
		dateTime.setEnabled(!element.isReadonly());
		if (!parameters.getParameterBoolean(DEFAULT)) {
			dateTime.setFocus();
		}
		renderer.getRendererBinding().bindElement(element, dateTime);
	}

	@Override
	public void dispose() {
	}
}