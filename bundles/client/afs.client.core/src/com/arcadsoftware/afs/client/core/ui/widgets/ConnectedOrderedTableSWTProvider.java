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
/**
 * This widget manages a Connected Table with UP/DOWN buttons to change order in list
 */
package com.arcadsoftware.afs.client.core.ui.widgets;

import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.client.editors.swtwidgets.viewer.BeanMapTableViewer;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

public class ConnectedOrderedTableSWTProvider extends ConnectedTableWithUserDefinedButtonBarSWTProvider {

	public static final String ATTRIBUTE_ORDER = "order"; //$NON-NLS-1$

	private String orderAttribute; // attribute that manages Order

	@Override
	public void create(ISWTRenderer swtRenderer,
			ILayoutParameters layoutParameters, Element element,
			MetaDataEntity structure) {
		orderAttribute = layoutParameters.getParameter(ATTRIBUTE_ORDER);
		super.create(swtRenderer, layoutParameters, element, structure);
	}

	@Override
	protected void createControlAfterTable(Composite parent) {
		if ((orderAttribute != null) && (orderAttribute.length() > 0)) {
			final Composite bar = new Composite(parent, SWT.NONE);
			bar.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false));
			bar.setLayout(new GridLayout(1, true));
			createOrderButtonUp(bar);
			createOrderButtonDown(bar);
			final BeanMapTableViewer v = getViewer();
			v.setSortOnColumn(false);
		}
		super.createControlAfterTable(parent);
	}

	private Button createOrderButtonUp(Composite parent) {
		final Button b = getRenderer().getToolkit().createButton(parent, null, SWT.PUSH);
		b.setImage(AFSIcon.PREVIOUS_ARROW.image());
		b.setToolTipText(Activator.resString("moveup.action.tooltip")); //$NON-NLS-1$
		b.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false));
		b.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
						final BeanMap selection = getSelectedBeanMap();
						if (selection != null) {
							final BeanMapList l = getViewer().getBeanMapList();
							final int index = l.findIndex(selection.getId());
							if (index > 0) {
								final BeanMap prev = l.get(index - 1);
								final int previousOrder = prev.getInt(orderAttribute);
								prev.put(orderAttribute, previousOrder + 1);
								selection.put(orderAttribute, previousOrder);
								getHelper().update(prev);
								getHelper().update(selection);
								final IObservableList<?> list = getRenderer().getRendererBinding().getObservableLink(element, true, null);
								list.move(index, index - 1);
								getRenderer().forceDirty();
							}
						}
					}
				});
		b.setEnabled(!renderer.isReadOnly());
		return b;
	}

	private Button createOrderButtonDown(Composite parent) {
		final Button b = getRenderer().getToolkit().createButton(parent, null, SWT.PUSH);
		b.setImage(AFSIcon.NEXT_ARROW.image());
		b.setToolTipText(Activator.resString("movedown.action.tooltip")); //$NON-NLS-1$
		b.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false));
		b.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
						final BeanMap selection = getSelectedBeanMap();
						if (selection != null) {
							final BeanMapList l = getViewer().getBeanMapList();
							final int index = l.findIndex(selection.getId());
							if (index < (l.size() - 1)) {
								final BeanMap next = l.get(index + 1);
								final int nextOrder = next.getInt(orderAttribute);
								next.put(orderAttribute, nextOrder - 1);
								selection.put(orderAttribute, nextOrder);
								getHelper().update(next);
								getHelper().update(selection);
								final IObservableList<?> list = getRenderer().getRendererBinding().getObservableLink(element, true, null);
								list.move(index, index + 1);
								getRenderer().forceDirty();
							}
						}
					}
				});
		b.setEnabled(!renderer.isReadOnly());
		return b;
	}
}
