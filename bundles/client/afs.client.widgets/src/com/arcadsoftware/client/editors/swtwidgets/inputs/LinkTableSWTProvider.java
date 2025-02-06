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

import java.util.HashMap;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

import com.arcadsoftware.aev.core.ui.common.FormToolKitUtils;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.client.editors.swtwidgets.internal.Messages;
import com.arcadsoftware.client.editors.swtwidgets.viewer.BeanMapTableViewer;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.script.IScriptAction;

public class LinkTableSWTProvider implements IInputSWTProvider {

	private BeanMapTableViewer viewer;

	@Override
	public void create(final ISWTRenderer renderer, ILayoutParameters parameters, Element element,
			MetaDataEntity structure) {
		if (!(element instanceof MetaDataLink)) {
			return;
		}
		final MetaDataLink link = (MetaDataLink) element;
		final MetaDataEntity entity = link.getRefEntity();
		if (entity == null) {
			return;
		}
		Action select = null;
		final String selectAction = parameters.getParameter("selectAction"); //$NON-NLS-1$
		if (selectAction != null) {
			select = new Action(renderer.getLocalizedMessage(parameters.getParameter("selectLabel", //$NON-NLS-1$
					Messages.linkTable_label))) {
				@Override
				public void run() {
					final HashMap<String, Object> parameters = new HashMap<>(3);
					parameters.put(IScriptAction.PARAM_ENTITY, entity);
					parameters.put("list", viewer.getBeanMapList()); //$NON-NLS-1$
					parameters.put("link", link); //$NON-NLS-1$
					final Object result = renderer.runScriptAction(selectAction, parameters);
					if (result instanceof BeanMap) {
						renderer.addLinkitem(link, (BeanMap) result);
					} else if (result instanceof BeanMapList) {
						for (final BeanMap bean : (BeanMapList) result) {
							renderer.addLinkitem(link, bean);
						}
					}
				}
			};
			select.setAccelerator(SWT.CR);
			if (parameters.getParameter("selectIcon") != null) { //$NON-NLS-1$
				select.setImageDescriptor(renderer.getImageDescriptor(parameters.getParameter("selectIcon"))); //$NON-NLS-1$
			}
		}
		final Action edit = new Action(Messages.linkTable_edit,
				PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_ELCL_REMOVE)) {
			@Override
			public void run() {
				final Object o = viewer.getFirstSelectedObject();
				if (o instanceof BeanMap) {
					// Open Editor for edition...
					final HashMap<String, Object> parameters = new HashMap<>(2);
					parameters.put(IScriptAction.PARAM_ENTITY, entity);
					parameters.put(IScriptAction.PARAM_PARENT, renderer.getParent());
					parameters.put(IScriptAction.PARAM_ITEM, o);
					renderer.runScriptAction("openEditor", parameters);
				}
			}

			@Override
			public boolean isEnabled() {
				return !viewer.getSelection().isEmpty();
			}
		};
		if (parameters.getParameter(IConstants.ICON) != null) {
			edit.setImageDescriptor(renderer.getImageDescriptor(parameters.getParameter(IConstants.ICON)));
		}
		final Action remove = new Action(Messages.linkTable_remove,
				PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_ELCL_REMOVE)) {
			@Override
			public void run() {
				final Object o = viewer.getFirstSelectedObject();
				if (o instanceof BeanMap) {
					renderer.removeLinkitem(link, (BeanMap) o);
				}
			}

			@Override
			public boolean isEnabled() {
				return !viewer.getSelection().isEmpty();
			}
		};
		remove.setAccelerator(SWT.DEL);
		// Create the Table
		viewer = new BeanMapTableViewer(renderer, parameters, element, select, edit, remove) {
			@Override
			protected void doOnDoubleClick(IStructuredSelection selection) {
				edit.run();
			}
		};
		// Update Form toolbar state when selection change.
		viewer.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (!renderer.isReadOnly()) {
					renderer.updateFormToolbar();
				}
			}
		});
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			final GridData layoutData = new GridData();
			layoutData.horizontalSpan = 3;
			if (parameters.getParameterBoolean(IConstants.FILL_HORIZONTAL, true)) {
				layoutData.horizontalAlignment = GridData.FILL;
				layoutData.grabExcessHorizontalSpace = true;
			}
			if (parameters.getParameterBoolean(IConstants.FILL_VERTICAL, true)) {
				layoutData.verticalAlignment = GridData.FILL;
				layoutData.grabExcessVerticalSpace = true;
			}
			// Traitement du tag height
			final int height = parameters.getParameterInteger(IConstants.HEIGHT, -1);
			if (height > -1) {
				layoutData.heightHint = height;
			}
			viewer.getTable().setLayoutData(layoutData);
		}
		renderer.getToolkit().adapt(viewer.getTable(), true, true);
		FormToolKitUtils.paintBordersFor(renderer.getToolkit(), renderer.getParent());

		renderer.getRendererBinding().bindElement(element, viewer);
	}

	@Override
	public void dispose() {
	}

}
