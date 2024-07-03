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

import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapListEvent;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.beanmap.IBeanMapListListener;
import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IBeanMapContainerList;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataFormater;
import com.arcadsoftware.metadata.MetaDataLink;

/**
 * The checkTable editor allow to represent the association between a data and a limited number of possible values.
 * <p>
 * The tree will be filled this all possibles data, loaded just one time when the widget is created.
 * <p>
 * TODO Implement a hierarchical representation of items... (with parent references or "group by" parameter...)
 *
 * @author ARCAD Software
 */
public class CheckTreeSWTProvider implements IInputSWTProvider {

	@Override
	public void create(final ISWTRenderer renderer, ILayoutParameters parameters, final Element element,
			MetaDataEntity structure) {
		if (!(element instanceof MetaDataLink)) {
			return;
		}
		final MetaDataEntity entity = ((MetaDataLink) element).getRefEntity();
		if (entity == null) {
			return;
		}
		String format = parameters.getParameter("format"); //$NON-NLS-1$
		if ((format == null) || (format.trim().length() == 0)) {
			if (entity.getAttribute("name") != null) { //$NON-NLS-1$
				format = "%name%"; //$NON-NLS-1$
			} else if (entity.getAttribute("code") != null) { //$NON-NLS-1$
				format = "%code%"; //$NON-NLS-1$
			} else if (entity.getAttribute("text") != null) { //$NON-NLS-1$
				format = "%text%"; //$NON-NLS-1$
			} else {
				format = null;
			}
		}
		final MetaDataFormater formater;
		if (format != null) {
			formater = new MetaDataFormater(format, entity);
		} else {
			formater = null;
		}
		// Create the Widget:
		final Tree tree = new Tree(renderer.getParent(), SWT.CHECK);
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
			tree.setLayoutData(layoutData);
		}
		renderer.getToolkit().adapt(tree, true, true);
		// Create the viewer :
		final CheckboxTreeViewer viewer = new CheckboxTreeViewer(tree);
		viewer.setContentProvider(new ITreeContentProvider() {
			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			}

			@Override
			public void dispose() {
			}

			@Override
			public boolean hasChildren(Object element) {
				return false;
			}

			@Override
			public Object getParent(Object element) {
				return null;
			}

			@Override
			public Object[] getElements(Object inputElement) {
				if (inputElement instanceof BeanMapList) {
					return ((BeanMapList) inputElement).toArray();
				}
				return null;
			}

			@Override
			public Object[] getChildren(Object parentElement) {
				return null;
			}
		});
		viewer.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				if ((formater == null) || !(element instanceof IBeanMap)) {
					return super.getText(element);
				}
				return formater.format((IBeanMap) element);
			}
		});
		// Sorting capability
		if (parameters.getParameterBoolean("sorted")) { //$NON-NLS-1$
			viewer.setComparator(new ViewerComparator());
		}
		// Check state listening (update the model).
		viewer.addCheckStateListener(new ICheckStateListener() {
			@Override
			public void checkStateChanged(CheckStateChangedEvent event) {
				if (event.getChecked()) {
					renderer.addLinkitem((MetaDataLink) element, (BeanMap) event.getElement());
				} else {
					renderer.removeLinkitem((MetaDataLink) element, (BeanMap) event.getElement());
				}
			}
		});
		// The table contain all the data (they will be checked (or not according to the current BeanMap associated
		// values).
		// TODO loadList should support attributes selection... (connect to the formatter).
		renderer.getDataLoader().loadList(entity.getType(), new IBeanMapListListener() {
			@Override
			public void changed(final BeanMapListEvent event) {
				viewer.getTree().getDisplay().asyncExec(new Runnable() {
					@Override
					public void run() {
						viewer.setInput(event.getSource());
					}
				});
			}
		});
		renderer.getRendererBinding().bindElement(element, new IBeanMapContainerList() {
			@Override
			public void loadedListComplete(ISWTRenderer renderer) {
			}

			@Override
			public String getListType() {
				return entity.getType();
			}

			@Override
			public Widget getWidget() {
				return viewer.getTree();
			}

			@Override
			public void setBeanMapList(BeanMapList list) {
				if (list == null) {
					viewer.setCheckedElements(new Object[0]);
				} else {
					viewer.setCheckedElements(list.toArray());
				}
			}

			@Override
			public BeanMapList getBeanMapList() {
				try {
					return new BeanMapList((BeanMap[]) viewer.getCheckedElements());
				} catch (final ClassCastException e) {
					return new BeanMapList();
				}
			}

			@Override
			public String getAttributeList() {
				if (formater != null) {
					return formater.getAttributeCodes();
				}
				return null;
			}

			@Override
			public String getOrderList() {
				return null;
			}

			@Override
			public void addBeanMapToList(int index, BeanMap beanMap) {
				viewer.setChecked(beanMap, true);
			}
		});
		tree.setFocus();
	}

	@Override
	public void dispose() {
	}

}
