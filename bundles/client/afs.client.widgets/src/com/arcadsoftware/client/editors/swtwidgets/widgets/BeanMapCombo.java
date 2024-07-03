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
package com.arcadsoftware.client.editors.swtwidgets.widgets;

import java.util.Collections;
import java.util.Comparator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IBeanMapContainerList;
import com.arcadsoftware.editor.swt.IBeanMapContainerValue;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.renderer.ILoadedListListener;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataFormater;

public class BeanMapCombo implements IBeanMapContainerList, IBeanMapContainerValue, ILoadedListListener {

	BeanMapList list;
	protected Combo combo;
	protected BeanMap current;
	protected String sort;
	protected boolean translate;
	protected MetaDataFormater attributesFormater;
	protected MetaDataEntity structure;
	protected ISWTRenderer renderer;

	/**
	 * @param parent
	 * @param style
	 * @param parameters
	 * @param renderer
	 * @param element
	 * @param horizontalSpan
	 */
	public BeanMapCombo(Composite parent, int style, ILayoutParameters parameters, ISWTRenderer renderer,
			Element element, int horizontalSpan) {
		this.renderer = renderer;
		structure = renderer.getStructure(element);
		if (parameters.getParameter(IConstants.FORMAT) != null) {
			attributesFormater = new MetaDataFormater(parameters.getParameter(IConstants.FORMAT), structure);
		}
		translate = parameters.getParameterBoolean("translate"); //$NON-NLS-1$
		sort = parameters.getParameter("sort");
		combo = new Combo(parent, style);
		if (parent.getLayout() instanceof GridLayout) {
			combo.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, horizontalSpan, 1));
		}
		// TODO this(parent, style, parameters, renderer, renderer.getStructure(element), element, horizontalSpan);
	}

	/**
	 * @param parent
	 * @param style
	 * @param parameters
	 * @param renderer
	 * @param structure
	 * @param horizontalSpan
	 */
	public BeanMapCombo(Composite parent, int style, ILayoutParameters parameters, ISWTRenderer renderer,
			MetaDataEntity structure, int horizontalSpan) {
		this.renderer = renderer;
		this.structure = structure;
		if ((parameters.getParameter(IConstants.FORMAT) != null) && (structure != null)) {
			attributesFormater = new MetaDataFormater(parameters.getParameter(IConstants.FORMAT), structure);
		}
		translate = parameters.getParameterBoolean("translate"); //$NON-NLS-1$
		combo = new Combo(parent, style);
		if (parent.getLayout() instanceof GridLayout) {
			combo.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, horizontalSpan, 1));
		}
	}

	/**
	 * Create a BeanMapCombo out of dynamic editors: no renderer, no translate
	 *
	 * @param parent
	 * @param style
	 * @param format
	 * @param translate
	 * @param renderer
	 * @param element
	 * @param horizontalSpan
	 */
	public BeanMapCombo(Composite parent, int style, String format, MetaDataEntity structure,
			String label) {
		this.structure = structure;
		if (format != null) {
			attributesFormater = new MetaDataFormater(format, structure);
		}
		renderer = null;
		translate = false;

		final GridData gridData = new GridData(GridData.BEGINNING);
		gridData.horizontalAlignment = GridData.FILL;
		gridData.grabExcessHorizontalSpace = true;
		if (label != null) {
			new Label(parent, SWT.NONE).setText(label);
			new Label(parent, SWT.NONE).setText(":"); //$NON-NLS-1$
		} else {
			gridData.horizontalSpan = 3;
		}

		combo = new Combo(parent, style);
		if (parent.getLayout() instanceof GridLayout) {
			// new GridData(GridData.FILL, GridData.BEGINNING, true, false, horizontalSpan, 1)
			combo.setLayoutData(gridData);
		}
	}

	@Override
	public BeanMapList getBeanMapList() {
		return list;
	}

	protected String beanMaptoString(BeanMap bean) {
		String result;
		if (attributesFormater != null) {
			result = attributesFormater.format(bean);
		} else {
			result = bean.toString();
		}
		if (translate) {
			return renderer.getLocalizedMessage(result);
		}
		return result;
	}

	/**
	 * Set list of BeanMaps.
	 *
	 * @param list
	 *            list of beanMaps. Can be null.
	 */
	@Override
	public void setBeanMapList(BeanMapList list) {
		if (list == null) {
			list = new BeanMapList();
		}

		this.list = list;
		final String[] items = new String[list.size() + 1];
		items[0] = ""; //$NON-NLS-1$
		int curid = 0;
		if (current != null) {
			curid = current.getId();
		}
		int selection = -1;
		for (int i = 0; i < list.size(); i++) {
			final BeanMap beanMap = list.get(i);
			items[i + 1] = beanMaptoString(beanMap);
			if (beanMap.getId() == curid) {
				selection = i + 1;
			}
		}
		combo.setItems(items);
		// Force the selected value to current...
		if (selection > -1) {
			combo.select(selection);
		} else if (current != null) {
			combo.setText(beanMaptoString(current));
		}
	}

	@Override
	public void addBeanMapToList(int index, BeanMap beanMap) {
		if (list == null) {
			list = new BeanMapList();
		}
		list.add(beanMap);
	}

	@Override
	public BeanMap getBeanMapValue() {
		final int index = combo.getSelectionIndex();
		if ((index < 0) || (index > list.size())) {
			return current;
		}
		if (index == 0) {
			return null;
		}
		return list.get(index - 1);
	}

	@Override
	public void setBeanMapValue(BeanMap beanMap) {
		if (beanMap == null) {
			current = null;
			combo.setText(""); //$NON-NLS-1$
		} else if (!beanMap.equals(current)) {
			current = beanMap;
			if (list != null) {
				final int i = list.findIndex(beanMap.getId());
				if (i >= 0) {
					combo.select(i + 1);
					return;
				}
			}
			combo.setText(beanMaptoString(beanMap));
		}
	}

	@Override
	public void addSelectionListener(SelectionAdapter selectionAdapter) {
		combo.addSelectionListener(selectionAdapter);
	}

	@Override
	public Widget getWidget() {
		return combo;
	}

	@Override
	public String getListType() {
		return structure.getType();
	}

	@Override
	public void loadedListComplete(ISWTRenderer renderer) {
		if (list != null) {
			if ((sort != null) && (sort.length() > 0)) {
				Collections.sort(list, new Comparator<BeanMap>() {
					@Override
					public int compare(BeanMap beanMap1, BeanMap beanMap2) {
						return beanMap1.getString(sort).compareTo(beanMap2.getString(sort));
					}
				});
			}
			setBeanMapList(list);
		}
	}

	@Override
	public String getAttributeList() {
		return null;
	}

	@Override
	public String getOrderList() {
		return null;
	}
}
