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
package com.arcadsoftware.afs.client.core.ui.widgets;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.composites.AbstractConnectedComposite;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataFormater;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;

public class ConnectedBeanMapCombo extends AbstractConnectedComposite {
	private BeanMapList list;
	private Combo combo;
	private final String label;
	private final String description;
	private BeanMap current;
	private MetaDataFormater attributesFormater;

	// First Not Empty Index
	private final int firstNotEmpty;

	private final boolean readonly;

	public ConnectedBeanMapCombo(Composite parent, int style, ServerConnection connection, String label) {
		this(parent, SWT.NONE, connection, label, false, false);
	}

	public ConnectedBeanMapCombo(Composite parent, int style, ServerConnection connection, String label,
			boolean isfirstEmpty, boolean readonly) {
		this(parent, style, connection, label, isfirstEmpty, readonly, null);

	}

	public ConnectedBeanMapCombo(Composite parent, int style, ServerConnection connection, String label,
			boolean isfirstEmpty, boolean readonly, String description) {
		super(parent, style, connection, false);
		setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
		this.label = label;
		firstNotEmpty = isfirstEmpty ? 1 : 0;
		this.readonly = readonly;
		this.description = description;
		createContent(this);
	}

	@Override
	protected void format() {
		super.format();
		final GridLayout gridLayout = (GridLayout) getLayout();
		gridLayout.marginLeft = gridLayout.marginWidth = 0;
	}

	@Override
	public void createContent(Composite parent) {
		combo = GuiFormatTools.createLabelledCombo(parent, label, readonly);
		if (description != null) {
			GuiFormatTools.createDescription(parent, description);
		}
	}

	public void loadData(String type, String format) {
		if (format != null) {
			attributesFormater = new MetaDataFormater(format, helper.getEntity(type));
		}
		setBeanMapList(helper.getList(type));
	}

	public void loadData(String type, String format, ISearchCriteria criteria) {
		if (format != null) {
			attributesFormater = new MetaDataFormater(format, helper.getEntity(type));
		}
		setBeanMapList(helper.getList(type, criteria));
	}

	public void loadData(String type, String format, ISearchCriteria criteria, String orders) {
		if (format != null) {
			attributesFormater = new MetaDataFormater(format, helper.getEntity(type));
		}
		setBeanMapList(helper.getList(type, criteria, orders));
	}

	public void loadLinkedData(BeanMap master, String link, String type, String format) {
		if (format != null) {
			attributesFormater = new MetaDataFormater(format, helper.getEntity(type));
		}
		setBeanMapList(helper.getLinkList(master, link, type));
	}

	private String beanMaptoString(BeanMap bean) {
		String result;
		if (attributesFormater != null) {
			result = attributesFormater.format(bean);
		} else {
			result = bean.toString();
		}
		return result;
	}

	/**
	 * Set list of beanMaps
	 *
	 * @param list
	 *            list of beanMaps. Can be null
	 */
	public void setBeanMapList(BeanMapList list) {
		if (list == null) {
			list = new BeanMapList();
		}
		this.list = list;
		final String[] items = new String[list.size() + firstNotEmpty];

		if ((list.size() + firstNotEmpty) > 0) {
			items[0] = ""; //$NON-NLS-1$
		}

		int curid = 0;
		if (current != null) {
			curid = current.getId();
		}
		int selection = -1;
		for (int i = 0; i < list.size(); i++) {
			final BeanMap beanMap = list.get(i);
			items[i + firstNotEmpty] = beanMaptoString(beanMap);
			if (beanMap.getId() == curid) {
				selection = i + firstNotEmpty;
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

	/**
	 * Select the first not empty item
	 *
	 * @param index
	 */
	public void selectFirstNotEmpty() {
		if ((list != null) && (list.size() > 0)) {
			combo.select(firstNotEmpty);
			current = list.get(0);
		}
	}

	public void addBeanMapToList(int index, BeanMap beanMap) {
		if (list == null) {
			list = new BeanMapList();
		}
		list.add(beanMap);
	}

	public BeanMap getBeanMapValue() {
		final int index = combo.getSelectionIndex();
		if ((index < 0) || (index > list.size())) {
			return current;
		}
		if (index < firstNotEmpty) {
			return null;
		}
		return list.get(index - firstNotEmpty);
	}

	public void setBeanMapValue(BeanMap beanMap) {
		if (beanMap == null) {
			current = null;
			combo.setText(""); //$NON-NLS-1$
		} else if (!beanMap.equals(current)) {
			current = beanMap;
			if (list != null) {
				final int i = list.findIndex(beanMap.getId());
				if (i >= 0) {
					combo.select(i + firstNotEmpty);
					return;
				}
			}
			combo.setText(beanMaptoString(beanMap));
		}
	}

	/**
	 * Set current from beanMap Id
	 *
	 * @param id
	 */
	public void setBeanMapIdValue(int id) {
		if (id <= 0) {
			current = null;
			combo.setText(""); //$NON-NLS-1$
			combo.select(-1);
		} else if ((current == null) || (current.getId() != id)) {
			if (list != null) {
				final int i = list.findIndex(id);
				if (i >= 0) {
					current = list.get(i);
					combo.select(i + firstNotEmpty);
					return;
				}
			}
			if (current != null) {
				combo.setText(beanMaptoString(current));
			}
		}
	}

	public void addSelectionListener(SelectionAdapter selectionAdapter) {
		combo.addSelectionListener(selectionAdapter);
	}

	@Override
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		combo.setEnabled(enabled);
	}

}
