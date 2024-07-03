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
package com.arcadsoftware.afs.framework.ui.viewers;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumn;
import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumns;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTableLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.ColumnedDefaultTableLabelProvider;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTableViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.ColumnedActionSeparator;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public class BeanMapListTableViewer extends AbstractColumnedTableViewer {

	private final SimpleDateFormat defaultFormatter = new SimpleDateFormat("MM/dd/yyyy"); //$NON-NLS-1$
	private final Hashtable<Integer, String> positionToAttribute = new Hashtable<>();
	protected MetaDataEntity entity;
	private String attributeList;
	private List<Action> actions;

	public BeanMapListTableViewer(Composite parent, int style) {
		super(parent, style, false);
	}

	public BeanMapListTableViewer(Composite parent, int style, MetaDataEntity entity, String attributeList) {
		super(parent, style, false);
		this.attributeList = attributeList;
		this.entity = entity;
		init();
	}

	public void initialize(MetaDataEntity entity, String attributeList) {
		this.attributeList = attributeList;
		this.entity = entity;
		init();
	}

	@Override
	public AbstractColumnedTableLabelProvider createTableLabelProvider(AbstractColumnedViewer viewer) {
		return new ColumnedDefaultTableLabelProvider(viewer) {
			@Override
			protected Image getActualImage(Object element, int actualColumnIndex) {
				final Image image = getCustomColumnImage(element, actualColumnIndex);
				if (image == null) {
					return super.getActualImage(element, actualColumnIndex);
				}
				return image;
			}
		};
	}

	protected Image getCustomColumnImage(Object element, int actualColumnIndex) {
		return null;
	}

	protected String getColumnHeader(String attribute) {
		final MetaDataAttribute metaAttribute = entity.getAttribute(attribute);
		if (metaAttribute != null) {
			if (metaAttribute.getName() != null) {
				return metaAttribute.getName();
			}
		}
		return ""; //$NON-NLS-1$
	}

	protected int getColumnSize(String attribute) {
		final MetaDataAttribute metaAttribute = entity.getAttribute(attribute);
		if (metaAttribute != null) {
			return metaAttribute.getColSize();
		}
		return 100;
	}

	@Override
	public ArcadColumns getReferenceColumns() {
		final ArcadColumns refColumns = new ArcadColumns();
		final String[] values = attributeList.split(" "); //$NON-NLS-1$
		for (int i = 0; i < values.length; i++) {
			final ArcadColumn col = new ArcadColumn();
			final String value = values[i];
			col.setIdentifier(value);
			final String header = getColumnHeader(value);
			col.setName(header);
			col.setUserName(header);
			col.setVisible(ArcadColumn.VISIBLE);
			col.setPosition(i);
			col.setActualIndex(i);
			col.setWidth(getColumnSize(value));
			positionToAttribute.put(i, value);
			refColumns.add(col);
		}
		return refColumns;
	}

	@Override
	public Object getTypedValue(Object element, int columnIndex) {
		final String attributeName = positionToAttribute.get(columnIndex);
		final BeanMap beanMap = (BeanMap) element;
		final Object value = beanMap.get(attributeName);
		if (value != null) {
			return value;
		}
		return getValue(element, columnIndex);
	}

	public SimpleDateFormat getDateFormatter() {
		return defaultFormatter;
	}

	/**
	 * Get Attribute Name from Column Index
	 *
	 * @param columnIndex
	 * @return
	 */
	public String positionToAttribute(int columnIndex) {
		return positionToAttribute.get(columnIndex);
	}

	@Override
	public String getValue(Object element, int columnIndex) {
		final String attributeName = positionToAttribute.get(columnIndex);
		final BeanMap beanMap = ((BeanMap) element);
		final Object o = beanMap.get(attributeName);
		if (o instanceof Date) {
			SimpleDateFormat sd = getDateFormatter();
			if (sd == null) {
				sd = defaultFormatter;
			}
			return sd.format((Date) o);
		}
		if (o == null) {
			final MetaDataAttribute attribute = entity.getAttribute(attributeName);
			if ((attribute != null) && attribute.getType().equalsIgnoreCase(MetaDataAttribute.TYPE_DATE)) {
				SimpleDateFormat sd = getDateFormatter();
				if (sd == null) {
					sd = defaultFormatter;
				}
				return sd.format(new Date(0));
			}
		}
		return beanMap.getString(attributeName);
	}

	@Override
	protected List<Action> getNextActions() {
		final List<Action> result = new ArrayList<>();
		if (actions != null) {
			for (final Action action : actions) {
				if (action != null) {
					if (adaptActionToSelection(action)) {
						result.add(action);
					}
				} else {
					result.add(new ColumnedActionSeparator());
				}
			}
		}
		return result;
	}

	protected boolean adaptActionToSelection(Action action) {
		return true;
	}

	/**
	 * @param actions
	 *            the actions to set
	 */
	public void setActions(List<Action> actions) {
		this.actions = actions;
	}

	/**
	 * Get set actions
	 */
	public List<Action> getActions() {
		return actions;
	}

	/**
	 * Returns the current selected BeanMaps into a BeanMapList.
	 *
	 * @return the BeanMapList that contains all the selected elements of the list.
	 */
	public BeanMapList getSelectedBeanMapList() {
		final Iterator<?> it = ((IStructuredSelection) getViewer().getSelection()).iterator();
		final BeanMapList result = new BeanMapList();
		while (it.hasNext()) {
			final Object o = it.next();
			if (o instanceof BeanMap) {
				result.add((BeanMap) o);
			}
		}
		return result;
	}

	/**
	 * Returns the current selected BeanMaps into a BeanMapList
	 *
	 * @return the BeanMapList that contains all the selected elements of the list.
	 */
	public BeanMap getSelectedBeanMap() {
		final BeanMapList result = getSelectedBeanMapList();
		if (result.size() > 0) {
			return result.get(0);
		}
		return null;
	}

}
