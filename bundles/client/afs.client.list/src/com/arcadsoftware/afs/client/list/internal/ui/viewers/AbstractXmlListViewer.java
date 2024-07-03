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
package com.arcadsoftware.afs.client.list.internal.ui.viewers;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.aev.core.contentproviders.IObjectArrayProvider;
import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumn;
import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumns;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTableLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.ColumnedDefaultTableLabelProvider;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTableViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.mmk.lists.AbstractXmlList;
import com.arcadsoftware.mmk.lists.IListBrowseListener;
import com.arcadsoftware.mmk.lists.metadata.ListColumnDef;
import com.arcadsoftware.mmk.lists.metadata.ListMetaDatas;
import com.arcadsoftware.mmk.lists.metadata.StoreItem;

public abstract class AbstractXmlListViewer extends AbstractColumnedTableViewer {

	private final ListMetaDatas metadatas;

	private class SimpleBrowser implements IListBrowseListener {
		public int index = 0;
		public ListMetaDatas metadata;
		private final Object[] reciever;

		public SimpleBrowser(Object[] reciever, ListMetaDatas metadata) {
			this.reciever = reciever;
			this.metadata = metadata;
		}

		@Override
		public void elementBrowsed(StoreItem object) {
			final StoreItem dup = new StoreItem(metadata);
			final String values[] = object.getValues();
			for (int i = 0; i < values.length; i++) {
				final String value = values[i];
				dup.setValue(i, value);
			}

			reciever[index++] = dup;
		}
	}

	private class XmlListInput implements IObjectArrayProvider {

		private Object[] values;
		private final AbstractXmlList list;

		public XmlListInput(AbstractXmlList list) {
			this.list = list;
			populate();
		}

		public void populate() {
			list.load(true, false);
			values = new Object[list.getElementCount()];
			final IListBrowseListener listener = new SimpleBrowser(values, list.getMetadatas());
			list.addBrowseListener(listener);
			try {
				list.browse();
			} finally {
				list.removeBrowseListener(listener);
			}
		}

		@Override
		public Object[] getObjectArray() {
			return values;
		}

	}

	public AbstractXmlListViewer(Composite parent, int style, ListMetaDatas metadatas) {
		super(parent, style, false);
		this.metadatas = metadatas;
		init();
	}

	@Override
	public AbstractColumnedTableLabelProvider createTableLabelProvider(
			AbstractColumnedViewer viewer) {
		return new ColumnedDefaultTableLabelProvider(viewer) {
			@Override
			protected Image getActualImage(Object element, int actualColumnIndex) {
				if (actualColumnIndex == 0) {
					return AFSIcon.LIST.image();
				} else {
					final Image image = getCustomColumnImage(element, actualColumnIndex);
					if (image == null) {
						return super.getActualImage(element, actualColumnIndex);
					}
					return image;
				}
			}
		};
	}

	@Override
	public String getValue(Object element, int columnIndex) {
		if (element instanceof StoreItem) {
			final StoreItem si = (StoreItem) element;
			return si.getValue(columnIndex);
		}
		return "";
	}

	protected Image getCustomColumnImage(Object element, int actualColumnIndex) {
		return null;
	}

	@Override
	public ArcadColumns getReferenceColumns() {
		final ArcadColumns refColumns = new ArcadColumns();
		if (metadatas != null) {
			for (int i = 0; i < metadatas.count(); i++) {
				final ListColumnDef colDefinition = metadatas.getColumnDefAt(i);
				final ArcadColumn col = new ArcadColumn();
				final String name = colDefinition.getPropertyName();
				final String header = getColumnHeader(name);
				col.setIdentifier(name);
				col.setName(header);
				col.setUserName(header);
				col.setVisible(ArcadColumn.VISIBLE);
				col.setPosition(i);
				col.setActualIndex(i);
				col.setWidth(getColumnSize(name));
				refColumns.add(col);
			}
		}
		return refColumns;
	}

	@Override
	public String getIdentifier() {
		return null;
	}

	public void setListInput(AbstractXmlList list) {
		final XmlListInput xi = new XmlListInput(list);
		setInput(xi);
	}

	protected abstract String getColumnHeader(String propertyName);

	protected abstract int getColumnSize(String propertyName);

}
