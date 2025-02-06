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

import java.util.Iterator;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTableLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.ColumnedDefaultTableLabelProvider;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.tools.MetadataUtils;
import com.arcadsoftware.afs.framework.ui.viewers.BeanMapListTableViewer;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractBeanMapListSelectorDialog extends AbstractAFSDialog {

	public static BeanMapList selectList(AbstractBeanMapListSelectorDialog dialog) {
		if (dialog.open() == Window.OK) {
			return dialog.getSelection();
		}
		return null;
	}

	public static BeanMap select(AbstractBeanMapListSelectorDialog dialog) {
		if (dialog.open() == Window.OK) {
			return dialog.getSelected();
		}
		return null;
	}

	private MetaDataEntity entity;
	protected ServerConnection connection;
	protected DataAccessHelper helper;
	private BeanMapListTableViewer viewer;
	private boolean multiselection;
	private BeanMap selectedBeanMap;
	private BeanMapList selectedBeanMapList;

	public AbstractBeanMapListSelectorDialog(Shell parentShell, ServerConnection connection, boolean multiselection) {
		this(parentShell, connection, multiselection, true);
		this.multiselection = multiselection;
		initialize(connection);
	}

	public AbstractBeanMapListSelectorDialog(Shell parentShell, ServerConnection connection, boolean multiselection,
			boolean withInit) {
		super(parentShell, true, true);
		this.multiselection = multiselection;
		if (withInit) {
			initialize(connection);
		}
	}

	public AbstractBeanMapListSelectorDialog(Shell parentShell, boolean multiselection) {
		super(parentShell, true, true);
		this.multiselection = multiselection;
	}

	protected void initialize(ServerConnection connection) {
		this.connection = connection;
		helper = new DataAccessHelper(connection);
		entity = helper.getEntity(getType());
	}

	@SuppressWarnings("unchecked")
	private void setSelection(IStructuredSelection selection) {
		selectedBeanMapList = new BeanMapList();
		selectedBeanMap = null;
		if (!selection.isEmpty()) {
			if (multiselection) {
				final Iterator<BeanMap> it = selection.iterator();
				while (it.hasNext()) {
					selectedBeanMapList.add(it.next());
				}
				selectedBeanMap = selectedBeanMapList.get(0);
			} else {
				selectedBeanMap = (BeanMap) selection.getFirstElement();
			}
		}
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		final GridLayout gd = (GridLayout) composite.getLayout();
		gd.marginHeight = gd.marginWidth = 0;
		gd.marginLeft = gd.marginTop = gd.marginRight = gd.marginBottom = 0;
		final String attributeList = getAttributeList();
		int style = SWT.BORDER | SWT.FULL_SELECTION;
		if (multiselection) {
			style = style | SWT.MULTI;
		}
		viewer = new BeanMapListTableViewer(composite, style, entity, attributeList) {

			@Override
			protected void doOnDoubleClick(IStructuredSelection selection) {
				setSelection(selection);
				okPressed();
			}

			@Override
			protected void doOnSelectionChange(IStructuredSelection selection) {
				setSelection(selection);
			}

			@Override
			public String getIdentifier() {
				return null;
			}

			@Override
			public AbstractColumnedTableLabelProvider createTableLabelProvider(AbstractColumnedViewer newViewer) {
				final AbstractColumnedTableLabelProvider provider = new ColumnedDefaultTableLabelProvider(newViewer) {
					@Override
					protected Image getActualImage(Object element,
							int actualColumnIndex) {
						Image image = AbstractBeanMapListSelectorDialog.this.getActualImage(element, actualColumnIndex);
						if (image == null) {
							image = super.getActualImage(element, actualColumnIndex);
						}
						return image;
					}

					@Override
					public String getColumnText(Object element, int columnIndex) {
						final String value = AbstractBeanMapListSelectorDialog.this.getColumnText(element, columnIndex);
						if (value != null) {
							return value;
						}
						return super.getColumnText(element, columnIndex);
					}
				};
				return provider;
			}

			@Override
			protected int getColumnSize(String attribute) {
				final int size = AbstractBeanMapListSelectorDialog.this.getColumnSize(attribute);
				if (size != -1) {
					return size;
				}
				return super.getColumnSize(attribute);
			}

			@Override
			protected String getColumnHeader(String attribute) {
				String header = AbstractBeanMapListSelectorDialog.this.getColumnHeader(attribute);
				if (header == null) {
					header = super.getColumnHeader(attribute);
				}
				return header;
			}
		};
		viewer.getViewer().getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
		viewer.setInput(getInput());
		return composite;
	}

	public BeanMap getSelected() {
		return selectedBeanMap;
	}

	public BeanMapList getSelection() {
		return selectedBeanMapList;
	}

	public Image getElementIcon() {
		return null;
	}

	/**
	 * Return the type of the BeanMap you want to manage
	 *
	 * @return beanMap type
	 */
	public abstract String getType();

	public abstract String getAttributeList();

	public abstract BeanMapList getInput();

	public BeanMapListTableViewer getViewer() {
		return viewer;
	}

	protected Image getActualImage(Object element, int actualColumnIndex) {
		if (actualColumnIndex == 0) {
			return getElementIcon();
		} else {
			return null;
		}
	}

	protected int getColumnSize(String attribute) {
		final MetaDataAttribute metaDataAttribute = MetadataUtils.getInstance().resolveMetaDataAttribute(helper, entity,
				attribute);
		if (metaDataAttribute != null) {
			return metaDataAttribute.getColSize();
		}
		return -1;
	}

	protected String getColumnHeader(String attribute) {
		return null;
	}

	protected String getColumnText(Object element, int columnIndex) {
		return null;
	}

	protected MetaDataEntity getEntity() {
		return entity;
	}
}
