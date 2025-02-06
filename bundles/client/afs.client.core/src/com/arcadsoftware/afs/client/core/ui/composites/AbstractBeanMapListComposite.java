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
package com.arcadsoftware.afs.client.core.ui.composites;

import java.text.SimpleDateFormat;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.osgi.framework.Bundle;

import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTableLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTreeLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.ColumnedDefaultTableLabelProvider;
import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.tools.MetadataUtils;
import com.arcadsoftware.afs.client.core.ui.actions.IBeanMapActionListener;
import com.arcadsoftware.afs.client.core.ui.actions.IListActions;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapSelectionListener;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;
import com.arcadsoftware.afs.framework.ui.viewers.BeanMapListTableViewer;
import com.arcadsoftware.afs.framework.ui.viewers.BeanMapListTreeViewer;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractBeanMapListComposite extends Composite implements IListActions, IBeanMapActionListener {

	private final ListenerList<IBeanMapSelectionListener> beanMapSelectionListener = new ListenerList<>();
	private AbstractColumnedViewer viewer;
	protected MetaDataEntity entityStructure;
	private BeanMap selectedBeanMap;
	private Composite globalCmp;
	protected ServerConnection connection;
	protected DataAccessHelper helper;

	public AbstractBeanMapListComposite(Composite parent, MetaDataEntity entityStructure, ServerConnection connection) {
		super(parent, SWT.NONE);
		this.entityStructure = entityStructure;
		this.connection = connection;
		helper = new DataAccessHelper(connection);
		createContent();
	}

	public AbstractBeanMapListComposite(Composite parent, String entityType, ServerConnection connection) {
		super(parent, SWT.NONE);
		this.connection = connection;
		helper = new DataAccessHelper(connection);
		entityStructure = helper.getEntity(entityType);
		createContent();
	}

	protected void setActions(List<Action> actions) {
		if (viewer instanceof BeanMapListTreeViewer) {
			((BeanMapListTreeViewer) viewer).setActions(actions);
		}
		if (viewer instanceof BeanMapListTableViewer) {
			((BeanMapListTableViewer) viewer).setActions(actions);
		}
	}

	protected AbstractColumnedViewer createTableViewer(Composite parent) {
		int style = SWT.BORDER | SWT.FULL_SELECTION;
		if (enableMultiSelection()) {
			style = style | SWT.MULTI;
		}
		return new BeanMapListTableViewer(globalCmp, style, entityStructure,
				getDisplayedSelectClause()) {
			@Override
			protected void doOnDoubleClick(IStructuredSelection selection) {
				doOnDoubleClickEvent(selection);
			}

			@Override
			protected void doOnSelectionChange(IStructuredSelection selection) {
				if (!selection.isEmpty() && (selection.getFirstElement() instanceof BeanMap)) {
					selectedBeanMap = (BeanMap) selection.getFirstElement();
					fireBeanMapSelection(selectedBeanMap);
					doOnSelection(selectedBeanMap);
				}
			}

			@Override
			public String getIdentifier() {
				return getViewerIdentifier();
			}

			@Override
			public AbstractColumnedTableLabelProvider createTableLabelProvider(AbstractColumnedViewer newViewer) {
				AbstractColumnedTableLabelProvider provider = createSpecificTableLabelProvider(newViewer);
				if (provider == null) {
					provider = new ColumnedDefaultTableLabelProvider(newViewer) {
						@Override
						protected Image getActualImage(Object element,
								int actualColumnIndex) {
							if (actualColumnIndex == 0) {
								return getElementIcon(element);
							}
							final Image image = AbstractBeanMapListComposite.this.getCustomColumnImage(element,
									actualColumnIndex);
							if (image == null) {
								return super.getActualImage(element, actualColumnIndex);
							}
							return image;
						}

						@Override
						public String getValue(Object element, int columnIndex) {
							final String value = getCustomValue(element, columnIndex);
							if (value == null) {
								return super.getValue(element, columnIndex);
							}
							return value;
						}
					};
				}
				return provider;
			}

			@Override
			public SimpleDateFormat getDateFormatter() {
				final SimpleDateFormat sd = AbstractBeanMapListComposite.this.getDateFormatter();
				if (sd != null) {
					return sd;
				}
				return super.getDateFormatter();
			}

			@Override
			protected String getColumnHeader(String attribute) {
				final String header = AbstractBeanMapListComposite.this.getColumnHeader(attribute);
				if (header != null) {
					return header;
				}
				return super.getColumnHeader(attribute);
			}

			@Override
			protected int getColumnSize(String attribute) {
				final int size = AbstractBeanMapListComposite.this.getColumnSize(attribute);
				if (size != -1) {
					return size;
				}
				return super.getColumnSize(attribute);
			}

			@Override
			protected Image getCustomColumnImage(Object element,
					int actualColumnIndex) {
				final Image image = AbstractBeanMapListComposite.this.getCustomColumnImage(element, actualColumnIndex);
				if (image != null) {
					return image;
				}
				return super.getCustomColumnImage(element, actualColumnIndex);
			}
		};
	}

	protected void doOnSelection(BeanMap selected) {
	}

	protected AbstractColumnedViewer createTreeViewer(Composite parent) {
		return new BeanMapListTreeViewer(parent,
				SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI, entityStructure,
				getDisplayedSelectClause()) {
			@Override
			protected void doOnDoubleClick(IStructuredSelection selection) {
				doOnDoubleClickEvent(selection);
			}

			@Override
			protected void doOnSelectionChange(IStructuredSelection selection) {
				if (!selection.isEmpty() && (selection.getFirstElement() instanceof BeanMap)) {
					selectedBeanMap = (BeanMap) selection.getFirstElement();
					fireBeanMapSelection(selectedBeanMap);
				}
			}

			@Override
			public String getIdentifier() {
				return getViewerIdentifier();
			}

			@Override
			public AbstractColumnedTreeLabelProvider createTreeLabelProvider(AbstractColumnedViewer newViewer) {
				AbstractColumnedTreeLabelProvider provider = createSpecificTreeLabelProvider(newViewer);
				if (provider == null) {
					provider = super.createTreeLabelProvider(newViewer);
				}
				return provider;
			}

			@Override
			public IContentProvider createContentProvider() {
				final IContentProvider provider = createSpecificContentProvider();
				if (provider == null) {
					return super.createContentProvider();
				}
				return provider;
			}

			@Override
			public SimpleDateFormat getDateFormatter() {
				final SimpleDateFormat sd = AbstractBeanMapListComposite.this.getDateFormatter();
				if (sd != null) {
					return sd;
				}
				return super.getDateFormatter();
			}

			@Override
			protected String getColumnHeader(String attribute) {
				final String header = AbstractBeanMapListComposite.this.getColumnHeader(attribute);
				if (header != null) {
					return header;
				}
				return super.getColumnHeader(AbstractBeanMapListComposite.this.transformColumnHeader(attribute));
			}

			@Override
			protected int getColumnSize(String attribute) {
				final int size = AbstractBeanMapListComposite.this.getColumnSize(attribute);
				if (size != -1) {
					return size;
				}
				return super.getColumnSize(attribute);
			}

			@Override
			protected Image getCustomColumnImage(Object element, int actualColumnIndex) {
				final Image image = AbstractBeanMapListComposite.this.getCustomColumnImage(element, actualColumnIndex);
				if (image != null) {
					return image;
				}
				return super.getCustomColumnImage(element, actualColumnIndex);
			}

		};
	}

	protected Composite createViewerComposite(Composite parent) {
		return parent;
	}

	protected void addContentBeforeViewer(Composite parent) {
	}

	protected void addContentAfterViewer(Composite parent) {
	}

	protected boolean getDisplayAsTree() {
		return false;
	}

	protected AbstractColumnedViewer createViewer(Composite parent) {
		if (getDisplayAsTree()) {
			return createTreeViewer(parent);
		}
		return createTableViewer(parent);
	}

	protected void createContent() {
		final GridLayout layout = new GridLayout(1, true);
		layout.marginHeight = layout.marginWidth = 0;
		setLayout(layout);
		setLayoutData(new GridData(GridData.FILL_BOTH));
		globalCmp = GuiFormatTools.createComposite(this, 3, false, SWT.NONE, true);
		final GridLayout l = (GridLayout) globalCmp.getLayout();
		l.marginHeight = l.marginWidth = 0;
		globalCmp.layout();
		final Composite viewerComposite = createViewerComposite(globalCmp);
		addContentBeforeViewer(viewerComposite);
		viewer = createViewer(viewerComposite);
		addContentAfterViewer(viewerComposite);
		setActions(getActions());
		viewer.getViewer().getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
	}

	public static Label createLabelledLabel(Composite parent, String label) {
		final Label textLabel = new Label(parent, SWT.NONE | SWT.WRAP);
		GridData gridData = new GridData(SWT.CENTER, SWT.CENTER, false, false);
		textLabel.setLayoutData(gridData);
		textLabel.setText(label);
		final Label twopoints = new Label(parent, SWT.NONE);
		twopoints.setText(":"); //$NON-NLS-1$
		gridData = new GridData(SWT.BEGINNING, SWT.CENTER, false, false);
		twopoints.setLayoutData(gridData);
		final Label lbl = new Label(parent, SWT.NONE);
		lbl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		lbl.setData(textLabel);
		return lbl;
	}

	public ServerConnection getConnection() {
		return connection;
	}

	/**
	 * Return the viwer selection
	 *
	 * @return
	 */
	public IStructuredSelection getViewerSelection() {
		return viewer.getSelection();
	}

	/**
	 * Returns the current selected BeanMaps into a BeanMapList
	 *
	 * @return the BeanMapList that contains all the selected elements of the list.
	 */
	public BeanMapList getSelectedBeanMap() {
		final IStructuredSelection selection = viewer.getSelection();
		final Iterator<?> it = selection.iterator();
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
	 * Returns the first selected beanMap.
	 *
	 * @return The selected beanMap
	 */
	public BeanMap getSelectedResult() {
		return selectedBeanMap;
	}

	/**
	 * Uset his method to change the viewer Input
	 *
	 * @param list
	 *            the new BeanMapList you want to define as the current input
	 */
	public void contentChanged(BeanMapList list) {
		viewer.setInput(list);
		viewer.refresh();
	}

	/**
	 * Allows to add a new listener on the selection
	 *
	 * @param listener
	 *            a lister to the selection
	 */
	public void addBeanMapSelectionListener(IBeanMapSelectionListener listener) {
		beanMapSelectionListener.add(listener);
	}

	/**
	 * Allows to remove a existing listener
	 *
	 * @param listener
	 */
	public void removeBeanMapSelectionListener(IBeanMapSelectionListener listener) {
		beanMapSelectionListener.remove(listener);
	}

	private void fireBeanMapSelection(BeanMap selected) {
		for (final Object element : beanMapSelectionListener.getListeners()) {
			final IBeanMapSelectionListener listener = (IBeanMapSelectionListener) element;
			try {
				listener.beanMapSelected(selected);
			} catch (final Exception e) {
				LogUITools.logError(Activator.getDefault().getBundle(), e);
				beanMapSelectionListener.remove(listener);
			}
		}
	}

	/**
	 * Returns a column Header label computed from the attribute.
	 * <p>
	 * This function first calls the {@link #useDefaultColumnHeader(String) to know if you want to manually manage the
	 * return of the value.
	 * </p>
	 * <p>
	 * If the call to {@link #useDefaultColumnHeader(String) returns false, this method resolves the final
	 * MetadataAtribute and returns its name. </p>
	 *
	 * @param attribute
	 * @return the colunm label
	 */
	protected String getColumnHeader(String attribute) {
		if (useDefaultColumnHeader(attribute)) {
			return null;
		}
		final MetaDataAttribute metaDataAttribute = MetadataUtils.getInstance().resolveMetaDataAttribute(helper,
				entityStructure, attribute);
		if (metaDataAttribute != null) {
			return metaDataAttribute.getName();
		}
		return null;
	}

	/**
	 * Returns an indicator to say if you want the system to compute the column header using the primary entity. If
	 * return true, the system will try to compute the header using the attribute and the primary entity</br>
	 * If return false, the system will try to compute the value using the deeper entity and the attribute.
	 * <p>
	 * Example:<br>
	 * <ul>
	 * <li>if the primary entity is <code>artifact</code></li>
	 * <li>if the attribute equals to <code>creationrelease.number</code></br>
	 * </li>
	 * </ul>
	 * <p>
	 * if the method returns false: the system will return the label of the <code>number</code> attribute into the
	 * entity <code>release</code> (because the type of <code>creationrelease</code> is <code>release</code>).
	 * </p>
	 * <p>
	 * If this method return true, the system will return the label of the <code>creationrelease.number</code> attribute
	 * into the entity <code>artefact</code><br>
	 * In this case, you can also transform the attribute name before trying to resolve the name by using the
	 * {@link #transformColumnHeader(String)}.
	 * </p>
	 * </p>
	 *
	 * @param attribute
	 * @return
	 */
	protected boolean useDefaultColumnHeader(String attribute) {
		return false;
	}

	protected String transformColumnHeader(String attribute) {
		return attribute;
	}

	protected int getColumnSize(String attribute) {
		final MetaDataAttribute metaDataAttribute = MetadataUtils.getInstance().resolveMetaDataAttribute(helper,
				entityStructure, attribute);
		if (metaDataAttribute != null) {
			return metaDataAttribute.getColSize();
		}
		return -1;
	}

	protected Image getCustomColumnImage(Object element, int actualColumnIndex) {
		return null;
	}

	protected String getCustomValue(Object element, int actualColumnIndex) {
		return null;
	}

	/**
	 * Returns the underlying viewer
	 *
	 * @return
	 */
	public AbstractColumnedViewer getViewer() {
		return viewer;
	}

	/**
	 * Override this method to define you own viwer Identifier
	 *
	 * @return the new Viwer Identifier
	 */
	public String getViewerIdentifier() {
		return getClass().getName();
	}

	/**
	 * Override this method to define the behavior whene the user double click on a element on the list
	 *
	 * @param selection
	 *            The current selection selection
	 */
	protected void doOnDoubleClickEvent(IStructuredSelection selection) {
	}

	/**
	 * Override this method to define your own AbstractColumnedTableLabelProvider
	 *
	 * @param newViewer
	 *            the current BeanMapListTableViewer
	 * @return your own AbstractColumnedTableLabelProvider
	 */
	public AbstractColumnedTableLabelProvider createSpecificTableLabelProvider(AbstractColumnedViewer newViewer) {
		return null;
	}

	/**
	 * Override this method to define your own AbstractColumnedTableLabelProvider
	 *
	 * @param newViewer
	 *            the current BeanMapListTableViewer
	 * @return your own AbstractColumnedTableLabelProvider
	 */
	public AbstractColumnedTreeLabelProvider createSpecificTreeLabelProvider(AbstractColumnedViewer newViewer) {
		return null;
	}

	public IContentProvider createSpecificContentProvider() {
		return null;
	}

	/**
	 * Override this method to define your own SimpleDateFormatter that will be used to display the the Date BeanMap
	 * Value
	 *
	 * @return your own SimpleDateFormat object
	 */
	protected SimpleDateFormat getDateFormatter() {
		return null;
	}

	/**
	 * Define the action that will be available for the BeanMapList
	 */
	@Override
	public abstract List<Action> getActions();

	/**
	 * A user defined error message that will be displayed if an error occurred during retrieving data
	 *
	 * @return an error Message
	 */
	protected abstract UserMessage getSearchErrorMessage();

	/**
	 * Returns the parent bundle used to display the message If this reference is null, the internal activator will be
	 * used instead.
	 *
	 * @return a bunble reference
	 */
	protected abstract Bundle getParentBundle();

	/**
	 * Returns a blank separated attribute list of the attribute you want to see in the table
	 *
	 * @return
	 */
	protected abstract String createSelectClause();

	protected abstract Image getElementIcon(Object element);

	protected String getDisplayedSelectClause() {
		return createSelectClause();
	}

	protected boolean enableMultiSelection() {
		return true;
	}

	public Object getInput() {
		return viewer.getInput();
	}

	public void add(BeanMap beanMap) {
		if (getInput() instanceof BeanMapList) {
			((BeanMapList) getInput()).add(beanMap);
			viewer.refresh();
		}

	}

	public void remove(BeanMap beanMap) {
		if (getInput() instanceof BeanMapList) {
			((BeanMapList) getInput()).remove(beanMap);
			viewer.refresh();
		}
	}

	public void remove(BeanMapList toRemove) {
		if (getInput() instanceof BeanMapList) {
			final BeanMapList list = (BeanMapList) getInput();
			for (final BeanMap b : toRemove) {
				list.remove(b);
			}
			viewer.refresh();
		}
	}

	@Override
	public void actionDone(int type, BeanMap bean) {
		switch (type) {
		case IBeanMapActionListener.ACTION_ADD:
			final BeanMap completed = helper.read(bean.getType(), bean.getId(), createSelectClause());
			bean.addAll(completed);
			add(bean);
			break;
		case IBeanMapActionListener.ACTION_DELETE:
			remove(bean);
			break;
		case IBeanMapActionListener.ACTION_REFRESH:
			if ((viewer != null) && !viewer.getControl().isDisposed()) {
				viewer.refresh();
			}
			break;
		}
	}

	@Override
	public void actionDone(int type, BeanMapList list) {
		switch (type) {
		case IBeanMapActionListener.ACTION_DELETE:
			remove(list);
			break;
		case IBeanMapActionListener.ACTION_REFRESH:
			if ((viewer != null) && !viewer.getControl().isDisposed()) {
				viewer.refresh();
			}
			break;
		}
	}

}
