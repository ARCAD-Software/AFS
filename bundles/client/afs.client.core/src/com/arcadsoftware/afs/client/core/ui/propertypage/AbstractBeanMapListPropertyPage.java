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
package com.arcadsoftware.afs.client.core.ui.propertypage;

import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.osgi.framework.Bundle;

import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTableLabelProvider;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.composites.AbstractSearchListComposite;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.IdEqualCriteria;
import com.arcadsoftware.metadata.criteria.NotCriteria;

public abstract class AbstractBeanMapListPropertyPage extends AbstractConnectedPropertypage {

	protected MetaDataEntity entity;
	protected AbstractSearchListComposite composite;
	// protected BeanMapListTableViewer viewer;
	protected AbstractColumnedViewer viewer;
	DataAccessHelper helper;

	private Action editAction;

	public AbstractBeanMapListPropertyPage() {
		super();
		noDefaultAndApplyButton();
	}

	protected Button createButton(Composite parent,
			String text,
			ImageDescriptor imageDescriptor,
			int anchor,
			int offset,
			int width,
			int height) {

		final Button b = new Button(parent, SWT.PUSH);
		b.setText(text);

		final FormData fData = new FormData();
		fData.top = new FormAttachment(0, 0);
		fData.height = height;
		fData.width = width;
		if (anchor == 0) {// left anchor
			fData.left = new FormAttachment(0, offset);
		} else {
			fData.left = new FormAttachment(100, offset);
		}
		b.setLayoutData(fData);
		b.setImage(imageDescriptor.createImage());
		return b;
	}

	protected boolean getDisplayAsTree() {
		return false;
	}

	@Override
	protected Control createContents(Composite parent) {
		final Control c = super.createContents(parent);
		if (c != null) {
			return c;
		}

		helper = new DataAccessHelper(getServerConnection());
		entity = helper.getEntity(getType());
		composite = new AbstractSearchListComposite(parent, entity, getServerConnection(), false) {

			@Override
			public List<Action> getActions() {
				return AbstractBeanMapListPropertyPage.this.getActions();
			}

			@Override
			protected UserMessage getSearchErrorMessage() {
				return AbstractBeanMapListPropertyPage.this.getSearchErrorMessage();
			}

			@Override
			protected Bundle getParentBundle() {
				return AbstractBeanMapListPropertyPage.this.getParentBundle();
			}

			@Override
			protected String createSelectClause() {
				return AbstractBeanMapListPropertyPage.this.createSelectClause();
			}

			@Override
			protected String createSearchClause() {
				return AbstractBeanMapListPropertyPage.this.createSearchClause();
			}

			@Override
			protected String createOrderClause() {
				return AbstractBeanMapListPropertyPage.this.createOrderClause();
			}

			@Override
			protected Image getElementIcon(Object element) {
				return AbstractBeanMapListPropertyPage.this.getElementIcon(element);
			}

			@Override
			protected Image getCustomColumnImage(Object element,
					int actualColumnIndex) {
				return AbstractBeanMapListPropertyPage.this.getCustomColumnImage(element, actualColumnIndex);
			}

			@Override
			public AbstractColumnedTableLabelProvider createSpecificTableLabelProvider(
					AbstractColumnedViewer newViewer) {
				return AbstractBeanMapListPropertyPage.this.createSpecificTableLabelProvider(newViewer);
			}

			@Override
			protected void doOnDoubleClickEvent(IStructuredSelection selection) {
				editAction.run();
			}

			@Override
			public String getViewerIdentifier() {
				return null;
			}

			@Override
			protected String getDisplayedSelectClause() {
				return AbstractBeanMapListPropertyPage.this.getDisplayedSelectClause();
			}

		};

		editAction = new Action() {
			@Override
			public void run() {
				final BeanMap updated = composite.getSelectedResult();
				if (updated != null) {
					if (doUpdate(updated)) {
						// Check that the code is unique
						if (!alreadyExists(updated, true)) {
							if (helper.update(updated)) {
								composite.search();
							}
						} else {
							showViolationMessage(updated);
						}
					}
				}
			}
		};

		viewer = composite.getViewer();

		GridData layoutData = new GridData(GridData.FILL_BOTH);
		layoutData.horizontalSpan = 3;
		layoutData.grabExcessVerticalSpace = true;
		layoutData.grabExcessHorizontalSpace = true;
		composite.setLayoutData(layoutData);

		final Composite bar = new Composite(composite, SWT.NONE);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 3;
		layoutData.grabExcessHorizontalSpace = true;
		bar.setLayoutData(layoutData);
		bar.setLayout(new FormLayout());

		final Button bAdd = createButton(bar,
				Activator.resString(isCreateDeleteStyle() ? "button.create.text" : "button.add.text"),
				isCreateDeleteStyle() ? AFSIcon.CREATE.imageDescriptor() : AFSIcon.ADD.imageDescriptor(),
				1, -100, 100, 25);
		bAdd.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						final BeanMap added = doAdd();
						if (added != null) {
							// Check that the code is unique
							if (!alreadyExists(added, false)) {
								if (helper.create(added)) {
									composite.search();
								}
							} else {
								showViolationMessage(added);
							}
						}
					}
				});

		final Button bUpdate = createButton(bar,
				Activator.resString("button.edit.text"),
				AFSIcon.EDIT.imageDescriptor(),
				1, -208, 100, 25);
		bUpdate.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						editAction.run();
					}
				});

		final Button bDelete = createButton(bar,
				Activator.resString(isCreateDeleteStyle() ? "button.delete.text" : "button.remove.text"),
				isCreateDeleteStyle() ? AFSIcon.DELETE.imageDescriptor() : AFSIcon.REMOVE.imageDescriptor(),
				0, 0, 100, 25);
		bDelete.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {

						if (composite.enableMultiSelection()) {
							final BeanMapList toBeDeleted = composite.getSelectedBeanMap();
							final boolean result = confirmDeletion(null);
							if (result) {
								for (final BeanMap deleted : toBeDeleted) {
									doDelete(deleted, false);
								}
								composite.search();
							}
						} else {
							final BeanMap deleted = composite.getSelectedResult();
							if (deleted != null) {
								if (doDelete(deleted)) {
									composite.search();
								}
							}
						}
					}
				});

		additionalButon(bar);

		composite.search();

		return composite;
	}

	protected void additionalButon(Composite bar) {

	}

	private void showViolationMessage(BeanMap b) {
		final String message = String.format(Activator.resString("msg.error.code.alreadyExists"),
				b.getString(getCodeAttribute()));
		Activator.getDefault().openError(message);
	}

	private boolean alreadyExists(BeanMap b, boolean update) {
		BeanMapList l;
		final EqualCriteria equalCode = new EqualCriteria(getCodeAttribute(), b.getString(getCodeAttribute()));
		if (update) {
			// If test is made during update, we also check that
			// the id is different
			final IdEqualCriteria equalId = new IdEqualCriteria(b.getId());
			final NotCriteria notEqualId = new NotCriteria(equalId);
			final AndCriteria finalCriteria = new AndCriteria();
			finalCriteria.add(equalCode);
			finalCriteria.add(notEqualId);
			l = helper.getList(getType(), finalCriteria);
		} else {
			l = helper.getList(getType(), equalCode);
		}

		return (!l.isEmpty());

	}

	protected BeanMap doAdd() {
		return null;
	}

	protected boolean doUpdate(BeanMap updated) {
		return true;
	}

	protected boolean doDelete(BeanMap deleted) {
		return doDelete(deleted, true);
	}

	protected boolean doDelete(BeanMap deleted, boolean confirm) {
		boolean result = true;
		if (confirm) {
			result = confirmDeletion(deleted);
		}
		if (result) {
			return helper.remove(deleted);
		}
		return false;
	}

	protected boolean confirmDeletion(BeanMap deleted) {
		return Activator.getDefault().openConfirm(Activator.resString("msg.question.action.delete.confirmation"));
	}

	protected String getDisplayedSelectClause() {
		return createSelectClause();
	}

	protected String createSelectClause() {
		return getCodeAttribute() + " " + getNameAttribute();
	}

	protected String createSearchClause() {
		return null;
	}

	public List<Action> getActions() {
		return null;
	}

	protected String createOrderClause() {
		return getNameAttribute();
	}

	public String getCodeAttribute() {
		return "code";
	}

	public String getNameAttribute() {
		return "name";
	}

	public abstract String getType();

	protected Image getCustomColumnImage(Object element,
			int actualColumnIndex) {
		return null;
	}

	public AbstractColumnedTableLabelProvider createSpecificTableLabelProvider(
			AbstractColumnedViewer newViewer) {
		return null;
	}

	protected DataAccessHelper getHelper() {
		return helper;
	}

	protected boolean isCreateDeleteStyle() {
		return true;
	}

	protected abstract UserMessage getSearchErrorMessage();

	protected abstract Bundle getParentBundle();

	protected abstract Image getElementIcon(Object element);

}
