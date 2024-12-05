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
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.IdEqualCriteria;
import com.arcadsoftware.metadata.criteria.NotCriteria;

public abstract class AbstractBeanMapListPropertyPage extends AbstractConnectedPropertypage {

	protected MetaDataEntity entity;
	protected AbstractSearchListComposite composite;
	protected AbstractColumnedViewer viewer;
	private DataAccessHelper helper;
	private Action editAction;

	public AbstractBeanMapListPropertyPage() {
		super();
		noDefaultAndApplyButton();
	}

	protected Button createButton(Composite parent, String text, ImageDescriptor imageDescriptor, int anchor, int offset, int width, int height) {
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
		String text;
		ImageDescriptor img;
		if (isCreateDeleteStyle()) {
			text = "button.create.text";
			img = AFSIcon.CREATE.imageDescriptor();
		} else {
			text = "button.add.text";
			img = AFSIcon.ADD.imageDescriptor();
		}
		final Button bAdd = createButton(bar, Activator.resString(text), img, 1, -100, 100, 25);
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
		final Button bUpdate = createButton(bar, Activator.resString("button.edit.text"), //$NON-NLS-1$
				AFSIcon.EDIT.imageDescriptor(), 1, -208, 100, 25);
		bUpdate.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						editAction.run();
					}
				});
		if (isCreateDeleteStyle()) {
			text = "button.delete.text"; //$NON-NLS-1$
			img = AFSIcon.DELETE.imageDescriptor();
		} else {
			text = "button.remove.text"; //$NON-NLS-1$
			img = AFSIcon.REMOVE.imageDescriptor();
		}
		final Button bDelete = createButton(bar, Activator.resString(text), img, 0, 0, 100, 25);
		bDelete.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						if (composite.enableMultiSelection()) {
							if (confirmDeletion(null)) {
								for (final BeanMap deleted : composite.getSelectedBeanMap()) {
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

	protected void additionalButon(Composite bar) {}

	private void showViolationMessage(BeanMap b) {
		final String message = String.format(Activator.resString("msg.error.code.alreadyExists"), b.getString(getCodeAttribute())); //$NON-NLS-1$
		Activator.getDefault().openError(message);
	}

	private boolean alreadyExists(BeanMap b, boolean update) {
		String code = getCodeAttribute();
		// Check that there is a "code" for this entity...
		// FIXME this assume that a code, is always an "unique" data, which is just a convention. We should check the presence fs the "unique" metadata !
		if ((code != null) && (entity.getAttribute(code) != null)) {
			EqualCriteria equalCode = new EqualCriteria(code, b.getString(code));
			if (update && (b.getId() > 0)) {
				// If test is made during update, we also check that the id is different
				ISearchCriteria finalCriteria = new NotCriteria(new IdEqualCriteria(b.getId()));
				if (equalCode != null) {
					finalCriteria = new AndCriteria(finalCriteria, equalCode);
				}
				return helper.count(getType(), finalCriteria) > 0;
			}
			return helper.count(getType(), equalCode) > 0;
		}
		return false;
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
		if (confirm && !confirmDeletion(deleted)) {
			return false;
		}
		return helper.remove(deleted);
	}

	protected boolean confirmDeletion(BeanMap deleted) {
		return Activator.getDefault().openConfirm(Activator.resString("msg.question.action.delete.confirmation")); //$NON-NLS-1$
	}

	protected String getDisplayedSelectClause() {
		return createSelectClause();
	}

	protected String createSelectClause() {
		return getCodeAttribute() + " " + getNameAttribute(); //$NON-NLS-1$
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

	/**
	 * Override this method and return "null" if this entity does not use a "unique" code attribute.
	 * @return
	 */
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

	@Deprecated
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
