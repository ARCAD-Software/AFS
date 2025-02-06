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

import java.util.List;

import org.eclipse.jface.window.Window;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;

public class ConnectedTableWithDialogSWTProvider extends AbstractConnectedTableWithButtonBarSWTProvider {

	public static final int ACTION_ADD = 0;
	public static final int ACTION_UPDATE = 1;
	private static final String SAVEPARENT = "saveParent"; //$NON-NLS-1$

	private DynamicDialogManager dialogManager;
	protected MetaDataEntity structure;

	@Override
	public void create(ISWTRenderer swtRenderer,
			ILayoutParameters layoutParameters, Element entityElement,
			MetaDataEntity structure) {
		super.create(swtRenderer, layoutParameters, entityElement, structure);
		dialogManager = new DynamicDialogManager(swtRenderer, getLayoutParameters());
		this.structure = structure;
	}

	@Override
	protected boolean editBeanMap(BeanMap beanMap) {
		if (editionAllowed()) {
			if (dialogManager.getDialogParameter() != null) {
				final DynamicDialog dialog = dialogManager.createDialog(getRenderer().getParent().getShell(),
						getConnection(), beanMap, readOnlyEdition);
				if (!readOnlyEdition) {
					if (dialog.open() == Window.OK) {
						final int status = dialog.getSavedStatus();
						// This means that we ask to save the beanMap from outside
						// the dialog
						boolean result = false;
						if (status == DynamicDialog.SAVED_UNDEFINED) {
							final BeanMap resultBean = dialog.getResult();
							if (validate(resultBean, ACTION_UPDATE)) {
								result = getHelper().update(resultBean);
							} else {
								result = false;
							}
						} else {
							result = (status == DynamicDialog.SAVED_SUCCEED);
						}
						if (result) {
							if (dialogManager.getDialogParameter().getParameterBoolean(SAVEPARENT)) {
								if (renderer.isDirty()) {
									renderer.save();
								}
							}
							beanMap.addAll(dialog.getResult());
							getList().setBeanMapValue(beanMap);
							getList().refresh();
						}
						return result;
					}
				} else {
					dialog.open();
					return true;
				}
			}
			return true;
		} else {
			missingRight(getExpectedEditRight());
		}

		return false;
	}

	@Override
	protected void createBeanMap(final MetaDataLink link, final boolean withOpenEditor) {
		if (creationAllowed()) {
			if (dialogManager.getDialogParameter() != null) {
				final DynamicDialog dialog = dialogManager.createDialogForAddtion(getRenderer().getParent().getShell(),
						getConnection(), link.getType());
				if (dialog.open() == Window.OK) {
					final BeanMap beanMap = dialog.getResult();
					if (validate(beanMap, ACTION_ADD)) {
						additionalInformation(beanMap, link);
						if (getHelper().create(beanMap)) {
							renderer.addLinkitem(link, beanMap);
							getList().setBeanMapValue(beanMap);
							final String so = getSortOrder();
							if (so != null) {
								getList().sort(so);
							}
						}
					}
				}
			}
		} else {
			missingRight(getExpectedAddRight());
		}
	}

	protected boolean validate(BeanMap beanMap, int action) {
		return true;
	}

	protected void additionalInformation(BeanMap beanMap, final MetaDataLink link) {

	}

	protected String getSortOrder() {
		return null;
	}

	@Override
	public List<Integer> getExpectedEditRight() {
		return null;
	}

	@Override
	public List<Integer> getExpectedAddRight() {
		return null;
	}

	@Override
	public List<Integer> getExpectedDeleteRight() {
		return null;
	}

	@Override
	public void missingRight(List<Integer> expected) {

	}

}
