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
package com.arcadsoftware.afs.client.core.ui.propertypage;

import org.eclipse.swt.graphics.Point;

import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.dialogs.AbstractBeanMapDialog;
import com.arcadsoftware.beanmap.BeanMap;

public abstract class AbstractBeanMapWithDialogPropertyPage extends AbstractBeanMapListPropertyPage {

	protected AbstractBeanMapDialog dialog = null;

	protected AbstractBeanMapDialog createDialog() {
		if (dialog == null) {
			dialog = new AbstractBeanMapDialog(Activator.getDefault().getPluginShell(), getServerConnection(), false,
					true) {

				@Override
				public String getType() {
					return AbstractBeanMapWithDialogPropertyPage.this.getType();
				}

				@Override
				public String getLayoutName() {
					return AbstractBeanMapWithDialogPropertyPage.this.getLayoutName();
				}

				@Override
				public Point getSize() {
					return AbstractBeanMapWithDialogPropertyPage.this.getPoint();
				}

				@Override
				public String getTitle() {
					return AbstractBeanMapWithDialogPropertyPage.this.getDialogTitle();
				}

			};
		}
		return dialog;
	}

	@Override
	protected BeanMap doAdd() {
		final AbstractBeanMapDialog dialog = createDialog();
		dialog.setEditedBeanMap(null);
		return AbstractBeanMapDialog.create(dialog);
	}

	@Override
	protected boolean doUpdate(BeanMap updated) {
		return AbstractBeanMapDialog.edit(createDialog(), updated);
	}

	public abstract String getLayoutName();

	public abstract Point getPoint();

	public abstract String getDialogTitle();

}
