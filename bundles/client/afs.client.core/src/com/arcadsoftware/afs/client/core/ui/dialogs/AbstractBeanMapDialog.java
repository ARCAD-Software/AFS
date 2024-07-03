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
package com.arcadsoftware.afs.client.core.ui.dialogs;

import java.util.Hashtable;
import java.util.Set;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.composites.ConnectedDynamicEditorComposite;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.swt.IEntityAttributeProcessListener;
import com.arcadsoftware.metadata.MetaDataAttribute;

public abstract class AbstractBeanMapDialog extends AbstractAFSDialog {

	protected BeanMap initalBeanmap;
	protected BeanMap result;
	protected ServerConnection connection;
	protected ConnectedDynamicEditorComposite editor;
	protected boolean readonly;

	private class AttributeChecker implements IEntityAttributeProcessListener {
		@Override
		public void processAction(MetaDataAttribute entry) {
			String msg = Activator.resString("msg.error.attributeismandatory");
			msg = String.format(msg, entry.getName());
			Activator.getDefault().openError(msg);
		}
	}

	private final AttributeChecker checker = new AttributeChecker();

	public AbstractBeanMapDialog(Shell parentShell, ServerConnection connection, boolean resizable,
			boolean centered) {
		this(parentShell, connection, resizable, centered, false);
	}

	public AbstractBeanMapDialog(Shell parentShell, ServerConnection connection, boolean resizable,
			boolean centered, boolean readonly) {
		super(parentShell, resizable, centered);
		this.connection = connection;
		this.readonly = readonly;
	}

	public ServerConnection getConnection() {
		return connection;
	}

	public void setConnection(ServerConnection connection) {
		this.connection = connection;
	}

	public void setEditedBeanMap(BeanMap edited) {
		initalBeanmap = edited;
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		final GridLayout gl = (GridLayout) composite.getLayout();
		gl.marginWidth = gl.marginHeight = gl.marginTop = gl.marginBottom = gl.marginLeft = gl.marginRight = 0;
		editor = new ConnectedDynamicEditorComposite(getConnection(), composite, 0, getType(), getLayoutName(),
				readonly, false) {
			@Override
			protected ServerConnection getConnection() {
				return connection;
			}
		};
		final Hashtable<String, Object> virtualValues = getVirtualValues();
		if (virtualValues != null) {
			final Set<String> keySet = virtualValues.keySet();
			for (final String key : keySet) {
				final Object value = virtualValues.get(key);
				editor.getRenderer().putVirtualValue(key, value);
			}
		}
		editor.createPartControl(getLayoutName());
		final GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalSpan = 3;
		editor.setLayoutData(gridData);
		if (initalBeanmap != null) {
			editor.load(initalBeanmap);
			editor.getRenderer().fireActivatedEvent();
		} else {
			editor.loadEmptyEntity();
		}
		return composite;
	}

	protected Hashtable<String, Object> getVirtualValues() {
		return null;
	}

	@Override
	protected void okPressed() {
		result = new BeanMap(getType());
		if (initalBeanmap != null) {
			result.setId(initalBeanmap.getId());
		}
		result.addAll(editor.getCurrent());
		if ((editor == null) || editor.getRenderer().canSavedEditor(checker)) {
			doBeforeClosing(result);
			super.okPressed();
		}
	}

	public BeanMap getResult() {
		return result;
	}

	protected void doBeforeClosing(BeanMap result) {}

	public abstract String getType();

	public abstract String getLayoutName();

	public static BeanMap create(AbstractBeanMapDialog dialog) {
		if (dialog.open() == Window.OK) {
			return dialog.getResult();
		}
		return null;
	}

	public static boolean edit(AbstractBeanMapDialog dialog, BeanMap edited) {
		dialog.setEditedBeanMap(edited);
		if (dialog.open() == Window.OK) {
			edited.addAll(dialog.getResult());
			return true;
		}
		return false;
	}

	public static void browse(AbstractBeanMapDialog dialog, BeanMap edited) {
		dialog.setEditedBeanMap(edited);
		dialog.open();
	}
}
