/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.afs.client.server.admin.common.ui.settings.editors;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IEditorInput;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.server.admin.common.Activator;
import com.arcadsoftware.afs.client.server.admin.common.ui.settings.renderers.ActionManager;
import com.arcadsoftware.afs.client.server.admin.common.ui.settings.renderers.SectionComposite;
import com.arcadsoftware.afs.framework.ui.editors.AbstractAFSSavedEditorPart;
import com.arcadsoftware.afs.framework.ui.plugins.AbstractAFSUIPlugin;

public class SectionSettingEditor extends AbstractAFSSavedEditorPart {

	public static final String ID = "com.arcadsoftware.afs.client.server.admin.common.ui.settings.editors.ServerSettingEditor"; //$NON-NLS-1$

	SectionComposite sectionComposite;

	@Override
	public AbstractAFSUIPlugin getActivator() {
		return Activator.getInstance();
	}

	@Override
	public boolean matchInput(IEditorInput input) {
		return (input instanceof SectionSettingEditorInput);
	}

	@Override
	public Object getEditedObject(IEditorInput input) {
		return ((SectionSettingEditorInput) input).getConnector();
	}

	@Override
	public void updateName() {
		setPartName(((SectionSettingEditorInput) getEditorInput()).getName());
	}

	private ActionManager getActionManager() {
		return ((SectionSettingEditorInput) getEditorInput()).getActionManager();
	}

	@Override
	public void createPartControl(Composite parent) {
		GridLayout gl = new GridLayout(1, false);
		gl.marginHeight = gl.marginWidth = 0;
		parent.setLayout(gl);
		if (getActionManager().getForm() != null) {
			final ScrolledComposite scrolledComposite = new ScrolledComposite(parent, SWT.V_SCROLL);
			scrolledComposite.setExpandHorizontal(true);
			scrolledComposite.setExpandVertical(true);
			scrolledComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
			sectionComposite = new SectionComposite(scrolledComposite, SWT.BORDER, getActionManager());
			sectionComposite.fill(getActionManager().getForm());
			scrolledComposite.setContent(sectionComposite);
			// Only show vertical scrollbar
			scrolledComposite.addListener(SWT.Resize, event -> {
				int width = scrolledComposite.getClientArea().width;
				scrolledComposite.setMinSize(parent.computeSize(width, SWT.DEFAULT));
			});
		} else {
			Composite c = new Composite(parent, SWT.BORDER);
			gl = new GridLayout(2, false);
			gl.marginHeight = gl.marginWidth = 10;
			c.setLayout(gl);
			GridData gridData = new GridData(GridData.FILL_BOTH);
			gridData.grabExcessHorizontalSpace = true;
			gridData.grabExcessVerticalSpace = true;
			gridData.horizontalSpan = 3;
			c.setLayoutData(gridData);
			Label errorImage = new Label(c, SWT.NONE);
			errorImage.setImage(AFSIcon.ERROR.image());
			Label errorText = new Label(c, SWT.NONE);
			errorText.setText(Activator.resString("settingseditor.msg.error.loadingError")); //$NON-NLS-1$
		}
	}

	@Override
	public void saveContent(IProgressMonitor monitor) {
		// We don't use the Editor Save cycle.
	}

}
