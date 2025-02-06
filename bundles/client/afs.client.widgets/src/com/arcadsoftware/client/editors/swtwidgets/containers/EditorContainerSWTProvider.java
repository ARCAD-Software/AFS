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
package com.arcadsoftware.client.editors.swtwidgets.containers;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.framework.ui.plugins.LoggedUIPlugin;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapEvent;
import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.DynamicEditorComposite;
import com.arcadsoftware.editor.swt.IBeanMapChangedListener;
import com.arcadsoftware.editor.swt.IContainerSWTProvider;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.IEditorChangeListener;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.IInternalEditor;
import com.arcadsoftware.editor.swt.IReadOnlyableContainerSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.IValidatingSubWidgets;
import com.arcadsoftware.editor.swt.listener.IListenedWidget;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement an Editor Container SWT Widget provider for the dynamic editors.
 */
public class EditorContainerSWTProvider implements IReadOnlyableContainerSWTProvider, IValidatingSubWidgets,
		IInternalEditor, IListenedWidget {

	DynamicEditorComposite editorComposite;
	private String internalEditorId;
	ISWTRenderer renderer;
	private String saveBeforeLoadTitle;
	private String saveBeforeLoadMessage;
	private String saveErrorTitle;
	private String saveErrorMessage;
	private String loadErrorTitle;
	private String loadErrorMessage;
	private String id;
	private DynamicEditorComposite emptyEditorComposite;
	private StackLayout stackLayout;
	private Composite composite;
	ISWTRenderer editorRenderer;

	@Override
	public void create(ISWTRenderer swtRenderer, ILayoutParameters parameters, boolean isEmpty,
			MetaDataEntity structure) {
		renderer = swtRenderer;
		if (parameters.getParameterBoolean(IConstants.BORDER)) {
			composite = renderer.getToolkit().createComposite(renderer.getParent(), SWT.BORDER);
		} else {
			composite = renderer.getToolkit().createComposite(renderer.getParent());
		}
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			if (parameters.getParameterBoolean(IConstants.FILL_BOTH)) {
				composite.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
			} else {
				composite.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
			}
		}
		stackLayout = new StackLayout();
		composite.setLayout(stackLayout);
		editorComposite = createEditorComposite(composite, parameters.getParameter(IConstants.TYPE), parameters
				.getParameter(IConstants.LAYOUT_NAME));
		editorRenderer = editorComposite.getRenderer();
		emptyEditorComposite = createEditorComposite(composite, parameters.getParameter(IConstants.TYPE), parameters
				.getParameter(IConstants.LAYOUT_EMPTY_NAME));
		stackLayout.topControl = emptyEditorComposite;
		internalEditorId = parameters.getParameter(IConstants.ID);
		saveBeforeLoadTitle = renderer.getLocalizedMessage(parameters.getParameter(IConstants.SAVE_BEFORE_LOAD_TITLE));
		saveBeforeLoadMessage = renderer.getLocalizedMessage(parameters
				.getParameter(IConstants.SAVE_BEFORE_LOAD_MESSAGE));
		saveErrorTitle = renderer.getLocalizedMessage(parameters.getParameter(IConstants.SAVE_ERROR_TITLE));
		saveErrorMessage = renderer.getLocalizedMessage(parameters.getParameter(IConstants.SAVE_ERROR_MESSAGE));
		loadErrorTitle = renderer.getLocalizedMessage(parameters.getParameter(IConstants.LOAD_ERROR_TITLE));
		loadErrorMessage = renderer.getLocalizedMessage(parameters.getParameter(IConstants.LOAD_ERROR_MESSAGE));
		renderer.createSubContainer(this, parameters, composite);
		renderer.getInternalEditors().addInternalEditor(this);
		final String widgetId = parameters.getParameter(IConstants.WIDGET_ID);
		if (widgetId != null) {
			id = widgetId;
			editorRenderer.addSaveListener(new IBeanMapChangedListener() {
				@Override
				public void changed(BeanMapEvent event) {
					renderer
							.fireListenedWidgetChanged(EditorContainerSWTProvider.this,
									editorRenderer.getCurrentBean());
				}
			});
		}
	}

	protected DynamicEditorComposite createEditorComposite(Composite parent, String type, String layoutName) {
		final DynamicEditorComposite dynamicEditorComposite = new DynamicEditorComposite(parent, null, type, layoutName,
				renderer.isReadOnly());
		dynamicEditorComposite.loadEmptyEntity();
		final GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		dynamicEditorComposite.setLayoutData(gridData);
		return dynamicEditorComposite;
	}

	@Override
	public void dispose() {
		renderer.getInternalEditors().removeInternalEditor(this);
	}

	@Override
	public boolean acceptDecorator(IDecoratorSWTProvider provider) {
		return false;
	}

	@Override
	public boolean acceptInput(IInputSWTProvider provider) {
		return false;
	}

	@Override
	public boolean acceptSubContainer(IContainerSWTProvider provider) {
		return false;
	}

	@Override
	public boolean load(int beanMapId) {
		if (!editorComposite.isEmptyEntity() && editorComposite.isDirty()) {
			if (MessageDialog.openQuestion(LoggedUIPlugin.getShell(), saveBeforeLoadTitle, saveBeforeLoadMessage)) {
				if (!save()) {
					MessageDialog.openError(LoggedUIPlugin.getShell(), saveErrorTitle, saveErrorMessage);
					return false;
				}
			}
		}
		if (beanMapId == ISWTRenderer.EMPTY_ENTITY_ID) {
			stackLayout.topControl = emptyEditorComposite;
		} else {
			stackLayout.topControl = editorComposite;

		}
		composite.layout(true);
		if (!editorComposite.load(beanMapId)) {
			MessageDialog.openError(LoggedUIPlugin.getShell(), loadErrorTitle, loadErrorMessage);
			return false;
		}
		return true;
	}

	@Override
	public void reload() {
		editorComposite.reload();
	}

	@Override
	public boolean save() {
		if (!editorComposite.isEmptyEntity() && editorComposite.isDirty()) {
			return editorComposite.save();
		}
		return true;
	}

	@Override
	public String getInternalEditorId() {
		return internalEditorId;
	}

	@Override
	public void addChangeListener(IEditorChangeListener listener) {
		editorRenderer.addChangeListener(listener);
	}

	@Override
	public void removeChangeListener(IEditorChangeListener listener) {
		editorRenderer.removeChangeListener(listener);
	}

	@Override
	public boolean isDirty() {
		return editorComposite.isDirty();
	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public boolean canSavedEditor() {
		return editorRenderer.canSavedEditor();
	}

	@Override
	public void refreshEditorContent(BeanMap beanMap, ISWTRenderer swtRenderer) {
		if (editorRenderer != swtRenderer) {
			editorRenderer.refreshEditorContent(beanMap, null);
		}
	}

	@Override
	public void setParentRenderer(ISWTRenderer parentRenderer) {
		editorRenderer.setParentRenderer(parentRenderer);
	}

	public ISWTRenderer getParentRenderer() {
		return renderer;
	}

	@Override
	public ISWTRenderer getRenderer() {
		return editorRenderer;
	}

	@Override
	public void setReadOnly(boolean readOnly) {
		editorComposite.setEnabled(false);
	}

}
