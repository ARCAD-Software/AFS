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
package com.arcadsoftware.editor.swt;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.ApplicationWindow;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPersistableEditor;
import org.eclipse.ui.IWorkbenchPartConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.progress.UIJob;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapEvent;
import com.arcadsoftware.editor.implementation.swt.renderer.ILoadingListener;
import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;
import com.arcadsoftware.editor.swt.renderer.IRefreshEditorContent;

public class DynamicEditorPart extends EditorPart implements IPersistableEditor, IEditorChangeListener,
		IEditorTitleChangeListener, ILoadingListener, IRefreshEditorContent {

	protected SWTRenderer renderer;
	private boolean dirty = false;
	protected String layoutName;
	protected String editorRealm;
	private boolean allowsExternalRefresh = true;

	@Override
	public void doSave(IProgressMonitor monitor) {
		if ((renderer != null) && renderer.save()) {
			if ((getEditorInput() instanceof BeanMapEditorInput) && //
					(((BeanMapEditorInput) getEditorInput()).getId() == 0)) {
				((BeanMapEditorInput) getEditorInput()).getBeanMap().setId(renderer.getId());
			}
			// Blindage, la methode save declenche un change event
			// qui doit mettre a jour l'etat de l'editeur.
			dirty = false;
			firePropertyChange(PROP_DIRTY);
		}
	}

	@Override
	public void doSaveAs() {
		// Not supported.
	}

	@Override
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		// Test the Input.
		setSite(site);
		setInput(input);
		String type = null;
		int id = 0;
		if (input instanceof IBeanMapEditorInput) {
			type = ((IBeanMapEditorInput) input).getType();
			id = ((IBeanMapEditorInput) input).getId();
			layoutName = ((IBeanMapEditorInput) input).getLayoutName();
			editorRealm = ((IBeanMapEditorInput) input).getRealm();
		} else {
			final BeanMap bm = input.getAdapter(BeanMap.class);
			if (bm != null) {
				type = bm.getType();
				id = bm.getId();
			}
		}
		if (type == null) {
			throw new PartInitException("Input not supported or not initialized."); //$NON-NLS-1$
		}
		// Create the renderer.
		final Display display = Display.getCurrent();
		renderer = new SWTRenderer(display, getEditorRealm(), type, false) {
			@Override
			protected void dataLoaderCreated(SWTRenderer renderer) {
				initializeDataLoader(renderer);
			}

			@Override
			protected void editorLoaderCreated(SWTRenderer renderer) {
				initializeEditorLoader(renderer);
			}

			@Override
			public void requestSave() {
				doSave(new NullProgressMonitor());
			}

		};
		renderer.getLoadingListeners().addLoadingListener(this);
		renderer.addChangeListener(this);
		renderer.addTitleChangeListener(this);
		renderer.setReadOnly(((IBeanMapEditorInput) input).isReadOnly());
		// Load the BeanMap !
		if (!renderer.load(id)) {
			throw new PartInitException("Error during loading Data (possible that no connection was available)."); //$NON-NLS-1$
		}
		final Map<String, Object> virtualValues = getVirtualValues();
		if (virtualValues != null) {
<<<<<<< master
			for (String key: virtualValues.keySet()) {
				renderer.putVirtualValue(key, virtualValues.get(key));
=======
			final Set<String> keySet = virtualValues.keySet();
			for (final String key : keySet) {
				final Object value = virtualValues.get(key);
				renderer.putVirtualValue(key, value);
>>>>>>> 38f2e60 Clean-up AFS Client
			}
		}
		setFixedValues(renderer);
	}

	protected void setFixedValues(SWTRenderer renderer) {
		// to be overriden
	}

	protected Map<String, Object> getVirtualValues() {
		return null;
	}

	protected void initializeEditorLoader(SWTRenderer renderer) {
		// to be overriden
	}

	protected void initializeDataLoader(SWTRenderer renderer) {
		// to be overriden
	}

	public void addSaveListener(IBeanMapChangedListener listener) {
		renderer.addSaveListener(listener);
	}

	public void removeSaveListener(IBeanMapChangedListener listener) {
		renderer.removeSaveListener(listener);
	}

	public void addBeanMapSavedListener(IBeanMapSavedListener listener) {
		renderer.addBeanMapSaveListener(listener);
	}

	public void removeBeanMapSavedListener(IBeanMapSavedListener listener) {
		renderer.removeBeanMapSaveListener(listener);
	}

	public void addErrorOnSaveListener(IBeanMapErrorOnSaveListener listener) {
		renderer.addErrorOnSaveListener(listener);
	}

	public void removeOnSaveErrorListener(IBeanMapErrorOnSaveListener listener) {
		renderer.removeErrorOnSaveListener(listener);
	}

	protected String getEditorRealm() {
		return editorRealm;
	}

	@Override
	public boolean isDirty() {
		return dirty;
	}

	@Override
	public boolean isSaveAsAllowed() {
		return false;
	}

	@Override
	public void createPartControl(Composite parent) {
		// Render the Editor.
		renderer.createPartControl(parent, getLayoutName());
		final Image titleImage = renderer.getTitleImage();
		if (titleImage != null) {
			setTitleImage(titleImage);
		}
		// Define the selection provider.
		getSite().setSelectionProvider(renderer);
	}

	/**
	 * Reload content layout, in the same editor
	 */
	public void reloadPartControl(String layoutName) {
		// Render the Editor.
		final Composite parent = renderer.getParent();
		if (!parent.isDisposed()) {
			renderer.reloadPartControl(layoutName);
			parent.layout();
		}
	}

	private String getLayoutName() {
		return layoutName;
	}

	@Override
	public void setFocus() {
		renderer.setFocus();
	}

	@Override
	public void restoreState(IMemento memento) {
		renderer.restorState(memento);
	}

	@Override
	public void saveState(IMemento memento) {
		renderer.saveState(memento);
	}

	@Override
	public void changed(ISWTRenderer swtRenderer) {
		final boolean d = swtRenderer.isDirty() || swtRenderer.getInternalEditors().internalEditorsAreDirty();
		if (d != dirty) {
			dirty = d;
			new UIJob("Refresh") { //$NON-NLS-1$
				@Override
				public IStatus runInUIThread(IProgressMonitor monitor) {
					changedDirty();
					return Status.OK_STATUS;
				}
			}.schedule();
		}
	}

	protected void changedDirty() {
		firePropertyChange(IWorkbenchPartConstants.PROP_DIRTY);
	}

	@Override
	public void changed(ISWTRenderer renderer, String title) {
		changeTitle(title);
	}

	public void changeTitle(String title) {
		setPartName(title);
	}

	public void changeImage(Image image) {
		setTitleImage(image);
	}

	/**
	 * @return The ToolBar Actions
	 */
	public List<IAction> getToolBarActions() {
		if (renderer != null) {
			return renderer.getRendererActions().getToolBarActions();
		}
		return null;
	}

	/**
	 * @return The Menu Actions
	 */
	public Map<String, List<IAction>> getMenuActions() {
		if (renderer != null) {
			return renderer.getRendererActions().getMenuActions();
		}
		return null;
	}

	/**
	 * Open a message dialog and close the Editor.
	 */
	@Override
	public void loadingError() {
		new UIJob(Messages.DynamicEditorPart_LoadingErrorJobTitle) {
			@Override
			public IStatus runInUIThread(IProgressMonitor monitor) {
				MessageDialog.openError(getEditorSite().getShell(),
						Messages.DynamicEditorPart_LoadingErrorMessageTitle,
						Messages.DynamicEditorPart_LoadingErrorMessageDetail);
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
						.closeEditor(DynamicEditorPart.this, false);
				return Status.OK_STATUS;
			}
		}.schedule();
	}

	@Override
	public void dispose() {
		renderer.getLoadingListeners().removeLoadingListener(this);
		deleteActions();
		super.dispose();
	}

	private void deleteActions() {
		final Map<String, List<IAction>> actions = getMenuActions();
		if (actions != null) {
			for (final Map.Entry<String, List<IAction>> entry : actions.entrySet()) {
				MenuManager menuBarManager = null;
				IMenuManager menu = null;
				final IWorkbenchWindow activeWorkbenchWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
				if (activeWorkbenchWindow instanceof ApplicationWindow) {
					menuBarManager = ((ApplicationWindow) activeWorkbenchWindow).getMenuBarManager();
					if (menuBarManager != null) {
						for (final IContributionItem item : menuBarManager.getItems()) {
							if ((item instanceof MenuManager)
									&& ((MenuManager) item).getMenuText().equalsIgnoreCase(entry.getKey())) {
								menu = (IMenuManager) item;
								break;
							}
						}
					}
				}
				if (menu != null) {
					for (final IAction action : entry.getValue()) {
						for (final IContributionItem item : menu.getItems()) {
							if ((item instanceof ActionContributionItem)
									&& (((ActionContributionItem) item).getAction() == action)) {
								menu.remove(item);
							}
						}
					}
					menu.update(true);
					if (menuBarManager != null) {
						menuBarManager.update(true);
					}
				}
			}
		}
	}

	public void loadBeanMap(BeanMap newBeanMap) {
		renderer.loadBeanMap(new BeanMapEvent(newBeanMap));
	}

	public void setAllowExternalRefresh(boolean allowed) {
		allowsExternalRefresh = allowed;
	}

	@Override
	public void refreshEditorContent(BeanMap beanMap, ISWTRenderer swtRenderer) {
		if (allowsExternalRefresh || (renderer == swtRenderer)) {
			renderer.refreshEditorContent(beanMap, swtRenderer);
		}
	}

	public void refreshEditorContent() {
		renderer.refreshEditorContent(renderer.getCurrentBean(), renderer);
	}

	@Override
	public boolean isSameRenderer(ISWTRenderer swtRenderer) {
		return renderer == swtRenderer;
	}

	public BeanMap getEditedBeanMap() {
		return renderer.getCurrentBean();
	}

	/**
	 * Reload Editor Content
	 */
	public void reload() {
		renderer.reload();
	}

}
