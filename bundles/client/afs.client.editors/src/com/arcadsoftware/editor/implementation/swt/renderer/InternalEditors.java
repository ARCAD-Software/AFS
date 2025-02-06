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
package com.arcadsoftware.editor.implementation.swt.renderer;

import java.util.HashMap;
import java.util.Map;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.swt.IInternalEditor;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.renderer.IInternalEditors;
import com.arcadsoftware.editor.swt.renderer.ILoaderCallback;

/**
 * This class permits renderer used internal editors.
 */
public class InternalEditors implements IInternalEditors {

	private Map<String, IInternalEditor> internalEditors;
	private Map<String, Integer> loadingInternalEditors;
	private final SWTRenderer renderer;
	private final HashMap<String, Boolean> adds = new HashMap<>();

	protected InternalEditors(SWTRenderer renderer) {
		this.renderer = renderer;
	}

	@Override
	public void addInternalEditor(IInternalEditor editor) {
		if (internalEditors == null) {
			internalEditors = new HashMap<>();
		}
		internalEditors.put(editor.getInternalEditorId(), editor);
		editor.addChangeListener(renderer);
		if (loadingInternalEditors != null) {
			final Integer entityId = loadingInternalEditors.get(editor.getInternalEditorId());
			if (entityId != null) {
				loadingInternalEditors.remove(editor.getInternalEditorId());
				editor.load(entityId.intValue());
			}
		}
		editor.setParentRenderer(renderer);
	}

	@Override
	public void removeInternalEditor(IInternalEditor editor) {
		editor.removeChangeListener(renderer);
		if (internalEditors != null) {
			internalEditors.remove(editor.getInternalEditorId());
		}
		editor.setParentRenderer(null);
	}

	@Override
	public boolean loadInternalEditor(String editorId, int entityId) {
		return loadInternalEditor(editorId, entityId, null);
	}

	@Override
	public boolean loadInternalEditor(String editorId, int entityId, ILoaderCallback callback) {
		boolean result = false;
		adds.clear();
		if (internalEditors != null) {
			final IInternalEditor internalEditor = internalEditors.get(editorId);
			if (internalEditor != null) {
				// TODO [JB] NOtifier la FA?
				internalEditor.load(ISWTRenderer.EMPTY_ENTITY_ID);
				result = internalEditor.load(entityId);
				final SWTRenderer internalRenderer = (SWTRenderer) internalEditor.getRenderer();

				internalRenderer.fireActivatedEvent();

				if (callback != null) {
					callback.process(renderer, internalRenderer, editorId, entityId);
				}

				// // TODO [DL] � enlever quand solution plus propre pour g�rer les droits...
				// if (editorId.equals("interventionId")) { //$NON-NLS-1$
				// SWTRenderer internalRenderer = (SWTRenderer) internalEditor.getRenderer();
				// BeanMap intervention = internalRenderer.loadBeanMap("intervention", entityId); //$NON-NLS-1$
				// IRightControler rightControler = renderer.getRightControler();
				// if (renderer.getCurrentBean().getType().equals("incident")) { //$NON-NLS-1$
				// if (intervention.getInt("owner") != rightControler.getConnectedUserId() //$NON-NLS-1$
				// && intervention.getInt("owner") != 0) { //$NON-NLS-1$
				// internalRenderer.setParentReadOnly(true);
				// } else {
				// internalRenderer.setParentReadOnly(false);
				// }
				// } else if (renderer.getCurrentBean().getType().equals("problem")) { //$NON-NLS-1$
				// if (intervention.getInt("responsible") != rightControler.getConnectedUserId() //$NON-NLS-1$
				// && intervention.getInt("responsible") != 0) { //$NON-NLS-1$
				// internalRenderer.setParentReadOnly(true);
				// } else {
				// internalRenderer.setParentReadOnly(false);
				// }
				// }
				// }
			} else {
				addLoadingInternalEditor(editorId, entityId);
			}
		} else {
			addLoadingInternalEditor(editorId, entityId);
		}
		return result;
	}

	private void addLoadingInternalEditor(String editorId, int entityId) {
		if (loadingInternalEditors == null) {
			loadingInternalEditors = new HashMap<>();
		}
		loadingInternalEditors.put(editorId, Integer.valueOf(entityId));
	}

	public boolean saveInternalEditors() {
		boolean result = true;
		if (internalEditors != null) {
			for (final IInternalEditor internalEditor : internalEditors.values()) {
				result = result && internalEditor.save();
			}
		}
		return result;
	}

	protected boolean canSavedInternalEditors() {
		boolean result = true;
		if (internalEditors != null) {
			for (final IInternalEditor internalEditor : internalEditors.values()) {
				final Boolean added = adds.get(internalEditor.getInternalEditorId());
				if (internalEditor.isDirty() || ((added != null) && added.booleanValue())) {
					result = result && internalEditor.canSavedEditor();
				}
			}
		}
		return result;
	}

	@Override
	public boolean internalEditorsAreDirty() {
		boolean result = false;
		if (internalEditors != null) {
			for (final IInternalEditor internalEditor : internalEditors.values()) {
				if (internalEditor.isDirty()) {
					result = true;
					break;
				}
			}
		}
		return result;
	}

	protected void refreshEditorsContent(BeanMap beanMap, ISWTRenderer swtRenderer) {
		if (internalEditors != null) {
			for (final IInternalEditor editor : internalEditors.values()) {
				editor.refreshEditorContent(beanMap, swtRenderer);
			}
		}
	}

	@Override
	public boolean isAddForEditorId(String editorId) {
		return adds.get(editorId) == null ? false : adds.get(editorId).booleanValue();
	}

	@Override
	public void setAdd(String editorId) {
		adds.put(editorId, Boolean.TRUE);
	}

	@Override
	public IInternalEditor getInternalEditor(String editorId) {
		return internalEditors.get(editorId);
	}

}
