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
package com.arcadsoftware.afs.framework.ui.editors;

import java.util.ArrayList;

import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.EditorPart;

import com.arcadsoftware.afs.framework.services.IDynamicHelpIdProvider;
import com.arcadsoftware.afs.framework.ui.composites.AbstractAFSEditorComposite;
import com.arcadsoftware.afs.framework.ui.help.DynamicHelp;
import com.arcadsoftware.afs.framework.ui.internal.Activator;
import com.arcadsoftware.afs.framework.ui.plugins.AbstractAFSUIPlugin;

public abstract class AbstractAFSEditorPart extends EditorPart implements IDirtyListener {

	public static final int LABEL_MAXLENGTH = 30;

	private Object edited;
	protected ArrayList<AbstractAFSEditorComposite> childComposites;
	private boolean dirtyPart;
	private String initialName = ""; //$NON-NLS-1$
	private String errorMessage;
	private ArrayList<IObjectSaved> savedListeners = new ArrayList<IObjectSaved>();;

	public AbstractAFSEditorPart() {
		super();
		childComposites = new ArrayList<AbstractAFSEditorComposite>();
	}

	public void registerComposite(AbstractAFSEditorComposite childComposite) {
		childComposites.add(childComposite);
		childComposite.addChangeListener(this);
		childComposite.setEdited(edited);
	}

	public AbstractAFSEditorComposite[] getChilds() {
		return childComposites.toArray(new AbstractAFSEditorComposite[childComposites.size()]);
	}

	@Override
	public boolean isDirty() {
		return dirtyPart;
	}

	public void dirtyEvent(boolean dirty) {
		dirtyPart = dirty;
		firePropertyChange(PROP_DIRTY);
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	@Override
	public void doSaveAs() {
	}

	@Override
	public boolean isSaveAsAllowed() {
		return false;
	}

	@Override
	public void setFocus() {
		IEditorInput input = getEditorInput();
		if (input instanceof IDynamicHelpIdProvider) {
			DynamicHelp.showContextHelpId(((IDynamicHelpIdProvider) input).getDynamicHelpId());
		}
	}

	public void addSavedListener(IObjectSaved listener) {
		savedListeners.add(listener);
	}

	public void removeSavedListener(IObjectSaved listener) {
		savedListeners.remove(listener);
	}

	public void fireSaved(IObjectSaved entity) {
		for (final IObjectSaved l : savedListeners) {
			try {
				l.doAfterSaving(entity);
			} catch (RuntimeException e) {
				removeSavedListener(l);
			}
		}
	}

	protected String truncateName(String label) {
		if (label.length() > LABEL_MAXLENGTH) {
			return label.substring(0, LABEL_MAXLENGTH - 3) + "..."; //$NON-NLS-1$
		}
		return label;
	}

	public String getInitialName() {
		return initialName;
	}

	public void setInitialName(String initialName) {
		this.initialName = initialName;
	}

	public boolean doBeforeSaving() {
		if (!checkData()) {
			if (errorMessage != null) {
				getActivator().openError(errorMessage);
			}
			return false;
		}
		return true;
	}

	@Override
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		if ((input == null) || !matchInput(input)) {
			throw new PartInitException(Activator.resString("editor.error.initializeEditor")); //$NON-NLS-1$
		}
		setInput(input);
		edited = getEditedObject(input);
		if (edited != null) {
			setInitialName(getPartName());
			updateName();
			firePropertyChange(PROP_TITLE);
		}
		setSite(site);
	}

	public Object getEdited() {
		return edited;
	}

	public abstract boolean checkData();

	public abstract AbstractAFSUIPlugin getActivator();

	public abstract boolean matchInput(IEditorInput input);

	public abstract Object getEditedObject(IEditorInput input);

	/**
	 * This method is called to update the part name you c
	 */
	public abstract void updateName();

}
