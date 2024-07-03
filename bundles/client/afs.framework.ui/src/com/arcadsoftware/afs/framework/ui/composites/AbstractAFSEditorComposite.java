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
package com.arcadsoftware.afs.framework.ui.composites;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.afs.framework.ui.AbstractAFSListenerList;
import com.arcadsoftware.afs.framework.ui.editors.AbstractAFSEditorPart;
import com.arcadsoftware.afs.framework.ui.editors.IDirtyListener;
import com.arcadsoftware.afs.framework.ui.editors.IFireContentChangeProvider;

public abstract class AbstractAFSEditorComposite extends AbstractAFSComposite
		implements ModifyListener, SelectionListener, IFireContentChangeProvider {

	private class DirtyListener {

		private final AbstractAFSListenerList contentChangedListeners = new AbstractAFSListenerList(3);

		public void fireContentChanged() {
			for (final Object l : contentChangedListeners.getListeners()) {
				try {
					((IDirtyListener) l).dirtyEvent(dirty);
				} catch (final RuntimeException e1) {
					removeDirtyListener((IDirtyListener) l);
				}
			}
		}

		public void addDirtyListener(IDirtyListener listener) {
			contentChangedListeners.add(listener);
		}

		public void removeDirtyListener(IDirtyListener listener) {
			contentChangedListeners.remove(listener);
		}
	}

	private Object edited;
	private boolean dirty;
	private final DirtyListener l = new DirtyListener();
	private final AbstractAFSEditorPart parentEditorPart;

	public AbstractAFSEditorComposite(Composite parent, int style, AbstractAFSEditorPart editor) {
		this(parent, style, null, editor, true);
	}

	public AbstractAFSEditorComposite(Composite parent, int style, Object edited, AbstractAFSEditorPart editor) {
		this(parent, style, edited, editor, true);
	}

	public AbstractAFSEditorComposite(Composite parent, int style, AbstractAFSEditorPart editor, boolean withinit) {
		this(parent, style, null, editor, withinit);
	}

	public AbstractAFSEditorComposite(Composite parent, int style, Object edited, AbstractAFSEditorPart editor,
			boolean withinit) {
		super(parent, style);
		this.edited = edited;
		parentEditorPart = editor;
		format();
		if (withinit) {
			createContent();
		}
	}

	public void format() {
		final GridLayout gridLayout = new GridLayout(1, false);
		gridLayout.marginWidth = 0;
		gridLayout.marginHeight = 0;
		gridLayout.verticalSpacing = 0;
		setLayout(gridLayout);
		final GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		setLayoutData(gridData);
	}

	public void registerControl(Control c) {
		if (c instanceof Text) {
			((Text) c).addModifyListener(this);
		} else if (c instanceof Button) {
			((Button) c).addSelectionListener(this);
		} else if (c instanceof Combo) {
			((Combo) c).addSelectionListener(this);
			((Combo) c).addModifyListener(this);
		}
	}

	public void addChangeListener(IDirtyListener listener) {
		l.addDirtyListener(listener);
	}

	public void removeChangeListener(IDirtyListener listener) {
		l.removeDirtyListener(listener);
	}

	@Override
	public void modifyText(ModifyEvent arg0) {
		dirty = true;
		l.fireContentChanged();
	}

	@Override
	public void widgetSelected(SelectionEvent arg0) {
		dirty = true;
		l.fireContentChanged();
	}

	public boolean isDirty() {
		return dirty;
	}

	public void fireDirty() {
		dirty = true;
		l.fireContentChanged();
	}

	public void setDirty(boolean dirty) {
		this.dirty = dirty;
	}

	protected boolean checkData(Object edited) {
		return true;
	}

	@Override
	public void widgetDefaultSelected(SelectionEvent e) {
	}

	public Object getEdited() {
		return edited;
	}

	public void setEdited(Object edited) {
		this.edited = edited;
		toScreen();
	}

	public AbstractAFSEditorPart getParentEditorPart() {
		return parentEditorPart;
	}

	public abstract void createContent();

	public abstract void toScreen();

	public void fromScreen() {
	}

	public boolean checkData() {
		return true;
	}

	@Override
	public void fireContentChanged() {
		dirty = true;
		l.fireContentChanged();
	}
}
