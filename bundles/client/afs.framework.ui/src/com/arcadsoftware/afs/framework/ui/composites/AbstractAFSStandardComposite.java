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
package com.arcadsoftware.afs.framework.ui.composites;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.aev.core.ui.composite.AbstractArcadComposite;
import com.arcadsoftware.aev.core.ui.controlers.IContentChangeListener;
import com.arcadsoftware.aev.core.ui.listeners.AbstractChangeListener;

public abstract class AbstractAFSStandardComposite extends AbstractArcadComposite
		implements ModifyListener, SelectionListener {

	private class ContentChangeListener extends AbstractChangeListener {
	}

	private final ContentChangeListener l = new ContentChangeListener();
	protected boolean errorData;
	protected String errorMessage;
	protected boolean inModificationMode;
	protected boolean dirty;

	public AbstractAFSStandardComposite(Composite parent, int style) {
		this(parent, style, true);
	}

	public AbstractAFSStandardComposite(Composite parent, int style, boolean withInit) {
		super(parent, style);
		format();
		if (withInit) {
			createControlPage();
		}
	}

	protected void format() {
		setLayout(new GridLayout(3, false));
		final GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalSpan = 3;
		setLayoutData(gridData);
	}

	/**
	 * M�thode permettant la validation des informations saisies.<br>
	 * La surcharge de cette m�thode permet de d�clarer vos r�gles de validation de saisie.<br>
	 * Pour int�grer l'appel de cette fonction � vos contr�le de saisie, vous pouvez utiliser les m�thodes
	 * "addCheckDataListeners()" disponible sur les Combo et les Text.
	 *
	 * @return boolean : <b>True</b> si les informations saisies sont valides, <b>false</b> sinon.
	 */
	protected boolean checkData() {
		return true;
	}

	/**
	 * M�thode permettant d'ajouter un iSelectionListener et un ModifyListener d�clenchant la validation de la donn�es
	 * saisie pour une liste Combo.
	 *
	 * @param c
	 *            Combo : Combo � mettre sous contr�le
	 */
	protected void addCheckDataListeners(Combo c) {
		c.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				checkData();
			}
		});
		c.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				checkData();
			}
		});
	}

	/**
	 * M�thode permettant d'ajouter un ModifyListener d�clenchant la validation de la donn�es saisie pour un widget de
	 * type Text.
	 *
	 * @param c
	 *            Text : Text � mettre sous contr�le
	 */
	protected void addCheckDataListeners(Text t) {
		t.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				checkData();
			}
		});
	}

	/**
	 * M�thode permettant d'ajouter un iSelectionListener d�clenchant la validation de la donn�es saisie pour un widget
	 * de type Button.
	 *
	 * @param spinner
	 *            un {@link Spinner} � mettre sous contr�le
	 */
	protected void addCheckDataListeners(Spinner spinner) {
		spinner.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				checkData();
			}
		});
	}

	/**
	 * M�thode permettant d'ajouter un iSelectionListener d�clenchant la validation de la donn�es saisie pour un widget
	 * de type Button.
	 *
	 * @param b
	 *            Button : Button � mettre sous contr�le
	 */
	protected void addCheckDataListeners(Button b) {
		b.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent event) {
						checkData();
					}
				});
	}

	public boolean isErrorData() {
		return errorData;
	}

	public void setErrorData(boolean errorData) {
		this.errorData = errorData;
	}

	@Override
	public String getErrorMessage() {
		return errorMessage;
	}

	@Override
	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public boolean isInModificationMode() {
		return inModificationMode;
	}

	public void setInModificationMode(boolean inModificationMode) {
		this.inModificationMode = inModificationMode;
	}

	public void registerControl(Control c) {
		if (c instanceof Text) {
			((Text) c).addModifyListener(this);
		} else if (c instanceof Button) {
			((Button) c).addSelectionListener(this);
		} else if (c instanceof Combo) {
			((Combo) c).addSelectionListener(this);
		}
	}

	public void addChangeListener(IContentChangeListener listener) {
		l.addContentChangedListener(listener);
	}

	public void removeChangeListener(IContentChangeListener listener) {
		l.removeContentChangedListener(listener);
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

	@Override
	public void widgetDefaultSelected(SelectionEvent arg0) {
	}

	public boolean isDirty() {
		return dirty;
	}

	public void setDirty(boolean dirty) {
		this.dirty = dirty;
	}

	public boolean isValid() {
		return !errorData;
	}

	public abstract void createControlPage();

}
