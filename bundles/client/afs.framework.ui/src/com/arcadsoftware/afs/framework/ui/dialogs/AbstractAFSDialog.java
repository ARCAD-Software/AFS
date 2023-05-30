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
/*
 * Created on 11 mai 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.arcadsoftware.afs.framework.ui.dialogs;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.aev.core.ui.dialogs.DialogConstantProvider;
import com.arcadsoftware.afs.client.brands.AFSIcon;

/**
 * @version 1.0.0
 * 
 * Ajoute quelques méthode d'assistance à la création de controles
 * dans la fenetre...
 * 
 * <i> Copryright 2004, Arcad-Software</i>.
 */
public abstract class AbstractAFSDialog extends Dialog {

	private final boolean okButtonOnly;

	protected AbstractAFSDialog(Shell parentShell, boolean okButtonOnly) {
		super(parentShell);
		setBlockOnOpen(true);
		this.okButtonOnly = okButtonOnly;
	}

	protected AbstractAFSDialog(Shell parentShell) {
		this(parentShell, false);
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		// create OK and Cancel buttons by default
		createButton(parent, IDialogConstants.OK_ID, DialogConstantProvider.getInstance().OK_LABEL, true);
		if (!okButtonOnly) {
			createButton(parent, IDialogConstants.CANCEL_ID, DialogConstantProvider.getInstance().CANCEL_LABEL, false);
		}
	}

	@Override
	protected Control createButtonBar(Composite parent) {
		Composite composite = (Composite)super.createButtonBar(parent);
		GridLayout l = (GridLayout)composite.getLayout();
		l.marginHeight = l.marginWidth = 3;
		return composite;
	}	
	
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setImage(getDialogImage()); //$NON-NLS-1$
		int width = getWidth();
		int height= getHeight();
		// Size definition
		newShell.setSize(width, height);
		// Title assignment
		newShell.setText(getTitle());
		if (isCentered()) {
			final Rectangle bounds = newShell.getBounds();
			Monitor activeMonitor = null;
			for (Monitor monitor : newShell.getDisplay().getMonitors()) {
			    if (monitor.getBounds().intersects(bounds)) {
			    	activeMonitor = monitor;
			    }
			}
			// Center the dialog in the active monitor
			Rectangle parentBounds;
			if (activeMonitor != null) {
				parentBounds = activeMonitor.getClientArea();
			} else {
				parentBounds = newShell.getDisplay().getClientArea();
			}
			newShell.setLocation(parentBounds.x + (parentBounds.width - width) / 2, //
					parentBounds.y + (parentBounds.height - height) / 2);			
		}
	}

	protected Image getDialogImage() {
		return AFSIcon.ARCAD.image();
	}
	
	/**
	 * Méthode permettant d'ajouter un ModifyListener déclenchant la validation
	 * de la données saisie pour un widget de type Text.
	 * 
	 * @param c
	 *            Text : Text à mettre sous contrôle
	 */
	protected void addCheckDataListeners(Text t) {
		t.addModifyListener(e -> getButton(IDialogConstants.OK_ID).setEnabled(checkDataFromListeners()));
	}

	/**
	 * Méthode permettant d'ajouter un ISelectionListener déclenchant la
	 * validation de la données saisie pour un widget de type Button.
	 * 
	 * @param b
	 *            Button : Button à mettre sous contrôle
	 */
	protected void addCheckDataListeners(Button b) {
		b.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				getButton(IDialogConstants.OK_ID).setEnabled(checkDataFromListeners());
			}
		});
	}

	/**
	 * Méthode permettant d'ajouter un ISelectionListener et un ModifyListener
	 * déclenchant la validation de la données saisie pour une liste Combo.
	 * 
	 * @param c
	 *            Combo : Combo à mettre sous contrôle
	 */

	protected void addCheckDataListeners(Combo c) {
		c.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				getButton(IDialogConstants.OK_ID).setEnabled(checkDataFromListeners());
			}
		});
		c.addModifyListener(e -> getButton(IDialogConstants.OK_ID).setEnabled(checkDataFromListeners()));
	}

	/**
	 * Méthode permettant la validation des informations saisies.<br>
	 * La surcharge de cette mï¿½thode permet de déclarer vos règles de validation
	 * de saisie.<br>
	 * Pour intégrer l'appel de cette fonction à vos contrôle de saisie, vous
	 * pouvez utiliser les mï¿½thodes "addCheckDataListeners()" disponible sur les
	 * Combo et les Text.
	 * 
	 * @return boolean : <b>True</b> si les informations saisies sont valides,
	 *         <b>false</b> sinon.
	 */
	protected boolean checkDataFromListeners() {
		return true;
	}

	protected boolean isCentered() {
		return true;
	}
	
	public abstract int getHeight();
	
	public abstract int getWidth();
	
	public abstract String getTitle();
	
}