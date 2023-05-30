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
package com.arcadsoftware.afs.client.server.admin.common.ui.composites;

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
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.aev.core.ui.composite.AbstractArcadComposite;
import com.arcadsoftware.aev.core.ui.controlers.IContentChangeListener;
import com.arcadsoftware.aev.core.ui.listeners.AbstractChangeListener;

public abstract class AbstractSSCComposite extends AbstractArcadComposite implements ModifyListener,SelectionListener{ 

	
	/**
	 * 
	 */
	private static final long serialVersionUID = 6029586127893178089L;

	private class ContentChangeListener extends AbstractChangeListener {
		
	}
	
	
	ContentChangeListener l = new ContentChangeListener(); 
	
	protected boolean errorData=false;
	protected String errorMessage = null;
	protected boolean inModificationMode = false;
	
	protected boolean dirty = false;
	
	public AbstractSSCComposite(Composite parent, int style) {
		this(parent, style,true);
	}
	
	public AbstractSSCComposite(Composite parent, int style, boolean withInit) {
		super(parent, style);
		format();
		if (withInit){
			createControlPage();		
		}
	}

	protected void format() {
		GridLayout l = new GridLayout(3,false);
		this.setLayout(l);
		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace=true;
		gridData.grabExcessVerticalSpace=true;	
		gridData.horizontalSpan = 3;
		this.setLayoutData(gridData);		
	}

	/**
	 * Méthode permettant la validation des informations saisies.<br>
	 * La surcharge de cette méthode permet de déclarer vos règles
	 * de validation de saisie.<br> 
	 * Pour intégrer l'appel de cette fonction à vos contrôle de saisie, 
	 * vous pouvez utiliser les méthodes "addCheckDataListeners()" disponible
	 * sur les Combo et les Text. 
	 * 
	 * @return boolean : <b>True</b> si les informations saisies sont valides, 
	 *                   <b>false</b> sinon.
	 */
	protected boolean checkData(){return true;};
	
	/**
	 * Méthode permettant d'ajouter un iSelectionListener et
	 * un ModifyListener déclenchant la validation de la données 
	 * saisie pour une liste Combo.
	 * @param c Combo : Combo à mettre sous contrôle
	 */
	
	protected void addCheckDataListeners(Combo c){
		c.addSelectionListener(
			new SelectionAdapter(){
				/**
				 * 
				 */
				private static final long serialVersionUID = 5381748199218436213L;

				public void widgetSelected(SelectionEvent event){
					checkData();
				}	
			}
		);
		c.addModifyListener(
			new ModifyListener(){
				/**
				 * 
				 */
				private static final long serialVersionUID = -9209940763459477810L;

				public void modifyText(ModifyEvent e) {	
					checkData();												
				}

			}	
		);	
	}
	/**
	 * Méthode permettant d'ajouter un ModifyListener déclenchant
	 * la validation de la données saisie pour un widget de type Text.
	 * @param c Text : Text à mettre sous contrôle
	 */	
	protected void addCheckDataListeners(Text t){
		t.addModifyListener(
			new ModifyListener(){
				/**
				 * 
				 */
				private static final long serialVersionUID = -5923862166580187635L;

				public void modifyText(ModifyEvent e) {	
					checkData();												
				}
			}	
		);	
	}	
	
	/**
	 * Méthode permettant d'ajouter un iSelectionListener déclenchant
	 * la validation de la données saisie pour un widget de type Button.
	 * @param b Button : Button à mettre sous contrôle
	 */	
	protected void addCheckDataListeners(Button b){
		b.addSelectionListener(
			new SelectionAdapter(){
				/**
				 * 
				 */
				private static final long serialVersionUID = 5639803824779041869L;

				public void widgetSelected(SelectionEvent event){
					checkData();
				}	
			}
		);	
	}

	public boolean isErrorData() {
		return errorData;
	}

	public void setErrorData(boolean errorData) {
		this.errorData = errorData;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

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
		if (c instanceof Text)
			((Text)c).addModifyListener(this);
		if (c instanceof Button) {
			((Button )c).addSelectionListener(this);
		}
		if (c instanceof Combo) {
			((Combo )c).addSelectionListener(this);
		}				
	}

	public void addChangeListener(IContentChangeListener listener) {
		l.addContentChangedListener(listener);		
	}
	
	public void removeChangeListener(IContentChangeListener listener) {
		l.removeContentChangedListener(listener);		
	}
	
	public void modifyText(ModifyEvent arg0) {
		dirty = true;
		l.fireContentChanged();
	}
	public void widgetSelected(SelectionEvent arg0) {
		dirty = true;
		l.fireContentChanged();
	}
	

	public void widgetDefaultSelected(SelectionEvent arg0) {
	}


	public boolean isDirty() {
		return dirty;
	}


	public void setDirty(boolean dirty) {
		this.dirty = dirty;
	}

	public boolean isValid(){
		return !errorData;
	}
	
	public abstract void createControlPage();
	
}
