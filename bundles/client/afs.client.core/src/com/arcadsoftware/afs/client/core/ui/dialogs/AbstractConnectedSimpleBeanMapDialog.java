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
package com.arcadsoftware.afs.client.core.ui.dialogs;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.beanmap.BeanMap;

/**
 * This class is used to create/edit a BeanMap using a used defined interface.<br/>
 * <p>This means that no dynamic layout is used to edit the data</p>
 * <p>If you want to use such a feature use {@link AbstractBeanMapDialog} instead.  
 * @author ARCAD Software
 *
 */
public abstract class AbstractConnectedSimpleBeanMapDialog extends AbstractAFSDialog {

	protected ServerConnection connection;
	protected BeanMap edited;
	
	public AbstractConnectedSimpleBeanMapDialog(Shell parentShell, ServerConnection connection, boolean resizable,
			boolean centered) {
		super(parentShell, resizable, centered);
		this.connection = connection;
	}

	public ServerConnection getConnection() {
		return connection;
	}

	public void setConnection(ServerConnection connection) {
		this.connection = connection;
	}	
	
	public void setEditedBeanMap(BeanMap edited){
		this.edited = edited;
	}
	
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite)super.createDialogArea(parent);
		GridLayout gl  = (GridLayout)composite.getLayout();
		gl.marginWidth=gl.marginHeight=gl.marginTop=gl.marginBottom=gl.marginLeft = gl.marginRight = 0;
		createDialogContent(composite);
		if (edited!=null) {
			beanMapToScreen(edited);
		}
		return composite;
	}
	
	@Override
	protected void okPressed() {
		if (edited!=null) {
			screenToBeanMap(edited);
		}
		if (isValid(edited)) {
			doBeforeClosing(edited);
			super.okPressed();
		}
	}
	
	protected boolean isValid(BeanMap result) {
		return true;
	}
	
	
	public BeanMap getResult() {
		return edited;
	}
	
	protected void doBeforeClosing(BeanMap result){}
	
	/**
	 * 
	 * @return Returns the type of the edited BeanMap 
	 */
	public abstract String getType();
	
	/**
	 * Create the content of the dialog area.
	 * <p>This method is used to defined to user interface of the dialog</p>
	 * @param parent the parent composite
	 */
	public abstract void createDialogContent(Composite parent);	

	/**
	 * Override this method to define how to fill the edited BeanMap
	 * content using the value defined in the graphical interface
	 * <p>This method is called when the user click on the OK Button</p>
	 * @param edited The current Edited BeanMap
	 */	
	public abstract void screenToBeanMap(BeanMap edited);

	/**
	 * Override this method to define how to display the content
	 * of the edited bean map into the graphical interface. 
	 * @param edited The beanMap currently edited
	 */	
	public abstract void beanMapToScreen(BeanMap edited);
	
	
	public static BeanMap create(AbstractConnectedSimpleBeanMapDialog dialog){
		BeanMap bean = new BeanMap(dialog.getType());
		dialog.setEditedBeanMap(bean);
		if (dialog.open()==Window.OK){
			return dialog.getResult();
		}
		return null;
	}
	public static boolean edit(AbstractConnectedSimpleBeanMapDialog dialog,BeanMap edited){
		BeanMap bean = new BeanMap(dialog.getType());
		bean.addAll(edited);
		dialog.setEditedBeanMap(bean);
		if (dialog.open()==Window.OK){
			edited.addAll(dialog.getResult());
			return true;
		}
		return false;
	}	
	
	public static void browse(AbstractConnectedSimpleBeanMapDialog dialog,BeanMap edited){
		BeanMap bean = new BeanMap(dialog.getType());
		bean.addAll(edited);
		dialog.setEditedBeanMap(bean);
		dialog.open();
	}	
	
}
