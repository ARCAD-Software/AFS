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
package com.arcadsoftware.afs.client.core.ui.widgets;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.aev.core.ui.dialogs.DialogConstantProvider;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.dialogs.AbstractBeanMapDialog;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.ElementParameter;
import com.arcadsoftware.editor.swt.ISWTRenderer;

public class DynamicDialog extends AbstractBeanMapDialog {
	
	private static final String WIDTH="width";
	private static final String HEIGHT="height";
	private static final String LAYOUT_NAME="layoutName";
	private static final String TITLE="title";	
	public static final String SAVE="saveInDialog";
	public static final String SHOWDIALOGBUTTON="showDialogButton";
	
	public static final int SAVED_SUCCEED=0;
	public static final int SAVED_FAILED=1;
	public static final int SAVED_UNDEFINED=21;
	
	private String layoutName;
	private Point size;
	private String title;
	private boolean saveInDialog;
	private boolean showButton = true;
	private boolean readOnly = false;
	
	
	private int saved = SAVED_UNDEFINED;
	
	public DynamicDialog(Shell parentShell,  ServerConnection connection,
			BeanMap edited,ISWTRenderer renderer, ElementParameter e, boolean readonly, boolean addition) {
		super(parentShell,connection,true,true);	
		setEditedBeanMap(edited);
		String layoutName = e.getParameter(LAYOUT_NAME);				
		String title = e.getParameter(TITLE);				
		int width = e.getParameterInteger(WIDTH, 400);
		int height = e.getParameterInteger(HEIGHT, 300);
		this.layoutName=layoutName;
		this.size = new Point(width,height);
		this.title= renderer.getLocalizedMessage(title);
		if (!addition) { 
			saveInDialog =  e.getParameterBoolean(SAVE);
			this.readOnly = readonly;
		} else {
			saveInDialog =  false;
			this.readOnly = false;
		}
		
		String value = e.getParameter(SHOWDIALOGBUTTON);
		if (value==null) {
			showButton = true;
		} else {		
			showButton = e.getParameterBoolean(SHOWDIALOGBUTTON);
		}
				
	}	
	
	
	@Override
	protected Control createDialogArea(Composite parent) {
		Control c =  super.createDialogArea(parent);
		editor.setEnabled(!readOnly);
		return c;
	}
	
	
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		if (showButton) {
			if (!readOnly) {
				super.createButtonsForButtonBar(parent);
			} else {
				createButton(parent, IDialogConstants.CANCEL_ID,
						DialogConstantProvider.getInstance().CANCEL_LABEL,false);				
			}
		}		
	}
	
	@Override
	protected void doBeforeClosing(BeanMap result) {
		super.doBeforeClosing(result);
		if (saveInDialog) {
			if (!readOnly) {
				boolean saveResult = editor.save();
				saved = saveResult?SAVED_SUCCEED:SAVED_FAILED;
			} else {
				saved = SAVED_SUCCEED;
			}
		}		
	}
	

	
	@Override
	public String getType() {
		if (initalBeanmap!=null)
			return initalBeanmap.getType();
		else
			return "";
	}

	@Override
	public String getLayoutName() {
		return layoutName;
	}

	@Override
	public String getTitle() {
		return title;
	}

	
	public int getSavedStatus(){
		return saved;
	}



	@Override
	public Point getSize() {
		return size;
	}
	
}
