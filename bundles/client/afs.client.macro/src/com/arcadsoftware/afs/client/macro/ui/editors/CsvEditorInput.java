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

package com.arcadsoftware.afs.client.macro.ui.editors;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.PartInitException;

import com.arcadsoftware.aev.core.messages.MessageManager;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.macro.internal.Activator;
import com.arcadsoftware.afs.client.macro.model.CSVLines;

public class CsvEditorInput implements IEditorInput {
	
	private CSVLines input;
	private String editorName;
	
	public CsvEditorInput(CSVLines input,String editorName){
		super();
		this.input = input;
		this.editorName = editorName;
	}
	
	public CSVLines getInput() {
		return input;
	}
	
	public boolean exists() {
		return false;
	}

	public ImageDescriptor getImageDescriptor() {
		return AFSIcon.LIST.imageDescriptor();
	}

	public String getName() {
		return editorName;
	}

	public IPersistableElement getPersistable() {
		return null;
	}

	public String getToolTipText() {
		return Activator.resString("csv.editor.tooltip"); //$NON-NLS-1$
	}

	@SuppressWarnings("unchecked")
	public Object getAdapter(@SuppressWarnings("rawtypes") Class arg0) {
		return null;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object arg0) {	
		return false;
		
	}		
	
	
	/**
	 * Ouverture de l'éditeur associé à un input.
	 * 
	 * @param input
	 * @return
	 */
	public static CsvEditorPart openEditor(CsvEditorInput input) {
		try {
			IEditorPart editor = 
					Activator.getDefault()
				                 .getWorkbench()
				                 .getActiveWorkbenchWindow()
				                 .getActivePage()
				                 .openEditor(input,
				                		 CsvEditorPart.CSVFILE_EDITOR_ID);
		                     
				                     
			if ((editor != null) && (editor instanceof CsvEditorPart))
				return (CsvEditorPart) editor;
			return null;
		} catch (PartInitException e) {
			MessageManager.addExceptionBeta(e);
			return null;
		}
	}	

	
}
