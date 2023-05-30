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
import com.arcadsoftware.afs.client.macro.model.MacroLogItemDefinitions;

public class MacroLogItemEditorInput implements IEditorInput {
	
	MacroLogItemDefinitions logItemDefinitions;
	
	public MacroLogItemEditorInput(MacroLogItemDefinitions list){
		super();
		this.logItemDefinitions = list;
	}
	
	public MacroLogItemDefinitions getMacroLogItemDefinitions() {
		return logItemDefinitions;
	}
	
	public boolean exists() {
		return false;
	}

	public ImageDescriptor getImageDescriptor() {
		return AFSIcon.MACRO_LOG.imageDescriptor();
	}

	public String getName() {
		return logItemDefinitions.getLabel();
	}

	public IPersistableElement getPersistable() {
		return null;
	}

	public String getToolTipText() {
		return Activator.resString("macroLogItem.editor.tooltip"); //$NON-NLS-1$
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public Object getAdapter(Class arg0) {
		return null;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object arg0) {
		if (arg0 instanceof MacroLogItemEditorInput){
			MacroLogItemEditorInput ed = (MacroLogItemEditorInput)arg0;
			return ed.getMacroLogItemDefinitions().getLabel().equalsIgnoreCase(this.getMacroLogItemDefinitions().getLabel());
		}		
		return false;
		
	}		
	
	@Override
	public int hashCode() {
		return getMacroLogItemDefinitions().getLabel().hashCode();
	}
	
	/**
	 * Ouverture de l'éditeur associé à un input.
	 * 
	 * @param input
	 * @return
	 */
	public static MacroLogItemEditorPart openEditor(MacroLogItemEditorInput input) {
		try {
			IEditorPart editor = 
					Activator.getDefault()
				                 .getWorkbench()
				                 .getActiveWorkbenchWindow()
				                 .getActivePage()
				                 .openEditor(input,
				                		 MacroLogItemEditorPart.MACROLOGITEM_EDITOR_ID);
		                     
				                     
			if (editor instanceof MacroLogItemEditorPart)
				return (MacroLogItemEditorPart) editor;
			return null;
		} catch (PartInitException e) {
			MessageManager.addExceptionBeta(e);
			return null;
		}
	}	

	
}
