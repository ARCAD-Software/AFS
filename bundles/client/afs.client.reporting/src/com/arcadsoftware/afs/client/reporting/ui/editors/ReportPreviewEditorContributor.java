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
package com.arcadsoftware.afs.client.reporting.ui.editors;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.part.EditorActionBarContributor;

import com.arcadsoftware.afs.client.reporting.Activator;


public class ReportPreviewEditorContributor extends EditorActionBarContributor {
	
	
	private Action refreshAction;
	IToolBarManager toolBarManager;
	ReportPreviewEditor currentEditor;
	private boolean toolbarCreated = false;
	
	public ReportPreviewEditorContributor(){
		super();
		refreshAction = new Action(){
			@Override
			public void run() {
				currentEditor.refreshReport();
			}
		};
		refreshAction.setText(Activator.resString("report.action.refreshReport.text")); //$NON-NLS-1$
		refreshAction.setToolTipText(Activator.resString("report.action.refreshReport.tooltip")); //$NON-NLS-1$
		refreshAction.setImageDescriptor(Activator.getDefault().getImageDescriptor("REFRESH")); //$NON-NLS-1$
	}

    /* (non-Javadoc)
     * @see org.eclipse.ui.IEditorActionBarContributor#setActiveEditor(org.eclipse.ui.IEditorPart)
     */
    public void setActiveEditor(IEditorPart targetEditor) {
        if (targetEditor instanceof ReportPreviewEditor){
        	currentEditor = (ReportPreviewEditor)targetEditor; 
            manageToolbar();
        }
    }    
    
    private void manageToolbar(){
        if (currentEditor!=null) {
        	if (!toolbarCreated) {
	            toolBarManager.add(refreshAction);
	            toolbarCreated = true;
        	}
        }    	
    }    
    
    
    /* (non-Javadoc)
     * @see org.eclipse.ui.part.EditorActionBarContributor#contributeToToolBar(org.eclipse.jface.action.IToolBarManager)
     */
    public void contributeToToolBar(IToolBarManager toolBarManager) {
        super.contributeToToolBar(toolBarManager);
        this.toolBarManager = toolBarManager;
    }
}
