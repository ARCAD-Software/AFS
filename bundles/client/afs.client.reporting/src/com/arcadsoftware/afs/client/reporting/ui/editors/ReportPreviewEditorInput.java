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

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IPersistableElement;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.reporting.core.ReportHeader;

public class ReportPreviewEditorInput implements IEditorInput {	

    // URL : {proxy}{reportRunTarget}&__format={viewerType}{&otherParams}&__locale={locale}
	public static final String URL_FORMAT="%s%s&__format=%s%s&__locale=%s";
	
	private ServerConnection connection;
	private DataAccessHelper helper;
    ReportHeader report;
    
    private String viewerType;
    private String extraParams;

    public String getViewerType(){
        return viewerType;
    }
    
    public String getViewerExtraParameters(){
        return extraParams;
    }

    public void setViewerType(String viewerType){
        this.viewerType = viewerType;
    }

    public ReportHeader getReport(){
        return report;
    }

    public ReportPreviewEditorInput(ServerConnection connection,ReportHeader report, String viewerType, String extraParams){
        this.report = report;
        this.viewerType = viewerType;
        this.extraParams = extraParams;
        this.connection =  connection;
        helper = new DataAccessHelper(connection);
    }

    public boolean exists(){
        return false;
    }

    public ImageDescriptor getImageDescriptor(){
        return null;
    }

    public String getName(){
        return report.getName();
    }

    public IPersistableElement getPersistable(){
        return null;
    }

    public String getToolTipText(){
        return report.getDescription();
    }

    @Override
    public <T> T getAdapter(Class<T> adapter) {    
    	return null;
    }

    public static ReportPreviewEditor openEditor(ReportPreviewEditorInput input){
		ReportPreviewExternalEditor extEditor = new ReportPreviewExternalEditor(input);
		extEditor.preview();		
		return null;
    }
    
    
	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object arg0) {
		if (arg0 instanceof ReportPreviewEditorInput){
			if (arg0!=null) {
				ReportPreviewEditorInput ed = (ReportPreviewEditorInput)arg0;
				return ed.getReport().isEqualTo(this.report);
			}
		}		
		return false;
		
	}	    
	    
	@Override
	public int hashCode() {
		//Note: modifiy this method according to the equals method
		return super.hashCode();
	}
	
	public ServerConnection getConnection() {
		return connection;
	}
	
	public DataAccessHelper getHelper() {
		return helper;
	}
	
}
