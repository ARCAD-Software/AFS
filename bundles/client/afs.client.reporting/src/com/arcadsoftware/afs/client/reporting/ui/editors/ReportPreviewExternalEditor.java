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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Locale;

import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;

import com.arcadsoftware.aev.core.messages.MessageManager;
import com.arcadsoftware.afs.client.reporting.Activator;


public class ReportPreviewExternalEditor {

	protected ReportPreviewEditorInput reportInput;
	protected String currentUrl = null;
	
	private static String BROWSER_ID = "com.arcadsoftware.ReportPreviewEditor";
	
	private IWebBrowser extBrowser  = null;
	
	
    public ReportPreviewExternalEditor(){
    }
	public ReportPreviewExternalEditor(ReportPreviewEditorInput input){
		super();
		this.reportInput = input;
	}
    
    public void preview(){
        try {           
            String previewType = reportInput.getViewerType();
            String previewExtraParams = reportInput.getViewerExtraParameters();
            
            String reportUrl = reportInput.getReport().getUrlOrFilename();            
            String proxyUrl = reportInput.getHelper().getRedirection(Activator.PROXYURL);  

            currentUrl = String.format(ReportPreviewEditorInput.URL_FORMAT, 
            		proxyUrl, 
            		reportUrl, 
            		previewType, 
            		(previewExtraParams == null)?"":"&"+previewExtraParams, 
            		Locale.getDefault());
            
            extBrowser = createExternalBrowser(reportInput.getReport().getName());
            if (extBrowser != null){
            	extBrowser.openURL(new URL(currentUrl));
            }
            
        } catch(Exception e) {
        	MessageManager.addException(e,MessageManager.LEVEL_PRODUCTION);
        }
    }

    public void refreshReport(){
    	if (currentUrl!=null ){
    		try {
				extBrowser.openURL(new URL(currentUrl));
			} catch (PartInitException e) {
				MessageManager.addException(e,MessageManager.LEVEL_PRODUCTION);
			} catch (MalformedURLException e) {
				MessageManager.addException(e,MessageManager.LEVEL_PRODUCTION);
			}
    	}
    }
    
    /**
     * Create external browser where to display report
     * 
     * @param title
     * @return
     * @throws Exception
     */
    protected IWebBrowser createExternalBrowser(String title) throws Exception
	{		
		int style = IWorkbenchBrowserSupport.AS_EDITOR | IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.STATUS;

		// build URL for Browser Initialization		
		IWorkbenchBrowserSupport browserSupport = PlatformUI.getWorkbench().getBrowserSupport();
		IWebBrowser browserInstance = browserSupport.createBrowser(style, BROWSER_ID, title, null);			
		
		return browserInstance;
	}
}
