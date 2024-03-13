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
package com.arcadsoftware.afs.framework.application.conf;


import java.util.HashMap;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPreferenceConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.IWorkbenchConfigurer;

import com.arcadsoftware.afs.framework.application.IConfigurationManager;
import com.arcadsoftware.afs.framework.application.IMENUConst;
import com.arcadsoftware.afs.framework.application.conf.internal.Messages;

public abstract class AbstractConfigurationManager implements IConfigurationManager {

	public static final int MENU_FILE = 0;

	private HashMap<String,String> visibleMenus;
	private boolean perspectiveBarVisible = true;
	private boolean statusLineVisible;
	private boolean fastViewBarVisible;
	private boolean coolBarVisible;
	private boolean progressBarVisible;
	
	public AbstractConfigurationManager() {
		super();
		visibleMenus = new HashMap<String,String>();
		setVisibleMenus(visibleMenus);
	}
	
	/**
	 * Defines if the perspective bar is visible or not
	 * @param perspectiveBarVisible Indicates the visibility of the perspective bar
	 */
	public void setPerspectiveBarVisible(boolean perspectiveBarVisible) {
		this.perspectiveBarVisible = perspectiveBarVisible;
	}
	
	public boolean isPerspectiveBarVisible() {
		return perspectiveBarVisible;
	}
	
	/**
	 * Defines if the status Line is visible or not
	 * @param statusLineVisible Indicates the visibility of the status Line
	 */	
	public void setStatusLineVisible(boolean statusLineVisible) {
		this.statusLineVisible = statusLineVisible;
	}
	
	public boolean isStatusLineVisible() {
		return statusLineVisible;
	}	
	
	/**
	 * Defines if the Cool Bar is visible or not
	 * @param coolBarVisible Indicates the visibility of the Cool Bar
	 */		
	public void setCoolBarVisible(boolean coolBarVisible) {
		this.coolBarVisible = coolBarVisible;
	}
	
	public boolean isCoolBarVisible() {
		return coolBarVisible;
	}	
	
	/**
	 * Defines if the Progress Bar is visible or not
	 * @param progressBarVisible Indicates the visibility of the Progress Bar
	 */		
	public void setProgressBarVisible(boolean progressBarVisible) {
		this.progressBarVisible = progressBarVisible;
	}
	
	/**
	 * Returns the visibility indicator of the Progress Bar
	 * @return true if the Progress Bar must be displayed.
	 */		
	public boolean isProgressBarVisible() {
		return progressBarVisible;
	}		
	
	/**
	 * Defines if the FastView Bar is visible or not.
	 * @param fastViewBarVisible Indicates the visibility of the FastView Bar
	 */	
	public void setFastViewBarVisible(boolean fastViewBarVisible) {
		this.fastViewBarVisible = fastViewBarVisible;
	}
	
	public boolean isFastViewBarVisible() {
		return fastViewBarVisible;
	}	
	
	/**
	 * Returns the location identifier of the perspective Bar.<br/>
	 * This method must be override if you want to change the default value
	 * that is {@link IWorkbenchPreferenceConstants.TOP_LEFT}
	 * @return
	 */
	protected String getPerspectiveBarLocation() {
		return IWorkbenchPreferenceConstants.TOP_LEFT;
	}
	
	/**
	 * Returns if the tabs are displayed in the traditional way or not.<br> 
	 * This method must be override if you want to change the default value
	 * that is false.
	 * @return
	 */
	protected boolean useTraditionalStyleTabs() {
		return false;
	}
	
	/**
	 * Allows to set the Perspective Bar location.<br/>
	 * The location is set only if the perspective bar is visible.
	 * @param location a location constants defined in IWorkbenchPreferenceConstants
	 * @see IWorkbenchPreferenceConstants
	 */
	public void setPerspectiveBarLocation() {
		if (isPerspectiveBarVisible()) {
			PlatformUI.getPreferenceStore().setValue(
					IWorkbenchPreferenceConstants.DOCK_PERSPECTIVE_BAR,
					getPerspectiveBarLocation());					
		}
	}
	
	public void setPerspectiveBarStyle() {
		if (isPerspectiveBarVisible()) {
			PlatformUI.getPreferenceStore().setValue(
					IWorkbenchPreferenceConstants.SHOW_TRADITIONAL_STYLE_TABS,
					useTraditionalStyleTabs()); 							
		}
	}	
	
	public boolean isMenuVisible(String menuId) {
		return (visibleMenus.get(menuId) != null);
	}
	
	public String getMenuLabel(String menuId){
		String value = visibleMenus.get(menuId);
		if (value != null) {
			if (!value.isEmpty() && (value.charAt(0) == '!')) {
				return Messages.resString(value.substring(1));
			}
			return value;			
		}
		return Messages.resString(menuId);
	}
	
	public void doAfterInitialization(IWorkbenchConfigurer configurer) {}
	
	public void postStartup() {}
	
	public void preStartup() {}
	
	public boolean preShutdown() {
		return true;
	}
	
	public  void postShutdown() {}	
	
	/**
	 * Returns the default perspective Id of your application
	 * 
	 * @return Default Perspective Id
	 */
	public abstract String getDefaultPerspectiveId(); 
		
	/**
	 * Returns the title of your application
	 * 
	 * @return Title of the application
	 */	
	public abstract String getWindowTitle();
	
	public Point getInitialSize() {
		return new Point(950,750);
	}
	
	/**
	 * This method defines the default menu schema<br/>
	 * The visible menu items are stored into the <code>visibleMenus</code> HashMap.
	 * <p>
	 * - Add an entry into this map to declare a menu item visible or not.<br>
	 * - The key is a unique identifier of a menu item
	 * - the value is the label of this menu item.  
	 * </p>
	 * @param visibleMenus
	 */
	public void setVisibleMenus(HashMap<String,String> visibleMenus){
		visibleMenus.put(IMENUConst.FILE,Messages.resString("menu.file")); //$NON-NLS-1$
		visibleMenus.put(IMENUConst.FILE_QUIT,""); //$NON-NLS-1$
		visibleMenus.put(IMENUConst.EDIT,Messages.resString("menu.edit")); //$NON-NLS-1$
		visibleMenus.put(IMENUConst.EDIT_COPY,""); //$NON-NLS-1$
		visibleMenus.put(IMENUConst.EDIT_CUT,""); //$NON-NLS-1$
		visibleMenus.put(IMENUConst.EDIT_PAST,""); //$NON-NLS-1$
		visibleMenus.put(IMENUConst.HELP,Messages.resString("menu.help")); //$NON-NLS-1$
		visibleMenus.put(IMENUConst.HELP_CONTENTS,""); //$NON-NLS-1$
		visibleMenus.put(IMENUConst.HELP_SEARCH,""); //$NON-NLS-1$
		visibleMenus.put(IMENUConst.HELP_ABOUT,""); //$NON-NLS-1$
	}

	public int getShellStyle() {
		return 0;
	}
	
	public void adaptMainShell(Shell shell) {}
	
	/**
	 * Allow to define witch Action Set Extension will be used.
	 * Override this method to define which Actions will be used. 
	 * 
	 * <p>
	 * This process by removing action sets from the registry when application starts.
	 */
	public boolean isActionSetVisible(String actionSetId) {
		return true;
	}
}
