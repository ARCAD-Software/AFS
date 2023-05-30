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
package com.arcadsoftware.afs.framework.application.conf.internal;

import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ContributionItemFactory;
import org.eclipse.ui.actions.NewWizardMenu;
import org.eclipse.ui.ide.IDEActionFactory;

import com.arcadsoftware.afs.framework.application.AbstractBasicMenuFactory;
import com.arcadsoftware.afs.framework.application.IActionRegister;
import com.arcadsoftware.afs.framework.application.IMENUConst;
import com.arcadsoftware.afs.framework.application.action.RestartWorkbenchAction;

public class BasicMenuFactory extends AbstractBasicMenuFactory {
	
	
	public void createFileSubMenu(IActionRegister actionBarAdvisor,MenuManager fileMenu) {
		IWorkbenchWindow windows = actionBarAdvisor.getWorkbenchWindow();
		if (isMenuVisible(IMENUConst.FILE_NEW)){
			MenuManager newMenu = new MenuManager(getMenuLabel(IMENUConst.FILE_NEW),ActionFactory.NEW.getId());
			newMenu.add(new Separator(ActionFactory.NEW.getId()));
			newMenu.add(new NewWizardMenu(windows));
			newMenu.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
			fileMenu.add(newMenu);			
		}
		fileMenu.add(new Separator());
		fileMenu.add(new GroupMarker(IWorkbenchActionConstants.NEW_EXT));
		fileMenu.add(new Separator());
		addMenu(IMENUConst.FILE_CLOSE,actionBarAdvisor,fileMenu,windows,ActionFactory.CLOSE);
		addMenu(IMENUConst.FILE_CLOSEALL,actionBarAdvisor,fileMenu,windows,ActionFactory.CLOSE_ALL);
		fileMenu.add(new GroupMarker(IWorkbenchActionConstants.CLOSE_EXT));
		fileMenu.add(new Separator());
		addMenu(IMENUConst.FILE_SAVE,actionBarAdvisor,fileMenu,windows,ActionFactory.SAVE);
		addMenu(IMENUConst.FILE_SAVEAS,actionBarAdvisor,fileMenu,windows,ActionFactory.SAVE_AS);
		addMenu(IMENUConst.FILE_SAVEALL,actionBarAdvisor,fileMenu,windows,ActionFactory.SAVE_ALL);
		addMenu(IMENUConst.FILE_REVERT,actionBarAdvisor,fileMenu,windows,ActionFactory.REVERT);
		fileMenu.add(new Separator());
		addMenu(IMENUConst.FILE_MOVE,actionBarAdvisor,fileMenu,windows,ActionFactory.MOVE);
		addMenu(IMENUConst.FILE_RENAME,actionBarAdvisor,fileMenu,windows,ActionFactory.RENAME);
		addMenu(IMENUConst.FILE_REFRESH,actionBarAdvisor,fileMenu,windows,ActionFactory.REFRESH);
		fileMenu.add(new GroupMarker(IWorkbenchActionConstants.SAVE_EXT));
		fileMenu.add(new Separator());
		addMenu(IMENUConst.FILE_PRINT,actionBarAdvisor,fileMenu,windows,ActionFactory.PRINT);
		fileMenu.add(new GroupMarker(IWorkbenchActionConstants.PRINT_EXT));
		fileMenu.add(new Separator());
		addMenu(IMENUConst.FILE_OPENWS,actionBarAdvisor,fileMenu,windows,IDEActionFactory.OPEN_WORKSPACE);
		fileMenu.add(new GroupMarker(IWorkbenchActionConstants.OPEN_EXT));
		fileMenu.add(new Separator());
		addMenu(IMENUConst.FILE_IMPORT,actionBarAdvisor,fileMenu,windows,ActionFactory.IMPORT);
		addMenu(IMENUConst.FILE_EXPORT,actionBarAdvisor,fileMenu,windows,ActionFactory.EXPORT);		
		fileMenu.add(new GroupMarker(IWorkbenchActionConstants.IMPORT_EXT));
		fileMenu.add(new Separator());
		addMenu(IMENUConst.FILE_PROPERTIES,actionBarAdvisor,fileMenu,windows,ActionFactory.PROPERTIES);
		addMenu(IMENUConst.FILE_REOPENEDITORS,actionBarAdvisor,fileMenu,windows,ContributionItemFactory.REOPEN_EDITORS);
		fileMenu.add(new Separator());
		addMenu(IMENUConst.FILE_RESTART, actionBarAdvisor, fileMenu, new RestartWorkbenchAction());
		fileMenu.add(new GroupMarker(IWorkbenchActionConstants.MRU));
		fileMenu.add(new Separator());
		addMenu(IMENUConst.FILE_QUIT,actionBarAdvisor,fileMenu,windows,ActionFactory.QUIT);
		fileMenu.add(new GroupMarker(IWorkbenchActionConstants.FILE_END));				
	}
	
	public void createEditSubMenu(IActionRegister actionBarAdvisor,MenuManager editMenu){
		IWorkbenchWindow windows = actionBarAdvisor.getWorkbenchWindow();
		editMenu.add(new GroupMarker(IWorkbenchActionConstants.EDIT_START));
		addMenu(IMENUConst.EDIT_UNDO,actionBarAdvisor,editMenu,windows,ActionFactory.UNDO);
		addMenu(IMENUConst.EDIT_REDO,actionBarAdvisor,editMenu,windows,ActionFactory.REDO);
		editMenu.add(new GroupMarker(IWorkbenchActionConstants.UNDO_EXT));
		editMenu.add(new Separator());
		
		addMenu(IMENUConst.EDIT_CUT,actionBarAdvisor,editMenu,windows,ActionFactory.CUT);
		addMenu(IMENUConst.EDIT_COPY,actionBarAdvisor,editMenu,windows,ActionFactory.COPY);
		addMenu(IMENUConst.EDIT_PAST,actionBarAdvisor,editMenu,windows,ActionFactory.PASTE);
		editMenu.add(new GroupMarker(IWorkbenchActionConstants.CUT_EXT));
		editMenu.add(new Separator());
		addMenu(IMENUConst.EDIT_DELETE,actionBarAdvisor,editMenu,windows,ActionFactory.DELETE);
		addMenu(IMENUConst.EDIT_SELECTALL,actionBarAdvisor,editMenu,windows,ActionFactory.SELECT_ALL);
		editMenu.add(new Separator());
		addMenu(IMENUConst.EDIT_FIND,actionBarAdvisor,editMenu,windows,ActionFactory.FIND);
		editMenu.add(new GroupMarker(IWorkbenchActionConstants.FIND_EXT));
		editMenu.add(new Separator());

		addMenu(IMENUConst.EDIT_BOOKMARK,actionBarAdvisor,editMenu,windows,IDEActionFactory.BOOKMARK);
		addMenu(IMENUConst.EDIT_ADDTASK,actionBarAdvisor,editMenu,windows,IDEActionFactory.ADD_TASK);
		editMenu.add(new GroupMarker(IWorkbenchActionConstants.ADD_EXT));
		editMenu.add(new GroupMarker(IWorkbenchActionConstants.EDIT_END));
		editMenu.add(new Separator());			
	}	
	
	public void createHelpSubMenu(IActionRegister actionBarAdvisor,MenuManager helpMenu) {
		IWorkbenchWindow windows = actionBarAdvisor.getWorkbenchWindow();
		addMenu(IMENUConst.HELP_INTRO,actionBarAdvisor,helpMenu,windows,ActionFactory.INTRO);
		helpMenu.add(new Separator());
		addMenu(IMENUConst.HELP_CONTENTS,actionBarAdvisor,helpMenu,windows,ActionFactory.HELP_CONTENTS);
		addMenu(IMENUConst.HELP_SEARCH,actionBarAdvisor,helpMenu,windows,ActionFactory.HELP_SEARCH);
		addMenu(IMENUConst.HELP_DYNAMIC,actionBarAdvisor,helpMenu,windows,ActionFactory.DYNAMIC_HELP);		
		helpMenu.add(new Separator());
		helpMenu.add(new GroupMarker(IWorkbenchActionConstants.MB_ADDITIONS));
		helpMenu.add(new Separator());
		helpMenu.add(new GroupMarker(IWorkbenchActionConstants.HELP_END));
		helpMenu.add(new Separator());
		addMenu(IMENUConst.HELP_ABOUT,actionBarAdvisor,helpMenu,windows,ActionFactory.ABOUT);
	}	
	
	public void createWindowSubMenu(IActionRegister actionBarAdvisor,MenuManager windowMenu) {
		IWorkbenchWindow windows = actionBarAdvisor.getWorkbenchWindow();
		if (isMenuVisible(IMENUConst.WINDOW_OPEN_PERSPECTIVES)){
			MenuManager menuPerspective = new MenuManager(getMenuLabel(IMENUConst.WINDOW_OPEN_PERSPECTIVES), ActionFactory.OPEN_PERSPECTIVE_DIALOG.getId());			
			menuPerspective.add(ContributionItemFactory.PERSPECTIVES_SHORTLIST.create(windows));
			windowMenu.add(menuPerspective);
		}
		
		if (isMenuVisible(IMENUConst.WINDOW_OPEN_VIEWS)){
			MenuManager menu_show_view = new MenuManager(getMenuLabel(IMENUConst.WINDOW_OPEN_VIEWS), ActionFactory.SHOW_VIEW_MENU.getId());			
			menu_show_view.add(ContributionItemFactory.VIEWS_SHORTLIST.create(windows));
			menu_show_view.setMenuText(Messages.resString("menu.window.showView"));
			windowMenu.add(menu_show_view);
		}
		windowMenu.add(new Separator());
		addMenu(IMENUConst.WINDOW_SAVE_PERSPECTIVE, actionBarAdvisor, windowMenu, windows,
				ActionFactory.SAVE_PERSPECTIVE);
		addMenu(IMENUConst.WINDOW_RESET_PERSPECTIVE,actionBarAdvisor,windowMenu,windows,ActionFactory.RESET_PERSPECTIVE);
		addMenu(IMENUConst.WINDOW_CLOSE_PERSPECTIVE, actionBarAdvisor, windowMenu, windows,
				ActionFactory.CLOSE_PERSPECTIVE);
		addMenu(IMENUConst.WINDOW_CLOSE_ALLPERSPECTIVE, actionBarAdvisor, windowMenu, windows,
				ActionFactory.CLOSE_ALL_PERSPECTIVES);
		windowMenu.add(new Separator());
		addMenu(IMENUConst.WINDOW_PREFERENCES, actionBarAdvisor, windowMenu, windows, ActionFactory.PREFERENCES);
		
		windowMenu.add(new Separator());
		windowMenu.add(new GroupMarker(IWorkbenchActionConstants.MB_ADDITIONS));
		windowMenu.add(new Separator());
		windowMenu.add(new GroupMarker(IWorkbenchActionConstants.WINDOW_EXT));
	}		
	
}
