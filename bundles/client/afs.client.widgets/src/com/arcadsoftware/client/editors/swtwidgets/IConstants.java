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
package com.arcadsoftware.client.editors.swtwidgets;

/**
 * This class groups constants parameters and constants value for the dynamic
 * editors.
 */
public interface IConstants {
	
	public static final String ENABLED_IF = "enabledIf"; //$NON-NLS-1$
	public static final String COLS = "cols"; //$NON-NLS-1$
	public static final String BORDER = "border"; //$NON-NLS-1$
	public static final String EXPANDED = "expanded"; //$NON-NLS-1$
	public static final String LARGE = "large"; //$NON-NLS-1$
	public static final String TWISTIE = "twistie"; //$NON-NLS-1$
	public static final String TITLE = "title"; //$NON-NLS-1$
	public static final String LABEL = "label"; //$NON-NLS-1$
	public static final String ACTION = "action"; //$NON-NLS-1$
	public static final String GRAB_HORIZONTAL = "grabHorizontal"; //$NON-NLS-1$
	public static final String URL = "url"; //$NON-NLS-1$
	public static final String ORIENTATION = "orientation"; //$NON-NLS-1$
	public static final String EXPANDABLE = "expendable"; //$NON-NLS-1$
	public static final String DEFAULT = "default"; //$NON-NLS-1$
	public static final String DIGITS = "digits"; //$NON-NLS-1$
	public static final String MIN = "min"; //$NON-NLS-1$
	public static final String MAX = "max"; //$NON-NLS-1$
	public static final String EDITABLE = "editable"; //$NON-NLS-1$
	public static final String EDIT_ACTION = "editAction"; //$NON-NLS-1$
	public static final String ADD_EDITOR = "addEditor"; //$NON-NLS-1$
	public static final String ADD_ACTION = "addAction"; //$NON-NLS-1$
	public static final String ADD_ACTION_LABEL = "addActionLabel"; //$NON-NLS-1$
	public static final String EDIT_ACTION_LABEL = "editActionLabel"; //$NON-NLS-1$
	public static final String FORMAT = "format"; //$NON-NLS-1$
	public static final String SCROLL = "scroll"; //$NON-NLS-1$
	public static final String FILL_BOTH = "fillBoth"; //$NON-NLS-1$
	public static final String WIDGET_ID = "widgetId"; //$NON-NLS-1$
	public static final String LOAD_ERROR_MESSAGE = "loadErrorMessage"; //$NON-NLS-1$
	public static final String LOAD_ERROR_TITLE = "loadErrorTitle"; //$NON-NLS-1$
	public static final String SAVE_ERROR_MESSAGE = "saveErrorMessage"; //$NON-NLS-1$
	public static final String SAVE_ERROR_TITLE = "saveErrorTitle"; //$NON-NLS-1$
	public static final String SAVE_BEFORE_LOAD_MESSAGE = "saveBeforeLoadMessage"; //$NON-NLS-1$
	public static final String SAVE_BEFORE_LOAD_TITLE = "saveBeforeLoadTitle"; //$NON-NLS-1$
	public static final String ID = "id"; //$NON-NLS-1$
	public static final String TYPE = "type"; //$NON-NLS-1$
	public static final String LAYOUT_NAME = "layoutName"; //$NON-NLS-1$
	public static final String RATIO = "ratio"; //$NON-NLS-1$
	public static final String FILL_VERTICAL = "fillVertical"; //$NON-NLS-1$
	public static final String FILL_HORIZONTAL = "fillHorizontal"; //$NON-NLS-1$
	public static final String MENU_LABEL = "menuLabel"; //$NON-NLS-1$
	public static final String IN_TOOL_BAR = "inToolBar"; //$NON-NLS-1$
	public static final String ICON = "icon"; //$NON-NLS-1$
	public static final String LINKED = "linked"; //$NON-NLS-1$
	public static final String LINK_CODE = "linkCode"; //$NON-NLS-1$
	public static final String WIDGET_LISTENED = "widgetListened"; //$NON-NLS-1$
	public static final String INTERNAL_EDITOR_ID = "internalEditorId"; //$NON-NLS-1$
	public static final String BORDER_SPACE = "borderSpace"; //$NON-NLS-1$
	public static final String REMOVE_ACTION = "removeAction"; //$NON-NLS-1$
	public static final String REMOVE_ACTION_LABEL = "removeActionLabel"; //$NON-NLS-1$
	public static final String HTTP = "http"; //$NON-NLS-1$
	public static final String BOOLEAN = "boolean"; //$NON-NLS-1$
	public static final String ATTRIBUTE = "attribute"; //$NON-NLS-1$
	public static final String COLUMN = "column"; //$NON-NLS-1$
	public static final String USE_BOOLEAN_IMAGE = "useBooleanImage"; //$NON-NLS-1$
	public static final String HORIZONTAL = "horizontal"; //$NON-NLS-1$
	public static final String WITH_CHECK_BUTTON = "withCheckButton"; //$NON-NLS-1$
	public static final String REMOVE_ICON = "removeIcon"; //$NON-NLS-1$
	public static final String EDIT_ICON = "editIcon"; //$NON-NLS-1$
	public static final String ADD_ICON = "addIcon"; //$NON-NLS-1$
	public static final String WITH_MARGIN = "withMargin"; //$NON-NLS-1$
	public static final String ALIGN = "align"; //$NON-NLS-1$
	public static final String BOTTOM = "bottom"; //$NON-NLS-1$
	public static final String READ_ONLY = "readOnly"; //$NON-NLS-1$
	public static final String STRING = "string"; //$NON-NLS-1$
	public static final String MULTI = "multi"; //$NON-NLS-1$
	public static final String REMOVE_CONFIRMATION_MESSAGE = "removeConfirmationMessage"; //$NON-NLS-1$
	public static final String REMOVE_CONFIRMATION_TITLE = "removeConfirmationTitle"; //$NON-NLS-1$
	public static final String DATE_FORMAT = "dateFormat"; //$NON-NLS-1$
	public static final String EDITOR_ACTION = "editorAction"; //$NON-NLS-1$
	public static final String MANDATORY = "mandatory"; //$NON-NLS-1$
	public static final String LAYOUT_EMPTY_NAME = "layoutEmptyName"; //$NON-NLS-1$
	public static final String MENU_ICON = "menuIcon"; //$NON-NLS-1$
	public static final String IN_FORM_TOOL_BAR = "inFormToolBar"; //$NON-NLS-1$
	public static final String AT_RIGHT = "atRight"; //$NON-NLS-1$
	public static final String AT_BOTTOM = "atBottom"; //$NON-NLS-1$
	public static final String IN_INTERNAL_TOOL_BAR = "inInternalToolBar"; //$NON-NLS-1$
	public static final String WITH_TIME = "withTime"; //$NON-NLS-1$
	public static final String SHORTTIME = "shortTime"; //$NON-NLS-1$
	public static final String DEFAULT_VALUE = "defaultValue"; //$NON-NLS-1$
	public static final String ONLY_CLIENT = "onlyClient"; //$NON-NLS-1$
	public static final String ONLY_PROVIDER = "onlyProvider"; //$NON-NLS-1$
	public static final String VISIBLE = "visible"; //$NON-NLS-1$
	public static final String GET_FILE_PATH = "getFilePath"; //$NON-NLS-1$
	public static final String TABLE_VIEWER_ID = "tableViewerId"; //$NON-NLS-1$
	public static final String TREE_VIEWER_ID = "treeViewerId"; //$NON-NLS-1$
	public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
	public static final String BIND = "bind"; //$NON-NLS-1$
	public static final String TRANSLATE = "translate"; //$NON-NLS-1$
	public static final String ATTRIBUTEFILTER = "attributefilter"; //$NON-NLS-1$
	public static final String VALUE = "value"; //$NON-NLS-1$
	public static final String EQUALITY = "equals"; //$NON-NLS-1$

	public static final String EMPTY = ""; //$NON-NLS-1$
	public static final String TWO_POINTS = ":"; //$NON-NLS-1$
	//<FM number="2010/564" version="09.03.02" date=Dec 7, 2010 user=md>
	public static final String HEIGHT = "height"; //$NON-NLS-1$
	//</FM>
	
	public static final String STORE_VIEWER_STATE = "storeState"; //$NON-NLS-1$
	public static final String WIDTH = "width"; //$NON-NLS-1$
	public static final String BUTTON = "button";//$NON-NLS-1$
	public static final String BUTTONBAR = "buttonBar";//$NON-NLS-1$
	public static final String POSITION = "position";//$NON-NLS-1$
	public static final String BUTTONBARID ="buttonBarId";//$NON-NLS-1$
	public static final String BEFORE = "before";//$NON-NLS-1$
	public static final String AFTER = "after";//$NON-NLS-1$
	public static final String LEFT = "left";//$NON-NLS-1$	
	public static final String RIGHT = "right";//$NON-NLS-1$
	public static final String ANCHOR = "anchor";//$NON-NLS-1$
	public static final String OFFSET = "offset";	//$NON-NLS-1$
	public static final String ACTIONID = "actionId";//$NON-NLS-1$
	public static final String DBCLICK = "dbclick";//$NON-NLS-1$
	public static final String SHOW_INMENU = "showInMenu";//$NON-NLS-1$
	public static final String SHOW_INTOOLBAR = "showInToolbar";//$NON-NLS-1$	
	
	public static final String SECTION_DESCRIPTION = "description"; //$NON-NLS-1$
	
	
	public static final String COLSPAN = "colSpan"; //$NON-NLS-1$
	public static final String FILL_STYLE = "fillStyle"; //$NON-NLS-1$	
	public static final String FILL_STYLE_HORIZONTAL = "horizontal"; //$NON-NLS-1$	
	public static final String FILL_STYLE_VERTICAL = "vertical"; //$NON-NLS-1$
	public static final String FILL_STYLE_BOTH = "both"; //$NON-NLS-1$
	public static final String ISPASSWORD = "isPassword"; //$NON-NLS-1$
	
	
	public static final String ATTRIBUTE_LIST = "attributeList"; //$NON-NLS-1$
	public static final String ORDER_LIST = "orderList"; //$NON-NLS-1$
	public static final String EDITION_ATTRIBUTE = "editionAttribute"; //$NON-NLS-1$
	public static final String EDITION_TYPE = "editionType"; //$NON-NLS-1$
	
	public static final String MANDATORYATTRIBUTE = "mandatoryCondBy"; //$NON-NLS-1$
	public static final String TEXTLIMIT = "limit"; //$NON-NLS-1$
	
	public static final String SHOWTEXT = "showText"; //$NON-NLS-1$
	
	public static final String LINES = "lines"; //$NON-NLS-1$
	public static final String MONOSPACE_FONT = "monospaceFont"; //$NON-NLS-1$
	
}
