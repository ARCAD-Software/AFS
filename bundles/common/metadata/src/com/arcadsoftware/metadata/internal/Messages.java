/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
package com.arcadsoftware.metadata.internal;

import org.eclipse.osgi.util.NLS;

/**
 *
 */
public class Messages extends NLS {
	
	private static final String BUNDLE_NAME = "com.arcadsoftware.metadata.internal.messages"; //$NON-NLS-1$

	public static String AbstractMapperService_Debug_ComplexCondition;
	public static String AbstractMapperService_Error_InitializationFailed;
	public static String AbstractMapperService_Error_NoMultidomainForInGroupCriteria;
	public static String AbstractMapperService_Error_NoMultidomainForLinkEqualsCriteria;
	public static String AbstractMapperService_Error_NoMultidomainForMultiAttributesCriteria;
	public static String AbstractMapperService_Error_TooLargeSelection;
	public static String AbstractMapperService_Info_ComplexSelection;
	public static String Activator_ConsoleHelp_MapperAlias;
	public static String Activator_Debug_BadMapper;
	public static String Activator_Debug_BadRegistry;
	public static String Activator_Debug_EventNotFired;
	public static String Activator_Warn_BadMapper;
	public static String BeanMapEventHandler_Log_Dispatch_Error;
	public static String BeanMapEventHandler_Log_No_BeanMap;
	public static String BeanMapEventHandler_Log_No_second_BeanMap;
	public static String BeanMapEventHandler_Log_No_User;
	public static String BeanMapEventTracker_Post_Error;
	public static String Criteria_And;
	public static String Criteria_Between;
	public static String Criteria_BetweenEx;
	public static String Criteria_CaseSensitive;
	public static String Criteria_Contain;
	public static String Criteria_CurrentDate;
	public static String Criteria_CurrentTime;
	public static String Criteria_CurrentUser;
	public static String Criteria_DateFormat;
	public static String Criteria_DateTimeFormat;
	public static String Criteria_Day;
	public static String Criteria_EndWith;
	public static String Criteria_Equal;
	public static String Criteria_EqualString;
	public static String Criteria_False;
	public static String Criteria_Greater;
	public static String Criteria_GreaterOrEqual;
	public static String Criteria_Group;
	public static String Criteria_GroupType;
	public static String Criteria_HasRight;
	public static String Criteria_Hour;
	public static String Criteria_Id;
	public static String Criteria_IsCurrentUser;
	public static String Criteria_IsCurrentUserAttribute;
	public static String Criteria_IsNotNull;
	public static String Criteria_IsNull;
	public static String Criteria_IsTrue;
	public static String Criteria_Item;
	public static String Criteria_LinkedThrough;
	public static String Criteria_Lower;
	public static String Criteria_LowerOrEqual;
	public static String Criteria_MemberOf;
	public static String Criteria_Minute;
	public static String Criteria_Month;
	public static String Criteria_Not;
	public static String Criteria_Or;
	public static String Criteria_StartWith;
	public static String Criteria_SubstituteWith;
	public static String Criteria_True;
	public static String Criteria_User;
	public static String Criteria_WithParam;
	public static String Criteria_Year;
	public static String CriteriaContextBasic_CurrentUser;
	public static String CriteriaContextBasic_Entity;
	public static String CriteriaContextBasic_NumberOfLinks;
	public static String CriteriaContextBasic_NumberOfReferences;
	public static String DataAccess_Error_FileDownload;
	public static String DataAccess_Error_FileUpload;
	public static String DataAccess_Error_LoadingProperties;
	public static String LocalCache_SkipReload;
	public static String MetaDataEntity_Error_DataCount;
	public static String MetaDataEntity_Error_DataCreation;
	public static String MetaDataEntity_Error_DataDeletion;
	public static String MetaDataEntity_Error_DataSelect;
	public static String MetaDataEntity_Error_DataTest;
	public static String MetaDataEntity_Error_DataUpdate;
	public static String MetaDataEventHandler_Debug_NoEntity;
	public static String MetaDataEventHandler_Debug_NoMapper;
	public static String MetaDataEventHandler_Debug_NoRegistry;
	public static String MetaDataEventHandler_Error_SelectionEventWithOutSelection;
	public static String ProgressState_0;
	public static String ProgressState_1;
	public static String ProgressState_2;
	public static String ProgressState_3;
	public static String Criteria_UnLinkedThrough;
	public static String Criteria_Any;
	public static String Criteria_InSet;
	public static String Criteria_Recursive;

	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {}
}
