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
package com.arcadsoftware.email.internal;

public class HTMLParserOptions {
	  private static final String[] tokenImage = {
			    "<EOF>", //$NON-NLS-1$
			    "<TagName>", //$NON-NLS-1$
			    "<DeclName>", //$NON-NLS-1$
			    "\"<!--\"", //$NON-NLS-1$
			    "\"<!\"", //$NON-NLS-1$
			    "<Word>", //$NON-NLS-1$
			    "<LET>", //$NON-NLS-1$
			    "<NUM>", //$NON-NLS-1$
			    "<Entity>", //$NON-NLS-1$
			    "<Space>", //$NON-NLS-1$
			    "<SP>", //$NON-NLS-1$
			    "<Punct>", //$NON-NLS-1$
			    "<ArgName>", //$NON-NLS-1$
			    "\"=\"", //$NON-NLS-1$
			    "<TagEnd>", //$NON-NLS-1$
			    "<ArgValue>", //$NON-NLS-1$
			    "\"\\\'\"", //$NON-NLS-1$
			    "\"\\\"\"", //$NON-NLS-1$
			    "<token of kind 18>", //$NON-NLS-1$
			    "<Quote1Text>", //$NON-NLS-1$
			    "<CloseQuote1>", //$NON-NLS-1$
			    "<Quote2Text>", //$NON-NLS-1$
			    "<CloseQuote2>", //$NON-NLS-1$
			    "<CommentText1>", //$NON-NLS-1$
			    "\"-->\"", //$NON-NLS-1$
			    "<CommentText2>", //$NON-NLS-1$
			    "\">\"", //$NON-NLS-1$
			  };
	  
	  public static String[] getTokenimage() {
		return tokenImage;
	}
	  
}
