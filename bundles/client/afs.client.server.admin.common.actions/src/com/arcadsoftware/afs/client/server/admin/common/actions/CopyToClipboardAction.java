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
package com.arcadsoftware.afs.client.server.admin.common.actions;

import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.widgets.Display;

import com.arcadsoftware.afs.client.server.admin.common.ui.actions.AbstractCopyToClipboardAction;

public class CopyToClipboardAction extends AbstractCopyToClipboardAction {

	public CopyToClipboardAction(){
		super();
	}
	
	@Override
	public void run() {
		String content = getContent();
		Clipboard clipBoard = new Clipboard(Display.getCurrent());
        TextTransfer transfer = TextTransfer.getInstance();
        clipBoard.setContents(new Object[] { content }, new Transfer[] {transfer});
	}
		
}
