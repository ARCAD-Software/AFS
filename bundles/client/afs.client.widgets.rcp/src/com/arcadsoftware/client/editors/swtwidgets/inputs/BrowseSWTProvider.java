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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;

import com.arcadsoftware.client.editors.swtwidgets.internal.Messages;


/**
 * 
 */
public class BrowseSWTProvider extends AbstractBrowseSWTProvider {

	@Override
	protected FileInfo getFilename(Composite parent) {
		FileDialog dialog = new FileDialog(parent.getShell());
		dialog.setText(Messages.DownloadSWTProvider_FileDialogText);
		String s =dialog.open();
		if ((s==null) || (s.length()==0)){		
			return null;
		} else {
			return new FileInfo(s,dialog.getFileName());
		}
	}
	
}
