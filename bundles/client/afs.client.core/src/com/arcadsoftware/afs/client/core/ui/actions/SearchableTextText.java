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
package com.arcadsoftware.afs.client.core.ui.actions;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Text;

public class SearchableTextText implements ISearchableText{

	
	private Text text;
	public SearchableTextText(Text text) {
		this.text = text;
	}
	@Override
	public void setSelection(int start, int end) {
		text.setSelection(start, end);		
	}
	@Override
	public Point getSelection() {
		return text.getSelection();
	}
	@Override
	public String getText() {
		return text.getText();
	}
	@Override
	public String getSelectionText() {
		return text.getSelectionText();
	}
}
