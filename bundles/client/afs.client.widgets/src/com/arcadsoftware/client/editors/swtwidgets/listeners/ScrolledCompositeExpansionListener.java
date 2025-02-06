/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
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
package com.arcadsoftware.client.editors.swtwidgets.listeners;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.events.ExpansionEvent;
import org.eclipse.ui.forms.events.IExpansionListener;

/**
 * This class will be notified before and after the expandable control's expansion state changes. When the expansion
 * state changed, the first scrolled composite parent is refreshed.
 */
public class ScrolledCompositeExpansionListener implements IExpansionListener {

	@Override
	public void expansionStateChanged(ExpansionEvent e) {
		Composite c = ((Control) e.getSource()).getParent();
		while (c != null) {
			c.layout(true);
			c = c.getParent();
			if (c instanceof ScrolledComposite) {
				((ScrolledComposite) c).notifyListeners(SWT.Resize, null);
				break;
			}
		}
	}

	@Override
	public void expansionStateChanging(ExpansionEvent e) {
		// Do nothing
	}
}
