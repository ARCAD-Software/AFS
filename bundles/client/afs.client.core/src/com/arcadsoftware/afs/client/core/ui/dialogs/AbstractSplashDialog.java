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
package com.arcadsoftware.afs.client.core.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.aev.core.ui.dialogs.ArcadDialog;
import com.arcadsoftware.afs.client.brands.AFSIcon;

public abstract class AbstractSplashDialog extends ArcadDialog {

	private boolean centered = true;

	public AbstractSplashDialog(Shell parentShell) {
		this(parentShell, false, false, true);
	}

	public AbstractSplashDialog(Shell parentShell, boolean OkButtonOnly, boolean resizable, boolean centered) {
		super(parentShell, OkButtonOnly);
		if (resizable) {
			int style = getShellStyle();
			style = style | SWT.RESIZE | SWT.MAX;
			setShellStyle(style);
		}
		this.centered = true;
	}

	@Override
	protected Control createButtonBar(Composite arg0) {
		final Composite composite = (Composite) super.createButtonBar(arg0);
		final GridLayout l = (GridLayout) composite.getLayout();
		l.marginHeight = l.marginWidth = 3;
		return composite;
	}

	@Override
	protected void configureShell(Shell newShell) {
		final Point size = getSize();
		final int width = size.x;
		final int height = size.y;
		super.configureShell(newShell);
		newShell.setSize(size);
		newShell.setText(getTitle());
		if (centered) {
			final Rectangle parentBounds = newShell.getDisplay().getPrimaryMonitor().getBounds();
			final int x = parentBounds.x + ((parentBounds.width - width) / 2);
			final int y = parentBounds.y + ((parentBounds.height - height) / 2);
			newShell.setLocation(x, y);
		}
		newShell.setImage(getImage());
	}

	public Image getImage() {
		return AFSIcon.ARCAD.image();
	}

	public abstract Point getSize();

	public abstract String getTitle();

}
