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
package com.arcadsoftware.afs.client.core.ui.composites;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.arcadsoftware.afs.client.core.internal.Activator;

/**
 * This composite shows only a label which indicates user not have rights.
 */
public class BasicResultNotAllowedComposite extends Composite {

	/**
	 * @param parent
	 *            the parent composite.
	 */
	public BasicResultNotAllowedComposite(Composite parent) {
		super(parent, SWT.NONE);
		setLayout(new FillLayout());
		new Label(this, SWT.NONE).setText(Activator.resString("search.label.notAllowed")); //$NON-NLS-1$
	}

}
