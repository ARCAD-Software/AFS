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

import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.framework.ui.composites.AbstractAFSEditorComposite;
import com.arcadsoftware.afs.framework.ui.editors.AbstractAFSEditorPart;

public abstract class AbstractBeanMapEditorComposite extends AbstractAFSEditorComposite {

	public AbstractBeanMapEditorComposite(Composite parent, int style,
			AbstractAFSEditorPart editor) {
		super(parent, style, editor);

	}

	@Override
	public void toScreen() {

	}

	@Override
	public void fromScreen() {

	}

}
