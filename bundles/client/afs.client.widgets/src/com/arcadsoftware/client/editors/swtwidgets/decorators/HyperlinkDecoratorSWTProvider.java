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
package com.arcadsoftware.client.editors.swtwidgets.decorators;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.URL;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.widgets.Hyperlink;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Hyperlink Decorator SWT Widget provider for the
 * dynamic editors.
 */
public class HyperlinkDecoratorSWTProvider implements IDecoratorSWTProvider {

	private Hyperlink hyperlink;

	public Widget create(ISWTRenderer renderer, ILayoutParameters parameters, MetaDataEntity structure) {
		hyperlink = renderer.getToolkit().createHyperlink(renderer.getParent(),
				parameters.getParameter(URL), SWT.NONE);
		if (renderer.getParent().getLayout() instanceof GridLayout)
			hyperlink.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false, 3, 1));
		return hyperlink;
	}

	public void dispose() {
		if (hyperlink != null && !hyperlink.isDisposed())
			hyperlink.dispose();
	}

}
