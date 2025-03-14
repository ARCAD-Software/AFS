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
package com.arcadsoftware.client.editors.swtwidgets.decorators;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.URL;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Image Decorator SWT Widget provider for the dynamic editors.
 */
public class ImageDecoratorSWTProvider implements IDecoratorSWTProvider {

	private Image image;

	@Override
	public Widget create(ISWTRenderer renderer, final ILayoutParameters parameters, MetaDataEntity structure) {
		final Label label = new Label(renderer.getParent(), SWT.NONE);
		final ImageDescriptor imageDescriptor = renderer.getImageDescriptor(parameters.getParameter(URL));
		if (imageDescriptor != null) {
			image = imageDescriptor.createImage();
			label.setImage(image);
		}
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			label.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false, 3, 1));
		}
		return label;
	}

	@Override
	public void dispose() {
		if ((image != null) && !image.isDisposed()) {
			image.dispose();
		}
	}

}
