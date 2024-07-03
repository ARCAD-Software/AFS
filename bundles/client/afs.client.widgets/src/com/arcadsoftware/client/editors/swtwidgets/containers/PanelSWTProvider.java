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
package com.arcadsoftware.client.editors.swtwidgets.containers;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.COLS;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ICON;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.SCROLL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.WITH_MARGIN;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IContainerSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Panel Container SWT Widget provider for the dynamic editors. Panel container can be contained
 * only by Panels container
 *
 * @see PanelsSWTProvider
 */
public class PanelSWTProvider implements IContainerSWTProvider {

	private Image image;

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, boolean isEmpty, MetaDataEntity structure) {
		final String icon = parameters.getParameter(ICON);
		if ((icon != null) && (icon.length() > 0)) {
			final ImageDescriptor id = renderer.getImageDescriptor(icon);
			if (id != null) {
				image = id.createImage();
			}
		}
		final String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL));
		if (parameters.getParameterBoolean(SCROLL)) {
			final ScrolledComposite body2 = new ScrolledComposite(renderer.getParent(), SWT.BORDER | SWT.V_SCROLL
					| SWT.H_SCROLL);
			body2.setExpandHorizontal(true);
			body2.setExpandVertical(true);
			final Composite body = createBody(renderer, parameters, body2);
			createPanel(renderer, parameters, label, body, body2);
			body2.setContent(body);
			body2.addControlListener(new ControlListener() {
				@Override
				public void controlMoved(ControlEvent e) {
				}

				@Override
				public void controlResized(ControlEvent e) {
					body2.setMinSize(body.computeSize(SWT.DEFAULT, SWT.DEFAULT));
				}
			});
		} else {
			final Composite body = createBody(renderer, parameters, renderer.getParent());
			createPanel(renderer, parameters, label, body, body);
		}
	}

	private Composite createBody(ISWTRenderer renderer, ILayoutParameters parameters, Composite parent) {
		final Composite body = renderer.getToolkit().createComposite(parent, SWT.NONE);
		final GridLayout layout = new GridLayout(parameters.getParameterInteger(COLS, 3), false);
		if (!parameters.getParameterBoolean(WITH_MARGIN)) {
			layout.marginBottom = 0;
			layout.marginHeight = 0;
			layout.marginLeft = 0;
			layout.marginRight = 0;
			layout.marginTop = 0;
			layout.marginWidth = 0;
		} else {
			final int margin = parameters.getParameterInteger("margin", 2);
			layout.marginBottom = margin;
			layout.marginHeight = margin;
			layout.marginLeft = margin;
			layout.marginRight = margin;
			layout.marginTop = margin;
			layout.marginWidth = margin;
		}
		body.setLayout(layout);
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			body.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
		}
		return body;
	}

	private void createPanel(ISWTRenderer renderer, ILayoutParameters parameters, String label, Composite composite,
			Control control) {
		if (renderer.getParent() instanceof CTabFolder) {
			final CTabItem ti = new CTabItem((CTabFolder) renderer.getParent(), SWT.NONE);
			ti.setText(label);
			if (image != null) {
				ti.setImage(image);
			}
			ti.setControl(control);
			renderer.createSubContainer(this, parameters, composite);
		} else if (renderer.getParent() instanceof TabFolder) {
			final TabItem ti = new TabItem((TabFolder) renderer.getParent(), SWT.NONE);
			ti.setText(label);
			if (image != null) {
				ti.setImage(image);
			}
			ti.setControl(control);
			renderer.createSubContainer(this, parameters, composite);
		}
	}

	@Override
	public void dispose() {
		if ((image != null) && !image.isDisposed()) {
			image.dispose();
		}
	}

}
