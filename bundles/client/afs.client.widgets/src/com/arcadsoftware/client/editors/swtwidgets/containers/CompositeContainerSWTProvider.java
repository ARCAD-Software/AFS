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
package com.arcadsoftware.client.editors.swtwidgets.containers;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.BORDER;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.COLS;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.FILL_BOTH;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.SCROLL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.WITH_MARGIN;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IContainerSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Composite Container SWT Widget provider for the
 * dynamic editors.
 */
public class CompositeContainerSWTProvider implements IContainerSWTProvider {

	public void create(ISWTRenderer renderer, ILayoutParameters parameters, boolean isEmpty, MetaDataEntity structure) {

		boolean withBorder = parameters.getParameterBoolean(BORDER);
		int style = SWT.NONE;
		if (withBorder) {
			style = SWT.BORDER;
		}
		Composite composite;
		GridLayout layout = new GridLayout(parameters.getParameterInteger(COLS, 3), false);
		if (!parameters.getParameterBoolean(WITH_MARGIN)) {
			layout.marginBottom = 0;
			layout.marginHeight = 0;
			layout.marginLeft = 0;
			layout.marginRight = 0;
			layout.marginTop = 0;
			layout.marginWidth = 0;
		}
		if (parameters.getParameterBoolean(SCROLL)) {
			int scrolledStyle = SWT.V_SCROLL | SWT.H_SCROLL;
			if (withBorder) {
				scrolledStyle = scrolledStyle | SWT.BORDER;
			}
			final ScrolledComposite body2 = new ScrolledComposite(renderer.getParent(), scrolledStyle);
			body2.setExpandHorizontal(true);
			body2.setExpandVertical(true);

			if (renderer.getParent().getLayout() instanceof GridLayout) {
				if (parameters.getParameterBoolean(FILL_BOTH)) {
					body2.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
				} else {
					body2.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
				}
			}
			composite = renderer.getToolkit().createComposite(body2, style);
			final Composite body = composite;
			body.setLayout(layout);
			//body.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
			body2.setContent(body);
			body2.addControlListener(new ControlListener() {
				public void controlMoved(ControlEvent e) {
					// Do nothing
				}

				public void controlResized(ControlEvent e) {
					body2.setMinSize(body.computeSize(SWT.DEFAULT, getTableHeight()));
				}
			});
			// renderer.getToolkit().adapt(body2);
			renderer.createSubContainer(this, parameters, body);
		} else {
			composite = renderer.getToolkit().createComposite(renderer.getParent(), style);
			if (renderer.getParent().getLayout() instanceof GridLayout) {
				composite.setLayoutData(ContainerUtils.createGridData(parameters));
			}	
			composite.setLayout(layout);
			renderer.createSubContainer(this, parameters, composite);
		}
	}

	public void dispose() {
		// Do nothing
	}
	
	protected int getTableHeight(){
		return SWT.DEFAULT;
	}

}
