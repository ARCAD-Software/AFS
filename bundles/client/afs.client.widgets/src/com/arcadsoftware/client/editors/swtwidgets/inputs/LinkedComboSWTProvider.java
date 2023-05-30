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

//import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LINKED;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LINK_CODE;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Combo;

import com.arcadsoftware.client.editors.swtwidgets.widgets.BeanMapLinkedCombo;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IBeanMapContainer;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Combo SWT Widget provider for the dynamic editors.
 */
//public class LinkedComboSWTProvider implements IInputSWTProvider {
public class LinkedComboSWTProvider extends AbstractSWTProvider {

//	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element,
//			MetaDataEntity structure) {
//		String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
//		if (label.length() > 0) {
//			renderer.getToolkit().createLabel(renderer.getParent(), label);
//			renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
//		}
//		int horizontalSpan = (label.length() > 0) ? 1 : 3;
//		BeanMapLinkedCombo combo = new BeanMapLinkedCombo(renderer.getParent(), SWT.READ_ONLY, parameters
//				.getParameter(LINK_CODE), structure.getAttribute(parameters.getParameter(LINKED)), parameters,
//				renderer, element, horizontalSpan);
//		Combo widget = (Combo) combo.getWidget();
//		widget.setEnabled(!element.isReadOnly() || !parameters.getParameterBoolean(READ_ONLY));
//		if (parameters.getParameterBoolean(DEFAULT))
//			widget.setFocus();
//		//TODO RAP
//		//renderer.getToolkit().paintBordersFor(renderer.getParent());		
//
//		if (parameters.getParameterBoolean(MANDATORY))
//			renderer.addMandatoryAttribute(element.getCode());
//		renderer.getRendererBinding().bindElement(element, combo);
//	}

//	public void dispose() {
//		// Do nothing
//	}

	@Override
	public IBeanMapContainer createContainer(ISWTRenderer renderer,
			ILayoutParameters parameters, Element element,
			MetaDataEntity structure) {
		String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		if (label.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
		}
		int horizontalSpan = (label.length() > 0) ? 1 : 3;
		BeanMapLinkedCombo combo = new BeanMapLinkedCombo(renderer.getParent(), SWT.READ_ONLY, parameters
				.getParameter(LINK_CODE), structure.getAttribute(parameters.getParameter(LINKED)), parameters,
				renderer, element, horizontalSpan);
		Combo widget = (Combo) combo.getWidget();
		configureWidget(widget, renderer, parameters, element);
		return combo;
	}

}
