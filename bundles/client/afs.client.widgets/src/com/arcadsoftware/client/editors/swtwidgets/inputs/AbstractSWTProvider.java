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



import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.MANDATORY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.READ_ONLY;

import org.eclipse.swt.widgets.Control;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IBeanMapContainer;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractSWTProvider implements IInputSWTProvider {

	
	public void create(ISWTRenderer renderer, ILayoutParameters parameters,
			Element element, MetaDataEntity structure) {
		IBeanMapContainer container = createContainer(renderer,parameters,element,structure);
		doAfterCreating(container,renderer,parameters,element,structure);
	}

	
	public void dispose() {
	}
	
	public void configureWidget(Control widget,ISWTRenderer renderer, ILayoutParameters parameters,Element element){
		widget.setEnabled(!element.isReadonly() || !parameters.getParameterBoolean(READ_ONLY));
		if (parameters.getParameterBoolean(DEFAULT))
			widget.setFocus();		
	}

	
	
	public abstract IBeanMapContainer createContainer(ISWTRenderer renderer, ILayoutParameters parameters,	
			Element element, MetaDataEntity structure);

	public void doAfterCreating(IBeanMapContainer container,ISWTRenderer renderer, ILayoutParameters parameters,
			Element element, MetaDataEntity structure){
		//renderer.getToolkit().paintBordersFor(renderer.getParent());		
		if (parameters.getParameterBoolean(MANDATORY))
			renderer.addMandatoryAttribute(element.getCode());
		renderer.getRendererBinding().bindElement(element, container);		
	}
	
}
