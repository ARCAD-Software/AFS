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
package com.arcadsoftware.afs.client.core.ui.widgets;


import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.widgets.Hyperlink;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.actions.EditorActionFactory;
import com.arcadsoftware.editor.swt.actions.IEditorAction;
import com.arcadsoftware.metadata.MetaDataEntity;

public class HyperlinkActionDecoratorSWTProvider extends AbstractHyperlinkConnectedDecoratorSWTProvider {

	private final static String URLTEXT = "urltext";
	
	IEditorAction action = null;
	
	@Override
	protected void processLinkActivated(BeanMap bean) {
		if (action != null){
			action.run();
		}
	}

	@Override
	public Widget createContent(ISWTRenderer renderer, ILayoutParameters parameters, MetaDataEntity structure) {
		return super.createContent(renderer, parameters, structure);
		
	}

	@Override
	protected String getHyperLinkText(ILayoutParameters parameters, BeanMap bean){
		String text = renderer.getLocalizedMessage(parameters.getParameter(URLTEXT, "")); //$NON-NLS-1$
		return text;
	}
	
	@Override
	protected Hyperlink createHyperlink(ILayoutParameters parameters, BeanMap bean){
		// get Action
		String actionId = parameters.getParameter(IConstants.ACTION, "");
		if (actionId != null && !actionId.isEmpty()){
			action = EditorActionFactory.getEditorAction(actionId);
			
			if (action != null) {
				action.setBeanMapSelector(renderer);
				action.setRenderer(renderer);
			}
		}
		
		return super.createHyperlink(parameters, bean);
	}

	@Override
	protected BeanMap retrieveRelatedBeanMap(ISWTRenderer renderer, ILayoutParameters parameters,
			MetaDataEntity structure) {
		return null;
	}
}
