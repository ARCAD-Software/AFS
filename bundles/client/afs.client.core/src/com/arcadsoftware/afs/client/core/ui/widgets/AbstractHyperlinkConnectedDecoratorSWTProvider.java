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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.Hyperlink;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractHyperlinkConnectedDecoratorSWTProvider extends AbstractConnectedDecoratorSWTProvider {
	
	protected Hyperlink hyperlink;
	
	@Override
	public Widget createContent(ISWTRenderer renderer, ILayoutParameters parameters, MetaDataEntity structure) {
		
		initHyperLinkLabel(renderer, parameters, "");

		final BeanMap bean = retrieveRelatedBeanMap(renderer, parameters, structure);
		hyperlink = createHyperlink(parameters, bean);
		
		return hyperlink;
	}
	/**
	 * Create HyperLink widget
	 * @param parameters
	 * @return
	 */
	protected Hyperlink createHyperlink(ILayoutParameters parameters, final BeanMap bean){
		Hyperlink hyperlink = renderer.getToolkit().createHyperlink(renderer.getParent(),
				"", SWT.NONE);
		if (renderer.getParent().getLayout() instanceof GridLayout)
			hyperlink.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false,1, 1));
		
		hyperlink.setText(getHyperLinkText(parameters, bean));
		
		hyperlink.addHyperlinkListener(				
			new HyperlinkAdapter() {
			   public void linkActivated(HyperlinkEvent e) {
				   processLinkActivated(bean);			   
			   }
			}
		);
		
		return hyperlink;
	}
	
	protected abstract BeanMap retrieveRelatedBeanMap(ISWTRenderer renderer,
			ILayoutParameters parameters, MetaDataEntity structure);
	
	/**
	 * Initialize HyperLink title label
	 * @param renderer
	 * @param parameters
	 * @param defaultLabel
	 */
	protected void initHyperLinkLabel(ISWTRenderer renderer, ILayoutParameters parameters, String defaultLabel){
		String label = renderer.getLocalizedMessage(parameters.getParameter(IConstants.LABEL,defaultLabel));
		if (label!=null) {
			if (label.length() > 0) {
				renderer.getToolkit().createLabel(renderer.getParent(), label);
				renderer.getToolkit().createLabel(renderer.getParent(), IConstants.TWO_POINTS);
			}
		}
	}
	
	@Override
	public void dispose() {
		if (hyperlink != null && !hyperlink.isDisposed())
			hyperlink.dispose();
	}
	
	/**
	 * Init Hyperlink text
	 * @param parameters
	 * @param bean
	 * @return
	 */
	protected abstract String getHyperLinkText(ILayoutParameters parameters, BeanMap bean);
		
	protected abstract void processLinkActivated(BeanMap bean);
	
}
