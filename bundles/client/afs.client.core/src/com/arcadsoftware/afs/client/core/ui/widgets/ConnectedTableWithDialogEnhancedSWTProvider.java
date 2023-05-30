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


import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataLink;


public class ConnectedTableWithDialogEnhancedSWTProvider extends ConnectedTableWithDialogSWTProvider {
	
	
	@Override
	protected void doRemovingLink(MetaDataLink link, BeanMap beanMap) {
		getHelper().remove(beanMap);
		super.doRemovingLink(link, beanMap);
	}
	
	@Override
	protected boolean doAfterRemovingLink() {
		renderer.save();
		renderer.refreshAllEditors(renderer.getCurrentBean(), renderer);
		return super.doAfterRemovingLink();	
	}
	
	@Override
	protected void additionalInformation(BeanMap beanMap,MetaDataLink link){
		if (link.getMetadata()!=null) {
			if (link.getMetadata().contains("reverseLink")) {
				String parentAttibute = link.getMetadata().getString("reverseLink");
				beanMap.put(parentAttibute, renderer.getSelectedBeanMap().getId());
			}
		}
	}
	
}
