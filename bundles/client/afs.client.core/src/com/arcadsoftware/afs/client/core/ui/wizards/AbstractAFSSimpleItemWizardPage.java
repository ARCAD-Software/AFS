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
package com.arcadsoftware.afs.client.core.ui.wizards;

import org.eclipse.jface.resource.ImageDescriptor;

import com.arcadsoftware.aev.core.model.ArcadEntity;
import com.arcadsoftware.aev.core.ui.wizards.AbstractSimpleItemWizardPage;

public abstract class AbstractAFSSimpleItemWizardPage extends
		AbstractSimpleItemWizardPage {

	public AbstractAFSSimpleItemWizardPage(String pageName, String title,
			ImageDescriptor titleImage) {
		super(pageName, title, titleImage);
	}

	public AbstractAFSSimpleItemWizardPage(String pageName, String title,
			String description, ArcadEntity item) {
		super(pageName, title, description, item);
	}

	public AbstractAFSSimpleItemWizardPage(String pageName, String title,
			String description) {
		super(pageName, title, description);
	}

	public AbstractAFSSimpleItemWizardPage(String pageName) {
		super(pageName);
	}

	@Override
	public void makePageData() {
		
	}
	
}
