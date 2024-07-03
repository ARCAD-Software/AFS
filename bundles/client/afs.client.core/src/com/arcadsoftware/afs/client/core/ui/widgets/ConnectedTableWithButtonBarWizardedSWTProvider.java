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
package com.arcadsoftware.afs.client.core.ui.widgets;

import java.util.List;

import org.osgi.framework.Bundle;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.actions.wizarded.AbstractConnectedWizardedAddAction;
import com.arcadsoftware.afs.client.core.ui.wizards.GenericConnectedWizardPage;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.messages.UserMessageManager;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;

public class ConnectedTableWithButtonBarWizardedSWTProvider extends AbstractConnectedTableWithButtonBarSWTProvider {

	protected MetaDataEntity structure;
	private GenericConnectedWizardPage mainPage;
	private AbstractConnectedWizardedAddAction addAction;
	private ILayoutParameters layoutParameters;

	@Override
	public void create(ISWTRenderer swtRenderer,
			ILayoutParameters layoutParameters, Element entityElement,
			MetaDataEntity structure) {
		super.create(swtRenderer, layoutParameters, entityElement, structure);
		this.structure = structure;
		this.layoutParameters = layoutParameters;
	}

	private AbstractConnectedWizardedAddAction createAddAction(ILayoutParameters layoutParameters,
			final String entityType) {
		final AbstractConnectedWizardedAddAction addAction = new AbstractConnectedWizardedAddAction(getConnection()) {

			@Override
			public List<Integer> getExpectedRigths() {
				return getExpectedAddRight();
			}

			@Override
			public String getType() {
				return entityType;
			}

			@Override
			public Bundle getBundle() {
				return Activator.getDefault().getBundle();
			}

			@Override
			public UserMessage getErrorMessage() {
				return ConnectedTableWithButtonBarWizardedSWTProvider.this.getErrorMessage();
			}

			@Override
			public void initBeanMap(BeanMap beanmap) {
				ConnectedTableWithButtonBarWizardedSWTProvider.this.initBeanMap(beanmap);
			}

			@Override
			public void addConnectedWizardPages(ServerConnection connexion) {
				mainPage = new GenericConnectedWizardPage(connexion,
						"main", //$NON-NLS-1$
						getWizardpageTitle(),
						getWizardpageDescription(),
						getType(),
						getlayoutName());
				addConnectedWizardPage(mainPage);
			}

			@Override
			public String getWizardTitle() {
				return ConnectedTableWithButtonBarWizardedSWTProvider.this.getWizardTitle();
			}

		};
		return addAction;
	}

	protected UserMessage getErrorMessage() {
		return UserMessageManager.getInstance().getMessage(getErrorCode());
	}

	@Override
	protected void createBeanMap(final MetaDataLink link, final boolean withOpenEditor) {
		if (creationAllowed()) {
			addAction = createAddAction(layoutParameters, link.getType());
			addAction.run();
			if (addAction.isRunOk()) {
				final BeanMap beanMap = addAction.getCurrentBeanMap();
				renderer.addLinkitem(link, beanMap);
				getList().setBeanMapValue(beanMap);
				doAfterCreating(beanMap);
				final String so = getSortOrder();
				if (so != null) {
					getList().sort(so);
				}
			}
		} else {
			missingRight(getExpectedAddRight());
		}
	}

	@Override
	public List<Integer> getExpectedEditRight() {
		return null;
	}

	@Override
	public List<Integer> getExpectedAddRight() {
		return null;
	}

	@Override
	public List<Integer> getExpectedDeleteRight() {
		return null;
	}

	@Override
	public void missingRight(List<Integer> expected) {}

	protected String getErrorCode() {
		return layoutParameters.getParameter("errorcode");
	}

	protected String getlayoutName() {
		return layoutParameters.getParameter("layoutname");
	}

	protected String getWizardTitle() {
		final String value = layoutParameters.getParameter("wizardtitle");
		return renderer.getLocalizedMessage(value);
	}

	protected String getSortOrder() {
		return layoutParameters.getParameter("sortorder");
	}

	protected String getWizardpageTitle() {
		final String value = layoutParameters.getParameter("wizardpagetitle");
		return renderer.getLocalizedMessage(value);
	}

	protected String getWizardpageDescription() {
		final String value = layoutParameters.getParameter("wizardpagedescription");
		return renderer.getLocalizedMessage(value);
	}

	protected void initBeanMap(BeanMap beanmap) {
		if (layoutParameters.getParameterBoolean("autolink")) {
			if (element.getMetadata() != null) {
				if (element.getMetadata().contains("reverseLink")) {
					final String parentAttibute = element.getMetadata().getString("reverseLink");
					beanmap.put(parentAttibute, renderer.getSelectedBeanMap().getId());
				}
			}
		}
	}

	@Override
	protected void doRemovingLink(MetaDataLink link, BeanMap beanMap) {
		if (layoutParameters.getParameterBoolean("autolink")) {
			getHelper().remove(beanMap);
			super.doRemovingLink(link, beanMap);
		}
	}

	@Override
	protected boolean doAfterRemovingLink() {
		if (layoutParameters.getParameterBoolean("autolink")) {
			renderer.save();
			renderer.refreshAllEditors(renderer.getCurrentBean(), renderer);
			return super.doAfterRemovingLink();
		}
		return super.doAfterRemovingLink();
	}

	private void doAfterCreating(BeanMap beanMap) {
		if (layoutParameters.getParameterBoolean("autolink")) {
			renderer.save();
			renderer.refreshAllEditors(renderer.getCurrentBean(), renderer);
		}
	}
}
