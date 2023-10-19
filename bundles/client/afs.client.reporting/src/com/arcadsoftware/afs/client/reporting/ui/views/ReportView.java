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
package com.arcadsoftware.afs.client.reporting.ui.views;

import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.arcadsoftware.aev.core.messages.MessageManager;
import com.arcadsoftware.aev.core.tools.XMLTools;
import com.arcadsoftware.aev.core.ui.viewers.columned.ColumnedActionSeparator;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.AFSFormatTools;
import com.arcadsoftware.afs.client.core.ui.composites.BasicResultNotAllowedComposite;
import com.arcadsoftware.afs.client.core.ui.views.AbstractConnectedView;
import com.arcadsoftware.afs.client.reporting.Activator;
import com.arcadsoftware.afs.client.reporting.core.ReportHeader;
import com.arcadsoftware.afs.client.reporting.core.ReportHeaders;
import com.arcadsoftware.afs.client.reporting.ui.actions.ReportGenerateAsHTMLAction;
import com.arcadsoftware.afs.client.reporting.ui.actions.ReportGenerateAsPdfAction;
import com.arcadsoftware.afs.client.reporting.ui.viewers.ReportHeaderViewer;

public class ReportView extends AbstractConnectedView {

	public static final String XMLNODE_CATEGORY = "category"; //$NON-NLS-1$
	public static final String XMLNODE_DESCRIPTION = "description"; //$NON-NLS-1$
	public static final String XMLNODE_NAME = "name"; //$NON-NLS-1$
	public static final String XMLNODE_REPORT = "report"; //$NON-NLS-1$
	public static final String XMLNODE_URL = "url"; //$NON-NLS-1$
	public static final String XMLNODE_VERSION = "version"; //$NON-NLS-1$

	private ReportGenerateAsPdfAction generatePDFAction;
	private ReportHeaders headers;
	private Composite mainComposite;
	private Composite parentComposite;
	private ReportGenerateAsHTMLAction previewAction;
	private Action refreshAction;
	private ReportHeaderViewer viewer;

	public ReportView() {
		super();
	}

	@Override
	protected void connectionChanged(final ServerConnection connection) {
		createContent(parentComposite);
		parentComposite.layout();
		makeActions();
		loadReports();
		setInterface();
		hookContextMenu();
		if (viewer != null) {
			viewer.setInput(headers);
		}
	}

	private void createContent(final Composite parent) {
		if (mainComposite != null) {
			mainComposite.dispose();
		}
		mainComposite = AFSFormatTools.createComposite(parent, 1, false);
		if (parent.getLayout() instanceof GridLayout) {
			final GridData gridData = new GridData(GridData.FILL_BOTH);
			gridData.grabExcessHorizontalSpace = true;
			gridData.grabExcessVerticalSpace = true;
			mainComposite.setLayoutData(gridData);
		}
		final GridLayout gd = (GridLayout) mainComposite.getLayout();
		gd.marginLeft = gd.marginTop = gd.marginRight = gd.marginBottom = 1;
		gd.marginHeight = gd.marginWidth = 0;
		if (isAllowed()) {
			viewer = new ReportHeaderViewer(mainComposite, SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL) {
				@Override
				protected void doOnDoubleClick(final IStructuredSelection selection) {
					// Use HTML since PDF does not offer as many features as HTML for report View (eg: links,
					// tooltips,...)
					previewAction.run();
				}

				@Override
				protected Action[] makeActions() {
					final Action[] superActions = super.makeActions();
					final Action[] additionalActions = ReportView.this.getViewerActions();
					final Action[] actions = new Action[superActions.length + additionalActions.length];
					System.arraycopy(superActions, 0, actions, 0, superActions.length);
					// Ajout de l'action d'affichage de l'éditeur de tris
					for (int i = 0; i < additionalActions.length; i++) {
						actions[superActions.length + i] = additionalActions[i];
					}
					return actions;
				}
			};
			viewer.getViewer().getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
			viewer.setInput(headers);
		} else {
			new BasicResultNotAllowedComposite(mainComposite);
		}
	}

	@Override
	public void createPartControl(final Composite parent) {
		parentComposite = parent;
	}

	@Override
	protected void defineLocalToolbar(final IToolBarManager manager) {
		if (manager.find("refresh") == null) {
			manager.add(refreshAction);
		}
	}

	private Element getElement(final Node node, final String element) {
		final NodeList children = node.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			final Node child = children.item(i);
			if (child instanceof Element && child.getNodeName().equalsIgnoreCase(element)) {
				return (Element) child;
			}
		}
		return null;
	}

	protected List<Integer> getExecutionRights() {
		return null;
	}

	private ReportHeader getSelectedReportHeader() {
		if (viewer.getSelected() instanceof ReportHeader) {
			return viewer.getSelected();
		}
		return null;
	}

	public Action[] getViewerActions() {
		final Action[] actions = new Action[3];
		actions[0] = new ColumnedActionSeparator();
		actions[1] = generatePDFAction;
		actions[2] = previewAction;
		return actions;
	}

	protected boolean isAllowed() {
		return true;
	}

	private String loadData() {
		final String xml = getHelper().getXml(Activator.XMLLIST_URL);
		return xml;
	}

	private void loadReports() {
		final String xmlList = loadData();
		if (xmlList != null) {
			try {
				headers = new ReportHeaders();
				final Document doc = XMLTools.loadXMLFromString(xmlList);
				final NodeList reportNodes = doc.getElementsByTagName(XMLNODE_REPORT);
				for (int i = 0; i < reportNodes.getLength(); i++) {
					final Element instElement = (Element) reportNodes.item(i);
					final String name;
					final String url;
					final String description;
					final String category;
					final String version;
					final Element nameElement = getElement(instElement, XMLNODE_NAME);
					if (nameElement == null) {
						name = ""; //$NON-NLS-1$
					} else {
						name = nameElement.getTextContent();
					}
					final Element urlElement = getElement(instElement, XMLNODE_URL);
					if (urlElement == null) {
						url = ""; //$NON-NLS-1$
					} else {
						url = urlElement.getTextContent();
					}
					final Element descriptionElement = getElement(instElement, XMLNODE_DESCRIPTION);
					if (descriptionElement == null) {
						description = ""; //$NON-NLS-1$
					} else {
						description = descriptionElement.getTextContent(); //$NON-NLS-1$
					}
					final Element categoryElement = getElement(instElement, XMLNODE_CATEGORY);
					if (categoryElement == null) {
						category = ""; //$NON-NLS-1$
					} else {
						category = categoryElement.getTextContent();
					}
					final Element versionElement = getElement(instElement, XMLNODE_VERSION);
					if ((nameElement == null) || (versionElement == null)) {
						version = ""; //$NON-NLS-1$
					} else {
						version = versionElement.getTextContent();
					}
					final ReportHeader header = new ReportHeader(url, name, description, category, version);
					headers.add(header);
				}
				headers.sort();
				headers.setLevels();
			} catch (final Exception e) {
				MessageManager.addException(e, MessageManager.LEVEL_PRODUCTION);
			}
		}
	}

	private void makeActions() {
		if (previewAction == null) {
			previewAction = new ReportGenerateAsHTMLAction(getConnection()) {
				@Override
				public List<Integer> getExpectedRigths() {
					return getExecutionRights();
				}

				@Override
				protected ReportHeader getReport() {
					return getSelectedReportHeader();
				}
			};
		}
		if (generatePDFAction == null) {
			generatePDFAction = new ReportGenerateAsPdfAction(getConnection()) {
				@Override
				public List<Integer> getExpectedRigths() {
					return getExecutionRights();
				}

				@Override
				protected ReportHeader getReport() {
					return getSelectedReportHeader();
				}
			};
		}
		if (refreshAction == null) {
			refreshAction = new Action() {
				@Override
				public void run() {
					if (isAllowed()) {
						loadReports();
						viewer.setInput(headers);
					}
				}

			};
			refreshAction.setId("refresh");
			refreshAction.setText(Activator.resString("report.action.refresh.text")); //$NON-NLS-1$
			refreshAction.setToolTipText(Activator.resString("report.action.refresh.tooltip")); //$NON-NLS-1$
			refreshAction.setImageDescriptor(AFSIcon.REFRESH.imageDescriptor());
		}
	}
}
