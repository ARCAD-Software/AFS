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
package com.arcadsoftware.afs.client.users.ui.views;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.XMLConstants;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamReader;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.views.AbstractSecuredView;
import com.arcadsoftware.afs.client.users.AuthType;
import com.arcadsoftware.afs.client.users.IUsersConsts;
import com.arcadsoftware.afs.client.users.LDAPAccessHelper;
import com.arcadsoftware.afs.client.users.internal.Activator;
import com.arcadsoftware.afs.client.users.ui.actions.AbstractImportAction;
import com.arcadsoftware.afs.client.users.ui.actions.RefreshListAction;
import com.arcadsoftware.afs.client.users.ui.composites.ImportCandidatesCheckViewer;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

public abstract class AbstractUsersImportView extends AbstractSecuredView {

	protected Button bImport;
	protected Text filterNameText;
	protected AbstractImportAction importAction;
	protected Composite parentComposite;
	protected RefreshListAction refreshAction;
	private ImportCandidatesCheckViewer candidates;
	private AuthType selectedAuth;
	private boolean defaultSelected;

	public AbstractUsersImportView() {
		super();
		defaultSelected = true;
	}

	/**
	 * Complete User bean
	 *
	 * @param user
	 */
	protected void completeUserForImport(final BeanMap user) {
	}

	@Override
	protected void connectionChanged(final ServerConnection connection) {
		createContent(parentComposite);
		parentComposite.layout();
	}

	private void createBeforeContent(final Composite parent) {
		final Composite optionsComposite = new Composite(parent, SWT.NONE);
		optionsComposite.setLayoutData(GridDataFactory.fillDefaults().grab(true, false).span(3, 1).create());
		optionsComposite.setLayout(new GridLayout(2, false));
		final List<AuthType> possibleImports = new ArrayList<>();
		try {
			final String xml = getConnection().getDataAccess().getWebServicesAccess().get("authservices");
			final XMLInputFactory factory = XMLInputFactory.newFactory();
			factory.setProperty(XMLConstants.ACCESS_EXTERNAL_DTD, ""); // Compliant
			factory.setProperty(XMLConstants.ACCESS_EXTERNAL_SCHEMA, ""); // compliant
			final XMLStreamReader reader = factory.createXMLStreamReader(new StringReader(xml));
			while (reader.hasNext()) {
				if ((reader.next() == XMLStreamConstants.START_ELEMENT)
						&& reader.getName().getLocalPart().equals("string")) {
					final String text = reader.getElementText();
					if (text != null) {
						final AuthType at = AuthType.fromCode(text);
						if ((at != null) && at.importAllowed()) {
							possibleImports.add(at);
						}
					}
				}
			}
		} catch (final Exception e) {
			Activator.getDefault().error(
					"Error during loading of Authentication mode available for user import: " + e.getMessage(), e);
		}
		final Group sourceGroup = GuiFormatTools.createGroup(optionsComposite,
				Activator.resString("user.action.import.source.group.label"), possibleImports.size());
		sourceGroup.setLayoutData(GridDataFactory.fillDefaults().grab(false, false).create());
		for (final AuthType auth : possibleImports) {
			createAuthButton(sourceGroup, auth);
		}
		extendOptionsComposite(optionsComposite);
	}

	protected void extendOptionsComposite(final Composite optionsComposite) {
	}

	@SuppressWarnings("incomplete-switch")
	private void createAuthButton(final Group sourceGroup, final AuthType auth) {
		final Button result = new Button(sourceGroup, SWT.RADIO);
		switch (auth) {
		case IBMI:
			result.setText(Activator.resString("user.action.import.type.ibmi"));
			break;
		case LDAP:
			result.setText(Activator.resString("user.action.import.type.ldap"));
			break;
		}
		result.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectedAuth = auth;
			}
		});
		if (defaultSelected) {
			result.setSelection(true);
			selectedAuth = auth;
		}
		defaultSelected = false;
	}

	protected void createButtonBar(final Composite parent) {
		bImport = new Button(parent, SWT.PUSH);
		bImport.setText(Activator.resString("users.import.action.text")); //$NON-NLS-1$
		bImport.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent arg0) {
				importAction.run();
			}
		});
		final GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
		gridData.horizontalSpan = 3;
		bImport.setLayoutData(gridData);
	}

	/**
	 * Create Composite for Candidates Viewer
	 *
	 * @param parent
	 * @return
	 */
	protected ImportCandidatesCheckViewer createCandidatesViewer(final Composite parent) {
		// create view
		final GridLayout layout = new GridLayout(3, false);
		parent.setLayout(layout);
		createBeforeContent(parent);
		filterNameText = GuiFormatTools.createLabelledTextWithButton(parent,
				Activator.resString("users.import.filter.label"), "", false,
				Activator.resString("users.import.search.button.label"));
		filterNameText.setText("");
		final Button bsearch = (Button) filterNameText.getData();
		bsearch.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				importUsers();
			}
		});
		final ImportCandidatesCheckViewer viewer = ImportCandidatesCheckViewer.newCheckList(parent);
		viewer.setLabelProvider(createSpecificLabelProvider());
		viewer.setContentProvider(createSpecificContentProvider());
		final GridData data = new GridData(GridData.FILL_BOTH);
		data.grabExcessVerticalSpace = true;
		data.horizontalSpan = 3;
		viewer.getControl().setLayoutData(data);
		return viewer;
	}

	/**
	 * Create Content
	 *
	 * @param parent
	 */
	protected void createContent(final Composite parent) {
		// create view
		if (isAllowed()) {
			if (candidates == null) {
				candidates = createCandidatesViewer(parent);
				initActions();
				createButtonBar(parent);
				initData();
			}
		} else {
			MessageDialog.openError(Activator.getDefault().getPluginShell(),
					Activator.resString("msg.error.users.import.title"),
					Activator.resString("msg.error.users.import.rights.text")); //$NON-NLS-1$
		}
	}

	@Override
	public void createPartControl(final Composite parent) {
		parentComposite = parent;
		super.createPartControl(parent);
	}

	protected IContentProvider createSpecificContentProvider() {
		return new IStructuredContentProvider() {

			@Override
			public void dispose() {
				// Do nothing
			}

			@Override
			public Object[] getElements(final Object inputElement) {
				if (inputElement instanceof BeanMapList) {
					return ((BeanMapList) inputElement).toArray();
				}
				return new Object[0];
			}

			@Override
			public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
				// Do nothing
			}
		};
	}

	protected IBaseLabelProvider createSpecificLabelProvider() {
		return new LabelProvider() {
			@Override
			public String getText(final Object element) {
				if (element instanceof BeanMap) {
					final BeanMap bm = (BeanMap) element;
					if (IUsersConsts.ENTITY_USER.equals(bm.getType())) {
						return getUserDefinition(bm);
					}
				}
				return "";
			}
		};
	}

	protected void doAfterImport() {
	}

	public abstract List<Integer> getExpectedImportRights();

	/**
	 * Get Import Button
	 *
	 * @return
	 */
	protected Button getImportButton() {
		return bImport;
	}

	private String getUserDefinition(final BeanMap user) {
		switch (selectedAuth) {
		case IBMI:
			return String.format("%s [%s]",
					user.getString(IUsersConsts.IBMI_IMPORT_LOGIN), user.getString("description"));
		case LDAP:
			final StringBuilder userDef = new StringBuilder();
			if (user.contains(IUsersConsts.LDAPIMPORT_LOGIN)) {
				userDef.append(user.getString(IUsersConsts.LDAPIMPORT_LOGIN) + " ");
			}
			if (user.contains(IUsersConsts.PROP_USER_FIRSTNAME)) {
				userDef.append("[" + user.getString(IUsersConsts.PROP_USER_FIRSTNAME) + "] ");
			}

			if (user.contains(IUsersConsts.PROP_USER_LASTNAME)) {
				userDef.append(user.getString(IUsersConsts.PROP_USER_LASTNAME) + " ");
			}
			if (user.contains(IUsersConsts.PROP_USER_EMAIL)) {
				userDef.append("[" + user.getString(IUsersConsts.PROP_USER_EMAIL) + "]");
			}
			return userDef.toString();
		default:
			return "";
		}
	}

	/**
	 * Complete User bean
	 *
	 * @param user
	 */
	protected abstract int[] getUserProfiles(final BeanMap user);

	/**
	 * Get Viewer
	 *
	 * @return
	 */
	protected ImportCandidatesCheckViewer getViewer() {
		return candidates;
	}

	/**
	 * Init basic actions
	 */
	protected void initActions() {
		if (importAction == null) {
			importAction = new AbstractImportAction(getConnection()) {

				@Override
				protected void completeUserBeanMap(final BeanMap user) {
					completeUserForImport(user);
				}

				@Override
				protected void doAfterRun() {
					AbstractUsersImportView.this.doAfterImport();
					if (runOk) {
						refreshAction.run();
					}
				}

				@Override
				protected void doBeforeRun() {
					super.doBeforeRun();
					AbstractUsersImportView.this.initializeSelection();
				}

				@Override
				public List<Integer> getExpectedRigths() {
					return AbstractUsersImportView.this.getExpectedImportRights();
				}

				@Override
				protected int[] getProfiles(final BeanMap user) {
					return getUserProfiles(user);
				}

				@Override
				protected Object[] getSelectedItems() {
					return candidates.getCheckedElements();
				}

				@Override
				protected AuthType getSelectedAuthType() {
					return selectedAuth;
				}
			};
		}
		if (refreshAction == null) {
			refreshAction = new RefreshListAction(getConnection()) {
				@Override
				protected void doBeforeRun() {
					candidates.clearAllCheckedElements();
				}

				@Override
				protected String getFilter() {
					return filterNameText.getText();
				}

				@Override
				protected TableViewer getViewer() {
					return candidates;
				}

				@Override
				public List<Integer> getExpectedRigths() {
					return getExpectedImportRights();
				}

				@Override
				protected boolean execute() {
					importUsers();
					return true;
				}
			};
		}
	}

	protected abstract void initData();

	protected abstract void initializeSelection();

	private void importUsers() {
		switch (selectedAuth) {
		case IBMI:
			loadIBMiProfiles();
			break;
		case LDAP:
			loadLDAPUsers();
			break;
		default:
		}
	}

	private void loadIBMiProfiles() {
		try {
			final String filter = filterNameText.getText().toUpperCase();
			final Set<String> existingProfiles = new HashSet<>();
			for (final BeanMap b : getConnection().getDataAccess().getList(AuthType.IBMI.code())) {
				existingProfiles.add(b.getString("login"));
			}
			final BeanMapList totalList = getConnection().getDataAccess().getList(
					String.format("/admin/%s/import", AuthType.IBMI.resourceSuffix()), IUsersConsts.ENTITY_USER);
			if (totalList != null) {
				final BeanMapList list = new BeanMapList(totalList.size());
				for (final BeanMap b : totalList) {
					final String login = b.getString(IUsersConsts.IBMI_IMPORT_LOGIN);
					if ((login != null) && (login.toUpperCase().contains(filter)) && existingProfiles.contains(login)) {
						list.add(b);
					}
				}
				getViewer().setInput(list);
			}
		} catch (final Exception e) {
			Activator.getDefault().error("AbstractUsersImportView::loadIBMiProfiles", e);
		}
	}

	private void loadLDAPUsers() {
		// call server
		final LDAPAccessHelper ldapHelper = new LDAPAccessHelper(getConnection());
		String filter = null;
		if (filterNameText.getText().length() > 0) {
			filter = filterNameText.getText();
		}
		candidates.setInput(ldapHelper.getImportCandidates(filter));
	}

	@Override
	public void setFocus() {
	}

}