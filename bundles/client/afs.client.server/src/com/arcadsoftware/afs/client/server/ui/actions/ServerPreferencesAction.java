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
package com.arcadsoftware.afs.client.server.ui.actions;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.PropertyDialogAction;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.AFSRightManager;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.actions.ISecuredAction;
import com.arcadsoftware.afs.client.server.connection.ConnectionManager;
import com.arcadsoftware.afs.client.server.internals.Activator;

public abstract class ServerPreferencesAction extends AbstractServerAction implements ISecuredAction {

	private static final String SERVER_ACTION_PREFERENCES_TOOLTIP = "server.action.preferences.tooltip";
	private static final String SERVER_ACTION_PREFERENCES_TEXT = "server.action.preferences.text";
	private Shell shell;
	
	@Override
	protected void setInterface() {
		setText(Activator.resString(SERVER_ACTION_PREFERENCES_TEXT)); //$NON-NLS-1$
		setToolTipText(Activator.resString(SERVER_ACTION_PREFERENCES_TOOLTIP));//$NON-NLS-1$
		setImageDescriptor(AFSIcon.SERVER_PREFERENCES.imageDescriptor());
	}
	
	protected ServerConnection retrieveConnection(){
		ServerConnection connection  = getServerConnection();
		if (connection==null) {
			connection = ConnectionManager.getInstance().connect(getServerToManage(),true);	
			setServerConnection(connection);
		}
		return connection;
	}
	
	@Override
	protected boolean execute() {
		final ServerConnection connection  = retrieveConnection();	
		if (!isAllowed()) {
			AFSRightManager.getRightManager().missingRight(getExpectedRigths());
			return false;
		}
		if ((connection != null) && connection.isConnected()) {
			PropertyDialogAction pda = new PropertyDialogAction(() -> shell, new ISelectionProvider() {
				private ArrayList<ISelectionChangedListener> listeners = new ArrayList<>();
				private ISelection selection;
				public void setSelection(ISelection selection) {
					this.selection = selection;
					SelectionChangedEvent event = new SelectionChangedEvent(this, selection);
					for(ISelectionChangedListener listener:listeners) {
						listener.selectionChanged(event);
					}
				}
				public void removeSelectionChangedListener(ISelectionChangedListener listener) {
					listeners.remove(listener);
				}
				public ISelection getSelection() {
					return selection;
				}
				public void addSelectionChangedListener(ISelectionChangedListener listener) {
					listeners.add(listener);
				}
			});
			pda.getSelectionProvider().setSelection(new IStructuredSelection() {
				public boolean isEmpty() {
					return false;
				}
				public List<ServerConnection> toList() {
					ArrayList<ServerConnection> result = new ArrayList<>(1);
					result.add(connection);
					return result;
				}
				public Object[] toArray() {
					return toList().toArray();
				}
				public int size() {
					return 1;
				}
				public Iterator<ServerConnection> iterator() {
					return new Iterator<ServerConnection>() {
						private boolean first = true;
						public boolean hasNext() {
							return first;
						}
						public ServerConnection next() {
							if (first) {
								first = false;
								return connection;
							} else {
								throw new NoSuchElementException();
							}
						}
						@Override
						public void remove() {
							//nothing to remove
						}
					};
				}
				public Object getFirstElement() {
					return connection;
				}
			});
			pda.setText(Activator.resString(SERVER_ACTION_PREFERENCES_TEXT)); //$NON-NLS-1$
			pda.setToolTipText(Activator.resString(SERVER_ACTION_PREFERENCES_TOOLTIP));//$NON-NLS-1$
			pda.setImageDescriptor(AFSIcon.PREFERENCES.imageDescriptor());
			// Open dialog
			PreferenceDialog dialog = pda.createDialog();
			dialog.getShell().setText(Activator.resString(SERVER_ACTION_PREFERENCES_TEXT));
			dialog.getShell().setImage(AFSIcon.PREFERENCES.image());
			dialog.open();			
			return true;
		}
		return false;
	}
	
	protected abstract ServerConnection getServerConnection();
	protected abstract void setServerConnection(ServerConnection connection);
	
	public ServerPreferencesAction() {
		setText(Activator.resString(SERVER_ACTION_PREFERENCES_TEXT)); //$NON-NLS-1$
		setToolTipText(Activator.resString(SERVER_ACTION_PREFERENCES_TOOLTIP));//$NON-NLS-1$
		setImageDescriptor(AFSIcon.PREFERENCES.imageDescriptor());
	}

	public Shell getShell() {
		return shell;
	}

	public void setShell(Shell shell) {
		this.shell = shell;
	}
		
	@Override
	public boolean isAllowed() {
		return getServerConnection().isAllowed(getExpectedRigths());
	}
	
}
