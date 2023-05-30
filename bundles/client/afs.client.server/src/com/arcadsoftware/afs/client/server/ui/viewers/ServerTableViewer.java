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
package com.arcadsoftware.afs.client.server.ui.viewers;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumn;
import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumns;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTableLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.ColumnedDefaultTableLabelProvider;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTableViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.server.internals.Activator;

public class ServerTableViewer extends AbstractColumnedTableViewer {


	public ServerTableViewer(Composite parent, int style) {
		super(parent, style);
	}

	public Server getServerFromSelection(ISelection selection) {
		IStructuredSelection sel = (IStructuredSelection)selection;
		if (!sel.isEmpty()) {
			Object o = sel.getFirstElement();
			if (o instanceof Server) {
				return (Server)o;
			}
		}
		return null;
	}
	
	public Server getSelectedServer(){
		IStructuredSelection selection = getSelection();
		if (!selection.isEmpty()) {
			Object o = selection.getFirstElement();
			if (o instanceof Server) {
				return (Server)o;
			}
		}
		return null;
	}
	
	@Override
	public AbstractColumnedTableLabelProvider createTableLabelProvider(
			AbstractColumnedViewer viewer) {
		return new ColumnedDefaultTableLabelProvider(viewer){
			@Override
			protected Image getActualImage(Object element, int actualColumnIndex) {
				if (actualColumnIndex==0) {
					if (element instanceof Server) {
						String id = ((Server)element).getIconID();
						return Activator.getInstance().getImage(id);
					}
				}
				return null;
			}
		};
	}

	@Override
	public String getValue(Object element, int columnIndex) {
		if (element instanceof Server) {
			switch (columnIndex) {
			case 0: return ((Server)element).getName();
			case 1: return ((Server)element).getUrl();
			default:
				break;
			}
		}
		return ""; //$NON-NLS-1$
	}

	
	
	
	@Override
	public ArcadColumns getReferenceColumns() {
		ArcadColumns cols = new ArcadColumns();
		ArcadColumn col = 
			new ArcadColumn("servername",Activator.resString("serverview.header.servername"),ArcadColumn.VISIBLE,0,200,0); //$NON-NLS-1$ //$NON-NLS-2$
		cols.add(col);
		col = 
			new ArcadColumn("url",Activator.resString("serverview.header.url"),ArcadColumn.VISIBLE,1,200,1); //$NON-NLS-1$ //$NON-NLS-2$
		cols.add(col);
		return cols;
	}

	@Override
	protected Action[] makeActions() {
		Action[] superActions = super.makeActions();
		Action[] additionalActions = getViewerActions();
		Action[] actions = new Action[superActions.length + additionalActions.length];
		System.arraycopy(superActions, 0, actions, 0, superActions.length);
		// Ajout de l'action d'affichage de l'éditeur de tris
		for (int i = 0; i < additionalActions.length; i++) {
			actions[superActions.length + i] = additionalActions[i];
		}
		return actions;
	}	
	
	protected Action[] getViewerActions() {
		return new Action[0];
	}
	
	
}
