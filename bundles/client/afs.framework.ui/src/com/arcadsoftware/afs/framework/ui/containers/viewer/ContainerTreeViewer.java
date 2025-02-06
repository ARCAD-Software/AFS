/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
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
package com.arcadsoftware.afs.framework.ui.containers.viewer;

import java.util.StringTokenizer;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerDropAdapter;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumn;
import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumns;
import com.arcadsoftware.aev.core.ui.container.Container;
import com.arcadsoftware.aev.core.ui.container.IContainer;
import com.arcadsoftware.aev.core.ui.contentproviders.ContainerTreeContentProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.ContainerLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTreeLabelProvider;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTreeViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;

public class ContainerTreeViewer extends AbstractColumnedTreeViewer {

	IContainer dropSource;
	Action doubleClickAction = null;

	public ContainerTreeViewer(Composite parent, int style, boolean withInit) {
		super(parent, style, withInit);
		getViewer().addSelectionChangedListener(new ContainerTreeSelectionChangedListener());
		initializeDragAndDropManagement();
		// hookDoubleClickAction();
	}

	public ContainerTreeViewer(Composite parent, int style) {
		this(parent, style, true);
	}

	@Override
	protected void setOptions() {
		getTree().setHeaderVisible(false);
		getTree().setLinesVisible(false);
	}

	@Override
	protected void doOnDoubleClick(IStructuredSelection selection) {
		// Do nothing
	}

	@Override
	public AbstractColumnedTreeLabelProvider createTreeLabelProvider(AbstractColumnedViewer viewer) {
		return new ContainerLabelProvider(viewer);
	}

	@Override
	public IContentProvider createContentProvider() {
		return new ContainerTreeContentProvider();
	}

	@Override
	public ArcadColumns getReferenceColumns() {
		final ArcadColumns cols = new ArcadColumns();
		cols.add(new ArcadColumn("container", "", "", ArcadColumn.VISIBLE, 0, 800)); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		return cols;
	}

	@Override
	public String getValue(Object element, int columnIndex) {
		if (element instanceof IContainer) {
			return ((IContainer) element).getLabel();
		}
		return ""; //$NON-NLS-1$
	}

	@Override
	protected Action[] makeActions() {
		return new Action[0];
	}

	private IContainer getContainerFromKey(String key, TreeItem node) {
		IContainer container = null;
		final Object o = node.getData();
		if ((o instanceof IContainer) && (((IContainer) o).getUniqueKey().equals(key))) {
			return (IContainer) o;
		}
		final TreeItem[] nodes = node.getItems();
		for (final TreeItem node2 : nodes) {
			if (node2.getData() instanceof IContainer) {
				final IContainer c = (IContainer) node2.getData();
				if (c.getUniqueKey().equals(key)) {
					return c;
				}
				container = getContainerFromKey(key, node2);
				if (container != null) {
					return container;
				}
			}
		}
		return container;
	}

	IContainer getContainerFromKey(String key) {
		final Tree tree = getTree();
		final TreeItem[] rootnodes = tree.getItems();
		for (final TreeItem rootnode : rootnodes) {
			final IContainer c = getContainerFromKey(key, rootnode);
			if (c != null) {
				return c;
			}
		}
		return null;
	}

	/*
	 * @author ARCAD Software Pour changer le mod�le de ce commentaire de type g�n�r�, allez � :
	 * Fen�tre&gt;Pr�f�rences&gt;Java&gt;G�n�ration de code&gt;Code et commentaires
	 */
	private class ContainerTreeDragSourceListener implements DragSourceListener {
		/**
		 * @param viewer
		 */
		public ContainerTreeDragSourceListener(StructuredViewer viewer) {
			// Do nothing
		}

		@Override
		public void dragStart(DragSourceEvent event) {
			final IContainer c = getSelectedElement();
			dropSource = c;
			if (c != null) {
				event.doit = c.isDragable();
			} else {
				event.doit = false;
			}
		}

		@Override
		public void dragSetData(DragSourceEvent event) {
			final IContainer c = getSelectedElement();
			if (c != null) {
				event.data = c.getUniqueKey();
			} else {
				event.data = null;
			}
		}

		@Override
		public void dragFinished(DragSourceEvent event) {
			// Do nothing
		}
	}

	private class ContainerTreeDropTargetListener extends ViewerDropAdapter {
		protected ContainerTreeDropTargetListener(Viewer viewer) {
			super(viewer);
		}

		@Override
		public boolean performDrop(Object data) {
			if (getCurrentTarget() instanceof IContainer) {
				final IContainer c = getContainerFromKey(((IContainer) getCurrentTarget()).getUniqueKey());
				if (c != null) {
					if (dropSource != null) {
						return c.performDrop(dropSource);
						// Drag � partir d'une vue exterieur
					}
					// en fonction du type d'�l�ment dropp�, faire le traitement
					// adequat
					// else if (componentDropped){
					// ComponentWithKey cd = (ComponentWithKey)data;
					// if (getCurrentTarget() instanceof
					// IAcceptDroppedComponents){
					// if (deleguateComponentAction)
					// return doComponentAction(c,cd.getComponents());
					// else
					// return
					// ((IAcceptDroppedComponents)c).performDropComponents(cd.getComponents());
					// }
				}
			}
			return false;
		}

		@Override
		public boolean validateDrop(Object target, int operation, TransferData transferType) {
			// ComponentDropped = false;
			if (target != null) {
				if (dropSource != null) {
					return ((IContainer) target).valideDrop(dropSource);
				}
				// ComponentDropped =
				// ComponentWithKeyTransfer.getInstance().isSupportedType(transferType);
				return ((IContainer) target).valideDrop(transferType);
			}
			return false;
			// return true;
		}
	}

	protected void doOnSelect(Object selected) {

	}

	protected class ContainerTreeSelectionChangedListener implements ISelectionChangedListener {
		@Override
		public void selectionChanged(SelectionChangedEvent event) {
			if (event.getSelection().isEmpty()) {
				return;
			}
			if (event.getSelection() instanceof IStructuredSelection) {
				final IStructuredSelection sel = (IStructuredSelection) event.getSelection();
				doOnSelect(sel.getFirstElement());
			}
		}
	}

	protected void initializeDragAndDropManagement() {
		if (getViewer() != null) {
			final int ops = DND.DROP_COPY | DND.DROP_MOVE;
			final Transfer[] transfers = new Transfer[] { TextTransfer.getInstance() };
			getViewer().addDragSupport(ops, transfers, new ContainerTreeDragSourceListener(getViewer()));
			final int ops2 = DND.DROP_COPY | DND.DROP_MOVE;
			final Transfer[] transfers2 = new Transfer[] { TextTransfer.getInstance() };
			getViewer().addDropSupport(ops2, transfers2, new ContainerTreeDropTargetListener(getViewer()));
		}
	}

	public IContainer getSelectedElement() {
		final IStructuredSelection selection = getSelection();
		final Object o = selection.getFirstElement();
		if (o instanceof IContainer) {
			return (IContainer) selection.getFirstElement();
		}
		return null;
	}

	// private void hookDoubleClickAction() {
	// doubleClickAction = new Action() {
	// @Override
	// public void run() {
	// if (!getSelection().isEmpty()) {
	// IStructuredSelection selection = getSelection();
	// doOnDoubleClick(selection);
	// }
	// }
	// };
	// getViewer().addDoubleClickListener(new IDoubleClickListener() {
	// public void doubleClick(DoubleClickEvent event) {
	// doubleClickAction.run();
	// }
	// });
	// }

	public void refresh(Object element) {
		if ((element != null) && (element instanceof Container)) {
			((Container) element).refresh();
		}
		getViewer().refresh(element);
	}

	public void expandFromKey(String key) {
		TreeItem node = null;
		final StringTokenizer st = new StringTokenizer(key, "/", false); //$NON-NLS-1$
		String s = ""; //$NON-NLS-1$
		while (st.hasMoreElements()) {
			s = s.concat("/").concat(st.nextToken()); //$NON-NLS-1$
			node = expandFromAbsoluteKey(s);
		}
		if (node != null) {
			getTree().setSelection(new TreeItem[] { node });
		}
	}

	public TreeItem getNodeFromAbsoluteKey(String key) {
		final Tree tree = getTree();
		final TreeItem[] rootnodes = tree.getItems();
		for (final TreeItem rootnode : rootnodes) {
			final TreeItem node = getNodeFromKey(key, rootnode);
			if (node != null) {
				return node;
			}
		}
		return null;
	}

	private TreeItem expandFromAbsoluteKey(String key) {
		final Tree tree = getTree();
		final TreeItem[] rootnodes = tree.getItems();
		for (final TreeItem rootnode : rootnodes) {
			final TreeItem node = getNodeFromKey(key, rootnode);
			if (node != null) {
				node.setExpanded(true);
				getTreeViewer().expandToLevel(node.getData(), 1);
				return node;
			}
		}
		return null;
	}

	private TreeItem getNodeFromKey(String key, TreeItem node) {
		TreeItem resultNode = null;
		final Object o = node.getData();
		if ((o instanceof IContainer) && (((IContainer) o).getUniqueKey().equals(key))) {
			return node;
		}
		final TreeItem[] nodes = node.getItems();
		for (final TreeItem node2 : nodes) {
			if (node2.getData() instanceof IContainer) {
				final IContainer c = (IContainer) node2.getData();
				if (c.getUniqueKey().equals(key)) {
					return node2;
				}
				resultNode = getNodeFromKey(key, node2);
				if (resultNode != null) {
					return resultNode;
				}
			}
		}
		return resultNode;
	}
}
