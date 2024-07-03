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
package com.arcadsoftware.client.editors.swtwidgets.viewer;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ATTRIBUTE;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.COLUMN;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.EMPTY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TREE_VIEWER_ID;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.USE_BOOLEAN_IMAGE;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.aev.core.collections.ArcadCollection;
import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumn;
import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumns;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTreeLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.ColumnedDefaultTreeLabelProvider;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTreeViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.client.editors.swtwidgets.inputs.ITreeSWTProvider;
import com.arcadsoftware.client.editors.swtwidgets.model.BeanMapArcadEntity;
import com.arcadsoftware.editor.ElementParameter;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IBeanMapContainerList;
import com.arcadsoftware.editor.swt.IBeanMapContainerValue;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.actions.IRefreshableAction;
import com.arcadsoftware.editor.swt.renderer.ILoadedListListener;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public class BeanMapTreeViewer extends AbstractColumnedTreeViewer
		implements IBeanMapContainerList, IBeanMapContainerValue, ILoadedListListener {

	private static final String BEAN_MAP_TREE_VIEWER = "BeanMapTreeViewer"; //$NON-NLS-1$

	private final MetaDataEntity structure;
	protected ArcadColumns cols;
	protected BeanMapList list;
	SelectionAdapter selectionAdapter;
	private final List<Action> actionsList;
	ILayoutParameters parameters;
	ISWTRenderer renderer;
	protected ITreeSWTProvider swtProvider;
	List<ElementParameter> elems;
	protected BeanMap selection;

	// private Map<String, Map<Integer, BeanMap>> beanMapsValues = new HashMap<String, Map<Integer, BeanMap>>();
	protected Map<String, MetaDataEntity> structures = new HashMap<>();

	public BeanMapTreeViewer(Composite parent, int style, ISWTRenderer renderer, ILayoutParameters parameters,
			Element element, List<Action> actionsList) {
		super(parent, style, false);
		structure = renderer.getStructure(element);
		this.actionsList = actionsList;
		this.parameters = parameters;
		this.renderer = renderer;
		renderer.addLoadedList(this);
		init();
	}

	@Override
	public AbstractColumnedTreeLabelProvider createTreeLabelProvider(AbstractColumnedViewer viewer) {
		if (parameters.getParameterBoolean(USE_BOOLEAN_IMAGE)) {
			return new ColumnedDefaultTreeLabelProvider(viewer) {
				@Override
				protected Image getActualImage(Object element, int actualColumnIndex) {
					Image result = null;
					final String entryKey = cols.items(actualColumnIndex).getIdentifier().replaceAll(
							BEAN_MAP_TREE_VIEWER + '.' + structure.getType() + '.', EMPTY);
					final MetaDataAttribute attribute = structure.getAttribute(entryKey);
					if (MetaDataAttribute.TYPE_BOOLEAN.equals(attribute.getType())) {
						final BeanMapArcadEntity beanMapArcadEntity = (BeanMapArcadEntity) element;
						final Object attributeValue = beanMapArcadEntity.getBeanMap().get(entryKey);
						if (attributeValue != null) {
							result = attributeValue.equals(Integer.valueOf(1)) ? AFSIcon.CHECKBOX_FILLED.image()
									: AFSIcon.CHECKBOX_EMPTY.image();
						}
					}
					if (result == null) {
						result = super.getActualImage(element, actualColumnIndex);
					}
					return result;
				}
			};
		}
		return new ColumnedDefaultTreeLabelProvider(viewer);
	}

	@Override
	public String getIdentifier() {
		final String viewerId = parameters.getParameter(TREE_VIEWER_ID);
		if (viewerId == null) {
			return super.getIdentifier() + '.' + structure.getType();
		} else {
			return super.getIdentifier() + '.' + structure.getType() + '.' + viewerId;
		}
	}

	@Override
	public ArcadColumns getReferenceColumns() {
		cols = new ArcadColumns();
		elems = parameters.getListElementParameter(COLUMN);

		if ((elems != null) && !elems.isEmpty()) {
			for (final ElementParameter element : elems) {
				final String label = renderer.getLocalizedMessage(parameters.getElementParameter(element, LABEL));
				final String key = parameters.getElementParameter(element, ATTRIBUTE);
				final MetaDataAttribute attribute = structure.getAttribute(key);
				if (attribute != null) {
					final int visible = (attribute.isVisible()) ? ArcadColumn.VISIBLE : ArcadColumn.HIDDEN;
					final String columnLabel = (label != null) ? label : attribute.getName();
					cols.add(new ArcadColumn(BEAN_MAP_TREE_VIEWER + '.' + structure.getType() + '.' + key,
							columnLabel, columnLabel, visible, cols.count(), attribute.getColSize()));
				}
			}
		} else {
			for (final Map.Entry<String, MetaDataAttribute> entry : structure.getAttributes().entrySet()) {
				final MetaDataAttribute attribute = entry.getValue();
				final int visible = (attribute.isVisible()) ? ArcadColumn.VISIBLE : ArcadColumn.HIDDEN;
				cols.add(new ArcadColumn(BEAN_MAP_TREE_VIEWER + '.' + structure.getType() + '.' + entry.getKey(),
						attribute.getName(), attribute.getName(), visible, cols.count(), attribute.getColSize()));
			}
		}
		return cols;
	}

	@Override
	public String getValue(Object element, int columnIndex) {
		return swtProvider.getValue(element, columnIndex);
	}

	protected boolean mustBeCleanValue() {
		return false;
	}

	@Override
	public void addBeanMapToList(int index, BeanMap beanMap) {
		if (list == null) {
			list = new BeanMapList();
		}
		list.add(beanMap);
	}

	@Override
	public BeanMapList getBeanMapList() {
		return list;
	}

	@Override
	public void setBeanMapList(BeanMapList list) {
		final BeanMap newSelection = getBeanMapValue();
		if (newSelection != null) {
			selection = newSelection;
		}
		this.list = list;
		final ArcadCollection collection = swtProvider.createContentData(list);
		if (collection != null) {
			setInput(collection);
		} else {
			setInput(list);
		}
		refresh();
		if (selection != null) {
			setBeanMapValue(selection);
		}
	}

	@Override
	public Widget getWidget() {
		return getViewer().getControl();
	}

	@Override
	public String getListType() {
		return structure.getType();
	}

	@Override
	public void loadedListComplete(ISWTRenderer renderer) {
		if (list != null) {
			setBeanMapList(list);
		}
	}

	@Override
	public void addSelectionListener(SelectionAdapter newSelectionAdapter) {
		selectionAdapter = newSelectionAdapter;
		getViewer().addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				selectionAdapter.widgetSelected(null);
			}
		});
	}

	@Override
	protected Action[] makeActions() {
		Action[] actions;
		if (actionsList == null) {
			actions = super.makeActions();
		} else {
			final Action[] superActions = super.makeActions();
			actions = new Action[superActions.length + actionsList.size()];
			System.arraycopy(superActions, 0, actions, 0, superActions.length);
			for (int i = 0; i < actionsList.size(); i++) {
				actions[superActions.length + i] = actionsList.get(i);
			}
		}
		for (final Action action : actions) {
			if (action instanceof IRefreshableAction) {
				((IRefreshableAction) action).refresh();
			}
		}
		return actions;
	}

	@Override
	public BeanMap getBeanMapValue() {
		if (getSelection().isEmpty()) {
			return null;
		}
		final Object o = getSelection().getFirstElement();
		if (o instanceof BeanMap) {
			return (BeanMap) o;
		} else if (o instanceof BeanMapArcadEntity) {
			return ((BeanMapArcadEntity) getSelection().getFirstElement()).getBeanMap();
		} else {
			return null;
		}
	}

	@Override
	public void setBeanMapValue(BeanMap beanMap) {
		// getTable().select(list.indexOf(beanMap));
		getViewer().setSelection(new StructuredSelection(beanMap));
	}

	public ArcadColumns getCols() {
		return cols;
	}

	public MetaDataEntity getStructure() {
		return structure;
	}

	public void setSwtProvider(ITreeSWTProvider swtProvider) {
		this.swtProvider = swtProvider;
	}

	@Override
	public String getAttributeList() {
		return null;
	}

	@Override
	public String getOrderList() {
		return null;
	}

	@SuppressWarnings("unchecked")
	/**
	 * Get selected list of beanMaps The returned items can have different types since they can come from different
	 * levels.
	 *
	 * @return
	 */
	public BeanMapList getSelected() {
		final BeanMapList selected = new BeanMapList();
		final IStructuredSelection ss = getSelection();
		final Iterator<Object> it = ss.iterator();
		while (it.hasNext()) {
			final Object o = it.next();
			if (o instanceof BeanMapArcadEntity) {
				final BeanMapArcadEntity entity = (BeanMapArcadEntity) o;
				selected.add(entity.getBeanMap());
			} else if (o instanceof BeanMap) {
				selected.add((BeanMap) o);
			}
		}
		return selected;
	}
}