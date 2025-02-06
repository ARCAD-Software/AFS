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
package com.arcadsoftware.client.editors.swtwidgets.viewer;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ATTRIBUTE;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.COLUMN;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DATE_FORMAT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.EMPTY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.FORMAT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TABLE_VIEWER_ID;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.USE_BOOLEAN_IMAGE;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.aev.core.collections.ArcadCollection;
import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumn;
import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumns;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTableLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.ColumnedDefaultTableLabelProvider;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTableViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractInternalColumnedViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.impl.ColumnedInternalTableViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.impl.ColumnedTableViewer;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.client.editors.swtwidgets.internal.Messages;
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
import com.arcadsoftware.metadata.MetaDataFormater;

public class BeanMapTableViewer extends AbstractColumnedTableViewer
		implements IBeanMapContainerList, IBeanMapContainerValue, ILoadedListListener {

	private static List<Action> toActionList(Action[] actions) {
		final ArrayList<Action> result = new ArrayList<>(actions.length);
		for (final Action action : actions) {
			if (action != null) {
				result.add(action);
			}
		}
		return result;
	}

	private static final String BEAN_MAP_TABLE_VIEWER = "BeanMapTableViewer"; //$NON-NLS-1$

	private final MetaDataEntity structure;
	protected ArcadColumns cols;
	protected BeanMapList list;
	private SelectionAdapter selectionAdapter;
	private final List<Action> actionsList;
	private final ILayoutParameters parameters;
	private final ISWTRenderer renderer;
	private List<ElementParameter> columnElements;
	protected BeanMap selection;
	private String attributeList;
	private String orderList;
	protected Image defaultImage;
	private final Map<String, Map<Integer, BeanMap>> beanMapsValues = new HashMap<>();
	protected Map<String, MetaDataEntity> structures = new HashMap<>();

	/**
	 * Default constructor with :
	 * <ul>
	 * <li>The renderer parent.
	 * <li>Default style (single selection and scroll bars).
	 * </ul>
	 *
	 * @param renderer
	 * @param parameters
	 * @param element
	 * @param actions
	 */
	public BeanMapTableViewer(ISWTRenderer renderer, ILayoutParameters parameters, Element element, Action... actions) {
		this(renderer.getParent(), SWT.SINGLE | SWT.FULL_SELECTION | SWT.V_SCROLL | SWT.H_SCROLL, renderer, parameters,
				element,
				toActionList(actions));
	}

	public BeanMapTableViewer(Composite parent, int style, ISWTRenderer renderer, ILayoutParameters parameters,
			Element element, List<Action> actionsList) {
		super(parent, style, false);
		structure = renderer.getStructure(element);
		this.actionsList = actionsList;
		this.parameters = parameters;
		this.renderer = renderer;
		renderer.addLoadedList(this);
		init();
		createDefaultImage();
	}

	private void createDefaultImage() {
		final String defaultKey = parameters.getParameter(IConstants.ICON, ""); //$NON-NLS-1$
		if (defaultKey.length() > 0) {
			final ImageDescriptor desc = renderer.getImageDescriptor(defaultKey);
			if (desc != null) {
				defaultImage = desc.createImage();
			}
		}
	}

	@Override
	public AbstractColumnedTableLabelProvider createTableLabelProvider(AbstractColumnedViewer viewer) {
		if (parameters.getParameterBoolean(USE_BOOLEAN_IMAGE)) {
			return new ColumnedDefaultTableLabelProvider(viewer) {
				@Override
				protected Image getActualImage(Object element, int actualColumnIndex) {
					Image result = null;
					final String entryKey = cols.items(actualColumnIndex).getIdentifier()
							.replaceAll(BEAN_MAP_TABLE_VIEWER + '.' + structure.getType() + '.', ""); //$NON-NLS-1$
					final MetaDataAttribute attribute = structure.getAttribute(entryKey);
					if ((attribute != null) && MetaDataAttribute.TYPE_BOOLEAN.equals(attribute.getType())) {
						final BeanMapArcadEntity beanMapArcadEntity = (BeanMapArcadEntity) element;
						final Object attributeValue = beanMapArcadEntity.getBeanMap().get(entryKey);
						if (attributeValue != null) {
							if (attributeValue instanceof Boolean) {
								if ((boolean) attributeValue) {
									result = AFSIcon.CHECKBOX_FILLED.image();
								} else {
									result = AFSIcon.CHECKBOX_EMPTY.image();
								}
							} else if (attributeValue.equals(1)) {
								result = AFSIcon.CHECKBOX_FILLED.image();
							} else {
								result = AFSIcon.CHECKBOX_EMPTY.image();
							}
						}
					}
					if (result == null) {
						result = getDefaultImage(actualColumnIndex);
					}
					if (result == null) {
						result = super.getActualImage(element, actualColumnIndex);
					}
					return result;
				}
			};
		}
		return new ColumnedDefaultTableLabelProvider(viewer) {
			@Override
			protected Image getActualImage(Object element, int actualColumnIndex) {
				Image result = getDefaultImage(actualColumnIndex);
				if (result == null) {
					result = getUserDefinedActualImage(element, actualColumnIndex);
					if (result == null) {
						result = super.getActualImage(element, actualColumnIndex);
					}
				}
				return result;
			}
		};
	}

	protected Image getUserDefinedActualImage(Object element, int actualColumnIndex) {
		return null;
	}

	protected Image getDefaultImage(int actualColumnIndex) {
		if ((actualColumnIndex == 0) && (defaultImage != null)) {
			return defaultImage;
		}
		return null;
	}

	@Override
	public String getIdentifier() {
		final String viewerId = parameters.getParameter(TABLE_VIEWER_ID);
		if (viewerId == null) {
			return null;
		}
		return super.getIdentifier() + '.' + structure.getType() + '.' + viewerId;
	}

	@Override
	public ArcadColumns getReferenceColumns() {
		cols = new ArcadColumns();
		final StringBuilder attributes = new StringBuilder();
		columnElements = parameters.getListElementParameter(COLUMN);
		if ((columnElements != null) && !columnElements.isEmpty()) {
			for (final ElementParameter element : columnElements) {
				String label = renderer.getLocalizedMessage(parameters.getElementParameter(element, LABEL));
				final String key = parameters.getElementParameter(element, ATTRIBUTE);
				MetaDataAttribute attribute = null;
				String structureType = null;
				if (structure != null) {
					attribute = structure.getAttribute(key);
					structureType = structure.getType();
				}
				if (attribute != null) {
					int visible;
					if (attribute.isVisible()) {
						visible = ArcadColumn.VISIBLE;
					} else {
						visible = ArcadColumn.HIDDEN;
					}
					if (label == null) {
						label = attribute.getName();
					}
					cols.add(new ArcadColumn(BEAN_MAP_TABLE_VIEWER + '.' + structureType + '.' + key, label, label,
							visible, cols.count(),
							parameters.getElementParameterInteger(element, IConstants.WIDTH, attribute.getColSize())));
				} else if (structureType != null) { // We have a reference line (att.att.att)
					cols.add(new ArcadColumn(BEAN_MAP_TABLE_VIEWER + '.' + structureType + '.' + key, label, label,
							ArcadColumn.VISIBLE, cols.count(),
							parameters.getElementParameterInteger(element, IConstants.WIDTH, 100)));
				}
				if (attributes.length() > 0) {
					attributes.append(' ');
				}
				attributes.append(key);
			}
		} else {
			for (final Map.Entry<String, MetaDataAttribute> entry : structure.getAttributes().entrySet()) {
				final MetaDataAttribute attribute = entry.getValue();
				int visible;
				if (attribute.isVisible()) {
					visible = ArcadColumn.VISIBLE;
				} else {
					visible = ArcadColumn.HIDDEN;
				}
				cols.add(new ArcadColumn(BEAN_MAP_TABLE_VIEWER + '.' + structure.getType() + '.' + entry.getKey(),
						attribute.getName(), attribute.getName(), visible, cols.count(), attribute.getColSize()));
				if (attributes.length() > 0) {
					attributes.append(' ');
				}
				attributes.append(attribute.getCode());
			}
		}
		attributeList = attributes.toString();
		return cols;
	}

	private String getAlternateColumnValue(BeanMap bean, String rawValue, int columnIndex) {
		final ElementParameter colElement = columnElements.get(columnIndex);
		if (colElement != null) {
			final String id = colElement.getParameter("id"); //$NON-NLS-1$
			if (colElement.getParameterBoolean("masked")) { //$NON-NLS-1$
				return formatMaskedValue(bean, rawValue, id);
			}
			if (id != null) {
				for (final ElementParameter av : parameters.getListElementParameter("alternateValue")) { //$NON-NLS-1$
					final String columndid = av.getParameter("columnid"); //$NON-NLS-1$
					final String value = av.getParameter("value"); //$NON-NLS-1$
					final String altvalue = av.getParameter("altvalue"); //$NON-NLS-1$
					// No value passed
					if ((columndid.equalsIgnoreCase(id)) && ((value == null) || (altvalue == null))) {
						return formatAlternativeValue(bean, rawValue, columndid);
					}
					if ((columndid.equalsIgnoreCase(id)) && (value.equalsIgnoreCase(rawValue))) {
						if (altvalue != null) {
							return renderer.getLocalizedMessage(altvalue);
						}
						return formatAlternativeValue(bean, value, columndid);
					}
				}
			}
		}
		return rawValue;
	}

	protected String formatAlternativeValue(BeanMap bean, String rawValue, String columndid) {
		return rawValue;
	}

	protected String formatMaskedValue(BeanMap bean, String rawValue, String columndid) {
		return rawValue;
	}

	@Override
	public String getValue(Object element, int columnIndex) {
		Object value = null;
		final String entryKey = cols.items(columnIndex).getIdentifier()
				.replaceAll(BEAN_MAP_TABLE_VIEWER + '.' + structure.getType() + '.', EMPTY);
		final MetaDataAttribute attribute = structure.getAttribute(entryKey);
		final BeanMapArcadEntity beanMapArcadEntity = (BeanMapArcadEntity) element;
		final Object attributeValue = beanMapArcadEntity.getBeanMap().get(entryKey);
		if (attribute != null) {
			if (MetaDataAttribute.TYPE_BOOLEAN.equals(attribute.getType())) {
				if ((parameters.getParameterBoolean(USE_BOOLEAN_IMAGE))) {
					value = ""; //$NON-NLS-1$
				} else {
					value = getBooleanValue(attributeValue);
				}
			} else if (MetaDataAttribute.TYPE_STRING.equals(attribute.getType())) {
				if (attributeValue == null) {
					value = ""; //$NON-NLS-1$
				}
			}
		} else {
			// If attribute not found, we just try to get a value from the beanMap
			// Means that this key has not been declared as an official attribute
			value = beanMapArcadEntity.getBeanMap().getString(entryKey);
		}
		if (value == null) {
			final String dateFormat = getColumnDateFormat(columnIndex);
			if ((dateFormat != null) && (attributeValue instanceof Date)) {
				value = new SimpleDateFormat(dateFormat).format((Date) attributeValue);
			} else {
				String format = null;
				if ((columnElements != null) && !columnElements.isEmpty()) {
					format = parameters.getElementParameter(columnElements.get(columnIndex), FORMAT);
				}
				if ((format != null) && (attributeValue != null)) {
					BeanMap beanMap = null;
					final String type = structure.getAttribute(entryKey).getType();
					Map<Integer, BeanMap> currentType = beanMapsValues.get(type);
					if (currentType != null) {
						final BeanMap currentBeanMap = currentType.get(attributeValue);
						if (currentBeanMap != null) {
							beanMap = currentBeanMap;
						}
					}
					if (beanMap == null) {
						beanMap = renderer.loadBeanMap(type, ((Integer) attributeValue).intValue());
						if (currentType == null) {
							currentType = new HashMap<>();
							beanMapsValues.put(type, currentType);
						}
						currentType.put((Integer) attributeValue, beanMap);
					}
					MetaDataEntity currentStructure = structures.get(type);
					if (currentStructure == null) {
						currentStructure = renderer.getStructure(type);
						structures.put(type, currentStructure);
					}
					value = new MetaDataFormater(format, currentStructure).format(beanMap);
				} else {
					// value = attributeValue;
					final BeanMap bean = beanMapArcadEntity.getBeanMap();
					if (attributeValue != null) {
						value = getAlternateColumnValue(bean, attributeValue.toString(), columnIndex);
					} else {
						value = attributeValue;
					}
				}
			}
		}
		if (value != null) {
			return translateValue(cleanValue(value.toString()), columnIndex);
		}
		return translateValue("", columnIndex); //$NON-NLS-1$
	}

	private String getColumnDateFormat(int columnIndex) {
		final String result = parameters.getElementParameter(columnElements.get(columnIndex), DATE_FORMAT);
		if (result == null) {
			return null;
		}
		// service Formatted as {beanmap type}:{/url}
		if (!result.contains(":/") || (result.indexOf(':') != result.lastIndexOf(':')) || (result.indexOf(' ') >= 0)) {
			return renderer.getLocalizedMessage(result);
		}
		// Load value from Web-Service; the web service returns a single value as a beanMap property
		final String[] split = result.split(":"); //$NON-NLS-1$
		final BeanMap content = renderer.getDataLoader().loadContent(split[1], split[0]);
		if ((content == null) || content.isEmpty()) {
			return null;
		}
		final Collection<Object> values = content.values();
		if (values.isEmpty()) {
			return null;
		}
		return (String) values.iterator().next();
	}

	protected String translateValue(String value, int columnIndex) {
		return value;
	}

	@SuppressWarnings("deprecation")
	@Override
	public Object getTypedValue(Object element, int columnIndex) {
		Object value = null;
		final String entryKey = cols.items(columnIndex).getIdentifier()
				.replaceAll(BEAN_MAP_TABLE_VIEWER + '.' + structure.getType() + '.', ""); //$NON-NLS-1$
		final Object attributeValue = ((BeanMapArcadEntity) element).getBeanMap().get(entryKey);
		String format = null;
		if ((columnElements != null) && !columnElements.isEmpty()) {
			format = parameters.getElementParameter(columnElements.get(columnIndex), FORMAT);
		}
		if ((format != null) && (attributeValue != null)) {
			BeanMap beanMap = null;
			final String type = structure.getAttribute(entryKey).getType();
			Map<Integer, BeanMap> currentType = beanMapsValues.get(type);
			if (currentType != null) {
				final BeanMap currentBeanMap = currentType.get(attributeValue);
				if (currentBeanMap != null) {
					beanMap = currentBeanMap;
				}
			}
			if (beanMap == null) {
				beanMap = renderer.loadBeanMap(type, ((Integer) attributeValue).intValue());
				if (currentType == null) {
					currentType = new HashMap<>();
					beanMapsValues.put(type, currentType);
				}
				currentType.put((Integer) attributeValue, beanMap);
			}
			MetaDataEntity currentStructure = null;
			currentStructure = structures.get(type);
			if (currentStructure == null) {
				currentStructure = renderer.getStructure(type);
				structures.put(type, currentStructure);
			}
			value = new MetaDataFormater(format, currentStructure).nonFormat(beanMap);
		} else {
			value = attributeValue;
		}
		if (value instanceof String) {
			return cleanValue((String) value);
		}
		return value;
	}

	private String cleanValue(String value) {
		if (mustBeCleanValue()) {
			return value.replace('\n', ' ').replace('\r', ' ');
		}
		return value;
	}

	protected boolean mustBeCleanValue() {
		return false;
	}

	private String getBooleanValue(Object object) {
		if (object instanceof Boolean) {
			if (((Boolean) object).booleanValue()) {
				return Messages.yes;
			}
			return Messages.no;
		}
		if ("0".equals(object.toString())) {
			return Messages.no;
		}
		return Messages.yes;
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
		final ArcadCollection collection = new ArcadCollection();
		for (final BeanMap beanMap : list) {
			collection.add(new BeanMapArcadEntity(beanMap));
			if (beanMap.equals(selection)) {
				selection = beanMap;
			}
		}
		setInput(collection);
		refresh();
		if (selection != null) {
			setBeanMapValue(selection);
		}
	}

	@Override
	public void addBeanMapToList(int index, BeanMap beanMap) {
		if (list == null) {
			list = new BeanMapList();
		}
		list.add(beanMap);
	}

	@Override
	public Widget getWidget() {
		return getViewer().getControl();
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
	public BeanMap getBeanMapValue() {
		if (getSelection().isEmpty()) {
			return null;
		}
		return ((BeanMapArcadEntity) getSelection().getFirstElement()).getBeanMap();
	}

	@SuppressWarnings("unchecked")
	public BeanMapList getSelected() {
		final BeanMapList selected = new BeanMapList();
		final Iterator<Object> it = getSelection().iterator();
		while (it.hasNext()) {
			final Object o = it.next();
			if (o instanceof BeanMapArcadEntity) {
				selected.add(((BeanMapArcadEntity) o).getBeanMap());
			}
		}
		return selected;
	}

	@Override
	public void setBeanMapValue(BeanMap beanMap) {
		getTable().select(list.indexOf(beanMap));
		getViewer().setSelection(new StructuredSelection(new BeanMapArcadEntity(beanMap)));
	}

	@Override
	protected Action[] makeActions() {
		final Action[] actions;
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

	public ArcadColumns getCols() {
		return cols;
	}

	public MetaDataEntity getStructure() {
		return structure;
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
	public String getAttributeList() {
		return attributeList;
	}

	@Override
	public String getOrderList() {
		return orderList;
	}

	public void sort(String sortOrder) {
		final boolean desc = sortOrder.startsWith("-"); //$NON-NLS-1$
		if (sortOrder.startsWith("+") || sortOrder.startsWith("-")) { //$NON-NLS-1$ //$NON-NLS-2$
			sortOrder = sortOrder.substring(1);
		}
		final String attribute = sortOrder;
		getViewer().setComparator(null);
		// sort according to due date
		getViewer().setComparator(new ViewerComparator() {
			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				final BeanMap b1 = ((BeanMapArcadEntity) e1).getBeanMap();
				final BeanMap b2 = ((BeanMapArcadEntity) e2).getBeanMap();
				if (desc) {
					return -b1.compareTo(b2, attribute);
				}
				return b1.compareTo(b2, attribute);
			}
		});
	}

	@Override
	public AbstractInternalColumnedViewer createViewer(Composite viewerParent, int viewerStyle) {
		// Compare BeanMapArcadEntity content
		final ColumnedTableViewer tableViewer = new ColumnedTableViewer(viewerParent, viewerStyle) {
			@Override
			protected boolean equals(Object a, Object b) {
				if ((getComparer() == null) && (a instanceof BeanMapArcadEntity) && (b instanceof BeanMapArcadEntity)) {
					return ((BeanMapArcadEntity) a).getBeanMap().equals(((BeanMapArcadEntity) b).getBeanMap());
				}
				return super.equals(a, b);
			}
		};
		return new ColumnedInternalTableViewer(tableViewer);
	}
}
