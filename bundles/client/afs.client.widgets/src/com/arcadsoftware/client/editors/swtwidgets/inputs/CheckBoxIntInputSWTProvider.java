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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT_VALUE;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.READ_ONLY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;

import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.IWidgetValue;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a CheckBox SWT Widget provider that gets an integer as entry. It is able to manage an integer
 * property as a 2-state property for final User
 */
public class CheckBoxIntInputSWTProvider implements IInputSWTProvider, IWidgetValue {

	ISWTRenderer renderer;
	private static String ON_MIN_VALUE = "onMin";
	private static String ON_MAX_VALUE = "onMax";
	private static String OFF_MIN_VALUE = "offMin";
	private static String OFF_MAX_VALUE = "offMax";
	int onMin;
	int onMax;
	int offMin;
	int offMax;
	private Button checkBox;

	public CheckBoxIntInputSWTProvider() {
		super();
	}

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element,
			MetaDataEntity structure) {
		this.renderer = renderer;

		// parameters : inclusive limits
		onMin = stringInInt(parameters.getParameter(ON_MIN_VALUE));
		onMax = stringInInt(parameters.getParameter(ON_MAX_VALUE));
		offMin = stringInInt(parameters.getParameter(OFF_MIN_VALUE));
		offMax = stringInInt(parameters.getParameter(OFF_MAX_VALUE));

		// Compute
		computeIntervals();

		final String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		if (label.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
		}

		checkBox = new Button(renderer.getParent(), SWT.CHECK);
		if (label.length() == 0) {
			final GridData layoutData = new GridData();
			layoutData.horizontalSpan = 3;
			checkBox.setLayoutData(layoutData);
		}
		checkBox.setEnabled(!element.isReadonly() && !parameters.getParameterBoolean(READ_ONLY));
		if (parameters.getParameterBoolean(DEFAULT)) {
			checkBox.setFocus();
		}
		final boolean value = parameters.getParameterBoolean(DEFAULT_VALUE, false);
		checkBox.setSelection(value);

		// TODO RAP
		// renderer.getToolkit().paintBordersFor(renderer.getParent());
		renderer.getRendererBinding().bindElement(element, this);
	}

	@Override
	public void dispose() {
		if (!checkBox.isDisposed()) {
			checkBox.dispose();
		}
		checkBox = null;
	}

	@Override
	public Control getWidget() {
		return checkBox;
	}

	@Override
	public Object getValue() {
		final boolean select = checkBox.getSelection();
		if (select) {
			return onMin;
		} else {
			return offMin;
		}
	}

	@Override
	public void addSelectionListener(SelectionListener selectionListener) {
		checkBox.addSelectionListener(selectionListener);
	}

	@Override
	public void setValue(Object newValue) {
		if ((newValue != null) && (newValue instanceof Integer)) {
			boolean check = false;
			final int val = ((Integer) newValue).intValue();
			if (onMin != -1) { // check ON interval first
				if (onMin > offMin) {
					check = val >= onMin;
				} else {
					check = val < offMin;
				}
			}
			checkBox.setSelection(check);
		}
	}

	@Override
	public Object getValueType() {
		return Integer.class;
	}

	/**
	 * Load value from Web-Service; the web service returns a single value as a beanMap property
	 *
	 * @param service
	 *            Formatted as {type}:{url}
	 * @return
	 */
	private int loadValueFromURL(String service) {
		int result = -1;
		if (service.contains(":")) { //$NON-NLS-1$
			final String[] split = service.split(":"); //$NON-NLS-1$
			// load content
			final BeanMap content = renderer.getDataLoader().loadContent(split[1], split[0]);
			if ((content != null) && !content.isEmpty()) {
				final Set<Entry<String, Object>> entries = content.entrySet();
				// get the first integer key (Note::: we should get only one property here!!! )
				for (final Entry<String, Object> entry : entries) {
					final Object value = entry.getValue();
					if (value instanceof Integer) {
						result = ((Integer) value).intValue();
						break;
					}
					try {
						result = Integer.parseInt(value.toString());
						break;
					} catch (final NumberFormatException e) {
					}
				}
			}
		}
		return result;
	}

	/**
	 * Compute intervals. onMin and offMin MUST be computed.
	 *
	 * @param onMin
	 * @param onMax
	 * @param offMin
	 * @param offMax
	 */
	private void computeIntervals() {
		if (onMin == -1) {
			if (onMax != -1) {
				if (offMin == -1) {
					onMin = 0;
					offMin = onMax + 1;
				} else { // offMin is defined
					if (offMin < onMax) {
						if ((offMax == -1) || (offMax > onMax)) {
							// ????
						} else {
							onMin = offMax + 1;
						}
					} else { // offMin >= onMax
						onMin = 0;
					}
				}
			} else { /// max1 is defined
				if (offMax != -1) {
					onMin = offMax + 1;
					if (offMin == -1) {
						offMin = 0;
					}
				} else {
					if (offMin != -1) {
						onMin = 0;
						onMax = offMin - 1;
					} // else ???
				}
			}
		} else { // (onMin is defined
			if (onMax != -1) {
				if (offMin == -1) {
					if (offMax == -1) {
						offMin = onMax + 1;
					} else if (offMax < onMin) {
						offMin = 0;
					} else if (offMax > onMax) {
						offMin = onMax + 1;
					} else {
						// nothing into interval2
					}
					// we get onMin and onMax ... enough
				} else { // offMin != -1
					if (offMax == -1) {
						if (offMin > onMax) {
							// ok : onMin, onMax, offMin
						} else if (offMin < onMin) {
							offMax = onMin - 1;
						}
					} else { // offMax != -1
						// ...
					}
				}
			} else { // OnMax == -1
				if (offMin == -1) {
					if (offMax == -1) {
						offMin = 0;
						offMax = onMin - 1;
					} // ...
				} else { // (offMin != -1)
					if (offMin < onMin) {
						offMax = onMin - 1;
					} else if (offMin > onMin) {
						onMax = offMin - 1;
					}
				}
			}
		}
	}

	/**
	 * Get int value from String
	 *
	 * @param value
	 * @return
	 */
	private int stringInInt(String value) {
		int result = -1;
		if (value != null) {
			try {
				result = Integer.parseInt(value);
			} catch (final NumberFormatException e) {
				// if string, this is URL to get value from server
				result = loadValueFromURL(value);
			}
		}
		return result;
	}
}