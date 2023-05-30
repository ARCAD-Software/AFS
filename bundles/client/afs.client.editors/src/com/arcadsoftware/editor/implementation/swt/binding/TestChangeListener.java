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
package com.arcadsoftware.editor.implementation.swt.binding;

import org.eclipse.core.databinding.observable.value.IValueChangeListener;
import org.eclipse.core.databinding.observable.value.ValueChangeEvent;

import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;
import com.arcadsoftware.metadata.MetaDataTest;

public class TestChangeListener implements IValueChangeListener {

	private static final String VALUE = "value"; //$NON-NLS-1$
	private static final String OLDVALUE = "oldvalue"; //$NON-NLS-1$
	private static final String ACCEPT = "accept"; //$NON-NLS-1$
	private SWTRenderer renderer;
	private  MetaDataTest  test;
	private boolean updating = false;

	/**
	 * @param renderer
	 * @param test
	 */
	public TestChangeListener(SWTRenderer renderer, MetaDataTest test) {
		super();
		this.renderer = renderer;
		this.test = test;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.core.databinding.observable.value.IValueChangeListener#handleValueChange(org.eclipse.core.databinding
	 * .observable.value.ValueChangeEvent)
	 */
	public void handleValueChange(ValueChangeEvent event) {
		if (updating) {
			return;
		}
		//TODO RAP
//		IScriptEngine engine = Activator.getInstance().openScriptEngine();
//		if (engine != null) {
//			try {
//				updating = true;
//				boolean exec = false;
//				// Only observable Attributes or links can be binded !
//				for (String code : test.getAttributes()) {
//					BeanMapObservableValue value = renderer.getRendererBinding().getObservableAttribute(code);
//					if (value == null) {
//						// BeanMapLink link = links.get(code);
//						// TODO need a warper (isEmpty(),length(),getValue(idx,code),remove(idx)).
//					} else {
//						engine.bind(code, value.getValue());
//						exec = true;
//					}
//				}
//				if (exec) {
//					engine.bind(ACCEPT, Boolean.valueOf(renderer.isValided(test)));
//					engine.bind(OLDVALUE, event.diff.getOldValue());
//					engine.bind(VALUE, event.diff.getNewValue());
//					try {
//						engine.eval(test.getTest());
//						for (String code : test.getAttributes()) {
//							BeanMapObservableValue value = renderer.getRendererBinding().getObservableAttribute(code);
//							if (value == null) {
//								// BeanMapLink link = links.get(code);
//								// TODO need a warper (proceed to remove(idx)...).
//							} else {
//								Object o = engine.getValue(code);
//								if (o == null) {
//									if (value.doGetValue() != null) {
//										value.setValue(null);
//									}
//								} else if (!o.equals(value.doGetValue())) {
//									value.setValue(o);
//								}
//							}
//						}
//						Object accept = engine.getValue(ACCEPT);
//						if (accept instanceof Boolean) {
//							renderer.updateTest(test, ((Boolean) accept).booleanValue());
//						}
//					} catch (ScriptExecutionException e) {
//						Activator.getInstance().debug(
//								"Error during Script Execution (Validating Attribute Changing Event).", e); //$NON-NLS-1$
//					}
//				}
//			} finally {
//				Activator.getInstance().closeStriptEngine(engine);
//				updating = false;
//			}
//		}
	}

}
