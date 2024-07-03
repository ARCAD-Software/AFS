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
package com.arcadsoftware.afs.framework.ui.beanmap.composites;

import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

public class BeanMapConverter {

	public static Object getValue(Control control) {
		if (control instanceof Text) {
			return ((Text) control).getText();
		}
		if (control instanceof Combo) {
			return ((Combo) control).getText();
		}
		if (control instanceof Button) {
			return ((Combo) control).getSelection();
		}
		return null;
	}

	public static void setValue(Control control, Object value) {
	}
}
