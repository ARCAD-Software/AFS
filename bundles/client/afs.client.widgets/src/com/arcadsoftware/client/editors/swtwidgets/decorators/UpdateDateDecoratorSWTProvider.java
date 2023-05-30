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
package com.arcadsoftware.client.editors.swtwidgets.decorators;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;

import java.util.Calendar;
import java.util.Date;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.IUpdateDateChanged;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Label Decorator SWT Widget provider for the dynamic
 * editors.
 */
public class UpdateDateDecoratorSWTProvider implements IDecoratorSWTProvider, IUpdateDateChanged {

	private DateTime date;
	private ISWTRenderer renderer;
	private Calendar calendar = Calendar.getInstance();

	public Widget create(ISWTRenderer swtRenderer, ILayoutParameters parameters, MetaDataEntity structure) {
		this.renderer = swtRenderer;
		renderer.getUpdateDateListeners().addUpdateDateChanged(this);
		renderer.getToolkit().createLabel(renderer.getParent(),
				renderer.getLocalizedMessage(parameters.getParameter(LABEL)));
		renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
		date = new DateTime(renderer.getParent(), SWT.DATE);
		date.setEnabled(false);
		if (renderer.getParent().getLayout() instanceof GridLayout)
			date.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		return date;
	}

	public void dispose() {
		renderer.getUpdateDateListeners().removeUpdateDateChanged(this);
	}

	public void updateDateChanged() {
		Date updateDate = renderer.getUpdateDate();
		if (updateDate != null) {
			calendar.setTime(updateDate);
			date
					.setDate(calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH), calendar
							.get(Calendar.DAY_OF_MONTH));
		}
	}
}
