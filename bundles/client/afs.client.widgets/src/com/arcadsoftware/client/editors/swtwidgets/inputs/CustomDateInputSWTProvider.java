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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.MANDATORY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.READ_ONLY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.WITH_CHECK_BUTTON;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.WITH_TIME;

import java.util.Calendar;
import java.util.Date;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.client.editors.swtwidgets.internal.Messages;
import com.arcadsoftware.client.editors.swtwidgets.widgets.CustomDateTime;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Date SWT Widget provider for the dynamic editors.
 */
public class CustomDateInputSWTProvider implements IInputSWTProvider {

	private static final Date EMPTY_DATE = new Date(0);
	private static final String OK = Messages.DateInputSWTProvider_okButton;
	private static final String BUTTON_LABEL = "..."; //$NON-NLS-1$

	private CustomDateTime customDateTimeDate;
	private DateTime dateTime;
	private boolean withCheckButton;
	private Button checkBox;
	private Button button;
	private CustomDateTime customDateTimeTime;
	private DateTime dialogDateTime;
	private boolean withTime;
	boolean shortTime;

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element, MetaDataEntity structure) {
		final String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		if (label.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
		}
		withTime = parameters.getParameterBoolean(WITH_TIME);
		shortTime = parameters.getParameterBoolean(IConstants.SHORTTIME, false);
		final boolean readonly = element.isReadonly() || parameters.getParameterBoolean(READ_ONLY);
		withCheckButton = parameters.getParameterBoolean(WITH_CHECK_BUTTON);
		final int horizontalSpan = (label.length() > 0) ? 1 : 3;
		final Composite composite = createDefaultComposite(renderer, horizontalSpan);
		if (withCheckButton) {
			checkBox = new Button(composite, SWT.CHECK);
			checkBox.setSelection(true);
			checkBox.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (readonly) {
						dateTime.setEnabled(false);
						button.setEnabled(false);
					} else {
						dateTime.setEnabled(checkBox.getSelection());
						button.setEnabled(checkBox.getSelection());
					}
					if (withTime) {
						customDateTimeTime.getDateTime().setEnabled(checkBox.getSelection());
					}
					if (!readonly) {
						if (!checkBox.getSelection()) {
							customDateTimeDate.setValue(EMPTY_DATE);
						} else {
							customDateTimeDate.setValue(new Date());
						}
					}
				}
			});
			customDateTimeDate = new CustomDateTime(composite, SWT.DATE | SWT.BORDER) {
				@Override
				public Object getValue() {
					Date value = EMPTY_DATE;
					if (checkBox.getSelection()) {
						value = (Date) super.getValue();
					}
					if (withTime && (value != null)) {
						final Calendar currentCalendar = Calendar.getInstance();
						currentCalendar.setTime(value);
						final DateTime d = customDateTimeTime.getDateTime();
						final int hours = d.getHours();
						final int minutes = d.getMinutes();
						currentCalendar.set(Calendar.HOUR_OF_DAY, hours);
						currentCalendar.set(Calendar.MINUTE, minutes);
						value = currentCalendar.getTime();
					}
					return value;
				}

				@Override
				public void addSelectionListener(SelectionListener selectionListener) {
					super.addSelectionListener(selectionListener);
					checkBox.addSelectionListener(selectionListener);
					if (withTime) {
						customDateTimeTime.addSelectionListener(selectionListener);
					}
				}

				@Override
				public void setValue(Object newValue) {
					super.setValue(newValue);
					checkBox.setSelection((newValue != null) && (newValue != EMPTY_DATE));
					if (readonly) {
						dateTime.setEnabled(false);
						button.setEnabled(false);
					} else {
						dateTime.setEnabled(checkBox.getSelection());
						button.setEnabled(checkBox.getSelection());
					}
					if (withTime) {
						customDateTimeTime.setValue(newValue);
						customDateTimeTime.getDateTime().setEnabled(checkBox.getSelection());
					}
				}
			};
			if (withTime) {
				int timeStyle = SWT.TIME | SWT.BORDER;
				if (shortTime) {
					timeStyle = timeStyle | SWT.SHORT;
				}
				customDateTimeTime = new CustomDateTime(composite, timeStyle);
			}
		} else {
			customDateTimeDate = new CustomDateTime(composite, SWT.DATE | SWT.BORDER) {
				@Override
				public Object getValue() {
					Date value = (Date) super.getValue();
					if (withTime) {
						final Date timeValue = (Date) customDateTimeTime.getValue();
						final Calendar currentCalendar = Calendar.getInstance();
						currentCalendar.setTime(value);
						final int year = currentCalendar.get(Calendar.YEAR);
						final int month = currentCalendar.get(Calendar.MONTH);
						final int day = currentCalendar.get(Calendar.DAY_OF_MONTH);
						currentCalendar.setTime(timeValue);
						final int style = customDateTimeDate.getWidget().getStyle();

						if (((style & SWT.CALENDAR) != 0) || ((style & SWT.DATE) != 0)) {
							currentCalendar.set(year, month, day, currentCalendar.get(Calendar.HOUR_OF_DAY),
									currentCalendar
											.get(Calendar.MINUTE),
									currentCalendar.get(Calendar.SECOND));
						} else {
							currentCalendar.set(year, month, day, 0, 0, 0);
						}
						value = currentCalendar.getTime();
					}
					return value;
				}

				@Override
				public void addSelectionListener(SelectionListener selectionListener) {
					super.addSelectionListener(selectionListener);
					if (withTime) {
						customDateTimeTime.addSelectionListener(selectionListener);
					}
				}

				@Override
				public void setValue(Object newValue) {
					super.setValue(newValue);
					if (withTime) {
						customDateTimeTime.setValue(newValue);
					}
				}
			};
			if (withTime) {
				customDateTimeTime = new CustomDateTime(composite, SWT.TIME | SWT.BORDER);
			}
		}
		dateTime = customDateTimeDate.getDateTime();
		if (parameters.getParameterBoolean(DEFAULT)) {
			dateTime.setFocus();
		}
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			dateTime.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			if (withTime) {
				customDateTimeTime.getDateTime().setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			}
		}
		button = renderer.getToolkit().createButton(composite, BUTTON_LABEL, SWT.PUSH);
		button.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				showDateChoiceDialog(label);
			}
		});
		if (element.isReadonly() || parameters.getParameterBoolean(READ_ONLY)) {
			button.setEnabled(false);
			dateTime.setEnabled(false);
			if (checkBox != null) {
				checkBox.setEnabled(false);
			}
		} else {
			button.setEnabled(true);
			dateTime.setEnabled(true);
			if (checkBox != null) {
				checkBox.setEnabled(true);
			}
		}
		if (parameters.getParameterBoolean(MANDATORY)) {
			renderer.addMandatoryAttribute(element.getCode());
		}
		renderer.getRendererBinding().bindElement(element, customDateTimeDate);
	}

	@Override
	public void dispose() {
		// Do nothing
	}

	private Composite createDefaultComposite(ISWTRenderer renderer, int horizontalSpan) {
		final Composite composite = renderer.getToolkit().createComposite(renderer.getParent(), SWT.NONE);
		int numColumns = 2;
		if (withCheckButton) {
			numColumns = 3;
		}
		if (withTime) {
			numColumns++;
		}
		final GridLayout gridLayout = new GridLayout(numColumns, false);
		gridLayout.marginBottom = gridLayout.marginHeight = gridLayout.marginLeft = gridLayout.marginRight = gridLayout.marginTop = gridLayout.marginWidth = 0;
		composite.setLayout(gridLayout);
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			composite.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, horizontalSpan, 1));
		}
		return composite;
	}

	void showDateChoiceDialog(String title) {
		final Shell dialog = new Shell(Display.getCurrent(), SWT.NONE);
		dialog.setText(title);
		final GridLayout gridLayout = new GridLayout();
		gridLayout.marginHeight = gridLayout.marginWidth = gridLayout.verticalSpacing = 0;
		dialog.setLayout(gridLayout);
		dialogDateTime = new DateTime(dialog, SWT.CALENDAR);
		dialogDateTime.setDate(dateTime.getYear(), dateTime.getMonth(), dateTime.getDay());
		final Button ok = new Button(dialog, SWT.PUSH | SWT.CENTER);
		ok.setText(OK);
		ok.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		ok.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				final Calendar currentCalendar = Calendar.getInstance();
				currentCalendar.set(dialogDateTime.getYear(), dialogDateTime.getMonth(), dialogDateTime.getDay());
				customDateTimeDate.setValue(currentCalendar.getTime());
				dialog.close();
			}
		});
		dialog.setDefaultButton(ok);
		dialog.addShellListener(new ShellListener() {
			@Override
			public void shellIconified(ShellEvent e) {
			}

			@Override
			public void shellDeiconified(ShellEvent e) {
			}

			@Override
			public void shellDeactivated(ShellEvent e) {
				dialog.close();
			}

			@Override
			public void shellClosed(ShellEvent e) {
			}

			@Override
			public void shellActivated(ShellEvent e) {
				dialogDateTime.setFocus();
			}
		});
		dialog.pack();
		final Point size = dialog.getSize();
		final Rectangle screen = button.getDisplay().getClientArea();
		Point pos = button.getLocation();
		pos.y += button.getSize().y;
		pos = button.getParent().toDisplay(pos);
		if ((pos.x + size.x) > screen.width) {
			pos.x = screen.width - size.x;
		}
		if ((pos.y + size.y) > screen.height) {
			pos.y -= button.getSize().y + size.y;
			if (pos.y < 0) {
				pos.y = 0;
			}
		}
		dialog.setLocation(pos);
		dialog.open();
	}

}
