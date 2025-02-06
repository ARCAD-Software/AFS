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
/**
 * This class provides the FindInText action. It manages the Dialog to enter text and selection in original text.
 * This feature provides Backward and Forward searches.
 * Is provided the option of wrapping around text when reaching limits.
 * Is provided search of Regular expressions.
 */
package com.arcadsoftware.afs.client.core.ui.actions;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.aev.core.ui.dialogs.DialogConstantProvider;
import com.arcadsoftware.aev.core.ui.mementos.UserPrefsMementoTools;
import com.arcadsoftware.aev.core.ui.mementos.UserPrefsSettings;
import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.dialogs.AbstractAFSDialog;

public class AbstractFindInTextAction extends AbstractAFSDialog implements Listener {

	protected final static String USER_PREF_WRAP = "wrapAround";
	protected final static String USER_PREF_USEREGEX = "useRegEx";

	private static int MARGIN = 2;
	private static int AREA_WIDTH = 300;
	private static int AREA_HEIGHT = 60;
	private static int BUTTON_HEIGHT = 40;
	private static int TITLE_HEIGHT = 35;

	private final Point position;
	// private Text globalTextWidget;
	private Text textWidget;
	private Point internalAreaSize;

	private final ISearchableText searchableText;

	private Button wrapButton = null;
	private Button useRegexButton = null;

	public AbstractFindInTextAction(Shell parentShell, Point position, Point size, Text text) {
		this(parentShell, position, size, new SearchableTextText(text));
	}

	public AbstractFindInTextAction(Shell parentShell, Point position, Point size, ISearchableText searchableText) {
		super(parentShell, false, false);
		this.position = new Point(position.x - 1, position.y - 2);
		if (size == null) {
			internalAreaSize = new Point(AREA_WIDTH, AREA_HEIGHT);
		} else {
			internalAreaSize = size;
		}
		this.searchableText = searchableText;
	}

	@Override
	public Point getSize() {
		final Point size = new Point(internalAreaSize.x + 20, internalAreaSize.y + BUTTON_HEIGHT + TITLE_HEIGHT);
		size.x = Math.max(200, size.x);
		return size;
	}

	@Override
	public String getTitle() {
		return "Find";
	}

	@Override
	protected int getShellStyle() {
		return SWT.ON_TOP | SWT.TITLE | SWT.CLOSE;
	}

	@Override
	protected void configureShell(Shell newShell) {
		final Point size = getSize();
		super.configureShell(newShell);
		newShell.setSize(size);
		newShell.setText(getTitle());
		newShell.setLocation(position.x, position.y - size.y);
		newShell.setImage(getImage());

		newShell.getParent().getDisplay().addFilter(SWT.MouseDown, this);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		// create a composite with standard margins and spacing
		final Composite composite = new Composite(parent, SWT.NONE);
		final GridLayout layout = new GridLayout();
		layout.marginHeight = layout.marginWidth = layout.marginBottom = layout.marginTop = 5;
		layout.verticalSpacing = layout.horizontalSpacing = MARGIN;
		composite.setLayout(layout);
		composite.setLayoutData(new GridData(GridData.FILL_BOTH));

		textWidget = new Text(composite, SWT.BORDER);
		textWidget.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent e) {
			}

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				handleDefaultSelection(e);
			}
		});
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		// gd.heightHint=internalAreaSize.y / 3;
		// gd.widthHint=internalAreaSize.x;
		textWidget.setLayoutData(gd);

		wrapButton = GuiFormatTools.createCheckButton(composite,
				Activator.resString("findintext.option.wraparound.label"), SWT.NONE);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		wrapButton.setLayoutData(gd);

		useRegexButton = GuiFormatTools.createCheckButton(composite,
				Activator.resString("findintext.option.useregex.label"), SWT.NONE);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		useRegexButton.setLayoutData(gd);

		final String defaultText = searchableText.getSelectionText();
		if (defaultText != null) {
			textWidget.setText(defaultText);
		}

		init();
		return composite;
	}

	public void handleDefaultSelection(SelectionEvent e) {
		okPressed();
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		Button tmp = createButton(parent, IDialogConstants.BACK_ID,
				DialogConstantProvider.getInstance().BACK_LABEL, false);
		tmp.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				searchAndSelect(false);
			}
		});
		tmp = createButton(parent, IDialogConstants.NEXT_ID,
				DialogConstantProvider.getInstance().NEXT_LABEL, false);
		tmp.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				searchAndSelect(true);
			}
		});
	}

	@Override
	public void handleEvent(Event event) {
	}

	/**
	 * Search string and select if found
	 *
	 * @param searched
	 *            Text to be found
	 * @param forward
	 *            true to search forward, false to search backward.
	 * @return
	 */
	protected void searchAndSelect(boolean forward) {
		String searched = textWidget.getText();
		final String globalText = searchableText.getText();

		final int searchLen = searched.length();
		final int globalLen = globalText.length();
		if ((searchLen == 0) || (globalLen == 0) || (searchLen > globalLen)) {
			return;
		}

		final boolean loop = wrapButton.getSelection();
		final Point currSelection = searchableText.getSelection();
		Point newSelection = null;

		if (!useRegexButton.getSelection()) {
			searched = Pattern.quote(searched);
		}

		newSelection = searchRegEx(globalText, searched, currSelection.x, forward);
		if (forward) {
			// check if selection is exactly the same as the current one : if it is, search the next one
			if ((currSelection.x != currSelection.y) && (newSelection != null)) {
				// if the same selection as current, find at pos + 1
				if (currSelection.equals(newSelection)) {
					newSelection = searchRegEx(globalText, searched, currSelection.x + 1, forward);
				}
			}
			// if nothing was found and wrap around
			if ((newSelection == null) && loop) {
				newSelection = searchRegEx(globalText, searched, 0, forward);
			}
		} else { // backward
			if ((newSelection == null) && loop) {
				newSelection = searchRegEx(globalText, searched, globalLen, forward);
			}
		}

		if (newSelection != null) {
			searchableText.setSelection(newSelection.x, newSelection.y);
		}
	}

	/**
	 * Get new selection
	 *
	 * @param text
	 * @param regex
	 * @param fromPosition
	 * @param forward
	 * @return point where x is the selection start and y the selection end
	 */
	private Point searchRegEx(String text, String regex, int fromPosition, boolean forward) {
		Point selection = new Point(-1, -1);

		final Pattern p = Pattern.compile(regex);
		final Matcher m = p.matcher(text);

		int start = -1;
		while (m.find()) {
			start = m.start();

			if (forward) {
				if (start >= fromPosition) {
					selection = new Point(m.start(), m.end());
					break;
				}
			} else { // backward
				if (start < fromPosition) {
					selection.x = start;
					selection.y = m.end();
				} else {
					break;
				}
			}
		}
		if (selection.x == -1) {
			return null;
		} else {
			return selection;
		}
	}

	private void init() {
		UserPrefsSettings settings = UserPrefsMementoTools.getInstance().getPrefsSetting(getUserPrefsId());
		if (settings == null) {
			// initialize
			final Map<String, String> prefs = new HashMap<>();
			prefs.put(USER_PREF_WRAP, Boolean.FALSE.toString());
			prefs.put(USER_PREF_USEREGEX, Boolean.FALSE.toString());
			settings = new UserPrefsSettings(getUserPrefsId(), prefs);

			UserPrefsMementoTools.getInstance().setCurrentSettings(settings);
		} else {
			Boolean b = Boolean.valueOf(settings.getPrefValue(USER_PREF_WRAP));
			wrapButton.setSelection(b.booleanValue());

			b = Boolean.valueOf(settings.getPrefValue(USER_PREF_USEREGEX));
			useRegexButton.setSelection(b.booleanValue());
		}
	}

	@Override
	public boolean close() {
		saveSettings();
		return super.close();
	}

	/**
	 * Save User Settings
	 */
	protected void saveSettings() {
		final UserPrefsSettings settings = UserPrefsMementoTools.getInstance().getPrefsSetting(getUserPrefsId());
		if (!wrapButton.isDisposed()) {
			settings.setPrefValue(USER_PREF_WRAP, Boolean.toString(wrapButton.getSelection()));
		}
		if (!useRegexButton.isDisposed()) {
			settings.setPrefValue(USER_PREF_USEREGEX, Boolean.toString(useRegexButton.getSelection()));
		}
		UserPrefsMementoTools.getInstance().setCurrentSettings(settings);
	}

	/**
	 * Identifier for User Preferences section
	 *
	 * @return
	 */
	protected String getUserPrefsId() {
		return AbstractFindInTextAction.class.getName();
	}
}
