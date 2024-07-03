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
package com.arcadsoftware.afs.client.macro.internal.ui.viewers;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumn;
import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumns;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTableLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.ColumnedDefaultTableLabelProvider;
import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTableViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.client.macro.internal.Activator;
import com.arcadsoftware.afs.client.macro.model.MacroLogItemDefinition;

public class MacroLogItemTableViewer extends AbstractColumnedTableViewer {

	public static final String MSG_INFO = "*INFO"; //$NON-NLS-1$
	public static final String MSG_INQ = "*IN";//$NON-NLS-1$
	public static final String MSG_RQS = "*RQS"; //$NON-NLS-1$
	public static final String MSG_COMP = "*COMP"; //$NON-NLS-1$
	public static final String MSG_DIAG = "*DIAG"; //$NON-NLS-1$
	public static final String MSG_NOTIFY = "*NOTIFY"; //$NON-NLS-1$
	public static final String MSG_ESCAPE = "*ESCAPE"; //$NON-NLS-1$
	public static final String MSG_STATUS = "*STATUS"; //$NON-NLS-1$

	private final List<String> includedMessageTypes = new ArrayList<>();

	private class FilterAction extends Action {

		String messageType;
		MacroLogItemTableViewer viewer;

		public FilterAction(MacroLogItemTableViewer viewer, String text, String messageType) {
			super(text, IAction.AS_CHECK_BOX);
			this.messageType = messageType;
			this.viewer = viewer;
			setChecked(true);
		}

		@Override
		public void run() {
			if (isChecked()) {
				includeMessageType(messageType);
			} else {
				removeMessageType(messageType);
			}
			viewer.getViewer().refresh();
		}

	}

	private FilterAction filterCompletionMessage;
	private FilterAction filterDiagnosticMessage;
	private FilterAction filterEscapeMessage;
	private FilterAction filterInformationMessage;
	private FilterAction filterInquiryMessage;
	private FilterAction filterNotifyMessage;
	private FilterAction filterRequestMessage;
	private FilterAction filterStatusMessage;
	private List<Action> filterActions = new ArrayList<>();

	private class LogItemFilter extends ViewerFilter {

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers. Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public boolean select(Viewer viewer, Object parentElement, Object element) {
			final MacroLogItemDefinition mli = (MacroLogItemDefinition) element;
			return isIncludedInMessageType(mli.getType());
		}
	}

	public void includeMessageType(String messageType) {
		if (includedMessageTypes.indexOf(messageType) < 0) {
			includedMessageTypes.add(messageType);
		}
	}

	public void removeMessageType(String messageType) {
		final int res = includedMessageTypes.indexOf(messageType);
		if (res >= 0) {
			includedMessageTypes.remove(res);
		}
	}

	public boolean isIncludedInMessageType(String messageType) {
		return (includedMessageTypes.indexOf(messageType) >= 0);
	}

	public MacroLogItemTableViewer(Composite parent, int style) {
		super(parent, style, false);
		init();
	}

	protected List<Action> createActions() {
		final List<Action> result = new ArrayList<>();

		filterCompletionMessage = new FilterAction(this, Activator.resString("macro.msgtype.completion"), MSG_COMP);
		filterDiagnosticMessage = new FilterAction(this, Activator.resString("macro.msgtype.diagnostic"), MSG_DIAG);
		filterEscapeMessage = new FilterAction(this, Activator.resString("macro.msgtype.escape"), MSG_ESCAPE);
		filterInformationMessage = new FilterAction(this, Activator.resString("macro.msgtype.information"), MSG_INFO);
		filterInquiryMessage = new FilterAction(this, Activator.resString("macro.msgtype.inquiry"), MSG_INQ);
		filterNotifyMessage = new FilterAction(this, Activator.resString("macro.msgtype.notify"), MSG_NOTIFY);
		filterRequestMessage = new FilterAction(this, Activator.resString("macro.msgtype.request"), MSG_RQS);
		filterStatusMessage = new FilterAction(this, Activator.resString("macro.msgtype.status"), MSG_STATUS);

		result.add(filterCompletionMessage); // $NON-NLS-1$
		result.add(filterDiagnosticMessage); // $NON-NLS-1$
		result.add(filterEscapeMessage); // $NON-NLS-1$
		result.add(filterInformationMessage); // $NON-NLS-1$
		result.add(filterInquiryMessage); // $NON-NLS-1$
		result.add(filterNotifyMessage); // $NON-NLS-1$
		result.add(filterRequestMessage); // $NON-NLS-1$
		result.add(filterStatusMessage); // $NON-NLS-1$
		return result;
	}

	@Override
	public void init() {
		super.init();
		filterActions = createActions();
		includedMessageTypes.add(MSG_COMP);
		includedMessageTypes.add(MSG_DIAG);
		includedMessageTypes.add(MSG_ESCAPE);
		includedMessageTypes.add(MSG_INFO);
		includedMessageTypes.add(MSG_INQ);
		includedMessageTypes.add(MSG_NOTIFY);
		includedMessageTypes.add(MSG_RQS);
		includedMessageTypes.add(MSG_STATUS);
		getViewer().addFilter(new LogItemFilter());
	}

	@Override
	protected void fillContextMenu(IMenuManager manager) {
		super.fillContextMenu(manager);
		final ContributionItem sectionContributionItem = GuiFormatTools.createMenu(filterActions);
		manager.add(sectionContributionItem);

	}

	@Override
	public AbstractColumnedTableLabelProvider createTableLabelProvider(AbstractColumnedViewer viewer) {
		return new MacroLabelProvider(viewer);
	}

	@Override
	public String getValue(Object element, int columnIndex) {
		if (element instanceof MacroLogItemDefinition) {
			final MacroLogItemDefinition le = (MacroLogItemDefinition) element;
			switch (columnIndex) {
			case 0:
				return le.getSequence();
			case 1:
				return le.getId();
			case 2:
				return le.getType();
			case 3:
				return le.getStatus();
			case 4:
				return le.getSeverity();
			case 5:
				return le.getLevel1();
			case 6:
				return le.getLevel2();
			default:
				break;
			}
		}
		return "";
	}

	@Override
	public String getIdentifier() {
		return null;
	}

	@Override
	public ArcadColumns getReferenceColumns() {
		final ArcadColumns refColumns = new ArcadColumns();
		refColumns.add(new ArcadColumn("sequence", Activator.resString("macroLogItem.header.sequence"), //$NON-NLS-1$
				ArcadColumn.VISIBLE, 0, 100, 0));
		refColumns.add(
				new ArcadColumn("id", Activator.resString("macroLogItem.header.id"), ArcadColumn.VISIBLE, 1, 100, 1)); //$NON-NLS-1$
		refColumns.add(new ArcadColumn("type", Activator.resString("macroLogItem.header.type"), ArcadColumn.VISIBLE, 2, //$NON-NLS-1$
				100, 2));
		refColumns.add(new ArcadColumn("status", Activator.resString("macroLogItem.header.status"), ArcadColumn.VISIBLE, //$NON-NLS-1$
				3, 40, 3));
		refColumns.add(new ArcadColumn("severity", Activator.resString("macroLogItem.header.severity"), //$NON-NLS-1$
				ArcadColumn.VISIBLE, 4, 50, 4));
		refColumns.add(new ArcadColumn("level1", Activator.resString("macroLogItem.header.level1"), ArcadColumn.VISIBLE, //$NON-NLS-1$
				5, 500, 5));
		refColumns.add(new ArcadColumn("level2", Activator.resString("macroLogItem.header.level2"), ArcadColumn.VISIBLE, //$NON-NLS-1$
				6, 500, 6));
		return refColumns;
	}

	public void contributeToToolBar(IToolBarManager manager) {
		manager.add(filterCompletionMessage);
		manager.add(filterDiagnosticMessage);
		manager.add(filterEscapeMessage);
		manager.add(filterInformationMessage);
		manager.add(filterInquiryMessage);
		manager.add(filterNotifyMessage);
		manager.add(filterRequestMessage);
		manager.add(filterStatusMessage);
	}

	private static class MacroLabelProvider extends ColumnedDefaultTableLabelProvider implements ITableColorProvider {

		public MacroLabelProvider(AbstractColumnedViewer viewer) {
			super(viewer);
		}

		@Override
		public Color getForeground(Object element, int columnIndex) {
			return null;
		}

		@Override
		public Color getBackground(Object element, int columnIndex) {
			switch (((MacroLogItemDefinition) element).getStatus().toLowerCase()) {
			case "failure":
			case "error":
				return Activator.getDefault().getColor(Activator.COLOR_RED);
			case "warning":
				return Activator.getDefault().getColor(Activator.COLOR_YELLOW);
			default:
				return Activator.getDefault().getColor(Activator.COLOR_GREEN);
			}
		}

	}
}
