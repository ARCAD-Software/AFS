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
package com.arcadsoftware.client.editors.swtwidgets.containers;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.IMessage;
import org.eclipse.ui.forms.ManagedForm;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.ColumnLayout;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormText;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;

import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IContainerSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Form Container SWT Widget provider for the dynamic
 * editors.
 */
public class FormContainerSWTProvider implements IContainerSWTProvider {

	private class MessageHyperlinkAdapter extends HyperlinkAdapter {

		FormToolkit toolkit;

		public MessageHyperlinkAdapter(FormToolkit toolkit) {
			this.toolkit = toolkit;
		}

		@Override
		public void linkActivated(HyperlinkEvent e) {
			String title = e.getLabel();
			Object href = e.getHref();
			Point hl = ((Control) e.widget).toDisplay(0, 0);
			hl.x += 10;
			hl.y += 10;
			Shell shell = new Shell(managedForm.getForm().getShell(), SWT.ON_TOP | SWT.TOOL);
			shell.setImage(getImage(managedForm.getForm().getMessageType()));
			shell.setText(title);
			shell.setLayout(new FillLayout());
			FormText text = toolkit.createFormText(shell, true);
			configureFormText(managedForm.getForm().getForm(), text);
			if (href instanceof IMessage[]) {
				text.setText(createFormTextContent((IMessage[]) href), true, false);
			}
			shell.setLocation(hl);
			shell.pack();
			shell.open();
		}

		private String createFormTextContent(IMessage[] messages) {
			StringWriter sw = new StringWriter();
			PrintWriter pw = new PrintWriter(sw);
			pw.println("<form>");//$NON-NLS-1$
			for (int i = 0; i < messages.length; i++) {
				IMessage message = messages[i];
				pw.print("<li vspace=\"false\" style=\"image\" indent=\"16\" value=\"");//$NON-NLS-1$
				switch (message.getMessageType()) {
				case IMessageProvider.ERROR:
					pw.print("error"); //$NON-NLS-1$
					break;
				case IMessageProvider.WARNING:
					pw.print("warning"); //$NON-NLS-1$
					break;					
				case IMessageProvider.INFORMATION:				
					pw.print("info"); //$NON-NLS-1$
					break;
				default: //Keep looping
				}
				pw.print("\"> <a href=\"");//$NON-NLS-1$
				pw.print(i + "");//$NON-NLS-1$
				pw.print("\">");//$NON-NLS-1$
				if (message.getPrefix() != null) {
					pw.print(message.getPrefix());
				}
				pw.print(message.getMessage());
				pw.println("</a></li>");//$NON-NLS-1$
			}
			pw.println("</form>");//$NON-NLS-1$
			pw.flush();
			return sw.toString();
		}

		private void configureFormText(final Form form, FormText text) {
			text.addHyperlinkListener(new HyperlinkAdapter() {
				@Override
				public void linkActivated(HyperlinkEvent e) {
					try {
						int index = Integer.parseInt((String) e.getHref());
						IMessage[] messages = form.getChildrenMessages();
						IMessage message = messages[index];
						Control c = message.getControl();
						((FormText) e.widget).getShell().dispose();
						if (c != null) {
							c.setFocus();
						}
					} catch (NumberFormatException ex) {
						// Nothing to do here.
					}
				}
			});
			text.setImage("error", getImage(IMessageProvider.ERROR)); //$NON-NLS-1$
			text.setImage("warning", getImage(IMessageProvider.WARNING)); //$NON-NLS-1$
			text.setImage("info", getImage(IMessageProvider.INFORMATION)); //$NON-NLS-1$
		}

		private Image getImage(int type) {
			switch (type) {
			case IMessageProvider.ERROR:
				return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_ERROR_TSK);
			case IMessageProvider.WARNING:
				return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_WARN_TSK);
			case IMessageProvider.INFORMATION:		
				return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_INFO_TSK);
			default:
				return null;
			}			
		}
	}

	private ManagedForm managedForm;

	public void create(ISWTRenderer renderer, ILayoutParameters params, boolean isEmpty, MetaDataEntity structure) {
		String icon = params.getParameter(IConstants.ICON);
		ImageDescriptor id = null;
		if ((icon != null) && (icon.length() > 0)) {
			id =  renderer.getImageDescriptor(icon);
		}
		int cols = params.getParameterInteger(IConstants.COLS, 0);
		Layout layout;
		if (cols == 0) {
			layout = new ColumnLayout();
		} else {
			layout = createGridLayout(cols);
		}
		if (params.getParameterBoolean(IConstants.SCROLL)) {
			ScrolledForm form = renderer.getToolkit().createScrolledForm(renderer.getParent());
			managedForm = new ManagedForm(renderer.getToolkit(), form);
			form.getForm().addMessageHyperlinkListener(new MessageHyperlinkAdapter(renderer.getToolkit()));
			form.getBody().setLayout(layout);
			form.setText(getFormattedLabel(params.getParameter(IConstants.LABEL), renderer));
			if (id != null) {
				form.setImage(id.createImage());
			}
			renderer.getToolkit().decorateFormHeading(form.getForm());
			renderer.setFormToolBar(form.getToolBarManager());
			final ArrayList<IAction> actions = new ArrayList<>();
			// Il faut rattacher le ManagedForm au renderer pour gérer les sous
			// bindings et les bindings de tests
			renderer.createSubContainer(this, managedForm.getMessageManager(), params, form.getBody(), actions);
			for (IAction action:actions) {
				form.getToolBarManager().add(action);
			}
			form.layout(true, true);
		} else {
			Form form = renderer.getToolkit().createForm(renderer.getParent());
			form.getBody().setLayout(layout);
			form.setText(getFormattedLabel(params.getParameter(IConstants.LABEL), renderer));
			
			if (id != null) {
				form.setImage(id.createImage());
			}
			renderer.getToolkit().decorateFormHeading(form);
			renderer.setFormToolBar(form.getToolBarManager());
			ArrayList<IAction> actions = new ArrayList<>();
			renderer.createSubContainer(this, null, params, form.getBody(),actions);
			for (IAction action:actions) {
				form.getToolBarManager().add(action);
			}
			form.layout(true, true);
		}
	}

	private String getFormattedLabel(final String labelKey, ISWTRenderer renderer) {
		String label = renderer.getLocalizedMessage(labelKey);
		if(label != null && !label.isEmpty()) {
			StringBuilder result = new StringBuilder(label.length());
			StringBuilder tagName = null;
			for (char c: label.toCharArray()) {
				if (tagName != null) {
					if (c == '%') {
						if ((tagName == null) || (tagName.length() == 0)) {
							result.append('%');
						} else {
							String code = tagName.toString();
							Object value = renderer.getVirtualValue(code);
							if (value != null) {
								result.append(value.toString());
							} else {
								result.append(renderer.getCurrentBean().getString(code, "")); //$NON-NLS-1$
							}
						}
						tagName = null;
					} else {
						tagName.append(c);
					}
				} else if (c == '%') {
					tagName = new StringBuilder();
				} else {
					result.append(c);
				}
			}
		}
		return label;
	}
	
	private GridLayout createGridLayout(int cols) {
		GridLayout gridLayout = new GridLayout(cols, false);
		gridLayout.marginBottom = 0;
		gridLayout.marginHeight = 0;
		gridLayout.marginLeft = 0;
		gridLayout.marginRight = 0;
		gridLayout.marginTop = 0;
		gridLayout.marginWidth = 0;
		return gridLayout;
	}

	public void dispose() {
		if (managedForm != null) {
			managedForm.dispose();
		}
	}

}
