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

import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.core.databinding.observable.value.AbstractObservableValue;
import org.eclipse.core.databinding.validation.ValidationStatus;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.IMessageManager;

import com.arcadsoftware.metadata.Element;

public class MessageStatusObservable extends AbstractObservableValue<IStatus> {

	private static final String EMPTY = ""; //$NON-NLS-1$
	private IStatus status = null;
	private Element element;
	private IMessageManager manager;
	private Control control;

	public MessageStatusObservable(Element element, IMessageManager manager, Control control) {
		super(Realm.getDefault());
		this.element = element;
		this.manager = manager;
		this.control = control;
	}

	@Override
	protected IStatus doGetValue() {
		return status;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.core.databinding.observable.value.IObservableValue#getValueType()
	 */
	public Object getValueType() {
		return IStatus.class;
	}

	@Override
	protected void doSetValue(IStatus value) {
		if (value!=null) {
			if (value instanceof IStatus) {
				IStatus newstatus = (IStatus) value;
				try {
					if (ValidationStatus.ok().equals(newstatus)) {
						manager.removeMessage(element, control);
					} else {
						switch (newstatus.getSeverity()) {
						case IStatus.CANCEL:
						case IStatus.ERROR:
							manager.addMessage(element, EMPTY, null, IMessageProvider.ERROR, control);
							break;
						case IStatus.WARNING:
							manager.addMessage(element, EMPTY, null, IMessageProvider.WARNING, control);
							break;
						case IStatus.INFO:
							manager.addMessage(element, EMPTY, null, IMessageProvider.INFORMATION, control);
							break;
						default:
							manager.addMessage(element, EMPTY, null, IMessageProvider.NONE, control);
						}
					}
				} finally {
					status = newstatus;
				}
			}
		}
	}

}
