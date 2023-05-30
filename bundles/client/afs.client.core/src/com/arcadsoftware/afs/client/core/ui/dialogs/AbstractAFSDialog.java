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
package com.arcadsoftware.afs.client.core.ui.dialogs;

import java.util.Optional;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.aev.core.ui.dialogs.ArcadDialog;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.framework.ui.images.ImageManager;

public abstract class AbstractAFSDialog extends ArcadDialog {

	private boolean centered  = true;
	private boolean resizable = true;
	
	public AbstractAFSDialog(Shell parentShell, boolean resizable, boolean centered) {
		this(parentShell,false,resizable,centered);
	}

	public AbstractAFSDialog(Shell parentShell, boolean okButtonOnly,boolean resizable,boolean centered) {
		super(parentShell, okButtonOnly);		
		if (resizable) {
			int style = getShellStyle();
			style = style | SWT.RESIZE | SWT.MAX;
			setShellStyle(style);
		}		
		this.centered = centered;
		this.resizable = resizable;
	}

	@Override
	protected Control createButtonBar(Composite arg0) {
		Composite composite = (Composite)super.createButtonBar(arg0);
		GridLayout l = (GridLayout)composite.getLayout();
		l.marginHeight = l.marginWidth = 3;
		return composite;
	}	

	@Override
	protected void configureShell(Shell newShell) {
		Point size = getSize();
		int width = size.x;
		int height = size.y;
		super.configureShell(newShell);
		if (resizable) {
			newShell.setSize(size);	
		} else {
			newShell.setMinimumSize(size);
		}
		newShell.setText(getTitle());
		if (centered) {
			Rectangle parentBounds = newShell.getDisplay().getPrimaryMonitor().getBounds();
			if(getParentShell() != null)
				parentBounds = getParentShell().getBounds();
			int x = parentBounds.x + (parentBounds.width - width) / 2;
			int y = parentBounds.y + (parentBounds.height - height) / 2;
			newShell.setLocation(x, y);
		}	
		newShell.setImage(getImage());
	}
	
	/**
	 * Get Image: check if application defined an extension for branding image
	 * @return
	 */
	public Image getImage(){
		final IExtensionRegistry registry = Platform.getExtensionRegistry();
		final IConfigurationElement[] elements = registry.getConfigurationElementsFor("com.arcadsoftware.afs.client.branding.dialog.icon");
		final IConfigurationElement selectElement = Stream.of(elements).findFirst().orElse(null);
		if (selectElement != null) {
			final String bundleId = selectElement.getAttribute("bundleid");
			final String path = selectElement.getAttribute("path");
			if (StringUtils.isNotBlank(bundleId) && StringUtils.isNotBlank(path)){
				return Optional.ofNullable(ImageManager.getInstance().getImage(bundleId+":"+path)).orElse(AFSIcon.ARCAD.image());
			}			
		}
		return AFSIcon.ARCAD.image();
	}
	
	public abstract Point getSize();

	public abstract String getTitle();
	
}
