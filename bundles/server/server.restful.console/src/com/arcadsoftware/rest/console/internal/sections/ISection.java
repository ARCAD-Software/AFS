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
package com.arcadsoftware.rest.console.internal.sections;

import org.osgi.framework.Bundle;

import com.arcadsoftware.rest.console.IRestConsoleSection;
import com.arcadsoftware.rest.console.internal.Activator;

/**
 * Section services that are implemented within other Bundles.
 * 
 * <p>
 * These services are started and stopped when the corresponding Bunble is started or stopped. First implementation
 * of such sections are the declarative sections (declared into "consoles.xml" files). 
 * 
 * @author ARCAD Software
 */
public interface ISection extends IRestConsoleSection {

	public static final String PROPERTIES = "PROPERTIES"; //$NON-NLS-1$

	public void setBundle(Bundle bundle);
	
	public void setdefaultMessageFile(String sectionsParent);
	
	public void setActivator(Activator activator);

}
