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
package com.arcadsoftware.afs.client.users.ui.composites;

import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

public class ImportCandidatesCheckViewer extends CheckboxTableViewer {
	
	public ImportCandidatesCheckViewer(Table table){
		super(table);
	}
	
    public static ImportCandidatesCheckViewer newCheckList(Composite parent) {
        Table table = new Table(parent, SWT.CHECK | SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION);
        return new ImportCandidatesCheckViewer(table);
    }
    
	public BeanMapList getCheckedList(){
		BeanMapList list = new BeanMapList();
		Object[] items = getCheckedElements();
		for (Object object : items) {
			if (object instanceof BeanMap){
				list.add((BeanMap)object);
			}
		}		
		return list;		
	}
	
	public void clearAllCheckedElements(){
		setAllChecked(false);
	}
	
}
