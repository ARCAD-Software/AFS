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
package com.arcadsoftware.afs.client.core.ui.wizards;

import java.util.Map.Entry;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.composites.ConnectedDynamicEditorComposite;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.swt.IEditorChangeListener;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.MandatoryAttribute;
import com.arcadsoftware.metadata.MetaDataAttribute;

public abstract class AbstractConnectedLayoutWizardPage extends AbstractConnectedWizardPage 
implements IEditorChangeListener {
	
	protected ConnectedDynamicEditorComposite editor;


	public AbstractConnectedLayoutWizardPage(ServerConnection connection,
			String pageName, String title, String description) {
		super(connection,pageName, title, description);
	}

	public void screenToBeanMap(BeanMap beanmap) {
		beanmap.addAll(editor.getCurrent());
	}



	@Override
	protected void createControlPage(Composite parent) {
		editor = new ConnectedDynamicEditorComposite(getConnection(),parent,0, getType(), getLayoutName(),false,false){
			@Override
			protected ServerConnection getConnection() {		
				return connection;
			}
		};
		
		Hashtable<String, Object> virtualValues = getVirtualValues();
		if (virtualValues != null) {
			Set<String> keySet = virtualValues.keySet();
			Iterator<String> keys = keySet.iterator();
			while (keys.hasNext()) {
				String key = keys.next();
				Object value = virtualValues.get(key);
				editor.getRenderer().putVirtualValue(key, value);
				
			}
		}	
		editor.createPartControl(getLayoutName());
		
		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalSpan = 3;
		editor.setLayoutData(gridData);
		if ((initalBeanmap != null)){
			editor.load(initalBeanmap);
		} else {
			editor.loadEmptyEntity();
			setFixedValues(editor.getRenderer());	
		}		
		
		editor.getRenderer().addChangeListener(this);
		
		setPageComplete(checkData());		
	}

	public abstract String getType();

	public abstract String getLayoutName();

	protected Hashtable<String, Object> getVirtualValues() {
		return null;
	}
	
	public void setFixedValues(ISWTRenderer renderer){
		
	}
	
	@Override
	protected boolean checkData() {
		if (editor == null)
			return true;
		ISWTRenderer renderer = editor.getRenderer();
		if (!renderer.canSavedEditor(false)) {
			Set<Entry<String, MetaDataAttribute>> attributesEntries = renderer.getStructure().getAttributes().entrySet();
			for (Entry<String, MetaDataAttribute> entry : attributesEntries) {
				if (entry.getValue().isMandatory()) {
					Object attributeValue = renderer.getCurrentBean().get(entry.getKey());
					String name = entry.getValue().getName();
					if (attributeValue != null) {
						if (attributeValue instanceof String && ((String) attributeValue).length() == 0)
							return setMandatoryErrorMessage(name);
					} else
						return setMandatoryErrorMessage(name);
				}
			}
			if (renderer.getMandatoryAttributes() != null) {
				//for (String code : renderer.getMandatoryAttributes()) {
				for (MandatoryAttribute mandatoryAttribute: renderer.getMandatoryAttributes()) {
					String code = mandatoryAttribute.getCode();					
					boolean mandatory = true;
					if (mandatoryAttribute.isConditionned()) {
						String conditionnedBy = mandatoryAttribute.getConditionedBy();
						mandatory= renderer.getCurrentBean().getBoolean(conditionnedBy);
					}
					if (mandatory) {					
						Object attributeValue = renderer.getCurrentBean().get(code);
						String name = renderer.getStructure().getAttribute(code).getName();
						if (attributeValue != null) {
							if (attributeValue instanceof String && ((String) attributeValue).length() == 0)
								return setMandatoryErrorMessage(name);
						} else
							return setMandatoryErrorMessage(name);
					}
				}
			}
			return false;
		}
		setErrorMessage(null);
		return super.checkData();
	}

	public boolean setMandatoryErrorMessage(String name) {
		String message = Activator.resString("msg.error.missingvalue");
		message = String.format(message, name);
		setErrorMessage(message);
		return false;
	}

	public void changed(ISWTRenderer renderer) {
		setPageComplete(checkData());
	}

	@Override
	public void dispose() {
		editor.getRenderer().removeChangeListener(this);
		super.dispose();
	}


	
}
