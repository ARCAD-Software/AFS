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
package com.arcadsoftware.afs.client.core.ui.composites;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.osgi.framework.Bundle;

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;
import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.actions.AbstractConnectedBasicAction;
import com.arcadsoftware.afs.client.core.ui.managers.QueryManager;
import com.arcadsoftware.afs.client.core.ui.managers.SearchPreferenceManager;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.GreaterThanCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.IsNullCriteria;
import com.arcadsoftware.metadata.criteria.LowerThanCriteria;
import com.arcadsoftware.metadata.criteria.NotCriteria;
import com.arcadsoftware.metadata.xml.XmlCriteriaStream;

public abstract class AbstractSearchComposite extends Composite {

	private static final String AND_END_TAG = "</and>"; //$NON-NLS-1$
	private static final String AND_TAG = "<and>"; //$NON-NLS-1$

	class EnterKeyListener extends KeyAdapter {

		@Override
		public void keyPressed(KeyEvent e) {
			if ((e.keyCode == SWT.CR) || (e.keyCode == SWT.KEYPAD_CR)) {
				searchAction.run();
			}
		}
	}

	protected final SimpleDateFormat dateFormater = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"); //$NON-NLS-1$
	protected ServerConnection connection;
	protected MetaDataEntity entityStructure;
	protected Composite globalComposite;
	protected QueryManager queryManager;
	protected EnterKeyListener enterKeyListener = new EnterKeyListener();
	protected ArcadAction searchAction;
	private Spinner numberOfResult;

	public AbstractSearchComposite(Composite parent, MetaDataEntity structure,ServerConnection connection) {
		super(parent, SWT.BORDER);
		this.entityStructure = structure;
		this.connection = connection;
		this.setBackground(parent.getBackground());
		createContents();
	}

	public QueryManager getQueryManager() {
		return queryManager;
	}
	
	/**
	 * Creation of the content of this composite.
	 */
	public void createContents() {
		queryManager = createQueryManager(connection);
		// Global composite creation
		GridLayout layout = new GridLayout(1, true);
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		layout.marginBottom = 0;
		layout.marginTop = 0;
		layout.marginLeft = 0;
		setLayout(layout);
		if (getParent().getLayout() instanceof GridLayout) {
			setLayoutData(new GridData(GridData.FILL_BOTH));
		}
		globalComposite = GuiFormatTools.createComposite(this, 3, false, SWT.NONE, true);
		globalComposite.setBackground(this.getBackground());
		GridLayout l = (GridLayout) globalComposite.getLayout();
		l.marginHeight = 0;
		l.marginWidth = 0;
		l.marginBottom = 0;
		l.marginTop = 0;
		globalComposite.layout();

		// Call to an abstract method to define the content of the specific
		// part of the editor
		createSearchContents(globalComposite);

		createSearchAction();

		// In any case, we add a "search" button and a "Erase All button"
		if(showButtonbar()) {
		
			Composite buttonBar = new Composite(this, SWT.NONE);
			buttonBar.setBackground(this.getBackground());
			l = new GridLayout(3, false);
			l.marginHeight = 0;
			l.marginWidth =  0;
			l.marginTop = 0;
			l.marginBottom = 0;
			l.marginRight = 0;
			l.marginLeft = 5;
			l.verticalSpacing = 0 ;
			l.horizontalSpacing = 2;
			buttonBar.setLayout(l);
			GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
			gridData.grabExcessHorizontalSpace = true;
			gridData.horizontalSpan = 3;
			gridData.heightHint = 30;
			buttonBar.setLayoutData(gridData);
	
			Button bclear = new Button(buttonBar, SWT.PUSH);
			bclear.setBackground(this.getBackground());
			bclear.setText(Activator.resString("search.button.eraseall")); //$NON-NLS-1$
			bclear.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent arg0) {
					clearAll();
				}
			});
	
			Button bSearch = new Button(buttonBar, SWT.PUSH);
			bSearch.setBackground(this.getBackground());
			bSearch.setText(Activator.resString("search.button.search")); //$NON-NLS-1$
			bSearch.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent arg0) {
					searchAction.run();
				}
			});
	
			Composite composite = new Composite(buttonBar, SWT.NONE);
			composite.setBackground(this.getBackground());
			GridLayout gridLayout = new GridLayout(3, false);
			gridLayout.marginHeight = 0;
			gridLayout.marginWidth = 0;
			composite.setLayout(gridLayout);
	
			numberOfResult = GuiFormatTools.createLabelledSpinner(composite, Activator
					.resString("search.label.resultCount")); //$NON-NLS-1$
			numberOfResult.setBackground(this.getBackground());
			numberOfResult.setMinimum(10);
			numberOfResult.setSelection(getDefaultResultCount());
			numberOfResult.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {				
					SearchPreferenceManager.getInstance().setResultCount(numberOfResult.getSelection());
				}
			});
		}
		activeKeyListener(globalComposite);

	}
	
	protected int getDefaultResultCount() {
		return SearchPreferenceManager.getInstance().getResultCount();
	}
	
	
	protected boolean showButtonbar(){
		return true;
	}
	
	
	/**
	 * Create a QueryManager.<br/>
	 * we delegate the the current AsbtarcSearchComposite to provide
	 * a user-defined errorMessage and a Bundle reference used to display
	 * this message.  
	 * @param connection the current serverConnection
	 * @return a newly created QueryManager
	 */
	protected QueryManager createQueryManager(ServerConnection connection) {
		return new QueryManager(connection) {
			@Override
			protected UserMessage getErrorMessage() {
				return getSearchErrorMessage();
			}
			
			@Override
			protected Bundle getBundle() {
				return getParentBundle();
			}
		};
	}




	private void activeKeyListener(Composite c) {
		Control[] controls = c.getChildren();
		for (int i = 0; i < controls.length; i++) {

			if (controls[i] instanceof Composite) {
				activeKeyListener((Composite) controls[i]);
			} else if (controls[i] instanceof Text) {
				//System.out.println(controls[i].getClass());
				controls[i].addKeyListener(enterKeyListener);
			}
		}

	}

	protected void createSearchAction() {
		searchAction = new AbstractConnectedBasicAction(connection) {
			@Override
			protected boolean execute() {
				String selectClause = createSelectClause();
				if (selectClause != null)
					queryManager.setQuery(entityStructure, selectClause, createSearchClause(), createOrderClause());
				else
					queryManager.setQuery(entityStructure, createSearchClause());
				return true;
			}
			
			public List<Integer> getExpectedRigths() {
				ArrayList<Integer> rights = new ArrayList<Integer>();
				return rights;
			};
		};
	}

	/**
	 * Redefine this method in the inherited classes to define the SQL Where clause.
	 * 
	 * @return String : the Where Clause.
	 */
	public abstract String createSelectClause();
	/**
	 * Redefine this method in the inherited classes to define the specific part of the search editor.
	 * 
	 * @param parent
	 *            : the parent composite
	 */
	protected abstract void createSearchContents(Composite parent);

	/**
	 * Redefine this method in the inherited classes to define the SQL Where clause.
	 * 
	 * @return String : the Where Clause.
	 */
	protected abstract String createSearchClause();

	/**
	 * Redefine this method in the inherited classes to clean search fields
	 * 
	 */
	public abstract void clearAll();

	protected AndCriteria getDateAndCriteria(BeanMap beanMap, String attribute, String minDateAttribute,
			String maxDateAttribute) {
		AndCriteria and = null;
		Date dateinf = (Date) beanMap.get(minDateAttribute);
		Date datesup = (Date) beanMap.get(maxDateAttribute);
		if (dateinf != null) {
			and = new AndCriteria();
			Calendar cal = Calendar.getInstance();
			cal.setTime(dateinf);
			cal.set(Calendar.HOUR, 0);
			cal.set(Calendar.MINUTE, 0);
			cal.set(Calendar.SECOND, 0);
			dateinf = cal.getTime();
			GreaterThanCriteria c1 = new GreaterThanCriteria(attribute, dateFormater.format(dateinf));
			and.add(c1);
		}
		if ((datesup != null)) {
			if (and == null)
				and = new AndCriteria();
			Calendar cal = Calendar.getInstance();
			cal.setTime(datesup);
			cal.set(Calendar.HOUR, 23);
			cal.set(Calendar.MINUTE, 59);
			cal.set(Calendar.SECOND, 59);
			datesup = cal.getTime();
			LowerThanCriteria c2 = new LowerThanCriteria(attribute, dateFormater.format(datesup));
			and.add(c2);
		}
		return and;
	}

	protected AndCriteria getInProgressAndCriteria(BeanMap beanMap, String attribute, String inProgressAttribute,
			boolean defaultValue) {
		AndCriteria andInProgress = null;
		boolean inprogress = defaultValue;
		if (beanMap.get(inProgressAttribute) != null)
			inprogress = beanMap.getBoolean(inProgressAttribute);

		if (inprogress) {
			IsNullCriteria close = new IsNullCriteria(attribute);
			andInProgress = new AndCriteria();
			andInProgress.add(new NotCriteria(close));
		}
		return andInProgress;
	}

	protected String criteriaToXml(ISearchCriteria[] list) {
		StringBuilder builder = null;
		XmlCriteriaStream x = new XmlCriteriaStream();
		for (int i = 0; i < list.length; i++) {
			ISearchCriteria c = list[i];
			if (c != null) {
				if (builder == null)
					builder = new StringBuilder();
				builder.append(x.toXML(c));
			}
		}
		if (builder != null) {
			StringBuilder result = new StringBuilder();
			result.append(AND_TAG);
			result.append(builder);
			result.append(AND_END_TAG);
			return result.toString();
		}
		return ""; //$NON-NLS-1$
	}

	protected String createOrderClause() {
		return ""; //$NON-NLS-1$
	}
	
	/**
	 * A user defined error message that will be displayed if an error
	 * occurred during retrieving data
	 * @return an error Message
	 */
	protected abstract UserMessage getSearchErrorMessage();
	
	/**
	 * Returns the parent bundle used to display the message
	 * If this reference is null, the internal activator will be used instaed. 
	 * @return a bunble reference
	 */
	protected abstract Bundle getParentBundle();
	
	
	public void search(){
		searchAction.run();
	}
	
	public ServerConnection getConnection() {
		return connection;
	}
}
