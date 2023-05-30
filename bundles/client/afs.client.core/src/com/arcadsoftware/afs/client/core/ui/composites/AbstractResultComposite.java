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
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumns;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTableLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTreeLabelProvider;
import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.listeners.IBeanMapProvider;
import com.arcadsoftware.afs.client.core.model.BeanMapCollectionItem;
import com.arcadsoftware.afs.client.core.tools.MetadataUtils;
import com.arcadsoftware.afs.client.core.ui.actions.IBeanMapActionListener;
import com.arcadsoftware.afs.client.core.ui.actions.IListActions;
import com.arcadsoftware.afs.client.core.ui.listeners.ISearchListener;
import com.arcadsoftware.afs.client.core.ui.managers.QueryManager;
import com.arcadsoftware.afs.framework.ui.viewers.BeanMapListTableViewer;
import com.arcadsoftware.afs.framework.ui.viewers.BeanMapListTreeViewer;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractResultComposite extends Composite 
implements IListActions, ISearchListener, IBeanMapActionListener { 

	private  AbstractColumnedViewer viewer;
	
	protected MetaDataEntity entityStructure;
	BeanMap selectedBeanMap;
	Composite globalCmp;
	protected String selectClause = null;
	Composite infoPanel;
	Label infoLabel;
	private Button previousButton;
	private Button nextButton;
	protected QueryManager queryManager;
	private Label pageCountLabel;
	
	protected ServerConnection connection;
	protected DataAccessHelper helper;
	
	
	protected boolean autosearch = true; 
	
	
	protected boolean showInfo= true;
	
	

	public AbstractResultComposite(Composite parent, MetaDataEntity entityStructure, QueryManager queryManager,
			ServerConnection connection) {
		super(parent, SWT.NONE);
		this.entityStructure = entityStructure;
		this.queryManager = queryManager;
		this.connection = connection;
		helper = new DataAccessHelper(connection);
		showInfo = displayInfo();
		createContent();
	}

	public AbstractResultComposite(Composite parent, MetaDataEntity entityStructure, String selectClause,
			QueryManager queryManager,ServerConnection connection) {
		super(parent, SWT.NONE);
		this.entityStructure = entityStructure;
		this.selectClause = selectClause;
		this.queryManager = queryManager;
		this.connection = connection;
		helper = new DataAccessHelper(connection);
		showInfo = displayInfo();
		createContent();
	}

	public AbstractResultComposite(Composite parent, String entityType, String selectClause,ServerConnection connection) {
		super(parent, SWT.NONE);		
		this.selectClause = selectClause;	
		this.connection = connection;
		this.queryManager = new QueryManager(connection);
		helper = new DataAccessHelper(connection);
		this.entityStructure = helper.getEntity(entityType);
		queryManager.setResultListener(this);
		showInfo = displayInfo();
		createContent();
	}
	
	public AbstractResultComposite(Composite parent, String entityType, ServerConnection connection) {
		super(parent, SWT.NONE);			
		this.connection = connection;
		this.queryManager = new QueryManager(connection);
		helper = new DataAccessHelper(connection);
		this.entityStructure = helper.getEntity(entityType);
		queryManager.setResultListener(this);
		showInfo = displayInfo();
		createContent();
	}	
	
	
	public QueryManager getQueryManager() {
		return queryManager;
	}
	
	public void setQueryManager(QueryManager queryManager) {
		this.queryManager = queryManager;
	}
	
	public MetaDataEntity getEntityStructure() {
		return entityStructure;
	}
	
	protected boolean getDisplayAsTree(){
		return false;
	}
	
	protected int getViewerStyle(){
		return SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI;
	}
	protected AbstractColumnedViewer createTableViewer(Composite parent){
		AbstractColumnedViewer myViewer = new BeanMapListTableViewer(parent, getViewerStyle(), entityStructure,
				getDisplayedSelectClause()) {
			@Override
			protected void doOnDoubleClick(IStructuredSelection selection) {
				doOnDoubleClickEvent(selection);
			}

			@Override
			protected void doOnSelectionChange(IStructuredSelection selection) {
				if (!selection.isEmpty()) {
					if (selection.getFirstElement() instanceof BeanMap){
						selectedBeanMap = (BeanMap) selection.getFirstElement();
						doOnClickEvent(selectedBeanMap);
					}
				}
				setActionsEnability(this.getActions());
			}

			@Override
			public String getIdentifier() {
				return getViewerIdentifier();
			}

			@Override
			public AbstractColumnedTableLabelProvider createTableLabelProvider(AbstractColumnedViewer newViewer) {
				AbstractColumnedTableLabelProvider provider = createSpecificTableLabelProvider(newViewer);
				if (provider == null)
					provider = super.createTableLabelProvider(newViewer);
				return provider;
			}

			@Override
			public IContentProvider createContentProvider() {
				IContentProvider provider = createSpecificContentProvider();
				if (provider==null)
					return super.createContentProvider();
				return provider;
			}			
			
			@Override
			public SimpleDateFormat getDateFormatter() {
				SimpleDateFormat sd = AbstractResultComposite.this.getDateFormatter();
				if (sd != null)
					return sd;
				return super.getDateFormatter();
			}
			@Override
			protected String getColumnHeader(String attribute) {
				String header = AbstractResultComposite.this.getColumnHeader(attribute);
				if (header!=null)
					return header;
				return super.getColumnHeader(attribute);
			};
			
			@Override
			protected int getColumnSize(String attribute) {
				int size = AbstractResultComposite.this.getColumnSize(attribute);
				if (size!=-1)
					return size;
				return super.getColumnSize(attribute);
			};			
			
			@Override
			protected Image getCustomColumnImage(Object element,
					int actualColumnIndex) {
				Image image = AbstractResultComposite.this.getCustomColumnImage(element,actualColumnIndex);
				if (image!=null)
					return image;
				return super.getCustomColumnImage(element, actualColumnIndex);
			}
			
			@Override
			protected List<Action> getNextActions() {
				setActionsEnability(this.getActions());
				return super.getNextActions();
			}			
			
			@Override
			public ArcadColumns getReferenceColumns() {
				ArcadColumns userDefinedColumns = getUserDefinedReferenceColumns();
				if (userDefinedColumns==null){				
					return super.getReferenceColumns();
				}
				return userDefinedColumns;
			}
			
			@Override
			public String getValue(Object element, int columnIndex) {
				String attribute = positionToAttribute(columnIndex);
				String value = AbstractResultComposite.this.getValue(element, attribute, columnIndex);
				if (value==null) {
					return super.getValue(element, columnIndex);
				} else {
					return value;
				}
			}			
			
			@Override
			protected boolean adaptActionToSelection(Action action) {
				return AbstractResultComposite.this.adaptActionToSelection(action);
			}

			
			
			
		};
		return myViewer;
	}
	
	protected AbstractColumnedViewer createTreeViewer(Composite parent){
		AbstractColumnedViewer myViewer = new BeanMapListTreeViewer(parent, getViewerStyle(), entityStructure,
				getDisplayedSelectClause()) {
			@Override
			protected void doOnDoubleClick(IStructuredSelection selection) {
				doOnDoubleClickEvent(selection);
			}

			@Override
			protected void doOnSelectionChange(IStructuredSelection selection) {
				if (!selection.isEmpty()) {
					if (selection.getFirstElement() instanceof BeanMap){
						selectedBeanMap = (BeanMap) selection.getFirstElement();
						doOnClickEvent(selectedBeanMap);
					}
					if (selection.getFirstElement() instanceof BeanMapCollectionItem){
						BeanMapCollectionItem bci = (BeanMapCollectionItem)selection.getFirstElement();
						selectedBeanMap = bci.getBeanMap();
						selectedBeanMap.put("_item", bci);
						doOnClickEvent(selectedBeanMap);
					}					
					AbstractResultComposite.this.doOnSelectionChange(selection);
				}
			}

			@Override
			public String getIdentifier() {
				return getViewerIdentifier();
			}

			@Override
			public AbstractColumnedTreeLabelProvider createTreeLabelProvider(AbstractColumnedViewer newViewer) {
				 AbstractColumnedTreeLabelProvider provider = createSpecificTreeLabelProvider(newViewer);
				if (provider == null)
					provider =super.createTreeLabelProvider(newViewer);
				return provider;
			}			

			@Override
			public IContentProvider createContentProvider() {
				IContentProvider provider = createSpecificContentProvider();
				if (provider==null)
					return super.createContentProvider();
				return provider;
			}
			
			@Override
			public SimpleDateFormat getDateFormatter() {
				SimpleDateFormat sd = AbstractResultComposite.this.getDateFormatter();
				if (sd != null)
					return sd;
				return super.getDateFormatter();
			}
			@Override
			protected String getColumnHeader(String attribute) {
				String header = AbstractResultComposite.this.getColumnHeader(attribute);
				if (header!=null)
					return header;
				String newAttribute = AbstractResultComposite.this.transformColumnHeader(attribute);
				return super.getColumnHeader(newAttribute);
			};
			
			@Override
			protected int getColumnSize(String attribute) {
				int size = AbstractResultComposite.this.getColumnSize(attribute);
				if (size!=-1)
					return size;
				return super.getColumnSize(attribute);
			};				
			
			@Override
			public String getValue(Object element, int columnIndex) {
				String attribute = attributeFromPosition(columnIndex);
				String value = AbstractResultComposite.this.getValue(element, attribute, columnIndex);
				if (value==null) {
					return super.getValue(element, columnIndex);
				} else {
					return value;
				}
			}
			
			@Override
			protected Image getCustomColumnImage(Object element,int actualColumnIndex) {
				Image image = AbstractResultComposite.this.getCustomColumnImage(element,actualColumnIndex);
				if (image!=null)
					return image;
				return super.getCustomColumnImage(element, actualColumnIndex);
			}
			
			@Override
			public ArcadColumns getReferenceColumns() {
				ArcadColumns userDefinedColumns = getUserDefinedReferenceColumns();
				if (userDefinedColumns==null){				
					return super.getReferenceColumns();
				}
				return userDefinedColumns;
			}

			@Override
			protected List<Action> getNextActions() {
				setActionsEnability(this.getActions());
				return super.getNextActions();
			}
			
			@Override
			protected boolean adaptActionToSelection(Action action) {
				return AbstractResultComposite.this.adaptActionToSelection(action);
			}
			
		};
		return myViewer;
	}	
	
	
	protected ArcadColumns getUserDefinedReferenceColumns() {
		return null;
	}
	
	protected AbstractColumnedViewer createViewer(Composite parent){
		if (getDisplayAsTree())
			return createTreeViewer(parent);
		else
			return createTableViewer(parent);
	}
	
	protected void setActions(List<Action> actions){
		if (viewer instanceof BeanMapListTreeViewer) {
			((BeanMapListTreeViewer)viewer).setActions(actions);
		}
		if (viewer instanceof BeanMapListTableViewer) {
			((BeanMapListTableViewer)viewer).setActions(actions);
		}		
	}
	
	/**
	 * Disable actions here if necessary, according to selected beanmaps for instance
	 */
	protected void setActionsEnability(List<Action> actions) {
		// nothing by default
	}
	
	protected Composite createViewerComposite(Composite parent) {
		return parent;
	}
	
	protected void addContentBeforeViewer(Composite parent) {

	}
	
	protected void addContentAfterViewer(Composite parent) {

	}	
	
	protected boolean displayInfo(){
		return queryManager == null || (queryManager != null &&
				(!queryManager.isUserDefinedCount() || 
				(queryManager.isUserDefinedCount() && queryManager.getUserDefinedCount() > -1)));
	}
	
	protected void createContent() {
		GridLayout layout = new GridLayout(1, true);
		layout.marginHeight = layout.marginWidth = 0;
		setLayout(layout);
		setLayoutData(new GridData(GridData.FILL_BOTH));
		globalCmp = GuiFormatTools.createComposite(this, 3, false, SWT.NONE, true);
		GridLayout l = (GridLayout) globalCmp.getLayout();
		l.marginHeight = l.marginWidth = 0;
		
		globalCmp.layout();

		
		
		Composite viewerComposite =createViewerComposite(globalCmp); 
		
		addContentBeforeViewer(viewerComposite);
		viewer = createViewer(viewerComposite);
		addContentAfterViewer(viewerComposite);
		
		setActions(getActions());
		
		
		
		viewer.getViewer().getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
		
		if (showInfo){
			infoPanel = GuiFormatTools.createComposite(globalCmp, 12, false, SWT.BORDER, true);
			GridLayout gl =  (GridLayout)infoPanel.getLayout();
			gl.marginHeight = gl.marginWidth = 0;
			gl.marginRight = 0;
			gl.marginTop = gl.marginBottom = gl.marginLeft = gl.marginRight = 0;
			gl.marginLeft = 0;
			gl.verticalSpacing = 0 ;
			gl.horizontalSpacing = 0;		
			
			
			//gl.verticalSpacing = 0;		
			((GridLayout) infoPanel.getLayout()).horizontalSpacing = 0;
			GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
			gridData.grabExcessHorizontalSpace = true;
			gridData.horizontalSpan = 3;
			gridData.heightHint = 25;
	
			infoPanel.setLayoutData(gridData);
	
			Composite labelComposite = GuiFormatTools.createComposite(infoPanel, 3, false, SWT.NONE, true);
			((GridLayout) labelComposite.getLayout()).marginHeight = 5;
	
	
			infoLabel = createLabelledLabel(labelComposite, 
					Activator.resString("search.label.resultCount")); //$NON-NLS-1$
	
			Composite pagesComposite = GuiFormatTools.createComposite(infoPanel, 3, false, SWT.NONE, true);
			((GridLayout) pagesComposite.getLayout()).marginHeight = 5;
	
			pageCountLabel = createLabelledLabel(pagesComposite, 
					Activator.resString("search.label.pageCount")); //$NON-NLS-1$
	
			previousButton = GuiFormatTools.createButton(infoPanel, 
					Activator.resString("search.button.previous"), GridData.VERTICAL_ALIGN_BEGINNING, 3, 100); //$NON-NLS-1$
	
			previousButton.setEnabled(false);
			((GridData) previousButton.getLayoutData()).horizontalAlignment = SWT.END;
			previousButton.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					queryManager.executePreviousQuery();
				}
			});
	
			nextButton = GuiFormatTools.createButton(infoPanel, 
					Activator.resString("search.button.next"), //$NON-NLS-1$
					GridData.VERTICAL_ALIGN_BEGINNING | GridData.HORIZONTAL_ALIGN_END , 3, 100);
			nextButton.setEnabled(false);
			((GridData) nextButton.getLayoutData()).horizontalAlignment = SWT.END;
			nextButton.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					queryManager.executeNextQuery();
				}
			});
			if (isSearchManagementActivated()) {
				IDialogSettings ds = getSettingSession();
				if (ds!=null) {
					Object o = ds.get("autosearch");
					if (o==null) {
						setAutosearch(true);	
					} else {
						autosearch = ds.getBoolean("autosearch");
					}
				} else {
					
				}
			}
		}
	}

	public static Label createLabelledLabel(Composite parent, String label) {
		Label textLabel = new Label(parent, SWT.NONE | SWT.WRAP);
		GridData gridData = new GridData(SWT.CENTER, SWT.CENTER, false, false);
		textLabel.setLayoutData(gridData);

		textLabel.setText(label);
		Label twopoints = new Label(parent, SWT.NONE);
		twopoints.setText(":"); //$NON-NLS-1$
		gridData = new GridData(SWT.BEGINNING, SWT.CENTER, false, false);
		twopoints.setLayoutData(gridData);

		Label lbl = new Label(parent, SWT.NONE);
		lbl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		lbl.setData(textLabel);
		return lbl;
	}

	/**
	 * Returns a column Header label computed from the attribute.
	 * <p>
	 * This function first calls the  {@link #useDefaultColumnHeader(String) to know
	 * if you want to manually manage the return of the value. 
	 * </p>
	 * <p>
	 * If the call to {@link #useDefaultColumnHeader(String) returns false,
	 * this method resolves the final  MetadataAtribute and returns its name. 
	 * </p>
	 * @param attribute
	 * @return the colunm label
	 */
	protected String getColumnHeader(String attribute) {
		if (useDefaultColumnHeader(attribute)){
			return null;
		}		
		MetaDataAttribute metaDataAttribute =  
				MetadataUtils.getInstance().resolveMetaDataAttribute(helper,entityStructure,attribute);
		if (metaDataAttribute!=null) {
			return metaDataAttribute.getName();			
		}
		return null;
	}
	/**
	 * Returns an indicator to say if you want the system to compute
	 * the column header using the primary entity.
	 * If return true, the system will try to compute the header using
	 * the attribute and the primary entity</br>
	 * If return false, the system will try to compute the value using the 
	 * deeper entity and the attribute.
	 * <p>
	 * Example:<br>
	 * <ul>
	 * <li>if the primary entity is <code>artifact</code></li>
	 * <li>if the attribute equals to <code>creationrelease.number</code></br></li>
	 * </ul>
	 * <p> 
	 * 
	 * if the method returns false: the system will return the label of the
	 * <code>number</code> attribute into the entity <code>release</code> (because
	 * the type of <code>creationrelease</code> is <code>release</code>).
	 * </p>
	 * <p>
	 * If this method return true, the system will return the label of the
	 * <code>creationrelease.number</code> attribute into the entity <code>artefact</code><br>
	 * In this case, you can also transform the attribute name before trying to resolve
	 * the name by using the {@link #transformColumnHeader(String)}.
	 * </p>
	 * </p>
	 *   
	 * @param attribute
	 * @return
	 */
	protected boolean useDefaultColumnHeader(String attribute){
		return false;
	}
	
	protected String transformColumnHeader(String attribute){
		return attribute;
	}
	
	protected int getColumnSize(String attribute) {
		MetaDataAttribute metaDataAttribute =  
				MetadataUtils.getInstance().resolveMetaDataAttribute(helper,entityStructure,attribute);
		if (metaDataAttribute!=null) {
			return metaDataAttribute.getColSize();			
		}
		return -1;
	}
	
	public ServerConnection getConnection(){
		return connection;
	}
	
	/**
	 * Return the viwer selection
	 * @return
	 */
	public IStructuredSelection getViewerSelection() {
		return viewer.getSelection();
	}

	/**
	 * Returns the current selected BeanMaps into a BeanMapList
	 * @return the BeanMapList that contains all the selected elements of the list.
	 */
	public BeanMapList getSelectedBeanMap() {
		IStructuredSelection selection = viewer.getSelection();
		Iterator<?> it = selection.iterator();
		BeanMapList result = new BeanMapList();
		while (it.hasNext()) {
			Object o = it.next();
			if (o instanceof BeanMap) {
				result.add((BeanMap) o);
			} else if (o instanceof IBeanMapProvider) {
				result.add(((IBeanMapProvider) o).providedBeanMap());
			}
			
		}
		return result;
	}

	/**
	 * Returns the first selected beanMap. 
	 * @return The selected beanMap
	 */
	public BeanMap getSelectedResult() {
		return selectedBeanMap;
	}

	/**
	 * Uses his method to change the viewer Input
	 * @param list the new BeanMapList you want to define as the current input
	 */
	
	public void contentChanged(BeanMapList list) {
		setInput(list);
		viewer.refresh();
		if (showInfo) {
			previousButton.setEnabled(queryManager.canReadPreviousResults());
			nextButton.setEnabled(queryManager.canReadNextResults());
		}
	}
	
	public Object getInput(){
		return viewer.getInput();
	}
	
	public void setInput(Object list){
		viewer.setInput(list);
	}
	
	public void setElementCount(int count, int currentPage, int numberOfPages) {
		if (showInfo) {
			
			if (currentPage<numberOfPages) {
				infoLabel.setBackground(Activator.getDefault().getColor("blue"));
				infoLabel.setText(String.valueOf(count) +" "+Activator.resString("search.label.pending"));
			} else {
				infoLabel.setBackground(infoLabel.getParent().getBackground());
				infoLabel.setText(String.valueOf(count));
			}
			pageCountLabel.setText(String.valueOf(currentPage) + '/' + String.valueOf(numberOfPages));
		}
	}	
	
	/**
	 * Returns the underlying viewer
	 * @return
	 */
	public AbstractColumnedViewer getViewer() {
		return viewer;
	}

	/**
	 * Override this method to define you own viwer Identifier 
	 * @return the new Viwer Identifier
	 */
	public String getViewerIdentifier() {
		//return getClass().getName();
		return null;
	}

	/**
	 * Define the action that will be available for the BeanMapList
	 */
	public abstract List<Action> getActions();
	
	/**
	 * Override this method to define the behavior when the user
	 * double click on an element on the list
	 * @param selection The current selection selection 
	 */
	protected void doOnDoubleClickEvent(IStructuredSelection selection) {
	}

	/**
	 * Override this method to define the behavior when the user
	 * click on an element on the list
	 * @param selection The current selection selection 
	 */
	protected void doOnClickEvent(BeanMap selected) {
	}	
	
	/**
	 * Override this method to define the behavior when the user
	 * click on an element on the list
	 * @param selection The current selection selection 
	 */
	protected void doOnSelectionChange(IStructuredSelection selection) {
	}	
		
	
	
	/**
	 * Override this method to define your own AbstractColumnedTableLabelProvider
	 * @param newViewer the current BeanMapListTableViewer
	 * @return your own AbstractColumnedTableLabelProvider
	 */
	public AbstractColumnedTableLabelProvider createSpecificTableLabelProvider(AbstractColumnedViewer newViewer) {
		return null;
	}

	/**
	 * Override this method to define your own AbstractColumnedTableLabelProvider
	 * @param newViewer the current BeanMapListTableViewer
	 * @return your own AbstractColumnedTableLabelProvider
	 */
	public AbstractColumnedTreeLabelProvider createSpecificTreeLabelProvider(AbstractColumnedViewer newViewer) {
		return null;
	}	
	
	public IContentProvider createSpecificContentProvider() {
		return null;
	}	

	protected Image getCustomColumnImage(Object element, int actualColumnIndex){
		return null;
	}	
	
	/**
	 * Override this method to define your own SimpleDateFormatter that will 
	 * be used to display the the Date BeanMap Value
	 * @return your own SimpleDateFormat object
	 */
	protected SimpleDateFormat getDateFormatter() {
		return null;
	}	

	public String getValue(Object element, String attribute, int columnIndex){
		return null;
	}
	
	protected String getDisplayedSelectClause() {
		return createSelectClause();		
	}
	
	
	protected String createSelectClause() {
		return selectClause;
	}	
	
	public void add(BeanMap beanMap){
		if (getInput() instanceof BeanMapList) {
			BeanMapList list = (BeanMapList)getInput();
			list.add(0,beanMap);
			viewer.refresh();
		}
	}
	public void add(BeanMap beanMap, int index){
		if (getInput() instanceof BeanMapList) {
			BeanMapList list = (BeanMapList)getInput();
			if (index >= 0 && index < list.size())
				list.add(index, beanMap);
			else
				list.add(beanMap);
			viewer.refresh();
		}
	}
	
	public void addUnique(BeanMap beanMap){
		if (getInput() instanceof BeanMapList) {
			BeanMapList list = (BeanMapList)getInput();
			if (-1 == list.findIndex(beanMap.getId())){
				list.add(0,beanMap);
				viewer.refresh();
			}			
		}
	}
	
	public void add(BeanMapList beanMaps){
		if (getInput() instanceof BeanMapList) {
			BeanMapList list = (BeanMapList)getInput();
			list.addAll(0,beanMaps);
			viewer.refresh();
		}
		
	}
	
	/**
	 * Add BeanMaps after checking they are not already in list
	 * @param beanMaps list of beanMaps to be added
	 */
	public void addUnique(BeanMapList beanMaps){
		if (getInput() instanceof BeanMapList) {
			BeanMapList list = (BeanMapList)getInput();
			for (BeanMap beanMap : beanMaps) {
				if (-1 == list.findIndex(beanMap.getId())){
					list.add(0,beanMap);
				}
			}
			viewer.refresh();
		}
		
	}
	
	public void remove(BeanMap beanMap){
		if (getInput() instanceof BeanMapList) {
			BeanMapList list = (BeanMapList)getInput();
			list.remove(beanMap);
			viewer.refresh();
			removeBeanEditor(beanMap);
		}		
	}	
	
	public void remove(BeanMapList toRemove){
		if (getInput() instanceof BeanMapList) {
			BeanMapList list = (BeanMapList)getInput();
			for (BeanMap b :toRemove) {
				list.remove(b);
				removeBeanEditor(b);
			}
			viewer.refresh();
		}		
	}
	
	public void actionDone(int type, BeanMap bean) {
		switch (type) {
		case IBeanMapActionListener.ACTION_ADD:
			BeanMap completed = helper.read(bean.getType(), bean.getId(), getAttributList());
			bean.addAll(completed);
			add(bean);
			break;
		case IBeanMapActionListener.ACTION_DELETE:
			remove(bean);
			break;
		case IBeanMapActionListener.ACTION_RELOAD:
			// reload list
			queryManager.executeCurrentQuery();
			break;
		case IBeanMapActionListener.ACTION_REFRESH:
			if (viewer != null && !viewer.getControl().isDisposed())
				viewer.refresh();
			break;
		default:
			break;
		}
	}
 
	
	public void actionDone(int type, BeanMapList list) {
		switch (type) {
		case IBeanMapActionListener.ACTION_ADD:
			for (BeanMap bean : list) {
				BeanMap completed = helper.read(bean.getType(), bean.getId(), getAttributList());
				bean.addAll(completed);
			}			
			add(list);
			break;
		case IBeanMapActionListener.ACTION_DELETE:
			remove(list);
			break;
		case IBeanMapActionListener.ACTION_RELOAD:
			// reload list
			queryManager.executeCurrentQuery();
			break;
		case IBeanMapActionListener.ACTION_REFRESH:
			if (viewer != null && !viewer.getControl().isDisposed())
				viewer.refresh();
			break;
		default:
			break;
		}
	}
	
	public boolean isSearchManagementActivated() {
		return true;
	}
	
	protected String getAttributList(){
		return selectClause;
	}
	
	public void setAutosearch(boolean autosearch) {
		this.autosearch = autosearch;
		IDialogSettings ds = getSettingSession();
		if (ds!=null) {
			ds.put("autosearch", autosearch);
		}
	}
	
	public boolean isAutosearch() {
		return autosearch;
	}
	
	
	private IDialogSettings getSettingSession(){
		AbstractUIPlugin activator = getActivator();
		if (activator!=null) {
			String sessionName = entityStructure.getName().trim()+".search.settings";
			IDialogSettings pluginSetting = activator.getDialogSettings();
			IDialogSettings session = pluginSetting.getSection(sessionName);
			if (session==null)
				session = pluginSetting.addNewSection(sessionName);
			return session;
		}
		return null;		
	}
	
	
	public AbstractUIPlugin getActivator(){
		return null;
	}
	
	@Override
	public Composite getParentComposite() {
		return this;
	}
	
	protected boolean  adaptActionToSelection(Action action) {
		return true;
	}
	
	/**
	 * Force beanMap selection
	 * @param selectedBeanMap
	 */
	public void setSelectedBeanMap(BeanMap selectedBeanMap) {
		viewer.getViewer().setSelection(
			new StructuredSelection(selectedBeanMap)
		);
	}

	/**
	 * Force BeanMap List selection
	 * @param selectedBeanMap
	 */
	public void setSelectedBeanMapList(BeanMapList selectedBeanMapList) {
		viewer.getViewer().setSelection(
			new StructuredSelection(selectedBeanMapList.toArray())
		);
	}
	
	/**
	 * Remove BeanMap editor if exists after change occurred in list. 
	 * Empty by default. Must be redefined to be filled in inheriting classes.
	 * @param bean
	 */
	protected void removeBeanEditor(BeanMap bean){
	}
	
	/**
	 * Refresh BeanMap editor if exists after change occurred in list. 
	 * Empty by default. Must be redefined to be filled in inheriting classes.
	 * @param bean
	 */
	protected void refreshBeanEditor(BeanMap bean){		
	}
}
