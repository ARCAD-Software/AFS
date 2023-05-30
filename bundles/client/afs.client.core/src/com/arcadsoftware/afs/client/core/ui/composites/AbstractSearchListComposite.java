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
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.action.Action;
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
import org.osgi.framework.Bundle;

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTableLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTreeLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.ColumnedDefaultTableLabelProvider;
import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.tools.MetadataUtils;
import com.arcadsoftware.afs.client.core.ui.actions.AbstractConnectedBasicAction;
import com.arcadsoftware.afs.client.core.ui.actions.IBeanMapActionListener;
import com.arcadsoftware.afs.client.core.ui.actions.IListActions;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapListContentChangedListener;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapSelectionListener;
import com.arcadsoftware.afs.client.core.ui.managers.QueryManager;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;
import com.arcadsoftware.afs.framework.ui.viewers.BeanMapListTableViewer;
import com.arcadsoftware.afs.framework.ui.viewers.BeanMapListTreeViewer;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractSearchListComposite extends Composite 
implements IListActions, IBeanMapListContentChangedListener, IBeanMapActionListener{

	private ListenerList<IBeanMapSelectionListener> beanMapSelectionListener = new ListenerList<IBeanMapSelectionListener>();
	
	//private BeanMapListTableViewer viewer;
	private  AbstractColumnedViewer viewer;
	
	protected MetaDataEntity entityStructure;
	BeanMap selectedBeanMap;
	Composite globalCmp;

	protected QueryManager queryManager;
	
	protected ArcadAction searchAction;		
	protected ServerConnection connection;
	protected DataAccessHelper helper;
	
	private boolean displayInformationPanel  =true;
	Composite infoPanel;
	Label infoLabel;
	private Button previousButton;
	private Button nextButton;
	private Label pageCountLabel;
	
	private SearchKeyListener searchKeyListener = null;

	public AbstractSearchListComposite(Composite parent, MetaDataEntity entityStructure, 
			ServerConnection connection,boolean displayInformationPanel) {
		super(parent, SWT.NONE);
		this.entityStructure = entityStructure;
		this.connection = connection;
		helper = new DataAccessHelper(connection);
		this.displayInformationPanel = displayInformationPanel;
		createContent();
		createSearchAction();
	}
	
	public AbstractSearchListComposite(Composite parent, String entityType, 
			ServerConnection connection,boolean displayInformationPanel) {
		super(parent, SWT.NONE);		
		this.connection = connection;
		helper = new DataAccessHelper(connection);
		this.entityStructure = helper.getEntity(entityType);
		this.displayInformationPanel = displayInformationPanel;
		createContent();
		createSearchAction();
	}
	
	
	
	public AbstractSearchListComposite(Composite parent, MetaDataEntity entityStructure, 
			ServerConnection connection) {
		this(parent,entityStructure,connection,true );
	}

	public AbstractSearchListComposite(Composite parent, String entityType, 
			ServerConnection connection) {
		this(parent,entityType,connection,true );
	}
	
	protected void setActions(List<Action> actions){
		if (viewer instanceof BeanMapListTreeViewer) {
			((BeanMapListTreeViewer)viewer).setActions(actions);
		}
		if (viewer instanceof BeanMapListTableViewer) {
			((BeanMapListTableViewer)viewer).setActions(actions);
		}		
	}
	
	protected AbstractColumnedViewer createTableViewer(Composite parent){
		int style = SWT.BORDER | SWT.FULL_SELECTION;
		if (enableMultiSelection()) {
			style = style | SWT.MULTI;
		}
		BeanMapListTableViewer viewer = new BeanMapListTableViewer(parent, style, entityStructure,
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
						fireBeanMapSelection(selectedBeanMap);
						doOnSelection(selectedBeanMap);
					}
				}
				onSelectionChange(selection);
			}

			@Override
			public String getIdentifier() {
				return getViewerIdentifier();
			}

			@Override
			public AbstractColumnedTableLabelProvider createTableLabelProvider(AbstractColumnedViewer newViewer) {
				AbstractColumnedTableLabelProvider provider = createSpecificTableLabelProvider(newViewer);
				if (provider == null){
					provider = new ColumnedDefaultTableLabelProvider(newViewer) {
						@Override
						protected Image getActualImage(Object element,
								int actualColumnIndex) {
							if (actualColumnIndex==0) {
								return getElementIcon(element);
							} else{
								Image image = AbstractSearchListComposite.this.getCustomColumnImage(element,actualColumnIndex);
								if (image==null){
									return super.getActualImage(element, actualColumnIndex);
								}
								return image;
							}
						}
						
						@Override
						public String getValue(Object element, int columnIndex) {
							String value = getCustomValue(element,columnIndex);
							if (value==null) {
								return super.getValue(element, columnIndex);
							} else {
								return value;
							}
						}
						
					};
				}
				return provider;
			}

			@Override
			public SimpleDateFormat getDateFormatter() {
				SimpleDateFormat sd = AbstractSearchListComposite.this.getDateFormatter();
				if (sd != null)
					return sd;
				return super.getDateFormatter();
			}
			
			
			
			@Override
			protected String getColumnHeader(String attribute) {
				String header = AbstractSearchListComposite.this.getColumnHeader(attribute);
				if (header!=null)
					return header;
				return super.getColumnHeader(attribute);
			};
			
			@Override
			protected int getColumnSize(String attribute) {
				int size = AbstractSearchListComposite.this.getColumnSize(attribute);
				if (size!=-1)
					return size;
				return super.getColumnSize(attribute);
			};			
			
			@Override
			protected Image getCustomColumnImage(Object element,
					int actualColumnIndex) {
				Image image = AbstractSearchListComposite.this.getCustomColumnImage(element,actualColumnIndex);
				if (image!=null)
					return image;
				return super.getCustomColumnImage(element, actualColumnIndex);
			}	
			
			@Override
			protected boolean adaptActionToSelection(Action action) {
				return AbstractSearchListComposite.this.adaptActionToSelection(action);
			}

		};		
		
		return viewer;
	}
	
	protected boolean adaptActionToSelection(Action action) {
		return true;
	}
	
	protected void doOnSelection(BeanMap selected) {
		
		
	}

	protected boolean matchSelection(Object o) {
		return (o instanceof BeanMap);
	}
	
	protected BeanMap toBeanMap(Object o) {
		return (BeanMap)o;
	}
	
	
	protected AbstractColumnedViewer createTreeViewer(Composite parent){
		AbstractColumnedViewer myViewer = new BeanMapListTreeViewer(parent, SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI, entityStructure,
				getDisplayedSelectClause()) {
			@Override
			protected void doOnDoubleClick(IStructuredSelection selection) {
				doOnDoubleClickEvent(selection);
			}

			@Override
			protected void doOnSelectionChange(IStructuredSelection selection) {
				if (!selection.isEmpty()) {
					if (matchSelection(selection.getFirstElement())){
						selectedBeanMap = toBeanMap(selection.getFirstElement());
						fireBeanMapSelection(selectedBeanMap);
					}
				}
				onSelectionChange(selection);
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
				SimpleDateFormat sd = AbstractSearchListComposite.this.getDateFormatter();
				if (sd != null)
					return sd;
				return super.getDateFormatter();
			}
			@Override
			protected String getColumnHeader(String attribute) {
				String header = AbstractSearchListComposite.this.getColumnHeader(attribute);
				if (header!=null)
					return header;
				String newAttribute = AbstractSearchListComposite.this.transformColumnHeader(attribute);
				return super.getColumnHeader(newAttribute);
			};
			
			@Override
			protected int getColumnSize(String attribute) {
				int size = AbstractSearchListComposite.this.getColumnSize(attribute);
				if (size!=-1)
					return size;
				return super.getColumnSize(attribute);
			};				
			
//			@Override
//			public String getValue(Object element, int columnIndex) {
//				String attribute = attributeFromPosition(columnIndex);
//				String value = AbstractSearchListComposite.this.getValue(element, attribute, columnIndex);
//				if (value==null) {
//					return super.getValue(element, columnIndex);
//				} else {
//					return value;
//				}
//			}
			
			@Override
			protected Image getCustomColumnImage(Object element,int actualColumnIndex) {
				Image image = AbstractSearchListComposite.this.getCustomColumnImage(element,actualColumnIndex);
				if (image!=null)
					return image;
				return super.getCustomColumnImage(element, actualColumnIndex);
			}
			
		};
		return myViewer;
	}		
	
	protected Composite createViewerComposite(Composite parent) {
		return parent;
	}
	
	protected void addContentBeforeViewer(Composite parent) {

	}
	
	protected void addContentAfterViewer(Composite parent) {

	}	
		
	protected boolean getDisplayAsTree(){
		return false;
	}
	
	protected AbstractColumnedViewer createViewer(Composite parent){
		if (getDisplayAsTree())
			return createTreeViewer(parent);
		else
			return createTableViewer(parent);
	}
	
	protected SearchKeyListener createSearchKeyListener(){
		String attr  = getKeySearchAttribute();
		if (attr!=null) {
			return new SearchKeyListener(this, attr);
		}
		return null;
	}
	
	protected void createContent() {
		queryManager = createQueryManager(connection);
		queryManager.setResultListener(this);
		GridLayout layout = new GridLayout(1, true);
		layout.marginHeight = layout.marginWidth = 0;
		setLayout(layout);
		setLayoutData(new GridData(GridData.FILL_BOTH));
		globalCmp = GuiFormatTools.createComposite(this, 3, false, SWT.NONE, true);
		GridLayout l = (GridLayout) globalCmp.getLayout();
		l.marginHeight = l.marginWidth = 0;
		
		globalCmp.layout();

		//viewer = createTableViewer(globalCmp);
		Composite viewerComposite =createViewerComposite(globalCmp); 
		addContentBeforeViewer(viewerComposite);
		viewer = createViewer(viewerComposite);
		searchKeyListener = createSearchKeyListener();
		addContentAfterViewer(viewerComposite);
		setActions(getActions());
		
		
		viewer.getViewer().getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
		if (searchKeyListener!=null) {
			viewer.getViewer().getControl().addKeyListener(searchKeyListener);
		}
		
		if (displayInformationPanel) {
			infoPanel = GuiFormatTools.createComposite(globalCmp, 12, false, SWT.NONE, true);
			GridLayout gl =  (GridLayout)infoPanel.getLayout();
			gl.marginHeight = gl.marginWidth = 2;
			gl.marginRight = 0;
			gl.marginTop = gl.marginBottom = gl.marginLeft = gl.marginRight = 0;
			gl.marginLeft = 5;
			gl.verticalSpacing = 0 ;
			gl.horizontalSpacing = 2;		
			
			
			//gl.verticalSpacing = 0;		
			((GridLayout) infoPanel.getLayout()).horizontalSpacing = 0;
			GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
			gridData.grabExcessHorizontalSpace = true;
			gridData.horizontalSpan = 3;
			gridData.heightHint = 30;
	
			infoPanel.setLayoutData(gridData);
	
			Composite labelComposite = GuiFormatTools.createComposite(infoPanel, 3, false, SWT.NONE, true);
			((GridLayout) labelComposite.getLayout()).marginHeight = 3;
	
			infoLabel = createLabelledLabel(labelComposite, 
					Activator.resString("search.label.resultCount")); //$NON-NLS-1$
	
			Composite pagesComposite = GuiFormatTools.createComposite(infoPanel, 3, false, SWT.NONE, true);
			((GridLayout) pagesComposite.getLayout()).marginHeight = 3;
	
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
	
	protected MetaDataEntity getEntityStructure(){
		return entityStructure;
	}
	
	protected void createSearchAction() {
		searchAction = new AbstractConnectedBasicAction(connection) {
			@Override
			protected boolean execute() {
				String searchClause = createSearchClause();
				if (searchKeyListener!=null) {
					searchClause = searchKeyListener.createSearchClause(searchClause);
				}
				
				//if (searchClause!=null) {
					String selectClause = createSelectClause();
					if (selectClause != null)
						queryManager.setQuery(getEntityStructure(), selectClause, searchClause, createOrderClause());
					else
						queryManager.setQuery(getEntityStructure(), searchClause);
					return true;
				//}
				//return false;
			}
			
			public List<Integer> getExpectedRigths() {
				ArrayList<Integer> rights = new ArrayList<Integer>();
				return rights;
			};
		};
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
	 * Uset his method to change the viewer Input
	 * @param list the new BeanMapList you want to define as the current input
	 */
	public void contentChanged(BeanMapList list) {
		setInput(list);
		viewer.refresh();
		if (displayInformationPanel) {
			previousButton.setEnabled(queryManager.canReadPreviousResults());
			nextButton.setEnabled(queryManager.canReadNextResults());
		}
	}
	
	protected void setInput(Object list){
		viewer.setInput(list);
	}
	
	
	public void setElementCount(int count, int currentPage, int numberOfPages) {
		if (displayInformationPanel){
			infoLabel.setText(String.valueOf(count));
			pageCountLabel.setText(String.valueOf(currentPage) + '/' + String.valueOf(numberOfPages));
		}
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
			@Override
			public boolean isUserDefinedCount() {
				return defineCount();
			}
			@Override
			public int getUserDefinedCount() {
				return getDefaultCount();
			}
			
		};
	}	
	
	public boolean defineCount(){
		return false;
	}
	
	public int getDefaultCount(){
		return -1;
	}
	
	public void search() {
		searchAction.run();
	}
	
	/**
	 * Allows to add a new listener on the selection
	 * @param listener a lister to the selection
	 */
	public void addBeanMapSelectionListener(IBeanMapSelectionListener listener ) {
		beanMapSelectionListener.add(listener);
	}
	/**
	 *  Allows to remove a existing listener
	 * @param listener
	 */
	public void removeBeanMapSelectionListener(IBeanMapSelectionListener listener ) {
		beanMapSelectionListener.remove(listener);
	}

	private void fireBeanMapSelection(BeanMap selected){
		Object[] list = beanMapSelectionListener.getListeners();
		for (int i=0; i<list.length;i++) {
			IBeanMapSelectionListener listener = (IBeanMapSelectionListener)list[i];			
			try {
				listener.beanMapSelected(selected);
			} catch (Exception e) {
				LogUITools.logError(Activator.getDefault().getBundle(), e);
				beanMapSelectionListener.remove(listener);
			}
		}
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
	
	protected Image getCustomColumnImage(Object element, int actualColumnIndex){
		return null;
	}		
	
	
	protected String getCustomValue(Object element, int actualColumnIndex){
		return null;
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
		return getClass().getName();
	}
	
	/**
	 * Override this method to define the behavior whene the user
	 * double click on a element on the list
	 * @param selection The current selection selection 
	 */
	protected void doOnDoubleClickEvent(IStructuredSelection selection) {
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
	
	
	/**
	 * Override this method to define your own SimpleDateFormatter that will 
	 * be used to display the the Date BeanMap Value
	 * @return your own SimpleDateFormat object
	 */
	protected SimpleDateFormat getDateFormatter() {
		return null;
	}	
	
	protected String getKeySearchAttribute(){
		return null;
	}
	
	
	/**
	 * Define the action that will be available for the BeanMapList
	 */
	public abstract List<Action> getActions();
	
	/**
	 * A user defined error message that will be displayed if an error
	 * occurred during retrieving data
	 * @return an error Message
	 */
	protected abstract UserMessage getSearchErrorMessage();
	
	/**
	 * Returns the parent bundle used to display the message
	 * If this reference is null, the internal activator will be used instead. 
	 * @return a bunble reference
	 */
	protected abstract Bundle getParentBundle();	
	
	/**
	 * Returns a blank separated attribute list of the attribute you want to see in the table 
	 * @return
	 */
	protected abstract String createSelectClause();	
	
	/**
	 * Returns xml string that describes the search criterias 
	 * @return
	 */
	protected abstract String createSearchClause();
	
	/**
	 * Returns a order clause
	 * @return
	 */
	protected abstract String createOrderClause();
	
	

	
	
	protected abstract Image getElementIcon(Object element);

	protected String getDisplayedSelectClause() {
		return createSelectClause();		
	}
	
	public boolean enableMultiSelection(){
		return true;
	}
	
	public Object getInput(){
		return viewer.getInput();
	}
	public void add(BeanMap beanMap){
		if (getInput() instanceof BeanMapList) {
			BeanMapList list = (BeanMapList)getInput();
			list.add(beanMap);
			viewer.refresh();
		}
		
	}
	
	public void remove(BeanMap beanMap){
		if (getInput() instanceof BeanMapList) {
			BeanMapList list = (BeanMapList)getInput();
			list.remove(beanMap);
			removeBeanEditor(beanMap);
			viewer.refresh();
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
	
	/**
	 * Callback sent after specific actions with an event among 
	 * @param type
	 * @param bean
	 * @see IBeanMapActionListener#ACTION_ADD
	 * @see IBeanMapActionListener#ACTION_DELETE
	 * @see IBeanMapActionListener#ACTION_RELOAD
	 * @see IBeanMapActionListener#ACTION_REFRESH
	 */
	public void actionDone(int type, BeanMap bean) {
		final String selectClause = createSelectClause();
		switch (type) {
		case IBeanMapActionListener.ACTION_ADD:
			BeanMap completed = helper.read(bean.getType(), bean.getId(), selectClause);
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

	/**
	 * Callback sent after specific actions with an event among IBeanMapActionListener.ACTION_*
	 * @param type
	 * @param list
	 * @see IBeanMapActionListener#ACTION_ADD
	 * @see IBeanMapActionListener#ACTION_DELETE
	 * @see IBeanMapActionListener#ACTION_RELOAD
	 * @see IBeanMapActionListener#ACTION_REFRESH
	 */
	public void actionDone(int type, BeanMapList list) {
		switch (type) {
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
	
	
	public void setSelectedBeanMap(BeanMap selectedBeanMap) {
		viewer.getViewer().setSelection(
			new StructuredSelection(selectedBeanMap)
		);
	}
	
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
	protected void removeBeanEditor(BeanMap bean){}
	
	/**
	 * Refresh BeanMap editor if exists after change occurred in list. 
	 * Empty by default. Must be redefined to be filled in inheriting classes.
	 * @param bean
	 */
	protected void refreshBeanEditor(BeanMap bean){}
	
	/**
	 * This method is called each time a selection event occurs (selection OR seslection)
	 * even if the selection is empty unlike of {@link AbstractSearchListComposite#doOnSelection(BeanMap)}
	 * that is triggered only when the selection is not empty
	 * @param selection
	 */
	protected void onSelectionChange(IStructuredSelection selection) {}
	
	
}
