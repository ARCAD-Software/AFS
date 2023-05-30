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
package com.arcadsoftware.editor.implementation.swt.renderer;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Vector;

import org.eclipse.core.databinding.Binding;
import org.eclipse.core.databinding.observable.ChangeEvent;
import org.eclipse.core.databinding.observable.IChangeListener;
import org.eclipse.core.databinding.observable.list.IListChangeListener;
import org.eclipse.core.databinding.observable.list.ListChangeEvent;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.databinding.viewers.ObservableListTreeContentProvider;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.IMessageManager;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.progress.UIJob;

import com.arcadsoftware.aev.core.tools.StringTools;
import com.arcadsoftware.aev.core.ui.common.FormToolKitUtils;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.ui.plugins.LoggedUIPlugin;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapEvent;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.beanmap.IBeanMapListener;
import com.arcadsoftware.editor.IActionElement;
import com.arcadsoftware.editor.IEditorLoader;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.implementation.Activator;
import com.arcadsoftware.editor.implementation.EditorEngine;
import com.arcadsoftware.editor.implementation.IProviderFactory;
import com.arcadsoftware.editor.implementation.LayoutElement;
import com.arcadsoftware.editor.implementation.swt.BeanMapWarper;
import com.arcadsoftware.editor.implementation.swt.ColorMapTable;
import com.arcadsoftware.editor.implementation.swt.LinkMapWarper;
import com.arcadsoftware.editor.implementation.swt.LinkMapWarper.Operations;
import com.arcadsoftware.editor.implementation.swt.binding.BeanMapObservableLink;
import com.arcadsoftware.editor.implementation.swt.binding.BeanMapObservableLinkTreeFactory;
import com.arcadsoftware.editor.implementation.swt.binding.BeanMapStructureAdvisor;
import com.arcadsoftware.editor.implementation.swt.binding.MessageStatusObservable;
import com.arcadsoftware.editor.swt.DynamicEditorComposite;
import com.arcadsoftware.editor.swt.IBeanMapChangedListener;
import com.arcadsoftware.editor.swt.IBeanMapControlerListener;
import com.arcadsoftware.editor.swt.IBeanMapErrorOnSaveListener;
import com.arcadsoftware.editor.swt.IBeanMapSavedListener;
import com.arcadsoftware.editor.swt.IContainerSWTProvider;
import com.arcadsoftware.editor.swt.IControlValidityListener;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.IEditorChangeListener;
import com.arcadsoftware.editor.swt.IEditorTitleChangeListener;
import com.arcadsoftware.editor.swt.IEntityAttributeProcessListener;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTDataLoader;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.IValidatingSubWidgets;
import com.arcadsoftware.editor.swt.MandatoryAttribute;
import com.arcadsoftware.editor.swt.actions.IEditorAction;
import com.arcadsoftware.editor.swt.actions.IRefreshableAction;
import com.arcadsoftware.editor.swt.listener.IListenedWidget;
import com.arcadsoftware.editor.swt.listener.IListenerWidget;
import com.arcadsoftware.editor.swt.renderer.IActivated;
import com.arcadsoftware.editor.swt.renderer.IInternalEditors;
import com.arcadsoftware.editor.swt.renderer.ILoadedListListener;
import com.arcadsoftware.editor.swt.renderer.ILoadingListeners;
import com.arcadsoftware.editor.swt.renderer.IRefreshEditorContent;
import com.arcadsoftware.editor.swt.renderer.IRendererActions;
import com.arcadsoftware.editor.swt.renderer.IRendererBinding;
import com.arcadsoftware.editor.swt.renderer.IUpdateDateListeners;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataFormater;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.MetaDataTest;
import com.arcadsoftware.osgi.ISODateFormater;
import com.arcadsoftware.script.IScriptAction;
import com.arcadsoftware.script.ScriptExecutionException;

/**
 * This class define the interface between SWT widget providers and the rendering of theses objects.
 * 
 * FIXME Major inconsistency in this class...
 * 
 */
public class SWTRenderer extends EditorEngine implements ISWTRenderer, ISelectionProvider, IChangeListener,
		IListChangeListener, IEditorChangeListener {

	/*
	 * not-TO-DO on the pipe...
	 * 
	 * 2. Faire en sorte que tant que le BeanMap n'a pas été chargé l'éditeur est en mode ReadOnly ! (Avec un
	 * binding sur l'état Enabled <--> la propriété enabled du Renderer... ).
	 * 
	 * 3. Définir un éditeur comme éditeur par défaut (prend le focus).
	 * 
	 * 4. Finir la gestion des Links dans l'exécution de scripts.
	 * 
	 * 5. A éclater en plusieurs classes !
	 */

	private static final String ICONS_SLASH = "icons/"; //$NON-NLS-1$
	private static final String _0X = "0x"; //$NON-NLS-1$
	private static final String ATTRIBUTES = "attributes"; //$NON-NLS-1$
	private static final String LINKS = "links"; //$NON-NLS-1$
	private static final String OPERATION = "operation"; //$NON-NLS-1$
	private static final String ADD = "add"; //$NON-NLS-1$
	private static final String REMOVE = "remove"; //$NON-NLS-1$
	private static final String OP = "op"; //$NON-NLS-1$
	private static final String IDID = "idid"; //$NON-NLS-1$
	private static final String TRUE = "true"; //$NON-NLS-1$
	private static final String YES = "yes"; //$NON-NLS-1$
	private static final String TITLE = "title"; //$NON-NLS-1$
	private static final String NO = "no"; //$NON-NLS-1$
	private static final String BACKGROUND_COLOR = "background-color"; //$NON-NLS-1$
	private static final String BACKGROUND_COLOR_SWT = "background-color-swt"; //$NON-NLS-1$
	private static final String NONE = "none"; //$NON-NLS-1$
	private static final String BORDER = "border"; //$NON-NLS-1$
	private static final String LEFT_TO_RIGHT = "left to right"; //$NON-NLS-1$
	private static final String ORIENTATION = "orientation"; //$NON-NLS-1$
	private static final String FALSE = "false"; //$NON-NLS-1$
	private static final String READ_ONLY = "readOnly"; //$NON-NLS-1$
	private static final String ICON = "icon"; //$NON-NLS-1$
	private static final String EXTENSION_NAME_SWTDATALOADER = "loaderSWT"; //$NON-NLS-1$
	private static final String SAVEMESSAGE = "savemessage"; //$NON-NLS-1$
	private static final String EXCLAMATION_POINT = "!"; //$NON-NLS-1$
	private static final String HELPCONTEXTID = "helpcontext"; //$NON-NLS-1$
	

	/*private class MMessages {

		Element element;
		Control control;
		IMessageManager manager;

		public MMessages(Element element, IMessageManager manager, Control control) {
			//this.element = element;
			this.control = control;
			this.manager = manager;
		}

		public void addMessage(Object key, String messageText, boolean critical) {
			if (critical) {
				manager.addMessage(key, messageText, null, IMessageProvider.ERROR, control);
			} else {
				manager.addMessage(key, messageText, null, IMessageProvider.WARNING, control);
			}
		}

		public void removeMessage(Object key) {
			manager.removeMessage(key, control);
		}
	}*/

	private FormToolkit toolkit;
	private RendererBinding rendererBinding = new RendererBinding(this);
	private final IProviderFactory providerFactory = new SWTProviderFactory();
	private HashMap<String, MetaDataTest> unvalidTest = new HashMap<String, MetaDataTest>();
	//private HashMap<String, ArrayList<MMessages>> mmtable = new HashMap<String, ArrayList<MMessages>>();
	private ArrayList<IMessageManager> mmList = new ArrayList<IMessageManager>();
	private Composite parent;
	private Composite firstParent = null;
	private IMessageManager currentMessageManager = null;
	private IContainerSWTProvider parentProvider;
	private BeanMapWarper warper;
	private LinkMapWarper lwarper;
	private ISWTDataLoader loader;
	private int sourceId = 0;
	private boolean changefiring = false;
	private MetaDataFormater titleFormater;
	private ISWTRenderer parentRenderer;
	private ListenerList loadEvents = new ListenerList();
	private ListenerList changeEvents = new ListenerList();
	private ListenerList titleEvents = new ListenerList();
	private ListenerList saveEvents = new ListenerList();
	private ListenerList beforeSaveControlers = new ListenerList();
	private ListenerList beanMapSavedBeanListener = new ListenerList();
	private ListenerList validityEvents = new ListenerList();
	private ArrayList<IAction> currentIActions = new ArrayList<IAction>();
	private InternalEditors internalEditors = new InternalEditors(this);
	private LoadingListeners loadingListeners = new LoadingListeners();
	private UpdateDateListeners updateDateListeners = new UpdateDateListeners();
	private RendererActions rendererActions = new RendererActions();
	private Map<String, List<IListenerWidget>> listenerWidgets;
	private boolean readOnly = false;
	
	//private List<String> mandatoryAttributes;
	private List<MandatoryAttribute> mandatoryAttributes;
	
	
	private IToolBarManager formToolBarManager;
	private List<ILoadedListListener> loadedListListeners = new Vector<ILoadedListListener>();
	private boolean mustBeRefresh = false;
	private Map<String, Object> virtualValues;
	private int id = -1;
	private IRightControler rightControler;
	protected ArrayList<IToolBarManager> formToolbarManagers = new ArrayList<IToolBarManager>();
	private List<IActivated> activatedListeners = new ArrayList<IActivated>();
	private final Display parentDisplay;
	private ListenerList errorOnSavedListener = new ListenerList();

	public SWTRenderer(Display display,String realm, String type, boolean readOnly) {
		super(realm);
		this.parentDisplay = display;
		this.readOnly = readOnly;
		editorLoaderCreated(this);
		if (loadStructure(type)) {
			loader = (ISWTDataLoader) findLoader(EXTENSION_NAME_SWTDATALOADER);
			loader.setDisplay(parentDisplay);
			dataLoaderCreated(this);
			// Prepare the BeanMapContainer.
			warper = new BeanMapWarper(this);
			lwarper = new LinkMapWarper(this);
		}			
	}	
	
	public SWTRenderer(String realm, String type, boolean readOnly) {
		this(null,realm,type,readOnly);
	}
	
	public SWTRenderer(String realm, String type) {
		this(realm, type, false);
	}

	public int getId() {
		return id;
	}

	protected void editorLoaderCreated(SWTRenderer renderer){
		
	}
	
	protected void dataLoaderCreated(SWTRenderer renderer){
		
	}
	
	public ISWTDataLoader getDataLoader() {
		return loader;
	}

	public void dispose() {
		//mmtable.clear();
		mmList.clear();
		if (toolkit != null) {
			toolkit.dispose();
		}
		rendererBinding.disposeBindingContext();
		// Dispose the providers !
		disposeProviders(getLayoutElements());
		rendererBinding.dispose();
	}

	public boolean isReadOnly() {
		return readOnly;
	}

	public void setReadOnly(boolean readOnly) {
		this.readOnly = readOnly;
	}

	private void disposeProviders(List<LayoutElement> layoutElements) {
		for (LayoutElement element : layoutElements) {
			Object provider = element.getProvider();
			if (provider instanceof IContainerSWTProvider) {
				((IContainerSWTProvider) provider).dispose();
				disposeProviders(element.getContaint());
			} else if (provider instanceof IInputSWTProvider) {
				((IInputSWTProvider) provider).dispose();
			} else if (provider instanceof IDecoratorSWTProvider) {
				((IDecoratorSWTProvider) provider).dispose();
			}
		}
	}

	/**
	 * Create the SWT Editor into the specified parent.
	 * 
	 */
	public void createPartControl(Composite newParent, String layoutName) {
		toolkit = new FormToolkit(newParent.getDisplay());
		if (loadLayout(layoutName)) {
			if (getParam(READ_ONLY) != null) {
				readOnly = readOnly || TRUE.equalsIgnoreCase(getParam(READ_ONLY, FALSE).replace(YES, TRUE));
			}
			// Set some toolkit parameters...
			String p = getParam(ORIENTATION);
			// Correct support of BiDi platforms...
			if (p != null) {
				if (LEFT_TO_RIGHT.equalsIgnoreCase(p)) {
					toolkit.setOrientation(SWT.LEFT_TO_RIGHT);
				} else {
					FormToolKitUtils.setOrientationRightToLeft(toolkit);
				}
			}
			p = getParam(BORDER, NONE);
			if (!(NONE.equalsIgnoreCase(p) || NO.equalsIgnoreCase(p) || FALSE.equalsIgnoreCase(p))) {
				toolkit.setBorderStyle(SWT.BORDER);
			}
			p = getParam(BACKGROUND_COLOR, NONE);
			if (!NONE.equalsIgnoreCase(p)) {
				Color color = getColor(p);
				if (color != null) {
					toolkit.setBackground(color);
				}
			}
			p = getParam(BACKGROUND_COLOR_SWT, NONE);
			if (!NONE.equalsIgnoreCase(p)) {
				try {
					int color = Integer.parseInt(p);
					toolkit.setBackground(Display.getCurrent().getSystemColor(color));
				} catch (NumberFormatException e) {}
			}

			// Calculate the editor title.
			titleFormater = new MetaDataFormater(getLocalizedMessage(getParam(TITLE, layoutName)), getStructure());
			fireTitleChangedEvent();
			IChangeListener tlistener = new IChangeListener() {
				public void handleChange(ChangeEvent event) {
					fireTitleChangedEvent();
				}
			};
			for (MetaDataAttribute att : titleFormater.getAttributes()) {
				if (att!=null) {
					IObservableValue ao = rendererBinding.getObservableAttribute(att);
					ao.addChangeListener(tlistener);
				}
			}
			// Render the SWT widgets !
			parent = newParent;
			parentProvider = null;
			for (LayoutElement element : getLayoutElements()) {
				renderElement(element);
			}
			rendererBinding.createPartControl();
		}
	}
	
	@Override
	protected void clearLayout(){
		Composite parent = getParent();
		if (parent != null && !parent.isDisposed()){
			Control[] children = parent.getChildren();
		    for (int i = children.length - 1 ; i >=0; i--) {
		        children[i].dispose();
		    }
		    super.clearLayout();
		    // Clear and re-init renderer Binding
		    rendererBinding.dispose();
		    rendererBinding = new RendererBinding(this);
		}	   
	}
	
	/**
	 * Reload Layout: clear content and reload layout
	 * @param layoutName new Layout to load
	 */
	public void reloadPartControl(String layoutName) {	
		clearLayout();		
		createPartControl(parent, layoutName);
	}

	/**
	 * Load the BeanMap.
	 */
	public boolean load(int id) {
		boolean result = false;
		this.id = id;
		if (loader != null) {
			sourceId = id;
			if (sourceId != 0) {		
				loader.loadBeanMap(getStructure().getType(), id, new IBeanMapListener() {
					public void changed(BeanMapEvent event) {
						loadBeanMap(event);
					}
				});
			}
			result = true;
		}
		return result;
	}

	public void loadBeanMap(BeanMapEvent event) {
		if ((event == null) || (event.getSource() == null)) {
			loadingListeners.fireLoadingError();
		} else {
			IBeanMap result = event.getSource();
			synchronized (warper) {
				warper.warp(result);
				// Initialize local default values.
				setDefaultValues(warper);
			}
			synchronized (lwarper) {
				if (result.getId() != 0) {
					lwarper.setId(result.getId());
					sourceId = result.getId();
				} else {
					lwarper.setId(sourceId);
				}
				lwarper.clear();
			}
			rendererBinding.loadBeanMap();
			// Fire load events.
			final BeanMapEvent event2 = new BeanMapEvent(warper);
			for (final Object listener: loadEvents.getListeners()) {
				SafeRunnable.run(new SafeRunnable() {
					public void run() {
						((IBeanMapChangedListener)listener).changed(event2);
					}
				});
			}
			fireChangedEvent();
			if (mustBeRefresh) {
				refreshAllEditors(getCurrentBean(), this);
				mustBeRefresh = false;
			}
			if (parentDisplay != null) {
				parentDisplay.asyncExec(new Runnable() {
					public void run() {
						fireTitleChangedEvent();
					}
				});
			} else if ((parent != null) && !parent.isDisposed()) {
				parent.getDisplay().asyncExec(new Runnable() {
					public void run() {
						fireTitleChangedEvent();
					}
				});
			}
		}
	}

	public boolean save() {
		boolean result = false;
		if (loader != null) {
			if (internalEditors.canSavedInternalEditors() && canSavedEditor()) {
				if (internalEditors.saveInternalEditors()) {
					boolean saved = false;
					synchronized (warper) {
						if (warper.isDirty()) {
							if (sourceId == 0) {
								BeanMap newBeanMap = loader.createBeanMap(warper.getModifications());
								if (newBeanMap != null) {
									mustBeRefresh = true;
									id = newBeanMap.getId();
									sourceId = id;
									lwarper.setId(id);
									load(id);
									//load(newBeanMap.getId());
								} else {
									fireErrorOnSaveEvent(loader.getLastErrorMessage());
									fireErrorOnSaveEvent(loader.getLastErrorUserMessage());
									for (IMessageManager mm : mmList) {
										mm.addMessage(SAVEMESSAGE, "Error while recording changes.", null,
												IMessageProvider.ERROR);
									}
									return false;
								}
							} else {
								if (loader.updateBeanMap(warper.getModifications())) {
									mustBeRefresh = true;
									//reload();
								} else {
									fireErrorOnSaveEvent(loader.getLastErrorMessage());
									fireErrorOnSaveEvent(loader.getLastErrorUserMessage());
									for (IMessageManager mm : mmList) {
										mm.addMessage(SAVEMESSAGE, "Error while recording changes.", null,
												IMessageProvider.ERROR);
									}
									return false;
								}
							}
							saved = true;
						}
					}
					// We save the links here
					synchronized (lwarper) {
						if (lwarper.isDirty()) {
							//If the beammap has been created but no value assigned,
							//in this case, warper.isDirty()==false, so we need to force
							//creation if there are some links
							if (sourceId==0){
								BeanMap newBeanMap = loader.createBeanMap(warper.getModifications());
								if (newBeanMap != null) {
									mustBeRefresh = true;
									id = newBeanMap.getId();
									sourceId = id;
									lwarper.setId(id);
								}
							}
							//Now we can save the links
							lwarper.save();
							saved = true;
						}
					}
					if (saved) {
						reload();
					}
					// Remove any save error messages !
					for (IMessageManager mm : mmList) {
						mm.removeMessage(SAVEMESSAGE);
					}
					// fire an event !
					if (saved) {
						// Envoi de l'evenement specifique de gestion des
						// sauvegardes
						// FIXME L'emploi de fireBeanMapSavedEvent et saveEvents est redondant. 
						fireBeanMapSavedEvent();
						fireSaveEvent();
						// FIXME Sémantiquement le BeanMap n'a pas changé pourquoi déclencher un évènement Changed ?
						// FIXME Surtout que cet évènement vient déjà d'être déclanché 10 lignes plus haut (reload()).
						fireChangedEvent();
					}
					result = saved;
				}
			}
		}
		return result;
	}

	public void fireSaveEvent(){
		final BeanMapEvent event = new BeanMapEvent(warper);
		for (final Object listener: saveEvents.getListeners()) {
			SafeRunnable.run(new SafeRunnable() {
				public void run() {
					((IBeanMapChangedListener)listener).changed(event);
				}
			});
		}		
	}
	
	
	public boolean canSavedEditor(IEntityAttributeProcessListener callback){
		boolean result = true;
		for (MetaDataAttribute attribute : getStructure().getAttributes().values()) {
			if (attribute.isMandatory()) {
				Object attributeValue = getCurrentBean().get(attribute.getCode());
				//String name = entry.getValue().getName();
				if (attributeValue != null) {
					if ((attributeValue instanceof String) && (((String) attributeValue).length() == 0)) {
						result = false;
						callback.processAction(attribute);
						break;
					}
				} else {
					result = false;
					callback.processAction(attribute);
					break;
				}
			}
		}
		if (result) {
			if (mandatoryAttributes != null) {
				//for (String code : mandatoryAttributes) {
				for (MandatoryAttribute mandatoryAttribute : mandatoryAttributes) {
					String code = mandatoryAttribute.getCode();
					boolean mandatory = true;
					if (mandatoryAttribute.isConditionned()) {
						String conditionnedBy = mandatoryAttribute.getConditionedBy();
						mandatory= getCurrentBean().getBoolean(conditionnedBy);
					}
					if (mandatory) {
						Object attributeValue = getCurrentBean().get(code);
						//String name = getStructure().getAttribute(code).getName();
						if (attributeValue != null) {
							if ((attributeValue instanceof String) && (((String) attributeValue).length() == 0)) {
								result = false;
								callback.processAction(getStructure().getAttribute(code));
								break;
							}
						} else {
							result = false;
							callback.processAction(getStructure().getAttribute(code));
							break;
						}
					}
				}
			}
		}
		return result;		
	}
	
	public boolean canSavedEditor(boolean withErrorMessage) {
		final boolean showMessage = withErrorMessage;
		IEntityAttributeProcessListener callback = new IEntityAttributeProcessListener(){
			
			public void processAction(MetaDataAttribute entry) {
				if (showMessage) {
					openCannotSaveWarning(entry.getName());
				}				
			}
			
		};
		return canSavedEditor(callback);
//		
//		
//		boolean result = true;
//		Set<Entry<String, MetaDataAttribute>> attributesEntries = getStructure().getAttributes().entrySet();
//		for (Entry<String, MetaDataAttribute> entry : attributesEntries) {
//			if (entry.getValue().isMandatory()) {
//				Object attributeValue = getCurrentBean().get(entry.getKey());
//				String name = entry.getValue().getName();
//				if (attributeValue != null) {
//					if ((attributeValue instanceof String) && (((String) attributeValue).length() == 0)) {
//						result = false;
//						if (withErrorMessage) {
//							openCannotSaveWarning(name);
//						}
//						break;
//					}
//				} else {
//					result = false;
//					if (withErrorMessage) {
//						openCannotSaveWarning(name);
//					}
//					break;
//				}
//			}
//		}
//		if (result) {
//			if (mandatoryAttributes != null) {
//				for (String code : mandatoryAttributes) {
//					Object attributeValue = getCurrentBean().get(code);
//					String name = getStructure().getAttribute(code).getName();
//					if (attributeValue != null) {
//						if ((attributeValue instanceof String) && (((String) attributeValue).length() == 0)) {
//							result = false;
//							if (withErrorMessage) {
//								openCannotSaveWarning(name);
//							}
//							break;
//						}
//					} else {
//						result = false;
//						if (withErrorMessage) {
//							openCannotSaveWarning(name);
//						}
//						break;
//					}
//				}
//			}
//		}
//		return result;
	}

	public boolean canSavedEditor() {
		boolean result = canSavedEditor(true);
		
		// Complete with possible added controls
		if (beforeSaveControlers != null && beforeSaveControlers.size() >0){
			for (Object approver : beforeSaveControlers.getListeners()) {
				result &= ((IBeanMapControlerListener)approver).isValid();
			}
		}
		return result;
	}

	protected void openCannotSaveWarning(String name) {
		MessageDialog.openWarning(LoggedUIPlugin.getShell(), Activator.getInstance().resString(
				"editor.renderer.save.error.title"), //$NON-NLS-1$
				StringTools.substitute(
						Activator.getInstance().resString("editor.renderer.save.error.text"), "$name", name)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public void fireChangedEvent() {
		if (!changefiring) {
			changefiring = true;
			try {
				for (Object listener : changeEvents.getListeners()) {
					final IEditorChangeListener l = (IEditorChangeListener) listener;
					SafeRunnable.run(new SafeRunnable() {
						public void run() {
							l.changed(SWTRenderer.this);
						}
					});
				}
			} finally {
				changefiring = false;
			}
		}
		updateDateListeners.fireUpdateDateChanged();
	}

	protected void fireTitleChangedEvent() {
		if (titleFormater == null) {
			return;
		}
		final String title;
		if (warper == null) {
			title = titleFormater.format(null);
		} else {
			synchronized (warper) {
				title = titleFormater.format(warper);
			}
		}
		for (Object listener : titleEvents.getListeners()) {
			final IEditorTitleChangeListener l = (IEditorTitleChangeListener) listener;
			SafeRunnable.run(new SafeRunnable() {
				public void run() {
					l.changed(SWTRenderer.this, title);
				}
			});
		}
	}

	public void addChangeListener(IEditorChangeListener listener) {
		changeEvents.add(listener);
	}

	public void removeChangeListener(IEditorChangeListener listener) {
		changeEvents.remove(listener);
	}

	public void addTitleChangeListener(IEditorTitleChangeListener listener) {
		if (titleFormater != null) {
			listener.changed(this, titleFormater.format(warper));
		}
		titleEvents.add(listener);
	}

	public void removeTitleChangeListener(IEditorTitleChangeListener listener) {
		titleEvents.remove(listener);
	}

	private void renderElement(LayoutElement element) {
		Object provider = element.getProvider();
		if (provider instanceof IContainerSWTProvider) {
			((IContainerSWTProvider) provider).create(this, element, element.getContaint().isEmpty(), getStructure());
		} else if (provider instanceof IInputSWTProvider) {
			// FIXME A refaire sans modifier l'ENTITY !!!!!!
			boolean old = element.getElement().isReadonly();
			if (readOnly) {
				element.getElement().setReadonly(true);
			}
			((IInputSWTProvider) provider).create(this, element, element.getElement(), getStructure());
			if (readOnly) {
				element.getElement().setReadonly(old);
			}
		} else if (provider instanceof IDecoratorSWTProvider) {
			((IDecoratorSWTProvider) provider).create(this, element, getStructure());
		}
	}

	public ImageDescriptor getImageDescriptor(String key) {
		int pos = key.indexOf(":");  //$NON-NLS-1$
		ImageDescriptor result = null;
		if (pos>0) {
			String bundleId = key.substring(0,pos);
			String imageKey = key.substring(pos+1);
			result = Activator.imageDescriptorFromPlugin(bundleId, imageKey);
		} else {
			result = PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(key);				
		}	

		// Check local icons.
		if (result == null) {
			result = Activator.getImageDescriptor(ICONS_SLASH + key);
		}
		/*
		 * Check server icons. if (result == null) { result = getLoader().getImageDescriptor(key); }
		 */
		// get the necessary image from the server.
		if (result == null) {
			result = loader.loadImage(key);
		}
		return result;
	}

	public String getLocalizedMessage(String key) {
		String value = getMessage(key);
		if (value == null) {
			return key;
		}
		while (value.startsWith(EXCLAMATION_POINT)) {
			key = value.substring(1, value.length());
			value = getMessage(key);
			if (value == null) {
				return key;
			}
		}
		return value;
	}

	public FormToolkit getToolkit() {
		return toolkit;
	}

	private void addManagedControl(Binding binding, Element element, IMessageManager manager, Control control) {
		// Pour les tests...
		/*ArrayList<MMessages> list = mmtable.get(element.getCode());
		if (list == null) {
			list = new ArrayList<MMessages>();
		}
		list.add(new MMessages(element, manager, control));
		mmtable.put(element.getCode(), list);*/
		// Pour les Erreurs de bindings...
		rendererBinding.getBinding().bindValue(binding.getValidationStatus(),
				new MessageStatusObservable(element, manager, control), null, null);
	}

	@Override
	protected int getLayoutKind() {
		return IEditorLoader.LAYOUT_KIND_SWT;
	}

	@Override
	protected String getWidgetsExtension() {
		return "com.arcadsoftware.client.editor.swt"; ////$NON-NLS-1$
	}

	public Composite getParent() {
		return parent;
	}

	public void createSubContainer(IContainerSWTProvider parentContainerProvider, ILayoutParameters parameters,
			Composite compositeParent) {
		if (firstParent == null) {
			firstParent = compositeParent;
		}
		Composite oldParent = parent;
		IContainerSWTProvider oldParentProvider = parentProvider;
		try {
			parent = compositeParent;
			parentProvider = parentContainerProvider;
			if (parentProvider instanceof IValidatingSubWidgets) {
				for (LayoutElement element : ((LayoutElement) parameters).getContaint()) {
					if ((element.getProvider() instanceof IInputSWTProvider)
							&& ((IValidatingSubWidgets) parentProvider).acceptInput((IInputSWTProvider) element
									.getProvider())) {
						renderElement(element);
					} else if ((element.getProvider() instanceof IContainerSWTProvider)
							&& ((IValidatingSubWidgets) parentProvider)
									.acceptSubContainer((IContainerSWTProvider) element.getProvider())) {
						renderElement(element);
					} else if ((element.getProvider() instanceof IDecoratorSWTProvider)
							&& ((IValidatingSubWidgets) parentProvider).acceptDecorator((IDecoratorSWTProvider) element
									.getProvider())) {
						renderElement(element);
					}
				}
			} else {
				for (LayoutElement element : ((LayoutElement) parameters).getContaint()) {
					renderElement(element);
				}
			}
		} finally {
			parent = oldParent;
			parentProvider = oldParentProvider;
		}
	}

	public void createSubContainer(IContainerSWTProvider parentContainerProvider, IMessageManager messageManager,
			ILayoutParameters parameters, Composite parentComposite, ArrayList<IAction> actions) {
		// Collect this message manager for global messages...
		if (messageManager != null) {
			boolean newone = true;
			for (IMessageManager mm : mmList) {
				if (mm.equals(messageManager)) {
					newone = false;
					break;
				}
			}
			if (newone) {
				mmList.add(messageManager);
			}
		}
		// Process to sub-container creation.
		IMessageManager oldmanager = currentMessageManager;
		ArrayList<IAction> oldActions = currentIActions;
		try {
			if (messageManager != null) {
				currentMessageManager = messageManager;
			}
			if (actions != null) {
				currentIActions = actions;
			}
			createSubContainer(parentContainerProvider, parameters, parentComposite);
		} finally {
			currentMessageManager = oldmanager;
			currentIActions = oldActions;
		}
	}

	public IContainerSWTProvider getParentProvider() {
		return parentProvider;
	}

	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		warper.addSelectionChangedListener(listener);
	}

	public ISelection getSelection() {
		return warper;
	}

	public void removeSelectionChangedListener(ISelectionChangedListener listener) {
		warper.removeSelectionChangedListener(listener);
	}

	public void setSelection(ISelection selection) {
		IBeanMap value = null;
		if (selection instanceof BeanMapWarper) {
			if (warper != ((BeanMapWarper) selection)) {
				value = (BeanMapWarper) selection;
			}
		} else if (selection instanceof IBeanMap) {
			value = (IBeanMap) selection;
		} else if (selection instanceof IAdaptable) {
			value = (IBeanMap) ((IAdaptable) selection).getAdapter(IBeanMap.class);
		}
		if (value != null) {
			rendererBinding.importAttributes(value);
		}
	}

	public void loadBeanMap(String type, int id, IBeanMapListener listener) {
		if (loader == null) {
			return;
		}
		loader.loadBeanMap(type, id, listener);
	}

	public void addLoadListener(IBeanMapChangedListener listener) {
		loadEvents.add(listener);
	}

	public void removeLoadListener(IBeanMapChangedListener listener) {
		loadEvents.remove(listener);
	}

	public void addBeforeSaveControler(IBeanMapControlerListener listener) {
		beforeSaveControlers.add(listener);
	}
	public void removeBeforeSaveControler(IBeanMapControlerListener listener) {
		beforeSaveControlers.remove(listener);
	}	

	public void addSaveListener(IBeanMapChangedListener listener) {
		saveEvents.add(listener);
	}

	public boolean isDirty() {
		synchronized (warper) {
			if (warper.isDirty()) {
				return true;
			}
		}
		synchronized (lwarper) {
			return lwarper.isDirty();
		}
	}

	public void forceDirty() {
		synchronized (warper) {
			warper.forceDirty();
		}
		fireChangedEvent();
	}
	
	public boolean isRecordable() {
//		if (unvalidTest.isEmpty()) {
//			return true;
//		}
//		for (MetaDataTest test : unvalidTest.values()) {
//			if (test.isCritical()) {
//				return false;
//			}
//		}
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seecom.arcadsoftware.editor.swt.ISWTRenderer#removeSaveListener(com.
	 * arcadsoftware.editor.swt.IBeanMapChangedListener)
	 */
	public void removeSaveListener(IBeanMapChangedListener listener) {
		saveEvents.remove(listener);
	}

	public boolean isValided(MetaDataTest test) {
		return unvalidTest.get(test.getCode()) == null;
	}

	public void updateTest(MetaDataTest test, boolean isValid) {
//		if (isValid) {
//			if (unvalidTest.get(test.getCode()) != null) {
//				unvalidTest.remove(test.getCode());
//				fireTestUpdatedEvent(test, true);
//			}
//			// Remove messages...
//			for (IMessageManager manager : mmList) {
//				manager.removeMessage(test);
//			}
//			for (String code : test.getAttributes()) {
//				MetaDataAttribute att = getStructure().getAttribute(code);
//				if (att != null) {
//					ArrayList<MMessages> list = mmtable.get(att.getCode());
//					if (list != null) {
//						for (MMessages mm : list) {
//							mm.removeMessage(test);
//						}
//					}
//				}
//			}
//		} else {
//			if (unvalidTest.get(test.getCode()) == null) {
//				unvalidTest.put(test.getCode(), test);
//				fireTestUpdatedEvent(test, false);
//			}
//			// Add messages...
//			for (IMessageManager manager : mmList) {
//				if (test.isCritical()) {
//					manager.addMessage(test, test.getMessage(), null, IMessageProvider.ERROR);
//				} else {
//					manager.addMessage(test, test.getMessage(), null, IMessageProvider.WARNING);
//				}
//			}
//			for (String code : test.getAttributes()) {
//				MetaDataAttribute att = getStructure().getAttribute(code);
//				if (att != null) {
//					ArrayList<MMessages> list = mmtable.get(att.getCode());
//					if (list != null) {
//						for (MMessages mm : list) {
//							mm.addMessage(test, test.getMessage(), test.isCritical());
//						}
//					}
//				}
//			}
//		}
	}

	/*private void fireTestUpdatedEvent(MetaDataTest test, boolean valid) {
		final ControlValidityEvent event = new ControlValidityEvent(test, valid);
		Object[] listeners = validityEvents.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			final IControlValidityListener l = (IControlValidityListener) listeners[i];
			SafeRunnable.run(new SafeRunnable() {
				public void run() {
					l.validityChanged(event);
				}
			});
		}
	}*/

	/*
	 * (non-Javadoc)
	 * 
	 * @seecom.arcadsoftware.editor.swt.ISWTRenderer#addValidityListener(com.
	 * arcadsoftware.editor.swt.IControlValidityListener)
	 */
	public void addValidityListener(IControlValidityListener listener) {
		validityEvents.add(listener);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#removeValidityListener(com.
	 * arcadsoftware.editor.swt.IControlValidityListener)
	 */
	public void removeValidityListener(IControlValidityListener listener) {
		validityEvents.remove(listener);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#reload()
	 */
	public void reload() {
		if (sourceId != 0) {
			// Reload must be updated from the server !
			load(sourceId);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#getColor(java.lang.String)
	 */
	public Color getColor(String cssColor) {
		Color result = null;
		if (cssColor != null) {
			if (cssColor.charAt(0) == '#') {
				if (cssColor.length() == 7) {
					// Color is like #FFFFFF
					try {
						int red = Integer.decode(_0X + cssColor.substring(1, 3)).intValue();
						int green = Integer.decode(_0X + cssColor.substring(3, 5)).intValue();
						int blue = Integer.decode(_0X + cssColor.substring(5)).intValue();
						result = new Color(Display.getDefault(), new RGB(red, green, blue));
						// result = new Color(parent.getDisplay(), new RGB(red,
						// green, blue));
					} catch (Exception e) {
						result = null;
					}
				} else if (cssColor.length() == 4) {
					// Color is like #FFF
					try {
						int red = Integer.decode(_0X + cssColor.substring(1, 2) + cssColor.substring(1, 2)).intValue();
						int green = Integer.decode(_0X + cssColor.substring(2, 3) + cssColor.substring(2, 3))
								.intValue();
						int blue = Integer.decode(_0X + cssColor.substring(4) + cssColor.substring(4)).intValue();
						result = new Color(parent.getDisplay(), new RGB(red, green, blue));
					} catch (Exception e) {
						result = null;
					}
				}
			}
			String color = ColorMapTable.TABLE.get(cssColor);
			if (color != null) {
				result = getColor(color);
			}
		}
		return result;
	}

	/**
	 * @return a current BeanMap value copy.
	 */
	public BeanMap getCurrentBean() {
		synchronized (warper) {
			return warper.cloneCurrent();
		}
	}

	/**
	 * @return the list of actions provided by sub-container providers.
	 */
	public List<IAction> getProvidersActions() {
		return currentIActions;
	}

	public List<IAction> getGlobalActions() {
		ArrayList<IAction> actions = new ArrayList<IAction>(2);
		for (IActionElement action : getActions()) {
			actions.add(new SWTScriptAction(this, action));
		}
		return actions;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seecom.arcadsoftware.editor.swt.ISWTRenderer#declareGlobalAction(com. arcadsoftware.editor.IActionElement)
	 */
	public void declareGlobalAction(IActionElement action) {
		addAction(action);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#getAction(com.arcadsoftware .editor.IActionElement)
	 */
	public IAction getAction(IActionElement action) {
		return new SWTScriptAction(this, action);
	}

	/**
	 * 
	 */
	public void setFocus() {
		if (firstParent != null && !firstParent.isDisposed()) {
			firstParent.setFocus();
		}
	}

	/**
	 * @param memento
	 */
	public void restorState(IMemento memento) {
		synchronized (warper) {
			IMemento m = memento.getChild(ATTRIBUTES);
			if (m != null) {
				restoreBeanMap(m, warper);
			}
		}
		synchronized (lwarper) {
			IMemento m = memento.getChild(LINKS);
			if (m != null) {
				for (IMemento oms : m.getChildren(OPERATION)) {
					IMemento om = oms.getChild(ADD);
					if (om != null) {
						for (IMemento o : om.getChildren(OP)) {
							BeanMap bm = new BeanMap(o.getID(), o.getInteger(IDID).intValue());
							restoreBeanMap(o, bm);
							lwarper.updateLinkList(oms.getID(), LinkMapWarper.LINKLIST_ADD, bm);
						}
					}
					om = oms.getChild(REMOVE);
					if (om != null) {
						for (IMemento o : om.getChildren(OP)) {
							BeanMap bm = new BeanMap(o.getID(), o.getInteger(IDID).intValue());
							restoreBeanMap(o, bm);
							lwarper.updateLinkList(oms.getID(), LinkMapWarper.LINKLIST_REMOVE, bm);
						}
					}
				}
			}
		}
		// We do not need to fire any GUI events because this method
		// he's called before the construction of the editor !
		fireChangedEvent();
	}

	private void restoreBeanMap(IMemento am, IBeanMap bm) {
		for (String key : am.getAttributeKeys()) {
			bm.put(key, am.getString(key));
		}
	}

	/**
	 * @param memento
	 */
	public void saveState(IMemento memento) {
		synchronized (warper) {
			// Save Attributes Changes.
			saveBeanMap(memento.createChild(ATTRIBUTES), warper.getModifications());
		}
		synchronized (lwarper) {
			// Save links Changes.
			IMemento m = memento.createChild(LINKS);
			for (Entry<String, Operations> oe : lwarper.getPendingsOperations()) {
				IMemento om = m.createChild(OPERATION, oe.getKey());
				saveOperations(om.createChild(ADD), oe.getValue().addList);
				saveOperations(om.createChild(REMOVE), oe.getValue().removeList);
			}
		}
	}

	private void saveBeanMap(IMemento am, BeanMap bm) {
		for (Entry<String, Object> entry : bm.entrySet()) {
			if (entry.getValue() instanceof BeanMap) {
				am.putString(entry.getKey(), Integer.toString(((BeanMap) entry.getValue()).getId()));
			} else if (entry.getValue() instanceof Date) {
				am.putString(entry.getKey(), ISODateFormater.toString((Date) entry.getValue()));
			} else {
				am.putString(entry.getKey(), ((entry.getValue() == null) ? null : entry.getValue().toString()));
			}
		}
	}

	private void saveOperations(IMemento memento, HashMap<Integer, BeanMap> list) {
		for (BeanMap bm : list.values()) {
			IMemento b = memento.createChild(OP, bm.getType());
			b.putInteger(IDID, bm.getId());
			saveBeanMap(b, bm);
		}
	}

	public void addLinkitem(MetaDataLink link, BeanMap item) {
		rendererBinding.getObservableLink(link, true,null).add(item);
		loadListCompleted(item.getType());
	}

	public ITreeContentProvider getTreeContentProvider(MetaDataLink link, String fatherCode) {
		BeanMapObservableLink l = (BeanMapObservableLink) rendererBinding.getObservableLink(link, true,null);
		return new ObservableListTreeContentProvider(new BeanMapObservableLinkTreeFactory(this, l, fatherCode),
				new BeanMapStructureAdvisor(l, fatherCode));
	}

	public void removeLinkitem(MetaDataLink link, BeanMap item) {
		rendererBinding.getObservableLink(link, true,null).remove(item);
		loadListCompleted(item.getType());
	}

	public void registerAction(IAction action) {
		if (currentIActions != null) {
			currentIActions.add(action);
		}
	}

	public void handleChange(ChangeEvent event) {
		// this is a very verbose firing...
		fireChangedEvent();
	}

	public void handleListChange(ListChangeEvent event) {
		// this is a very verbose firing...
		fireChangedEvent();
	}

	public MetaDataEntity getStructure(Element element) {
		return getLoader().loadMetaDataEntity(element.getType());
	}

	public MetaDataEntity getStructure(String type) {
		return getLoader().loadMetaDataEntity(type);
	}

	public InputStream getBeanStream(String type, int id) {
		return loader.loadStream(type, id);
	}

	public boolean updateBeanStream(String type, int id, File file) {
		return loader.updateStream(type, id, file);
	}
	
	public String getUploadBeanStreamAddress(String type, int id){
		return loader.getUploadBeanStreamAddress(type,id);
	}
	

	public BeanMap loadBeanMap(String type, int id) {
		return loader.loadBeanMap(type, id);
	}

	public BeanMap loadBeanMap(String type, int id,String attributeList) {
		return loader.loadBeanMap(type, id, attributeList);
	}
	
	public BeanMap createBeanMap(BeanMap beanMap) {
		return loader.createBeanMap(beanMap);
	}

	@Override
	protected IProviderFactory getProviderFactory() {
		return providerFactory;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#getAction(java.lang.String)
	 */
	public IAction getAction(String code) {
		if (code == null) {
			return null;
		}
		for (IActionElement action : getActions()) {
			if (code.equals(action.getCode())) {
				return new SWTScriptAction(this, action);
			}
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#getTitleImage()
	 */
	public Image getTitleImage() {
		String key = getParam(ICON, null);
		ImageDescriptor imageDescriptor = null;
		if (key != null) {
			imageDescriptor = getImageDescriptor(key);
		}
		return (imageDescriptor != null) ? imageDescriptor.createImage() : null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#getHelpContextId()
	 */
	public String getHelpContextId() {
		return  getParam(HELPCONTEXTID, null);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.editor.swt.IEditorChangeListener#changed(com.arcadsoftware .editor.swt.ISWTRenderer)
	 */
	public void changed(ISWTRenderer renderer) {
		fireChangedEvent();
	}

	public Date getUpdateDate() {
		BeanMap bean = getCurrentBean();
		return (bean != null) ? bean.getDate() : null;
	}

	public BeanMap getSelectedBeanMap() {
		return getCurrentBean();
	}

	public void addListenerWidget(IListenerWidget listenerWidget, String widgetId) {
		if (listenerWidgets == null) {
			listenerWidgets = new HashMap<String, List<IListenerWidget>>();
		}
		List<IListenerWidget> list = listenerWidgets.get(widgetId);
		if (list == null) {
			list = new ArrayList<IListenerWidget>();
			listenerWidgets.put(widgetId, list);
		}
		list.add(listenerWidget);
	}

	public void fireListenedWidgetChanged(IListenedWidget listenedWidget, BeanMap beanMap) {
		if (listenerWidgets != null) {
			List<IListenerWidget> list = listenerWidgets.get(listenedWidget.getId());
			if (list != null) {
				for (IListenerWidget listenerWidget : list) {
					listenerWidget.refreshWidget(beanMap);
				}
			}
		}
	}

	public void refreshSelector(BeanMap beanMap) {
		// Do nothing
	}

	public void addMandatoryAttribute(String code) {
		if (mandatoryAttributes == null) {
			//mandatoryAttributes = new ArrayList<String>();
			mandatoryAttributes = new ArrayList<MandatoryAttribute>();
		}
		//mandatoryAttributes.add(code);
		mandatoryAttributes.add(new MandatoryAttribute(code));
	}

	public void addMandatoryAttribute(String code, String conditionnedBy){
		if (mandatoryAttributes == null) {

			mandatoryAttributes = new ArrayList<MandatoryAttribute>();
		}
		mandatoryAttributes.add(new MandatoryAttribute(code,conditionnedBy));		
	}
	
	
	public IInternalEditors getInternalEditors() {
		return internalEditors;
	}

	public ILoadingListeners getLoadingListeners() {
		return loadingListeners;
	}

	public IUpdateDateListeners getUpdateDateListeners() {
		return updateDateListeners;
	}

	public IRendererActions getRendererActions() {
		return rendererActions;
	}

	public IRendererBinding getRendererBinding() {
		return rendererBinding;
	}

	protected BeanMapWarper getWarper() {
		return warper;
	}

	protected LinkMapWarper getLWarper() {
		return lwarper;
	}

	protected IMessageManager getCurrentMessageManager() {
		return currentMessageManager;
	}

	protected void addManagedControl(Binding binding, Element element, Control control) {
		addManagedControl(binding, element, currentMessageManager, control);
	}

	public void refreshEditorContent(BeanMap beanMap, ISWTRenderer renderer) {
		internalEditors.refreshEditorsContent(beanMap, renderer);
		rendererBinding.refreshBean(beanMap);
	}

	
	private IStatus doRefreshEditors(BeanMap newBeanMap, final ISWTRenderer renderer){
		IWorkbenchPage[] pages = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getPages();
		if (pages != null) {
			for (IWorkbenchPage workbenchPage : pages) {
				IEditorReference[] editors = workbenchPage.getEditorReferences();
				if (editors != null) {
					for (IEditorReference editorReference : editors) {
						Object editor = editorReference.getEditor(false);
						if ((editor != null) && (editor instanceof IRefreshEditorContent)) {
							if (!((IRefreshEditorContent) editor).isSameRenderer(SWTRenderer.this)) {
								((IRefreshEditorContent) editor).refreshEditorContent(newBeanMap, renderer);
							}
						}
					}
				}
			}
		}
		return Status.OK_STATUS;		
	}
	
	
	public void refreshAllEditors(BeanMap beanMap, final ISWTRenderer renderer) {
		final BeanMap newBeanMap = loadBeanMap(beanMap);
		if (newBeanMap != null) {
			UIJob job = null;
			if (parentDisplay!=null) {
				job = new UIJob(parentDisplay,Activator.getInstance().resString("refreshAllEditorsJobUiName")) { //$NON-NLS-1$
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						return doRefreshEditors(newBeanMap,renderer);
					}
				};
			} else {
				job = new UIJob(Activator.getInstance().resString("refreshAllEditorsJobUiName")) { //$NON-NLS-1$
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						return doRefreshEditors(newBeanMap,renderer);
					}
				};				
			}
			if (job!=null){
				job.schedule();
			}			
		}
	}

	private BeanMap loadBeanMap(BeanMap beanMap) {
		return (beanMap != null) ? loadBeanMap(beanMap.getType(), beanMap.getId()) : beanMap;
	}

	public void setFormToolBar(IToolBarManager toolBarManager) {
		formToolBarManager = toolBarManager;
		formToolbarManagers.add(formToolBarManager);
	}

	public void addActionOnFormToolBar(IEditorAction action) {
		if (formToolBarManager != null) {
			formToolBarManager.add(action);
			formToolBarManager.update(true);
		}		
	}

	public void updateFormToolbar() {
		for (IToolBarManager formToolbarManagerItem : formToolbarManagers) {
			formToolbarManagerItem.update(true);
			IContributionItem[] items = formToolbarManagerItem.getItems();
			for (IContributionItem item : items) {
				if (item instanceof ActionContributionItem) {
					ActionContributionItem actionItem = (ActionContributionItem) item;
					if (actionItem.getAction() instanceof IRefreshableAction) {
						((IRefreshableAction) actionItem.getAction()).refresh();
					}
				}
			}
		}
	}

	public void selectBeanMap(BeanMap bm) {
		// Do nothing
	}

	public ISWTRenderer getParentRenderer() {
		return parentRenderer;
	}

	public void setParentRenderer(ISWTRenderer renderer) {
		parentRenderer = renderer;
	}

	public void addBeanMapSaveListener(IBeanMapSavedListener listener) {
		beanMapSavedBeanListener.add(listener);
	}

	public void removeBeanMapSaveListener(IBeanMapSavedListener listener) {
		beanMapSavedBeanListener.remove(listener);
	}

	protected void fireBeanMapSavedEvent() {
		Object[] listeners = beanMapSavedBeanListener.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			final IBeanMapSavedListener l = (IBeanMapSavedListener) listeners[i];
			SafeRunnable.run(new SafeRunnable() {
				public void run() {
					l.beanMapSaved(getCurrentBean());
				}
			});
		}
	}

	
	public void addErrorOnSaveListener(IBeanMapErrorOnSaveListener listener) {
		errorOnSavedListener.add(listener);
	}

	public void removeErrorOnSaveListener(IBeanMapErrorOnSaveListener listener) {
		errorOnSavedListener.remove(listener);
	}
	
	/**
	 * Fire message
	 * @param errorMessage
	 */
	protected void fireErrorOnSaveEvent(final String errorMessage) {
		Object[] listeners = errorOnSavedListener.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			final IBeanMapErrorOnSaveListener l = (IBeanMapErrorOnSaveListener) listeners[i];
			SafeRunnable.run(new SafeRunnable() {
				public void run() {
					l.onErrorOnSave(getCurrentBean(), errorMessage);
				}
			});
		}
	}
	
	/**
	 * Fire User message
	 * @param errorUserMessage
	 */
	protected void fireErrorOnSaveEvent(final UserMessage errorUserMessage) {
		Object[] listeners = errorOnSavedListener.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			final IBeanMapErrorOnSaveListener l = (IBeanMapErrorOnSaveListener) listeners[i];
			SafeRunnable.run(new SafeRunnable() {
				public void run() {
					l.onErrorOnSave(getCurrentBean(), errorUserMessage);
				}
			});
		}
	}
	
	
	public void loadListCompleted(String type) {
		if (loadedListListeners != null) {
			//On scinde la boucle pour eviter les problemes des modifications concurrentes
			ArrayList<ILoadedListListener> listerToDelete =  new ArrayList<ILoadedListListener>();
			for (ILoadedListListener listener : loadedListListeners) {
				try {
					if (listener.getListType().equals(type)) {
						listener.loadedListComplete(this);
					}
				} catch (RuntimeException e) {
					listerToDelete.add(listener);
					//removeLoadedList(listener);
				}
			}
//			for (ILoadedList listener : listerToDelete) {
//				listerToDelete.add(listener);
//			}			
		}
	}

	public void addLoadedList(ILoadedListListener listener) {
		loadedListListeners.add(listener);
	}

	public void removeLoadedList(ILoadedListListener listener) {
		loadedListListeners.remove(listener);
	}

	public Object getVirtualValue(String key) {
		return (virtualValues != null) ? virtualValues.get(key) : null;
	}

	public void putVirtualValue(String key, Object value) {
		if (virtualValues == null) {
			virtualValues = new HashMap<String, Object>();
		}
		virtualValues.put(key, value);
	}

	public Object put(String key, Object value){
		synchronized (warper) {
			return warper.put(key, value);
		}
	}
	
	/**
	 * @return the mandatoryAttributes
	 */
	//public List<String> getMandatoryAttributes() {
	public List<MandatoryAttribute> getMandatoryAttributes() {		
		return mandatoryAttributes;
	}
	
	
	
	

	public IRightControler getRightControler() {
		return rightControler;
	}

	public void setRightControler(IRightControler rightControler) {
		this.rightControler = rightControler;
	}

	public void setParentReadOnly(boolean readOnly) {
		if (parent instanceof DynamicEditorComposite) {
			((DynamicEditorComposite) parent).setInputEnabled(!readOnly);
		}
	}

	public IToolBarManager getFormToolBarManager() {
		return formToolBarManager;
	}
	
	public void addActivatedListeners(IActivated listener) {
		activatedListeners.add(listener);		
	}

	public void removeActivatedListeners(IActivated listener) {
		activatedListeners.remove(listener);		
	}

	public void fireActivatedEvent() {
		if (activatedListeners != null) {
			for (IActivated listener : activatedListeners) {
				try {
					listener.activated();
				} catch (RuntimeException e) {
					removeActivatedListeners(listener);
				}
			}
		}
		
	}

	public IEditorLoader getEditorLoader(){
		return getLoader();
	}

	public IMessageManager getMessageManager() {
		return currentMessageManager;
	}

	
	public Object runScriptAction(String name, Map<String, Object> parameters) {
		IScriptAction sa = providerFactory.getScriptAction(name);
		if (sa == null) {
			return null;
		}
		sa.set(IScriptAction.PARAM_ENTITY, getStructure());
		sa.set("renderer", this); //$NON-NLS-1$
		sa.set(IScriptAction.PARAM_ITEM, getCurrentBean());
		sa.set("readonly", readOnly); //$NON-NLS-1$
		sa.set("messageManager", currentMessageManager); //$NON-NLS-1$
		if (parameters != null) {
			for (Entry<String, Object> entry: parameters.entrySet()) {
				sa.set(entry.getKey(), entry.getValue());
			}
		}
		try {
			if (sa.run()) {
				return sa.get(IScriptAction.PARAM_RESULT);
			}
		} catch (ScriptExecutionException e) {}
		return null;
	}

	public void requestSave(){
		save();
	}
	
	
}
