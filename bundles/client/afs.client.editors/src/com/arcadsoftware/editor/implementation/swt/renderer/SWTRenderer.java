/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
import org.eclipse.ui.plugin.AbstractUIPlugin;
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
 * This class define the interface between SWT widget providers and the rendering of theses objects. FIXME Major
 * inconsistency in this class...
 */
public class SWTRenderer extends EditorEngine implements ISWTRenderer, ISelectionProvider, IChangeListener,
		IListChangeListener, IEditorChangeListener {

	/*
<<<<<<< master
	 * not-TO-DO on the pipe...
	 * 
	 * 2. Faire en sorte que tant que le BeanMap n'a pas �t� charg� l'�diteur est en mode ReadOnly ! (Avec un
	 * binding sur l'�tat Enabled <--> la propri�t� enabled du Renderer... ).
	 * 
	 * 3. Définir un �diteur comme �diteur par d�faut (prend le focus).
	 * 
	 * 4. Finir la gestion des Links dans l'ex�cution de scripts.
	 * 
	 * 5. A �clater en plusieurs classes !
=======
	 * not-TO-DO on the pipe... 2. Faire en sorte que tant que le BeanMap n'a pas été chargé l'éditeur est en mode
	 * ReadOnly ! (Avec un binding sur l'état Enabled <--> la propriété enabled du Renderer... ). 3. Définir un
	 * éditeur comme éditeur par défaut (prend le focus). 4. Finir la gestion des Links dans l'exécution de scripts.
	 * 5. A éclater en plusieurs classes !
>>>>>>> 38f2e60 Clean-up AFS Client
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

<<<<<<< master
	private final ListenerList<IBeanMapChangedListener> loadEvents = new ListenerList<>();
	private final ListenerList<IBeanMapControlerListener> beforeSaveControlers = new ListenerList<>();
	private final ListenerList<IEditorChangeListener> changeEvents = new ListenerList<>();
	private final ListenerList<IEditorTitleChangeListener> titleEvents = new ListenerList<>();
	private final ListenerList<IBeanMapChangedListener> saveEvents = new ListenerList<>();
	private final ListenerList<IBeanMapSavedListener> beanMapSavedBeanListener = new ListenerList<>();
	private final ListenerList<IBeanMapErrorOnSaveListener> errorOnSavedListener = new ListenerList<>();
=======
	/*
	 * private class MMessages { Element element; Control control; IMessageManager manager; public MMessages(Element
	 * element, IMessageManager manager, Control control) { //this.element = element; this.control = control;
	 * this.manager = manager; } public void addMessage(Object key, String messageText, boolean critical) { if
	 * (critical) { manager.addMessage(key, messageText, null, IMessageProvider.ERROR, control); } else {
	 * manager.addMessage(key, messageText, null, IMessageProvider.WARNING, control); } } public void
	 * removeMessage(Object key) { manager.removeMessage(key, control); } }
	 */

	private FormToolkit toolkit;
	private RendererBinding rendererBinding = new RendererBinding(this);
>>>>>>> 38f2e60 Clean-up AFS Client
	private final IProviderFactory providerFactory = new SWTProviderFactory();
<<<<<<< master
	private final InternalEditors internalEditors = new InternalEditors(this);
	private final ArrayList<IMessageManager> mmList = new ArrayList<IMessageManager>();
	private final List<IActivated> activatedListeners = new ArrayList<IActivated>();
	private final Display parentDisplay;
	private final RendererActions rendererActions = new RendererActions();
	private final LoadingListeners loadingListeners = new LoadingListeners();
	private final UpdateDateListeners updateDateListeners = new UpdateDateListeners();
	private final ArrayList<IToolBarManager> formToolbarManagers = new ArrayList<IToolBarManager>();
	private final List<ILoadedListListener> loadedListListeners = new Vector<ILoadedListListener>();
	private FormToolkit toolkit;
	private RendererBinding rendererBinding;
=======
	private final HashMap<String, MetaDataTest> unvalidTest = new HashMap<>();
	// private HashMap<String, ArrayList<MMessages>> mmtable = new HashMap<String, ArrayList<MMessages>>();
	private final ArrayList<IMessageManager> mmList = new ArrayList<>();
>>>>>>> 38f2e60 Clean-up AFS Client
	private Composite parent;
	private Composite firstParent;
	private IMessageManager currentMessageManager;
	private IContainerSWTProvider parentProvider;
	private BeanMapWarper warper;
	private LinkMapWarper lwarper;
	private ISWTDataLoader loader;
	private int sourceId;
	private boolean changefiring;
	private MetaDataFormater titleFormater;
	private ISWTRenderer parentRenderer;
<<<<<<< master
	private ArrayList<IAction> currentIActions;
=======
	private final ListenerList loadEvents = new ListenerList();
	private final ListenerList changeEvents = new ListenerList();
	private final ListenerList titleEvents = new ListenerList();
	private final ListenerList saveEvents = new ListenerList();
	private final ListenerList beforeSaveControlers = new ListenerList();
	private final ListenerList beanMapSavedBeanListener = new ListenerList();
	private final ListenerList validityEvents = new ListenerList();
	private ArrayList<IAction> currentIActions = new ArrayList<>();
	private final InternalEditors internalEditors = new InternalEditors(this);
	private final LoadingListeners loadingListeners = new LoadingListeners();
	private final UpdateDateListeners updateDateListeners = new UpdateDateListeners();
	private final RendererActions rendererActions = new RendererActions();
>>>>>>> 38f2e60 Clean-up AFS Client
	private Map<String, List<IListenerWidget>> listenerWidgets;
<<<<<<< master
	private boolean readOnly;
=======
	private boolean readOnly = false;

	// private List<String> mandatoryAttributes;
>>>>>>> 38f2e60 Clean-up AFS Client
	private List<MandatoryAttribute> mandatoryAttributes;
<<<<<<< master
=======

>>>>>>> 38f2e60 Clean-up AFS Client
	private IToolBarManager formToolBarManager;
<<<<<<< master
	private boolean mustBeRefresh;
=======
	private final List<ILoadedListListener> loadedListListeners = new Vector<>();
	private boolean mustBeRefresh = false;
>>>>>>> 38f2e60 Clean-up AFS Client
	private Map<String, Object> virtualValues;
	private int id;
	private IRightControler rightControler;
<<<<<<< master
=======
	protected ArrayList<IToolBarManager> formToolbarManagers = new ArrayList<>();
	private final List<IActivated> activatedListeners = new ArrayList<>();
	private final Display parentDisplay;
	private final ListenerList errorOnSavedListener = new ListenerList();
>>>>>>> 38f2e60 Clean-up AFS Client

	public SWTRenderer(Display display, String realm, String type, boolean readOnly) {
		super(realm);
<<<<<<< master
		id = -1;
		currentIActions = new ArrayList<IAction>();
		rendererBinding = new RendererBinding(this);
=======
>>>>>>> 38f2e60 Clean-up AFS Client
		parentDisplay = display;
		this.readOnly = readOnly;
		editorLoaderCreated(this);
		if (loadStructure(type)) {
			loader = (ISWTDataLoader) findLoader(EXTENSION_NAME_SWTDATALOADER);
			loader.setDisplay(parentDisplay);
			dataLoaderCreated(this);
			// Prepare the BeanMapContainer.
			warper = new BeanMapWarper(this);
			lwarper = new LinkMapWarper(this);
<<<<<<< master
		}			
	}	
	
	public SWTRenderer(String realm, String type, boolean readOnly) {
		this(null, realm, type, readOnly);
=======
		}
>>>>>>> 38f2e60 Clean-up AFS Client
	}

	public SWTRenderer(String realm, String type, boolean readOnly) {
		this(null, realm, type, readOnly);
	}

	public SWTRenderer(String realm, String type) {
		this(realm, type, false);
	}

	@Override
	public int getId() {
		return id;
	}

<<<<<<< master
	protected void editorLoaderCreated(SWTRenderer renderer) {}
	
	protected void dataLoaderCreated(SWTRenderer renderer) {}
	
=======
	protected void editorLoaderCreated(SWTRenderer renderer) {

	}

	protected void dataLoaderCreated(SWTRenderer renderer) {

	}

	@Override
>>>>>>> 38f2e60 Clean-up AFS Client
	public ISWTDataLoader getDataLoader() {
		return loader;
	}

	public void dispose() {
		// mmtable.clear();
		mmList.clear();
		if (toolkit != null) {
			toolkit.dispose();
		}
		rendererBinding.disposeBindingContext();
		// Dispose the providers !
		disposeProviders(getLayoutElements());
		rendererBinding.dispose();
	}

	@Override
	public boolean isReadOnly() {
		return readOnly;
	}

	public void setReadOnly(boolean readOnly) {
		this.readOnly = readOnly;
	}

	private void disposeProviders(List<LayoutElement> layoutElements) {
		for (final LayoutElement element : layoutElements) {
			final Object provider = element.getProvider();
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
				final Color color = getColor(p);
				if (color != null) {
					toolkit.setBackground(color);
				}
			}
			p = getParam(BACKGROUND_COLOR_SWT, NONE);
			if (!NONE.equalsIgnoreCase(p)) {
				try {
					final int color = Integer.parseInt(p);
					toolkit.setBackground(Display.getCurrent().getSystemColor(color));
				} catch (final NumberFormatException e) {
				}
			}
			// Calculate the editor title.
			titleFormater = new MetaDataFormater(getLocalizedMessage(getParam(TITLE, layoutName)), getStructure());
			fireTitleChangedEvent();
			final IChangeListener tlistener = new IChangeListener() {
				@Override
				public void handleChange(ChangeEvent event) {
					fireTitleChangedEvent();
				}
			};
<<<<<<< master
			for (MetaDataAttribute att : titleFormater.getAttributes()) {
				if (att!=null) {
					rendererBinding.getObservableAttribute(att).addChangeListener(tlistener);
=======
			for (final MetaDataAttribute att : titleFormater.getAttributes()) {
				if (att != null) {
					final IObservableValue ao = rendererBinding.getObservableAttribute(att);
					ao.addChangeListener(tlistener);
>>>>>>> 38f2e60 Clean-up AFS Client
				}
			}
			// Render the SWT widgets !
			parent = newParent;
			parentProvider = null;
			for (final LayoutElement element : getLayoutElements()) {
				renderElement(element);
			}
			rendererBinding.createPartControl();
		}
	}

	@Override
	protected void clearLayout() {
<<<<<<< master
		Composite parent = getParent();
		if (parent != null && !parent.isDisposed()) {
		    for (Control child: parent.getChildren()) {
		        child.dispose();
		    }
		    super.clearLayout();
		    // Clear and re-init renderer Binding
		    rendererBinding.dispose();
		    rendererBinding = new RendererBinding(this);
		}	   
=======
		final Composite parent = getParent();
		if ((parent != null) && !parent.isDisposed()) {
			final Control[] children = parent.getChildren();
			for (int i = children.length - 1; i >= 0; i--) {
				children[i].dispose();
			}
			super.clearLayout();
			// Clear and re-init renderer Binding
			rendererBinding.dispose();
			rendererBinding = new RendererBinding(this);
		}
>>>>>>> 38f2e60 Clean-up AFS Client
	}

	/**
	 * Reload Layout: clear content and reload layout
	 *
	 * @param layoutName
	 *            new Layout to load
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
					@Override
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
			final IBeanMap result = event.getSource();
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
<<<<<<< master
			for (final IBeanMapChangedListener listener: loadEvents) {
=======
			for (final Object listener : loadEvents.getListeners()) {
>>>>>>> 38f2e60 Clean-up AFS Client
				SafeRunnable.run(new SafeRunnable() {
					@Override
					public void run() {
<<<<<<< master
						listener.changed(event2);
=======
						((IBeanMapChangedListener) listener).changed(event2);
>>>>>>> 38f2e60 Clean-up AFS Client
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
					@Override
					public void run() {
						fireTitleChangedEvent();
					}
				});
			} else if ((parent != null) && !parent.isDisposed()) {
				parent.getDisplay().asyncExec(new Runnable() {
					@Override
					public void run() {
						fireTitleChangedEvent();
					}
				});
			}
		}
	}

	@Override
	public boolean save() {
		boolean result = false;
		if (loader != null) {
			if (internalEditors.canSavedInternalEditors() && canSavedEditor()) {
				if (internalEditors.saveInternalEditors()) {
					boolean saved = false;
					synchronized (warper) {
						if (warper.isDirty()) {
							if (sourceId == 0) {
								final BeanMap newBeanMap = loader.createBeanMap(warper.getModifications());
								if (newBeanMap != null) {
									mustBeRefresh = true;
									id = newBeanMap.getId();
									sourceId = id;
									lwarper.setId(id);
									load(id);
									// load(newBeanMap.getId());
								} else {
									fireErrorOnSaveEvent(loader.getLastErrorMessage());
									fireErrorOnSaveEvent(loader.getLastErrorUserMessage());
									for (final IMessageManager mm : mmList) {
										mm.addMessage(SAVEMESSAGE, "Error while recording changes.", null,
												IMessageProvider.ERROR);
									}
									return false;
								}
							} else {
								if (loader.updateBeanMap(warper.getModifications())) {
									mustBeRefresh = true;
									// reload();
								} else {
									fireErrorOnSaveEvent(loader.getLastErrorMessage());
									fireErrorOnSaveEvent(loader.getLastErrorUserMessage());
									for (final IMessageManager mm : mmList) {
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
							// If the beammap has been created but no value assigned,
							// in this case, warper.isDirty()==false, so we need to force
							// creation if there are some links
							if (sourceId == 0) {
								final BeanMap newBeanMap = loader.createBeanMap(warper.getModifications());
								if (newBeanMap != null) {
									mustBeRefresh = true;
									id = newBeanMap.getId();
									sourceId = id;
									lwarper.setId(id);
								}
							}
							// Now we can save the links
							lwarper.save();
							saved = true;
						}
					}
					if (saved) {
						reload();
					}
					// Remove any save error messages !
					for (final IMessageManager mm : mmList) {
						mm.removeMessage(SAVEMESSAGE);
					}
					// fire an event !
					if (saved) {
<<<<<<< master
						// Envoi de l'evenement specifique de gestion des sauvegardes
						// FIXME L'emploi de fireBeanMapSavedEvent et saveEvents est redondant. 
=======
						// Envoi de l'evenement specifique de gestion des
						// sauvegardes
						// FIXME L'emploi de fireBeanMapSavedEvent et saveEvents est redondant.
>>>>>>> 38f2e60 Clean-up AFS Client
						fireBeanMapSavedEvent();
						fireSaveEvent();
						// FIXME Sémantiquement le BeanMap n'a pas changé pourquoi déclencher un évènement Changed
						// ?
						// FIXME Surtout que cet évènement vient déjà d'être déclanché 10 lignes plus haut
						// (reload()).
						fireChangedEvent();
					}
					result = saved;
				}
			}
		}
		return result;
	}

	public void fireSaveEvent() {
		final BeanMapEvent event = new BeanMapEvent(warper);
<<<<<<< master
		for (final IBeanMapChangedListener listener: saveEvents) {
=======
		for (final Object listener : saveEvents.getListeners()) {
>>>>>>> 38f2e60 Clean-up AFS Client
			SafeRunnable.run(new SafeRunnable() {
				@Override
				public void run() {
<<<<<<< master
					listener.changed(event);
=======
					((IBeanMapChangedListener) listener).changed(event);
>>>>>>> 38f2e60 Clean-up AFS Client
				}
			});
		}
	}

	@Override
	public boolean canSavedEditor(IEntityAttributeProcessListener callback) {
		boolean result = true;
		for (final MetaDataAttribute attribute : getStructure().getAttributes().values()) {
			if (attribute.isMandatory()) {
				final Object attributeValue = getCurrentBean().get(attribute.getCode());
				// String name = entry.getValue().getName();
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
				// for (String code : mandatoryAttributes) {
				for (final MandatoryAttribute mandatoryAttribute : mandatoryAttributes) {
					final String code = mandatoryAttribute.getCode();
					boolean mandatory = true;
					if (mandatoryAttribute.isConditionned()) {
						final String conditionnedBy = mandatoryAttribute.getConditionedBy();
						mandatory = getCurrentBean().getBoolean(conditionnedBy);
					}
					if (mandatory) {
						final Object attributeValue = getCurrentBean().get(code);
						// String name = getStructure().getAttribute(code).getName();
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

	@Override
	public boolean canSavedEditor(boolean withErrorMessage) {
		final boolean showMessage = withErrorMessage;
		final IEntityAttributeProcessListener callback = new IEntityAttributeProcessListener() {

			@Override
			public void processAction(MetaDataAttribute entry) {
				if (showMessage) {
					openCannotSaveWarning(entry.getName());
				}
			}

		};
		return canSavedEditor(callback);
<<<<<<< master
=======
		//
		//
		// boolean result = true;
		// Set<Entry<String, MetaDataAttribute>> attributesEntries = getStructure().getAttributes().entrySet();
		// for (Entry<String, MetaDataAttribute> entry : attributesEntries) {
		// if (entry.getValue().isMandatory()) {
		// Object attributeValue = getCurrentBean().get(entry.getKey());
		// String name = entry.getValue().getName();
		// if (attributeValue != null) {
		// if ((attributeValue instanceof String) && (((String) attributeValue).length() == 0)) {
		// result = false;
		// if (withErrorMessage) {
		// openCannotSaveWarning(name);
		// }
		// break;
		// }
		// } else {
		// result = false;
		// if (withErrorMessage) {
		// openCannotSaveWarning(name);
		// }
		// break;
		// }
		// }
		// }
		// if (result) {
		// if (mandatoryAttributes != null) {
		// for (String code : mandatoryAttributes) {
		// Object attributeValue = getCurrentBean().get(code);
		// String name = getStructure().getAttribute(code).getName();
		// if (attributeValue != null) {
		// if ((attributeValue instanceof String) && (((String) attributeValue).length() == 0)) {
		// result = false;
		// if (withErrorMessage) {
		// openCannotSaveWarning(name);
		// }
		// break;
		// }
		// } else {
		// result = false;
		// if (withErrorMessage) {
		// openCannotSaveWarning(name);
		// }
		// break;
		// }
		// }
		// }
		// }
		// return result;
>>>>>>> 38f2e60 Clean-up AFS Client
	}

	@Override
	public boolean canSavedEditor() {
		boolean result = canSavedEditor(true);
<<<<<<< master
=======

>>>>>>> 38f2e60 Clean-up AFS Client
		// Complete with possible added controls
<<<<<<< master
		for (IBeanMapControlerListener approver: beforeSaveControlers) {
			result &= approver.isValid();
=======
		if ((beforeSaveControlers != null) && (beforeSaveControlers.size() > 0)) {
			for (final Object approver : beforeSaveControlers.getListeners()) {
				result &= ((IBeanMapControlerListener) approver).isValid();
			}
>>>>>>> 38f2e60 Clean-up AFS Client
		}
		return result;
	}

	protected void openCannotSaveWarning(String name) {
		MessageDialog.openWarning(LoggedUIPlugin.getShell(), Activator.getInstance().resString("editor.renderer.save.error.title"), //$NON-NLS-1$
				StringTools.substitute(Activator.getInstance().resString("editor.renderer.save.error.text"), "$name", name)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public void fireChangedEvent() {
		if (!changefiring) {
			changefiring = true;
			try {
<<<<<<< master
				for (final IEditorChangeListener listener: changeEvents) {
=======
				for (final Object listener : changeEvents.getListeners()) {
					final IEditorChangeListener l = (IEditorChangeListener) listener;
>>>>>>> 38f2e60 Clean-up AFS Client
					SafeRunnable.run(new SafeRunnable() {
						@Override
						public void run() {
							listener.changed(SWTRenderer.this);
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
<<<<<<< master
		for (final IEditorTitleChangeListener listener: titleEvents) {
=======
		for (final Object listener : titleEvents.getListeners()) {
			final IEditorTitleChangeListener l = (IEditorTitleChangeListener) listener;
>>>>>>> 38f2e60 Clean-up AFS Client
			SafeRunnable.run(new SafeRunnable() {
				@Override
				public void run() {
					listener.changed(SWTRenderer.this, title);
				}
			});
		}
	}

	@Override
	public void addChangeListener(IEditorChangeListener listener) {
		changeEvents.add(listener);
	}

	@Override
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
		final Object provider = element.getProvider();
		if (provider instanceof IContainerSWTProvider) {
			((IContainerSWTProvider) provider).create(this, element, element.getContaint().isEmpty(), getStructure());
		} else if (provider instanceof IInputSWTProvider) {
			// FIXME A refaire sans modifier l'ENTITY !!!!!!
			final boolean old = element.getElement().isReadonly();
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

	@Override
	public ImageDescriptor getImageDescriptor(String key) {
		final int pos = key.indexOf(":"); //$NON-NLS-1$
		ImageDescriptor result = null;
		if (pos > 0) {
			final String bundleId = key.substring(0, pos);
			final String imageKey = key.substring(pos + 1);
			result = AbstractUIPlugin.imageDescriptorFromPlugin(bundleId, imageKey);
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

	@Override
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

	@Override
	public FormToolkit getToolkit() {
		return toolkit;
	}

	private void addManagedControl(Binding binding, Element element, IMessageManager manager, Control control) {
<<<<<<< master
=======
		// Pour les tests...
		/*
		 * ArrayList<MMessages> list = mmtable.get(element.getCode()); if (list == null) { list = new
		 * ArrayList<MMessages>(); } list.add(new MMessages(element, manager, control)); mmtable.put(element.getCode(),
		 * list);
		 */
>>>>>>> 38f2e60 Clean-up AFS Client
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

	@Override
	public Composite getParent() {
		return parent;
	}

	@Override
	public void createSubContainer(IContainerSWTProvider parentContainerProvider, ILayoutParameters parameters,
			Composite compositeParent) {
		if (firstParent == null) {
			firstParent = compositeParent;
		}
		final Composite oldParent = parent;
		final IContainerSWTProvider oldParentProvider = parentProvider;
		try {
			parent = compositeParent;
			parentProvider = parentContainerProvider;
			if (parentProvider instanceof IValidatingSubWidgets) {
				for (final LayoutElement element : ((LayoutElement) parameters).getContaint()) {
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
				for (final LayoutElement element : ((LayoutElement) parameters).getContaint()) {
					renderElement(element);
				}
			}
		} finally {
			parent = oldParent;
			parentProvider = oldParentProvider;
		}
	}

	@Override
	public void createSubContainer(IContainerSWTProvider parentContainerProvider, IMessageManager messageManager,
			ILayoutParameters parameters, Composite parentComposite, ArrayList<IAction> actions) {
		// Collect this message manager for global messages...
		if (messageManager != null) {
			boolean newone = true;
			for (final IMessageManager mm : mmList) {
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
		final IMessageManager oldmanager = currentMessageManager;
		final ArrayList<IAction> oldActions = currentIActions;
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

	@Override
	public IContainerSWTProvider getParentProvider() {
		return parentProvider;
	}

	@Override
	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		warper.addSelectionChangedListener(listener);
	}

	@Override
	public ISelection getSelection() {
		return warper;
	}

	@Override
	public void removeSelectionChangedListener(ISelectionChangedListener listener) {
		warper.removeSelectionChangedListener(listener);
	}

	@Override
	public void setSelection(ISelection selection) {
		IBeanMap value = null;
		if (selection instanceof BeanMapWarper) {
			if (warper != ((BeanMapWarper) selection)) {
				value = (BeanMapWarper) selection;
			}
		} else if (selection instanceof IBeanMap) {
			value = (IBeanMap) selection;
		} else if (selection instanceof IAdaptable) {
			value = ((IAdaptable) selection).getAdapter(IBeanMap.class);
		}
		if (value != null) {
			rendererBinding.importAttributes(value);
		}
	}

	@Override
	public void loadBeanMap(String type, int id, IBeanMapListener listener) {
		if (loader == null) {
			return;
		}
		loader.loadBeanMap(type, id, listener);
	}

	@Override
	public void addLoadListener(IBeanMapChangedListener listener) {
		loadEvents.add(listener);
	}

	@Override
	public void removeLoadListener(IBeanMapChangedListener listener) {
		loadEvents.remove(listener);
	}

	@Override
	public void addBeforeSaveControler(IBeanMapControlerListener listener) {
		beforeSaveControlers.add(listener);
	}

	@Override
	public void removeBeforeSaveControler(IBeanMapControlerListener listener) {
		beforeSaveControlers.remove(listener);
	}

	@Override
	public void addSaveListener(IBeanMapChangedListener listener) {
		saveEvents.add(listener);
	}

	@Override
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

	@Override
	public void forceDirty() {
		synchronized (warper) {
			warper.forceDirty();
		}
		fireChangedEvent();
	}

	@Override
	public boolean isRecordable() {
<<<<<<< master
=======
		// if (unvalidTest.isEmpty()) {
		// return true;
		// }
		// for (MetaDataTest test : unvalidTest.values()) {
		// if (test.isCritical()) {
		// return false;
		// }
		// }
>>>>>>> 38f2e60 Clean-up AFS Client
		return true;
	}

<<<<<<< master
=======
	/*
	 * (non-Javadoc)
	 * @seecom.arcadsoftware.editor.swt.ISWTRenderer#removeSaveListener(com.
	 * arcadsoftware.editor.swt.IBeanMapChangedListener)
	 */
	@Override
>>>>>>> 38f2e60 Clean-up AFS Client
	public void removeSaveListener(IBeanMapChangedListener listener) {
		saveEvents.remove(listener);
	}

	public boolean isValided(MetaDataTest test) {
		return false;
	}

<<<<<<< master
	public void updateTest(MetaDataTest test, boolean isValid) {}
=======
	public void updateTest(MetaDataTest test, boolean isValid) {
		// if (isValid) {
		// if (unvalidTest.get(test.getCode()) != null) {
		// unvalidTest.remove(test.getCode());
		// fireTestUpdatedEvent(test, true);
		// }
		// // Remove messages...
		// for (IMessageManager manager : mmList) {
		// manager.removeMessage(test);
		// }
		// for (String code : test.getAttributes()) {
		// MetaDataAttribute att = getStructure().getAttribute(code);
		// if (att != null) {
		// ArrayList<MMessages> list = mmtable.get(att.getCode());
		// if (list != null) {
		// for (MMessages mm : list) {
		// mm.removeMessage(test);
		// }
		// }
		// }
		// }
		// } else {
		// if (unvalidTest.get(test.getCode()) == null) {
		// unvalidTest.put(test.getCode(), test);
		// fireTestUpdatedEvent(test, false);
		// }
		// // Add messages...
		// for (IMessageManager manager : mmList) {
		// if (test.isCritical()) {
		// manager.addMessage(test, test.getMessage(), null, IMessageProvider.ERROR);
		// } else {
		// manager.addMessage(test, test.getMessage(), null, IMessageProvider.WARNING);
		// }
		// }
		// for (String code : test.getAttributes()) {
		// MetaDataAttribute att = getStructure().getAttribute(code);
		// if (att != null) {
		// ArrayList<MMessages> list = mmtable.get(att.getCode());
		// if (list != null) {
		// for (MMessages mm : list) {
		// mm.addMessage(test, test.getMessage(), test.isCritical());
		// }
		// }
		// }
		// }
		// }
	}
>>>>>>> 38f2e60 Clean-up AFS Client

<<<<<<< master
	public void addValidityListener(IControlValidityListener listener) {}
=======
	/*
	 * private void fireTestUpdatedEvent(MetaDataTest test, boolean valid) { final ControlValidityEvent event = new
	 * ControlValidityEvent(test, valid); Object[] listeners = validityEvents.getListeners(); for (int i = 0; i <
	 * listeners.length; ++i) { final IControlValidityListener l = (IControlValidityListener) listeners[i];
	 * SafeRunnable.run(new SafeRunnable() { public void run() { l.validityChanged(event); } }); } }
	 */
>>>>>>> 38f2e60 Clean-up AFS Client

<<<<<<< master
	public void removeValidityListener(IControlValidityListener listener) {}
=======
	/*
	 * (non-Javadoc)
	 * @seecom.arcadsoftware.editor.swt.ISWTRenderer#addValidityListener(com.
	 * arcadsoftware.editor.swt.IControlValidityListener)
	 */
	@Override
	public void addValidityListener(IControlValidityListener listener) {
		validityEvents.add(listener);
	}
>>>>>>> 38f2e60 Clean-up AFS Client

<<<<<<< master
=======
	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#removeValidityListener(com.
	 * arcadsoftware.editor.swt.IControlValidityListener)
	 */
	@Override
	public void removeValidityListener(IControlValidityListener listener) {
		validityEvents.remove(listener);
	}

	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#reload()
	 */
	@Override
>>>>>>> 38f2e60 Clean-up AFS Client
	public void reload() {
		if (sourceId != 0) {
			// Reload must be updated from the server !
			load(sourceId);
		}
	}

<<<<<<< master
=======
	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#getColor(java.lang.String)
	 */
	@Override
>>>>>>> 38f2e60 Clean-up AFS Client
	public Color getColor(String cssColor) {
		Color result = null;
		if (cssColor != null) {
			if (cssColor.charAt(0) == '#') {
				if (cssColor.length() == 7) {
					// Color is like #FFFFFF
					try {
						final int red = Integer.decode(_0X + cssColor.substring(1, 3)).intValue();
						final int green = Integer.decode(_0X + cssColor.substring(3, 5)).intValue();
						final int blue = Integer.decode(_0X + cssColor.substring(5)).intValue();
						result = new Color(Display.getDefault(), new RGB(red, green, blue));
<<<<<<< master
					} catch (Exception e) {
=======
						// result = new Color(parent.getDisplay(), new RGB(red,
						// green, blue));
					} catch (final Exception e) {
>>>>>>> 38f2e60 Clean-up AFS Client
						result = null;
					}
				} else if (cssColor.length() == 4) {
					// Color is like #FFF
					try {
						final int red = Integer.decode(_0X + cssColor.substring(1, 2) + cssColor.substring(1, 2))
								.intValue();
						final int green = Integer.decode(_0X + cssColor.substring(2, 3) + cssColor.substring(2, 3))
								.intValue();
						final int blue = Integer.decode(_0X + cssColor.substring(4) + cssColor.substring(4)).intValue();
						result = new Color(parent.getDisplay(), new RGB(red, green, blue));
					} catch (final Exception e) {
						result = null;
					}
				}
			}
			final String color = ColorMapTable.TABLE.get(cssColor);
			if (color != null) {
				result = getColor(color);
			}
		}
		return result;
	}

	/**
	 * @return a current BeanMap value copy.
	 */
	@Override
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
		final ArrayList<IAction> actions = new ArrayList<>(2);
		for (final IActionElement action : getActions()) {
			actions.add(new SWTScriptAction(this, action));
		}
		return actions;
	}

<<<<<<< master
=======
	/*
	 * (non-Javadoc)
	 * @seecom.arcadsoftware.editor.swt.ISWTRenderer#declareGlobalAction(com. arcadsoftware.editor.IActionElement)
	 */
	@Override
>>>>>>> 38f2e60 Clean-up AFS Client
	public void declareGlobalAction(IActionElement action) {
		addAction(action);
	}

<<<<<<< master
=======
	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#getAction(com.arcadsoftware .editor.IActionElement)
	 */
	@Override
>>>>>>> 38f2e60 Clean-up AFS Client
	public IAction getAction(IActionElement action) {
		return new SWTScriptAction(this, action);
	}

<<<<<<< master
=======
	/**
	 *
	 */
>>>>>>> 38f2e60 Clean-up AFS Client
	public void setFocus() {
		if ((firstParent != null) && !firstParent.isDisposed()) {
			firstParent.setFocus();
		}
	}

	/**
	 * @param memento
	 */
	public void restorState(IMemento memento) {
		synchronized (warper) {
			final IMemento m = memento.getChild(ATTRIBUTES);
			if (m != null) {
				restoreBeanMap(m, warper);
			}
		}
		synchronized (lwarper) {
			final IMemento m = memento.getChild(LINKS);
			if (m != null) {
				for (final IMemento oms : m.getChildren(OPERATION)) {
					IMemento om = oms.getChild(ADD);
					if (om != null) {
						for (final IMemento o : om.getChildren(OP)) {
							final BeanMap bm = new BeanMap(o.getID(), o.getInteger(IDID).intValue());
							restoreBeanMap(o, bm);
							lwarper.updateLinkList(oms.getID(), LinkMapWarper.LINKLIST_ADD, bm);
						}
					}
					om = oms.getChild(REMOVE);
					if (om != null) {
						for (final IMemento o : om.getChildren(OP)) {
							final BeanMap bm = new BeanMap(o.getID(), o.getInteger(IDID).intValue());
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
		for (final String key : am.getAttributeKeys()) {
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
			final IMemento m = memento.createChild(LINKS);
			for (final Entry<String, Operations> oe : lwarper.getPendingsOperations()) {
				final IMemento om = m.createChild(OPERATION, oe.getKey());
				saveOperations(om.createChild(ADD), oe.getValue().addList);
				saveOperations(om.createChild(REMOVE), oe.getValue().removeList);
			}
		}
	}

	private void saveBeanMap(IMemento am, BeanMap bm) {
		for (final Entry<String, Object> entry : bm.entrySet()) {
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
		for (final BeanMap bm : list.values()) {
			final IMemento b = memento.createChild(OP, bm.getType());
			b.putInteger(IDID, bm.getId());
			saveBeanMap(b, bm);
		}
	}

	@Override
	public void addLinkitem(MetaDataLink link, BeanMap item) {
		rendererBinding.getObservableLink(link, true, null).add(item);
		loadListCompleted(item.getType());
	}

	@Override
	public ITreeContentProvider getTreeContentProvider(MetaDataLink link, String fatherCode) {
		final BeanMapObservableLink l = (BeanMapObservableLink) rendererBinding.getObservableLink(link, true, null);
		return new ObservableListTreeContentProvider(new BeanMapObservableLinkTreeFactory(this, l, fatherCode),
				new BeanMapStructureAdvisor(l, fatherCode));
	}

	@Override
	public void removeLinkitem(MetaDataLink link, BeanMap item) {
		rendererBinding.getObservableLink(link, true, null).remove(item);
		loadListCompleted(item.getType());
	}

	@Override
	public void registerAction(IAction action) {
		if (currentIActions != null) {
			currentIActions.add(action);
		}
	}

	@Override
	public void handleChange(ChangeEvent event) {
		// this is a very verbose firing...
		fireChangedEvent();
	}

	@Override
	public void handleListChange(ListChangeEvent event) {
		// this is a very verbose firing...
		fireChangedEvent();
	}

	@Override
	public MetaDataEntity getStructure(Element element) {
		return getLoader().loadMetaDataEntity(element.getType());
	}

	@Override
	public MetaDataEntity getStructure(String type) {
		return getLoader().loadMetaDataEntity(type);
	}

	@Override
	public InputStream getBeanStream(String type, int id) {
		return loader.loadStream(type, id);
	}

	@Override
	public boolean updateBeanStream(String type, int id, File file) {
		return loader.updateStream(type, id, file);
	}

	@Override
	public String getUploadBeanStreamAddress(String type, int id) {
		return loader.getUploadBeanStreamAddress(type, id);
	}

	@Override
	public BeanMap loadBeanMap(String type, int id) {
		return loader.loadBeanMap(type, id);
	}

	@Override
	public BeanMap loadBeanMap(String type, int id, String attributeList) {
		return loader.loadBeanMap(type, id, attributeList);
	}

	@Override
	public BeanMap createBeanMap(BeanMap beanMap) {
		return loader.createBeanMap(beanMap);
	}

	@Override
	protected IProviderFactory getProviderFactory() {
		return providerFactory;
	}

<<<<<<< master
=======
	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#getAction(java.lang.String)
	 */
	@Override
>>>>>>> 38f2e60 Clean-up AFS Client
	public IAction getAction(String code) {
		if (code == null) {
			return null;
		}
		for (final IActionElement action : getActions()) {
			if (code.equals(action.getCode())) {
				return new SWTScriptAction(this, action);
			}
		}
		return null;
	}

<<<<<<< master
=======
	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#getTitleImage()
	 */
	@Override
>>>>>>> 38f2e60 Clean-up AFS Client
	public Image getTitleImage() {
		final String key = getParam(ICON, null);
		ImageDescriptor imageDescriptor = null;
		if (key != null) {
			imageDescriptor = getImageDescriptor(key);
		}
		return (imageDescriptor != null) ? imageDescriptor.createImage() : null;
	}

<<<<<<< master
=======
	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.swt.ISWTRenderer#getHelpContextId()
	 */
	@Override
>>>>>>> 38f2e60 Clean-up AFS Client
	public String getHelpContextId() {
		return getParam(HELPCONTEXTID, null);
	}

<<<<<<< master
=======
	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.swt.IEditorChangeListener#changed(com.arcadsoftware .editor.swt.ISWTRenderer)
	 */
	@Override
>>>>>>> 38f2e60 Clean-up AFS Client
	public void changed(ISWTRenderer renderer) {
		fireChangedEvent();
	}

	@Override
	public Date getUpdateDate() {
		final BeanMap bean = getCurrentBean();
		return (bean != null) ? bean.getDate() : null;
	}

	@Override
	public BeanMap getSelectedBeanMap() {
		return getCurrentBean();
	}

	@Override
	public void addListenerWidget(IListenerWidget listenerWidget, String widgetId) {
		if (listenerWidgets == null) {
			listenerWidgets = new HashMap<>();
		}
		List<IListenerWidget> list = listenerWidgets.get(widgetId);
		if (list == null) {
			list = new ArrayList<>();
			listenerWidgets.put(widgetId, list);
		}
		list.add(listenerWidget);
	}

	@Override
	public void fireListenedWidgetChanged(IListenedWidget listenedWidget, BeanMap beanMap) {
		if (listenerWidgets != null) {
			final List<IListenerWidget> list = listenerWidgets.get(listenedWidget.getId());
			if (list != null) {
				for (final IListenerWidget listenerWidget : list) {
					listenerWidget.refreshWidget(beanMap);
				}
			}
		}
	}

	@Override
	public void refreshSelector(BeanMap beanMap) {
		// Do nothing
	}

	@Override
	public void addMandatoryAttribute(String code) {
		if (mandatoryAttributes == null) {
<<<<<<< master
			mandatoryAttributes = new ArrayList<MandatoryAttribute>();
=======
			// mandatoryAttributes = new ArrayList<String>();
			mandatoryAttributes = new ArrayList<>();
>>>>>>> 38f2e60 Clean-up AFS Client
		}
<<<<<<< master
=======
		// mandatoryAttributes.add(code);
>>>>>>> 38f2e60 Clean-up AFS Client
		mandatoryAttributes.add(new MandatoryAttribute(code));
	}

	@Override
	public void addMandatoryAttribute(String code, String conditionnedBy) {
		if (mandatoryAttributes == null) {

			mandatoryAttributes = new ArrayList<>();
		}
		mandatoryAttributes.add(new MandatoryAttribute(code, conditionnedBy));
	}

	@Override
	public IInternalEditors getInternalEditors() {
		return internalEditors;
	}

	@Override
	public ILoadingListeners getLoadingListeners() {
		return loadingListeners;
	}

	@Override
	public IUpdateDateListeners getUpdateDateListeners() {
		return updateDateListeners;
	}

	@Override
	public IRendererActions getRendererActions() {
		return rendererActions;
	}

	@Override
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

	@Override
	public void refreshEditorContent(BeanMap beanMap, ISWTRenderer renderer) {
		internalEditors.refreshEditorsContent(beanMap, renderer);
		rendererBinding.refreshBean(beanMap);
	}

	private IStatus doRefreshEditors(BeanMap newBeanMap, final ISWTRenderer renderer) {
		final IWorkbenchPage[] pages = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getPages();
		if (pages != null) {
			for (final IWorkbenchPage workbenchPage : pages) {
				final IEditorReference[] editors = workbenchPage.getEditorReferences();
				if (editors != null) {
					for (final IEditorReference editorReference : editors) {
						final Object editor = editorReference.getEditor(false);
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

	@Override
	public void refreshAllEditors(BeanMap beanMap, final ISWTRenderer renderer) {
		final BeanMap newBeanMap = loadBeanMap(beanMap);
		if (newBeanMap != null) {
			UIJob job = null;
			if (parentDisplay != null) {
				job = new UIJob(parentDisplay, Activator.getInstance().resString("refreshAllEditorsJobUiName")) { //$NON-NLS-1$
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						return doRefreshEditors(newBeanMap, renderer);
					}
				};
			} else {
				job = new UIJob(Activator.getInstance().resString("refreshAllEditorsJobUiName")) { //$NON-NLS-1$
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						return doRefreshEditors(newBeanMap, renderer);
					}
				};
			}
			if (job != null) {
				job.schedule();
			}
		}
	}

	private BeanMap loadBeanMap(BeanMap beanMap) {
		return (beanMap != null) ? loadBeanMap(beanMap.getType(), beanMap.getId()) : beanMap;
	}

	@Override
	public void setFormToolBar(IToolBarManager toolBarManager) {
		formToolBarManager = toolBarManager;
		formToolbarManagers.add(formToolBarManager);
	}

	@Override
	public void addActionOnFormToolBar(IEditorAction action) {
		if (formToolBarManager != null) {
			formToolBarManager.add(action);
			formToolBarManager.update(true);
		}
	}

	@Override
	public void updateFormToolbar() {
		for (final IToolBarManager formToolbarManagerItem : formToolbarManagers) {
			formToolbarManagerItem.update(true);
			final IContributionItem[] items = formToolbarManagerItem.getItems();
			for (final IContributionItem item : items) {
				if (item instanceof ActionContributionItem) {
					final ActionContributionItem actionItem = (ActionContributionItem) item;
					if (actionItem.getAction() instanceof IRefreshableAction) {
						((IRefreshableAction) actionItem.getAction()).refresh();
					}
				}
			}
		}
	}

	@Override
	public void selectBeanMap(BeanMap bm) {
		// Do nothing
	}

	@Override
	public ISWTRenderer getParentRenderer() {
		return parentRenderer;
	}

	@Override
	public void setParentRenderer(ISWTRenderer renderer) {
		parentRenderer = renderer;
	}

	@Override
	public void addBeanMapSaveListener(IBeanMapSavedListener listener) {
		beanMapSavedBeanListener.add(listener);
	}

	@Override
	public void removeBeanMapSaveListener(IBeanMapSavedListener listener) {
		beanMapSavedBeanListener.remove(listener);
	}

	protected void fireBeanMapSavedEvent() {
<<<<<<< master
		for (final IBeanMapSavedListener listener: beanMapSavedBeanListener) {
=======
		final Object[] listeners = beanMapSavedBeanListener.getListeners();
		for (final Object listener : listeners) {
			final IBeanMapSavedListener l = (IBeanMapSavedListener) listener;
>>>>>>> 38f2e60 Clean-up AFS Client
			SafeRunnable.run(new SafeRunnable() {
				@Override
				public void run() {
					listener.beanMapSaved(getCurrentBean());
				}
			});
		}
	}
<<<<<<< master
	
=======

>>>>>>> 38f2e60 Clean-up AFS Client
	public void addErrorOnSaveListener(IBeanMapErrorOnSaveListener listener) {
		errorOnSavedListener.add(listener);
	}

	public void removeErrorOnSaveListener(IBeanMapErrorOnSaveListener listener) {
		errorOnSavedListener.remove(listener);
	}

	/**
	 * Fire message
	 *
	 * @param errorMessage
	 */
	protected void fireErrorOnSaveEvent(final String errorMessage) {
<<<<<<< master
		for (final IBeanMapErrorOnSaveListener listener: errorOnSavedListener) {
=======
		final Object[] listeners = errorOnSavedListener.getListeners();
		for (final Object listener : listeners) {
			final IBeanMapErrorOnSaveListener l = (IBeanMapErrorOnSaveListener) listener;
>>>>>>> 38f2e60 Clean-up AFS Client
			SafeRunnable.run(new SafeRunnable() {
				@Override
				public void run() {
					listener.onErrorOnSave(getCurrentBean(), errorMessage);
				}
			});
		}
	}

	/**
	 * Fire User message
	 *
	 * @param errorUserMessage
	 */
	protected void fireErrorOnSaveEvent(final UserMessage errorUserMessage) {
<<<<<<< master
		for (final IBeanMapErrorOnSaveListener listener: errorOnSavedListener) {
=======
		final Object[] listeners = errorOnSavedListener.getListeners();
		for (final Object listener : listeners) {
			final IBeanMapErrorOnSaveListener l = (IBeanMapErrorOnSaveListener) listener;
>>>>>>> 38f2e60 Clean-up AFS Client
			SafeRunnable.run(new SafeRunnable() {
				@Override
				public void run() {
					listener.onErrorOnSave(getCurrentBean(), errorUserMessage);
				}
			});
		}
	}

	public void loadListCompleted(String type) {
		if (loadedListListeners != null) {
			// On scinde la boucle pour eviter les problemes des modifications concurrentes
			final ArrayList<ILoadedListListener> listerToDelete = new ArrayList<>();
			for (final ILoadedListListener listener : loadedListListeners) {
				try {
					if (listener.getListType().equals(type)) {
						listener.loadedListComplete(this);
					}
				} catch (final RuntimeException e) {
					listerToDelete.add(listener);
<<<<<<< master
=======
					// removeLoadedList(listener);
>>>>>>> 38f2e60 Clean-up AFS Client
				}
			}
<<<<<<< master
=======
			// for (ILoadedList listener : listerToDelete) {
			// listerToDelete.add(listener);
			// }
>>>>>>> 38f2e60 Clean-up AFS Client
		}
	}

	@Override
	public void addLoadedList(ILoadedListListener listener) {
		loadedListListeners.add(listener);
	}

	@Override
	public void removeLoadedList(ILoadedListListener listener) {
		loadedListListeners.remove(listener);
	}

	@Override
	public Object getVirtualValue(String key) {
		return (virtualValues != null) ? virtualValues.get(key) : null;
	}

	@Override
	public void putVirtualValue(String key, Object value) {
		if (virtualValues == null) {
			virtualValues = new HashMap<>();
		}
		virtualValues.put(key, value);
	}

<<<<<<< master
=======
	@Override
>>>>>>> 38f2e60 Clean-up AFS Client
	public Object put(String key, Object value) {
		synchronized (warper) {
			return warper.put(key, value);
		}
	}

	/**
	 * @return the mandatoryAttributes
	 */
<<<<<<< master
	public List<MandatoryAttribute> getMandatoryAttributes() {		
=======
	// public List<String> getMandatoryAttributes() {
	@Override
	public List<MandatoryAttribute> getMandatoryAttributes() {
>>>>>>> 38f2e60 Clean-up AFS Client
		return mandatoryAttributes;
	}

	public IRightControler getRightControler() {
		return rightControler;
	}

	public void setRightControler(IRightControler rightControler) {
		this.rightControler = rightControler;
	}

	@Override
	public void setParentReadOnly(boolean readOnly) {
		if (parent instanceof DynamicEditorComposite) {
			((DynamicEditorComposite) parent).setInputEnabled(!readOnly);
		}
	}

	@Override
	public IToolBarManager getFormToolBarManager() {
		return formToolBarManager;
	}

	@Override
	public void addActivatedListeners(IActivated listener) {
		activatedListeners.add(listener);
	}

	@Override
	public void removeActivatedListeners(IActivated listener) {
		activatedListeners.remove(listener);
	}

	@Override
	public void fireActivatedEvent() {
<<<<<<< master
		for (IActivated listener : activatedListeners) {
			try {
				listener.activated();
			} catch (RuntimeException e) {
				removeActivatedListeners(listener);
=======
		if (activatedListeners != null) {
			for (final IActivated listener : activatedListeners) {
				try {
					listener.activated();
				} catch (final RuntimeException e) {
					removeActivatedListeners(listener);
				}
>>>>>>> 38f2e60 Clean-up AFS Client
			}
		}
<<<<<<< master
=======

>>>>>>> 38f2e60 Clean-up AFS Client
	}

	public IEditorLoader getEditorLoader() {
		return getLoader();
	}

	@Override
	public IMessageManager getMessageManager() {
		return currentMessageManager;
	}
<<<<<<< master
	
=======

	@Override
>>>>>>> 38f2e60 Clean-up AFS Client
	public Object runScriptAction(String name, Map<String, Object> parameters) {
		final IScriptAction sa = providerFactory.getScriptAction(name);
		if (sa == null) {
			return null;
		}
		sa.set(IScriptAction.PARAM_ENTITY, getStructure());
		sa.set("renderer", this); //$NON-NLS-1$
		sa.set(IScriptAction.PARAM_ITEM, getCurrentBean());
		sa.set("readonly", readOnly); //$NON-NLS-1$
		sa.set("messageManager", currentMessageManager); //$NON-NLS-1$
		if (parameters != null) {
			for (final Entry<String, Object> entry : parameters.entrySet()) {
				sa.set(entry.getKey(), entry.getValue());
			}
		}
		try {
			if (sa.run()) {
				return sa.get(IScriptAction.PARAM_RESULT);
			}
		} catch (final ScriptExecutionException e) {
		}
		return null;
	}

	public void requestSave() {
		save();
	}
<<<<<<< master
	
=======

>>>>>>> 38f2e60 Clean-up AFS Client
}
