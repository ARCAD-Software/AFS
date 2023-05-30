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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.databinding.Binding;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.observable.IObservable;
import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapEvent;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.implementation.Activator;
import com.arcadsoftware.editor.implementation.swt.binding.BeanMapContainerObservableList;
import com.arcadsoftware.editor.implementation.swt.binding.BeanMapObservableLink;
import com.arcadsoftware.editor.implementation.swt.binding.BeanMapObservableLinkedList;
import com.arcadsoftware.editor.implementation.swt.binding.BeanMapObservableList;
import com.arcadsoftware.editor.implementation.swt.binding.BeanMapObservableValue;
import com.arcadsoftware.editor.implementation.swt.binding.ContainerObservableValue;
import com.arcadsoftware.editor.implementation.swt.binding.DateTimeObservableValue;
import com.arcadsoftware.editor.implementation.swt.binding.ModeltoTargetTreeUpdateStrategy;
import com.arcadsoftware.editor.implementation.swt.binding.TargettoModelTreeUpdateStrategy;
import com.arcadsoftware.editor.implementation.swt.binding.WidgetObservableValue;
import com.arcadsoftware.editor.swt.IBeanMapContainer;
import com.arcadsoftware.editor.swt.IBeanMapContainerLinkedList;
import com.arcadsoftware.editor.swt.IBeanMapContainerList;
import com.arcadsoftware.editor.swt.IBeanMapContainerValue;
import com.arcadsoftware.editor.swt.IWidgetValue;
import com.arcadsoftware.editor.swt.renderer.IRendererBinding;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataLink;

/**
 * This class is associated to a Renderer and manage data binding.
 */
public class RendererBinding implements IRendererBinding {

	private static final String WIDGET_NOT_OBSERVABLE_ERROR = "Widget not observable ! Try manual binding."; //$NON-NLS-1$
	//private static final String SCRIPT_EXECUTION_ERROR = "Error during Script Execution (Validating Attribute Changing Event)."; //$NON-NLS-1$
	//private static final String CLASS = "class"; //$NON-NLS-1$
	//private static final String NAME = "name"; //$NON-NLS-1$
	private static final String SPACE = " "; //$NON-NLS-1$
	//private static final String EXTENSION_NAME_SCRIPTACTION = "scriptAction"; //$NON-NLS-1$

	private DataBindingContext bindingContext = new DataBindingContext();
	private HashMap<String, BeanMapObservableLink> links = new HashMap<String, BeanMapObservableLink>();
	private HashMap<String, BeanMapObservableList> lists = new HashMap<String, BeanMapObservableList>();
	private HashMap<String, BeanMapObservableLinkedList> linklists = new HashMap<String, BeanMapObservableLinkedList>();
	private HashMap<String, BeanMapObservableValue> attributes = new HashMap<String, BeanMapObservableValue>();
	private SWTRenderer renderer;

	protected RendererBinding(SWTRenderer renderer) {
		this.renderer = renderer;
	}

	public void refreshBean(BeanMap item) {
		if ((item == null) || (item.getId() == 0)) {
			return;
		}
		synchronized (renderer.getWarper()) {
			if (renderer.getWarper().getType().equals(item.getType()) && (renderer.getWarper().getId() == item.getId())) {
				renderer.getWarper().updateCurrent(item);
				return;
			}
		}
		for (Map.Entry<String, BeanMapObservableValue> entry : attributes.entrySet()) {
			BeanMapObservableValue observableValue = entry.getValue();
			if (entry.getKey().equals(item.getType()) && observableValue != null) {
				Object value = observableValue.getValue();
				if (value instanceof BeanMap && ((BeanMap) value).getId() == item.getId()) {
					observableValue.changed(null);
					observableValue.changed(new BeanMapEvent(item));
				}
			}
		}
		for (BeanMapObservableLink link : links.values()) {
			if (link.getLink().getType().equals(item.getType())) {
				link.updateItem(item);
			}
		}
		for (BeanMapObservableList list : lists.values()) {
			if (list.getType().equals(item.getType())) {
				list.updateItem(item);
			}
		}
		for (BeanMapObservableLinkedList list : linklists.values()) {
			if (list.getType().equals(item.getType())) {
				list.updateItem(item);
			}
		}
	}

	public void populateList(String type) {
		BeanMapObservableLink link = links.get(type);
		if ((link != null) && (!link.isPopulated())) {
			link.populate();
		}
	}

	public IObservable getTreeRootCollection(MetaDataLink link, String fatherCode) {
		BeanMapObservableLink olink = (BeanMapObservableLink) getObservableLink(link, true,null);
		WritableList result = new WritableList(new ArrayList<BeanMap>(), BeanMap.class);
		ArrayList<Integer> cl = new ArrayList<Integer>();
		bindingContext.bindList(result, olink, new TargettoModelTreeUpdateStrategy(cl),
				new ModeltoTargetTreeUpdateStrategy(olink, fatherCode, 0, cl));
		return result;
	}

	private IObservableList getObservableLinkedList(MetaDataAttribute attribute, MetaDataAttribute sourceAttribute,
			String linkCode) {

		BeanMapObservableLinkedList result = linklists.get(sourceAttribute.getCode() + SPACE + linkCode);
		if (result == null) {
			result = new BeanMapObservableLinkedList(renderer, attribute, sourceAttribute, linkCode);
			// Old: bindingContext.bindValue(result,
			// getObservableAttribute(attribute), null, null);
			bindingContext.bindValue(result, getObservableAttribute(sourceAttribute), null, null);
			linklists.put(sourceAttribute.getCode() + SPACE + linkCode, result);
		}
		return result;
	}

	public IObservableValue getObservableAttribute(MetaDataAttribute element) {
		BeanMapObservableValue result = attributes.get(element.getCode());
		if (result == null) {
			result = new BeanMapObservableValue(renderer, element, renderer.getWarper());
			result.addChangeListener(renderer);
			attributes.put(element.getCode(), result);
		}
		return result;
	}

	public IObservableList getObservableList(String type, boolean autoPopulate) {
		BeanMapObservableList result = lists.get(type);
		if (result == null) {
			result = new BeanMapObservableList(renderer, type, autoPopulate);
			lists.put(type, result);
		} else if ((!result.isAutoPopulate()) && autoPopulate) {
			// Auto Populated list are prefered...
			result.setAutoPopulate(true);
		}
		return result;
	}

	public void populateLink(String linkName) {
		BeanMapObservableLink link = links.get(linkName);
		if ((link != null) && (!link.isPopulated())) {
			link.populate();
		}
	}

	public BeanMapObservableValue getObservableAttribute(String code) {
		return attributes.get(code);
	}

	public boolean execute(String[] elements, String script) {
		//TODO RAP
//		IScriptEngine engine = Activator.getInstance().openScriptEngine();
//		if (engine != null) {
//			// Disallow load or save of the warper during the script execution.
//			synchronized (renderer.getWarper()) {
//				try {
//					boolean exec = false;
//					// Only observable Attributes or links can be binded !
//					for (String code : elements) {
//						BeanMapObservableValue value = attributes.get(code);
//						if (value == null) {
//							// BeanMapLink link = links.get(code);
//							// TODO need a warper that will be "class loadable"
//							// into the script !
//							// (isEmpty(),length(),getValue(idx,code),remove(idx)).
//						} else {
//							engine.bind(code, renderer.getWarper().get(code));
//							exec = true;
//						}
//					}
//					if (exec) {
//						// Bind API actions !
//						bindScriptActions(engine);
//
//						// TODO Bind an object that can make the status invalid
//						// !
//						// (e.g. condition "toto" + failed/succeed +
//						// message(translated !))
//						try {
//							engine.eval(script);
//							for (String code : elements) {
//								BeanMapObservableValue value = attributes.get(code);
//								if (value == null) {
//									// BeanMapLink link = links.get(code);
//									// TODO need a warper (proceed to
//									// remove(idx)...).
//								} else {
//									Object o = engine.getValue(code);
//									if (o == null) {
//										if (renderer.getWarper().get(code) != null) {
//											value.setValue(null);
//										}
//									} else if (!o.equals(renderer.getWarper().get(code))) {
//										value.setValue(o);
//									}
//								}
//							}
//							return true;
//						} catch (ScriptExecutionException e) {
//							Activator.getInstance().debug(SCRIPT_EXECUTION_ERROR, e);
//						}
//					}
//				} finally {
//					Activator.getInstance().closeStriptEngine(engine);
//				}
//			}
//		}
		return false;
	}

//	private void bindScriptActions(IScriptEngine engine) {
//		IExtensionRegistry reg = Platform.getExtensionRegistry();
//		for (IConfigurationElement element : reg.getConfigurationElementsFor(renderer.getWidgetsExtension())) {
//			if (element.getName().equals(EXTENSION_NAME_SCRIPTACTION)) {
//				try {
//					engine.bind(element.getAttribute(NAME), element.createExecutableExtension(CLASS));
//				} catch (CoreException e) {
//					Activator.getInstance().debug(e);
//				} catch (InvalidRegistryObjectException ee) {
//					Activator.getInstance().debug(ee);
//				}
//			}
//		}
//	}

	public void importAttributes(IBeanMap beanMap) {
		synchronized (renderer.getWarper()) {
			for (Entry<String, Object> entry : beanMap.entrySet()) {
				// We ignore undeclared attributes !
				if ((renderer.getStructure() != null)
						&& (renderer.getStructure().getAttributes().get(entry.getKey()) != null)) {
					BeanMapObservableValue att = attributes.get(entry.getKey());
					if (att != null) {
						att.setValue(entry.getValue());
					} else {
						renderer.getWarper().put(entry.getKey(), entry.getValue());
					}
				}
			}
		}
	}

	public DataBindingContext getBinding() {
		return bindingContext;
	}

	public IObservableList getObservableLink(MetaDataLink element, boolean autoPopulate, String attributeList) {
		return getObservableLink(element, autoPopulate, attributeList, null);
	}
	
	public IObservableList getObservableLink(MetaDataLink element, boolean autoPopulate, String attributeList, String orderList) {
		BeanMapObservableLink result = links.get(element.getCode());
		if (result == null) {
			result = new BeanMapObservableLink(element, renderer.getLWarper(), renderer, autoPopulate,attributeList, orderList);
			result.addListChangeListener(renderer);
			links.put(element.getCode(), result);
		} else if ((!result.isAutoPopulate()) && autoPopulate) {
			// Auto Populated list are prefered...
			result.setAutoPopulate(true);
		}
		return result;
	}

	public void bindElement(Element element, ISelectionProvider selection) throws IllegalArgumentException {
		if (element instanceof MetaDataAttribute) {
			IObservableValue dataObservable = getObservableAttribute((MetaDataAttribute) element);
			IObservableValue valueObservable = ViewersObservables.observeSingleSelection(selection);
			Binding binding = bindingContext.bindValue(valueObservable, dataObservable, null, null);
			if ((renderer.getCurrentMessageManager() != null) && (selection instanceof Viewer)) {
				renderer.addManagedControl(binding, element, ((Viewer) selection).getControl());
			}
		} else if (element instanceof MetaDataLink) {
			IObservableList dataObservable = getObservableLink((MetaDataLink) element, true,null);
			// Create Widget part Observable Pattern...
			IObservableList listObservable = ViewersObservables.observeMultiSelection(selection);
			// Bind the observable parts...
			Binding binding = bindingContext.bindList(listObservable, dataObservable, null, null);
			if ((renderer.getCurrentMessageManager() != null) && (selection instanceof Viewer)) {
				renderer.addManagedControl(binding, element, ((Viewer) selection).getControl());
			}
		}
	}

	public void bindElement(Element element, IBeanMapContainer container) throws IllegalArgumentException {
		bindElement(element,container,null);
	}
	
	public void bindElement(Element element, IBeanMapContainer container, ILayoutParameters parameters) throws IllegalArgumentException {
		if (element instanceof MetaDataAttribute) {
			if (container instanceof IBeanMapContainerValue) {
				IObservableValue dataObservable = getObservableAttribute((MetaDataAttribute) element);
				if (dataObservable != null) {
					Binding binding = bindingContext.bindValue(new ContainerObservableValue(
							(IBeanMapContainerValue) container), dataObservable, null, null);
					if ((renderer.getCurrentMessageManager() != null) && (container.getWidget() instanceof Control)) {
						renderer.addManagedControl(binding, element, (Control) container.getWidget());
					}
				}
			}
			if (container instanceof IBeanMapContainerLinkedList) {
				IObservableList dataObservable = getObservableLinkedList((MetaDataAttribute) element,
						((IBeanMapContainerLinkedList) container).getSourceAttribute(),
						((IBeanMapContainerLinkedList) container).getLinkCode());
				if (dataObservable != null) {
					Binding binding = bindingContext.bindList(new BeanMapContainerObservableList(
							(IBeanMapContainerList) container), dataObservable, null, null);
					if ((renderer.getCurrentMessageManager() != null) && (container.getWidget() instanceof Control)) {
						renderer.addManagedControl(binding, element, (Control) container.getWidget());
					}
				}
			} else if (container instanceof IBeanMapContainerList) {
				IObservableList dataObservable = getObservableList(element.getType(), true);
				if (dataObservable != null) {
					Binding binding = bindingContext.bindList(new BeanMapContainerObservableList(
							(IBeanMapContainerList) container), dataObservable, null, null);
					if ((renderer.getCurrentMessageManager() != null) && (container.getWidget() instanceof Control)) {
						renderer.addManagedControl(binding, element, (Control) container.getWidget());
					}
				}
			}
		} else if (element instanceof MetaDataLink) {
			// if we got an association link we can bind it here.
			if (container instanceof IBeanMapContainerList) {
				
				String attributeList = ((IBeanMapContainerList)container).getAttributeList();
				
				String orderList = ((IBeanMapContainerList)container).getOrderList();
				
				IObservableList dataObservable;
				if(parameters != null){
					dataObservable = getObservableLink((MetaDataLink) element, true,attributeList,orderList, parameters);
				}else{
					dataObservable = getObservableLink((MetaDataLink) element, true,attributeList,orderList);
				}
				if (dataObservable != null) {
					Binding binding = bindingContext.bindList(new BeanMapContainerObservableList(
							(IBeanMapContainerList) container), dataObservable, null, null);
					if ((renderer.getCurrentMessageManager() != null) && (container.getWidget() instanceof Control)) {
						renderer.addManagedControl(binding, element, (Control) container.getWidget());
					}
				}
			}
		}
	}
	
	private IObservableList getObservableLink(MetaDataLink element, boolean autoPopulate, String attributeList,String orderList, ILayoutParameters parameters) {
		BeanMapObservableLink result = links.get(element.getCode());
		if (result == null) {
			result = new BeanMapObservableLink(element, renderer.getLWarper(), renderer, autoPopulate,attributeList,orderList, parameters);
			result.addListChangeListener(renderer);
			links.put(element.getCode(), result);
		} else if ((!result.isAutoPopulate()) && autoPopulate) {
			// Auto Populated list are prefered...
			result.setAutoPopulate(true);
		}
		return result;
	}

	public void bindElement(Element element, Control widget) throws IllegalArgumentException {
		// Create Data part Observable Pattern...
		if (element instanceof MetaDataAttribute) {
			IObservableValue dataObservable = getObservableAttribute((MetaDataAttribute) element);
			IObservableValue valueObservable = null;
			// DateTime binding !
			if (widget instanceof IWidgetValue) {
				valueObservable = new WidgetObservableValue((IWidgetValue) widget);
			} else if (widget instanceof DateTime) {
				valueObservable = new DateTimeObservableValue((DateTime) widget);
				
			//Traitement particulier des Text et des StyledText non pris en charge
			} else if (widget instanceof Text){	
				try {
					//l'observation se fait sur l'evenement Modify et non sur la selection
					//(comportement standard)
					valueObservable = SWTObservables.observeText(widget, SWT.Modify);
				} catch (IllegalArgumentException e2) {
					Activator.getInstance().debug(WIDGET_NOT_OBSERVABLE_ERROR, e2);
				}	
			//Traitement particulier des Text et des StyledText non pris en charge
			//pris en charge en version 3.6 mais pas comme on le desire
			} else if ((widget instanceof Button) || (widget instanceof Spinner)  || (widget instanceof Scale) ){	
				try {
					//l'observation se fait sur selection du widget on non surt le text
					// de ce dernier
					valueObservable = SWTObservables.observeSelection(widget);
				} catch (IllegalArgumentException e) {
					Activator.getInstance().debug(WIDGET_NOT_OBSERVABLE_ERROR, e);
				}				
			} else {
				try {
					valueObservable = SWTObservables.observeText(widget);
				} catch (IllegalArgumentException e) {
					try {
						valueObservable = SWTObservables.observeText(widget, SWT.Modify);
					} catch (IllegalArgumentException e2) {
						try {
							valueObservable = SWTObservables.observeSelection(widget);
						} catch (IllegalArgumentException e3) {
							Activator.getInstance().debug(WIDGET_NOT_OBSERVABLE_ERROR, e2);
						}
					}
				}
			}
			// Use different widget types to create a correct binding.
			if (valueObservable != null) {
				Binding binding = bindingContext.bindValue(valueObservable, dataObservable, null, null);
				if (renderer.getCurrentMessageManager() != null) {
					renderer.addManagedControl(binding, element, widget);
				}
			}
		} else if (element instanceof MetaDataLink) {
			IObservableList dataObservable = getObservableLink((MetaDataLink) element, true,null);
			// Create Widget part Observable Pattern...
			IObservableList listObservable = SWTObservables.observeItems(widget);
			// Bind the observable parts...
			Binding binding = bindingContext.bindList(listObservable, dataObservable, null, null);
			if (renderer.getCurrentMessageManager() != null) {
				renderer.addManagedControl(binding, element, widget);
			}
		}
	}

	public void bindElement(Element element, IWidgetValue widget) throws IllegalArgumentException {
		if (element instanceof MetaDataAttribute) {
			IObservableValue dataObservable = getObservableAttribute((MetaDataAttribute) element);
			IObservableValue valueObservable = new WidgetObservableValue(widget);
			Binding binding = bindingContext.bindValue(valueObservable, dataObservable, null, null);
			if (renderer.getCurrentMessageManager() != null) {
				renderer.addManagedControl(binding, element, widget.getWidget());
			}
		}
	}

	protected void loadBeanMap() {
		// Update the Attributes bindings...
		for (Entry<String, BeanMapObservableValue> entry : attributes.entrySet()) {
			// Fire events...
			entry.getValue().fireInitialization();
		}
		// The populated Link bindings initialization must be
		// relaunch !
		for (BeanMapObservableLink link : links.values()) {
			link.reset();
		}
	}

	protected void createPartControl() {
		// Binding of controller tests.
		// Take into account implicit tests :
		// Implicit tests are find by inferring dependencies between tests.
//		boolean resume = true;
//		while (resume) {
//			resume = false;
//			if (renderer.getStructure() != null && renderer.getStructure().getTests() != null) {
//				for (MetaDataTest test : renderer.getStructure().getTests()) {
//					// Test if we have at least on attribute currently
//					// observable
//					boolean doit = false;
//					for (String code : test.getAttributes()) {
//						BeanMapObservableValue attribute = attributes.get(code);
//						if (attribute != null) {
//							doit = true;
//							break;
//						}
//					}
//					if (doit) {
//						for (String code : test.getAttributes()) {
//							BeanMapObservableValue attribute = attributes.get(code);
//							if (attribute == null) {
//								MetaDataAttribute att = renderer.getStructure().getAttributes().get(code);
//								if (att == null) {
//									// it use an unknown attribute We remove
//									// it...
//									renderer.getStructure().getTests().remove(test);
//								} else {
//									getObservableAttribute(att);
//									resume = true;
//								}
//							}
//						}
//						if (resume) {
//							break;
//						}
//					}
//				}
//				for (MetaDataTest test : renderer.getStructure().getTests()) {
//					// Test if we have at least one attribute currently
//					// observable
//					if ((test.getAttributes() != null) && (test.getAttributes().length > 0)
//							&& (attributes.get(test.getAttributes()[0]) != null)) {
//						TestChangeListener listener = new TestChangeListener(renderer, test);
//						for (String code : test.getAttributes()) {
//							BeanMapObservableValue attribute = attributes.get(code);
//							if (attribute != null) {
//								attribute.addValueChangeListener(listener);
//							}
//						}
//					}
//				}
//			}
//		}
	}

	protected void disposeBindingContext() {
		if (bindingContext != null) {
			bindingContext.dispose();
		}
	}

	protected void dispose() {
		// dispose listeners...
		attributes.clear();
		links.clear();
		lists.clear();
		linklists.clear();
	}
}