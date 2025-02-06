/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
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
package com.arcadsoftware.editor.implementation;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import com.arcadsoftware.aev.core.messages.MessageManager;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.editor.IActionElement;
import com.arcadsoftware.editor.IEditorLoader;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataTest;

import io.github.xstream.mxparser.MXParser;

public abstract class EditorEngine {

	private static final String STAR = "*"; //$NON-NLS-1$
	private static final String SLASH = "/"; //$NON-NLS-1$
	private static final String LINK = "link"; //$NON-NLS-1$
	private static final String ATTRIBUTE = "attribute"; //$NON-NLS-1$
	public static final String REALM_UNIFIED = "unified"; //$NON-NLS-1$

	private static final String EXTENSION = "com.arcadsoftware.client.editor"; //$NON-NLS-1$
	// private static final String EXTENSION_AJAX = "com.arcadsoftware.client.editor.ajax"; //$NON-NLS-1$
	private static final String EXTENSION_NAME_LOADER = "loader"; //$NON-NLS-1$
	private static final String EXTENSION_ATTRIBUTE_REALM = "realm"; //$NON-NLS-1$
	private static final String EXTENSION_ATTRIBUTE_CLASS = "class"; //$NON-NLS-1$

	private static final String TAG_ID = "id"; //$NON-NLS-1$
	private static final String TAG_EDITOR = "editor"; //$NON-NLS-1$
	private static final String TAG_VALUE = "value"; //$NON-NLS-1$
	private static final String TAG_WIDGET = "input"; //$NON-NLS-1$
	private static final String TAG_CONTAINER = "container"; //$NON-NLS-1$
	private static final String TAG_ACTION = "action"; //$NON-NLS-1$
	private static final String TAG_PARAMETER = "param"; //$NON-NLS-1$
	private static final String TAG_MESSAGE = "message"; //$NON-NLS-1$
	private static final String TAG_MESSAGES = "messages"; //$NON-NLS-1$
	private static final String TAG_LANG = "lang"; //$NON-NLS-1$
	private static final String TAG_FILE = "file"; //$NON-NLS-1$
	// private static final String TAG_DECORATOR = "decorator"; //$NON-NLS-1$
	private static final String TAG_VIRTUAL = "virtual"; //$NON-NLS-1$
	private static final String TAG_NAME = "name"; //$NON-NLS-1$
	private static final String TAG_CODE = "code"; //$NON-NLS-1$
	private static final String TAG_TYPE = "type"; //$NON-NLS-1$
	private static final String TAG_RANK = "rank"; //$NON-NLS-1$
	private static final String TAG_SIZE = "size"; //$NON-NLS-1$
	private static final String TAG_LENGTH = "length"; //$NON-NLS-1$
	// private static final String TAG_REF = "ref"; //$NON-NLS-1$
	private static final String TAG_VISIBLE = "visible"; //$NON-NLS-1$
	private static final String TAG_MANDATORY = "mandatory"; //$NON-NLS-1$
	private static final String TAG_DEFAULT = "default"; //$NON-NLS-1$
	private static final String TAG_ATTRIBUTE = ATTRIBUTE;

	private static final String DEFAULT_INPUT_NAME = TAG_WIDGET;

	private String realm;
	private final IEditorLoader loader;

	private MetaDataEntity structure;
	private boolean structurecloned = false;
	private final List<LayoutElement> container = new ArrayList<>();
	private final List<IActionElement> actions = new ArrayList<>();
	private final Map<String, String> messages = new HashMap<>();
	private final Map<String, String> params = new HashMap<>();
	private final Map<String, Object> defaultValues = new HashMap<>();
	private final Map<String, Properties> propsCache = new HashMap<>();

	public EditorEngine() {
		this(null);
	}

	public EditorEngine(String realm) {
		super();
		if (realm == null) {
			this.realm = REALM_UNIFIED;
		} else {
			this.realm = realm;
		}
		loader = (IEditorLoader) findLoader(EXTENSION_NAME_LOADER);
	}

	protected abstract IProviderFactory getProviderFactory();

	protected abstract String getWidgetsExtension();

	protected abstract int getLayoutKind();

	/**
	 * Load the data structure information.
	 *
	 * @param type
	 * @return
	 */
	protected boolean loadStructure(String type) {
		if (loader == null) {
			return false;
		}
		structure = loader.loadMetaDataEntity(type);
		return structure != null;
	}

	/**
	 * Clear layout contents
	 */
	protected void clearLayout() {
		// first clear content
		clearParams();
		clearElements();
		clearActions();
		clearMessages();
		clearDefaultValues();
	}

	protected boolean loadLayout(String name) {
		propsCache.clear();
		if ((loader == null) || (structure == null)) {
			return false;
		}
		final String xml = loader.loadXMLLayoutDocument(name, structure.getType(), getLayoutKind());
		if ((xml == null) || (xml.length() == 0)) {
			return false;
		}
		// Parse the xml layout.
		final MXParser xpp = new MXParser();
		// xpp.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, true);
		try {
			final StringReader reader = new StringReader(xml);
			xpp.setInput(reader);
			// xpp.setInput(new ByteArrayInputStream(xml.getBytes("utf-8")),"utf-8");
			int eventType = xpp.getEventType();
			while (eventType != XmlPullParser.END_DOCUMENT) {
				if (eventType == XmlPullParser.START_TAG) {
					final String tag = xpp.getName();
					if (TAG_EDITOR.equals(tag)) {
						// Nothing to do...
					} else if (TAG_ACTION.equals(tag)) {
						parseAction(actions, xpp);
						// } else if (TAG_WIDGET.equals(tag) || TAG_DECORATOR.equals(tag)) {
						// parseWidget(container, null, xpp);
					} else if (TAG_CONTAINER.equals(tag)) {
						parseContainer(container, xpp);
					} else if (TAG_PARAMETER.equals(tag)) {
						parseParam(xpp);
					} else if (TAG_MESSAGES.equals(tag)) {
						parseMessages(xpp);
					} else if (TAG_VIRTUAL.equals(tag)) {
						parseVirtualAttribute(xpp);
					} else if (TAG_DEFAULT.equals(tag)) {
						parseDefault(xpp);
					} else {
						parseWidget(container, tag, xpp);
					}
				}
				eventType = xpp.getEventType();
				if (eventType != XmlPullParser.END_DOCUMENT) {
					eventType = xpp.next();
				}
			}
			return !container.isEmpty();
		} catch (final IOException e) {
			Activator.getInstance().log(e);
			return false;
		} catch (final XmlPullParserException e) {
			Activator.getInstance().log(e);
			return false;
		}

	}

	protected List<LayoutElement> getLayoutElements() {
		return container;
	}

	public void parseWidget(List<LayoutElement> currentContainer, String name, XmlPullParser xpp)
			throws XmlPullParserException, IOException {
		String currentName = name;
		if (currentName == null) {
			currentName = xpp.getAttributeValue(null, TAG_ID);
		}
		if (currentName == null) {
			currentName = DEFAULT_INPUT_NAME;
		}

		// Look for the entity attribute or link element.
		String ename = xpp.getAttributeValue(null, ATTRIBUTE);
		Element ee = null;
		if (ename != null) {
			ee = structure.getElement(ename);
			if (ee == null) {
				Activator.getInstance().log("Attribute: " + ename + " not found in the metadata.");
				xpp.nextText();
				return;
			}
		} else {
			ename = xpp.getAttributeValue(null, LINK);
			if (ename != null) {
				ee = structure.getLink(ename);
				if (ee == null) {
					Activator.getInstance().log("Link: " + ename + " not found in the metadata.");
					xpp.nextText();
					return;
				}
			}
		}
		// Look for a decorator !
		if (ee == null) {
			if (currentName != null) {
				LayoutElement el = loadDecorator(currentName, xpp);
				if (el != null) {
					currentContainer.add(el);
					return;
				}
				// lets try a Container !!!
				el = loadContainer(currentName, xpp);
				if (el != null) {
					currentContainer.add(el);
					return;
				}
			}
			xpp.nextText();
			return;
		}
		LayoutElement el;

		// Name & type
		el = loadInput(currentName, ee.getType(), ee, xpp);
		if (el != null) {
			currentContainer.add(el);
			return;
		}
		// Name & sous-type
		if (ee.getType().indexOf('/') > -1) {
			final String[] stype = ee.getType().split(SLASH);
			for (int i = stype.length - 1; i > 0; i--) {
				final StringBuilder type = new StringBuilder(stype[0]);
				type.append('/');
				for (int j = 1; j < i; j++) {
					type.append(stype[j]).append('/');
				}
				type.append('*');
				el = loadInput(currentName, type.toString(), ee, xpp);
				if (el != null) {
					currentContainer.add(el);
					return;
				}
			}
		}

		// Name
		el = loadInput(currentName, "*", ee, xpp);
		if (el != null) {
			currentContainer.add(el);
			return;
		}

		// We didn't find using Name so we're going to search using type only
		// Name ==null and Type
		el = loadInput(null, ee.getType(), ee, xpp);
		if (el != null) {
			currentContainer.add(el);
			return;
		}
		// We did'nt fint with type, we're going to search using subtype
		// Name==null and subtype
		if (ee.getType().indexOf('/') > -1) {
			final String[] stype = ee.getType().split(SLASH);
			for (int i = stype.length - 1; i > 0; i--) {
				final StringBuilder type = new StringBuilder(stype[0]);
				type.append('/');
				for (int j = 1; j < i; j++) {
					type.append(stype[j]).append('/');
				}
				type.append('*');
				el = loadInput(null, type.toString(), ee, xpp);
				if (el != null) {
					currentContainer.add(el);
					return;
				}
			}
		}
		// Last Chance ! we try using "DEFAULT_INPUT_NAME/*"
		el = loadInput(DEFAULT_INPUT_NAME, STAR, ee, xpp);
		if (el != null) {
			currentContainer.add(el);
			return;
		}
		xpp.nextText();

		// if (ee.getType().indexOf('/') > -1) {
		// String[] stype = ee.getType().split(SLASH);
		// for (int i = stype.length - 1; i > 0; i--) {
		// StringBuilder type = new StringBuilder(stype[0]);
		// type.append('/');
		// for (int j = 1; j < i; j++) {
		// type.append(stype[j]).append('/');
		// }
		// type.append('*');
		// el = loadInput(currentName, type.toString(), ee, xpp);
		// if (el != null) {
		// currentContainer.add(el);
		// return;
		// }
		// if (currentName != null) {
		// el = loadInput(null, type.toString(), ee, xpp);
		// if (el != null) {
		// currentContainer.add(el);
		// }
		// }
		// }
		// }
		// //*
		//
		//
		// //if ((currentName == null) && ee.isStringType()) {
		// if (ee.isStringType()) {
		// // Look for widget provider...
		// el = loadInput(currentName, ee.getType(), ee, xpp);
		// if (el != null) {
		// currentContainer.add(el);
		// return;
		// }
		// } else {
		// // Look for widget provider...
		// el = loadInput(currentName, ee.getType(), ee, xpp);
		// if (el != null) {
		// currentContainer.add(el);
		// return;
		// }
		// }
		//
		//
		//
		// if (currentName != null) { // Inutile si name == null on vient de le faire.
		// el = loadInput(null, ee.getType(), ee, xpp);
		// if (el != null) {
		// currentContainer.add(el);
		// return;
		// }
		// }
		// // Look for generic input
		// if (ee.getType().indexOf('/') > -1) {
		// String[] stype = ee.getType().split(SLASH);
		// for (int i = stype.length - 1; i > 0; i--) {
		// StringBuilder type = new StringBuilder(stype[0]);
		// type.append('/');
		// for (int j = 1; j < i; j++) {
		// type.append(stype[j]).append('/');
		// }
		// type.append('*');
		// el = loadInput(currentName, type.toString(), ee, xpp);
		// if (el != null) {
		// currentContainer.add(el);
		// return;
		// }
		// if (currentName != null) {
		// el = loadInput(null, type.toString(), ee, xpp);
		// if (el != null) {
		// currentContainer.add(el);
		// }
		// }
		// }
		// }
		// // Lock for a named default input.
		// el = loadInput(currentName, STAR, ee, xpp);
		// if (el != null) {
		// currentContainer.add(el);
		// return;
		// }
		// look for the super default input !
		// el = loadInput(DEFAULT_INPUT_NAME, STAR, ee, xpp);
		// if (el != null) {
		// currentContainer.add(el);
		// return;
		// }
		// xpp.nextText();
	}

	private LayoutElement loadDecorator(String name, XmlPullParser xpp) throws XmlPullParserException, IOException {
		return getProviderFactory().getDecorator(name, xpp);
	}

	private LayoutElement loadInput(String name, String type, Element ee, XmlPullParser xpp)
			throws XmlPullParserException, IOException {
		return getProviderFactory().getInput(name, type, ee, xpp);
	}

	public void parseAction(List<IActionElement> currentActions, XmlPullParser xpp) throws XmlPullParserException,
			IOException {
		final String id = xpp.getAttributeValue(null, TAG_ID);
		if (id != null) {
			final ActionElement action = new ActionElement();
			action.setCode(id);
			action.setName(xpp.getAttributeValue(null, "name")); //$NON-NLS-1$
			action.setIcon(xpp.getAttributeValue(null, "icon")); //$NON-NLS-1$
			action.setScript(xpp.getAttributeValue(null, "script")); //$NON-NLS-1$
			action.setElements(xpp.getAttributeValue(null, "elements")); //$NON-NLS-1$
			currentActions.add(action);
		}
		xpp.nextText();
	}

	private void parseDefault(XmlPullParser xpp) throws XmlPullParserException, IOException {
		final String id = xpp.getAttributeValue(null, TAG_ATTRIBUTE);
		final String value = xpp.getAttributeValue(null, TAG_VALUE);
		final String type = xpp.getAttributeValue(null, TAG_TYPE);
		xpp.getAttributeValue(null, TAG_TYPE);
		if (id != null) {
			if ("date".equalsIgnoreCase(type)) {
				if ("now".equalsIgnoreCase(value)) {
					defaultValues.put(id, new Date());
				}
			} else {
				defaultValues.put(id, value);
			}
		}
		xpp.nextText();
	}

	public void parseContainer(List<LayoutElement> currentContainer, XmlPullParser xpp)
			throws XmlPullParserException, IOException {
		final String name = xpp.getAttributeValue(null, TAG_ID);
		LayoutElement le = loadContainer(name, xpp);
		if ((le == null) && (name != null)) {
			le = loadContainer(null, xpp);
		}
		if (le == null) {
			Activator.getInstance().warn("No Container found..."); //$NON-NLS-1$
			xpp.nextText();
			return;
		}
		currentContainer.add(le);
	}

	private LayoutElement loadContainer(String name, XmlPullParser xpp) throws XmlPullParserException, IOException {
		return getProviderFactory().getContainer(this, name, xpp);
	}

	protected void parseParam(XmlPullParser xpp) throws XmlPullParserException, IOException {
		params.put(xpp.getAttributeValue(null, TAG_ID), xpp.getAttributeValue(null, TAG_VALUE));
		xpp.nextText();
	}

	protected void clearParams() {
		params.clear();
	}

	protected void clearElements() {
		container.clear();
	}

	protected void clearActions() {
		actions.clear();
	}

	protected void clearMessages() {
		messages.clear();
	}

	protected void clearDefaultValues() {
		defaultValues.clear();
	}

	private void parseMessages(XmlPullParser xpp) throws XmlPullParserException, IOException {
		final String fileURL = xpp.getAttributeValue(null, TAG_FILE);
		if (fileURL != null) {
			Properties prop = propsCache.get(fileURL);
			if (prop == null) {
				prop = loader.loadProperties(fileURL);
				if (prop != null) {
					propsCache.put(fileURL, prop);
				}
			}
			if (prop != null) {
				for (final Entry<Object, Object> entry : prop.entrySet()) {
					if (entry.getValue() != null) {
						messages.put(entry.getKey().toString(), entry.getValue().toString());
					}
				}
			}
			xpp.nextText();
			return;
		}
		final String lang = xpp.getAttributeValue(null, TAG_LANG);
		// Warning: the order of the multiple <messages> tag in the file will be
		// important.
		// last parsed messages will override first ones.
		if ((lang == null) || lang.equals(Locale.getDefault().getLanguage())
				|| lang.equals(Locale.getDefault().toString())) {
			int eventType = xpp.next();
			while (eventType != XmlPullParser.END_TAG) {
				if ((eventType == XmlPullParser.START_TAG) && TAG_MESSAGE.equals(xpp.getName())) {
					messages.put(xpp.getAttributeValue(null, TAG_ID), xpp.getAttributeValue(null, TAG_VALUE));
					xpp.nextText();
				}
				eventType = xpp.next();
			}
		} else {
			xpp.nextText();
		}
	}

	private void parseVirtualAttribute(XmlPullParser xpp) throws XmlPullParserException, IOException {
		final String code = xpp.getAttributeValue(null, TAG_CODE);
		String name = xpp.getAttributeValue(null, TAG_NAME);
		final String type = xpp.getAttributeValue(null, TAG_TYPE);
		if (code != null) {
			if (!structurecloned) {
				structure = structure.clone();
				structurecloned = true;
			}
			MetaDataAttribute attribute = structure.getAttribute(code);
			if (attribute != null) {
				// Modify an existing Attribute...
				try {
					attribute = (MetaDataAttribute) attribute.clone();
					updateAtrribute(xpp, attribute, name, type);
					structure.getAttributes().put(code, attribute);
				} catch (final CloneNotSupportedException e) {
					Activator.getInstance().log(e);
					xpp.nextText();
					return;
				}
			} else if ((type != null) && (type.length() > 0)) {
				// Add an new "fake" Attribute...
				if (name == null) {
					name = code;
				}
				attribute = new MetaDataAttribute((MetaDataEntity) null);
				attribute.setCode(code);
				updateAtrribute(xpp, attribute, name, type);
				structure.getAttributes().put(code, attribute);
			} else {
				xpp.nextText();
			}
		} else {
			xpp.nextText();
		}
	}

	private void updateAtrribute(XmlPullParser xpp, MetaDataAttribute attribute, String name, String type)
			throws XmlPullParserException, IOException {
		if ((name != null) && (name.length() > 0)) {
			attribute.setName(getMessage(name));
		}
		if ((type != null) && (type.length() > 0)) {
			attribute.setType(type);
		}
		String s = xpp.getAttributeValue(null, TAG_MANDATORY);
		if ((s != null) && (s.length() > 0)) {
			attribute.setMandatory("true".equalsIgnoreCase(s)); //$NON-NLS-1$
		}
		// s = xpp.getAttributeValue(null, TAG_REF);
		// if ((s != null) && (s.length() > 0)) {
		// attribute.setReference("true".equalsIgnoreCase(s)); //$NON-NLS-1$
		// }
		s = xpp.getAttributeValue(null, TAG_VISIBLE);
		if ((s != null) && (s.length() > 0)) {
			attribute.setVisible("true".equalsIgnoreCase(s)); //$NON-NLS-1$
		}
		s = xpp.getAttributeValue(null, TAG_RANK);
		if ((s != null) && (s.length() > 0)) {
			try {
				final int i = Integer.parseInt(s);
				attribute.setColRank(i);
			} catch (final NumberFormatException e) {
				MessageManager.addException(e, MessageManager.LEVEL_PRODUCTION);
			}
		}
		s = xpp.getAttributeValue(null, TAG_SIZE);
		if ((s != null) && (s.length() > 0)) {
			try {
				final int i = Integer.parseInt(s);
				attribute.setColSize(i);
			} catch (final NumberFormatException e) {
				MessageManager.addException(e, MessageManager.LEVEL_PRODUCTION);
			}
		}
		s = xpp.getAttributeValue(null, TAG_LENGTH);
		if ((s != null) && (s.length() > 0)) {
			try {
				final int i = Integer.parseInt(s);
				attribute.setLength(i);
			} catch (final NumberFormatException e) {
				MessageManager.addException(e, MessageManager.LEVEL_PRODUCTION);
			}
		}
		s = xpp.nextText();
		if ((s != null) && (s.length() > 0)) {
			s = s.trim();
			if (s.length() > 0) {
				attribute.setTest(s);
			}
		}
	}

	/**
	 * Look for the ILoader implementation corresponding to this realm.
	 */
	protected Object findLoader(String extensionName) {
		// Add any declared widget provider...
		final IExtensionRegistry reg = Platform.getExtensionRegistry();
		for (final IConfigurationElement element : reg.getConfigurationElementsFor(EXTENSION)) {
			if ((element.getName().equals(extensionName))
					&& (realm.equals(element.getAttribute(EXTENSION_ATTRIBUTE_REALM)))) {
				try {
					return element.createExecutableExtension(EXTENSION_ATTRIBUTE_CLASS);
				} catch (final CoreException e) {
					Activator.getInstance().log(e);
				}
			}
		}
		Activator.getInstance().warn(
				String.format("No \"%s\" loader found corresponding to realm \"%s\".", extensionName, realm)); //$NON-NLS-1$
		return null;
	}

	protected List<MetaDataTest> getTests(List<Element> elements) {
		return new ArrayList<>();
		// ArrayList<MetaDataTest> result = new ArrayList<MetaDataTest>();
		// if (structure != null) {
		// ArrayList<String> names = new ArrayList<String>();
		// for (Element element : elements) {
		// names.add(element.getCode());
		// }
		// for (MetaDataTest test : structure.getTests()) {
		// if (containtTest(test.getAttributes(), names)) {
		// result.add(test);
		// }
		// }
		// }
		// return result;
	}

	/*
	 * private boolean containtTest(String[] attributes, ArrayList<String> names) { for (String name1 : names) { for
	 * (String name2 : attributes) { if (name1.equals(name2)) { return true; } } } return false; }/
	 **/

	public MetaDataEntity getStructure() {
		return structure;
	}

	protected String getMessage(String key) {
		final String result = messages.get(key);
		if (result == null) {
			return key;
		}
		return result;
	}

	protected String getParam(String name) {
		return params.get(name);
	}

	protected String getParam(String name, String defaultValue) {
		final String result = params.get(name);
		if (result == null) {
			return defaultValue;
		}
		return result;
	}

	protected String setParam(String paramName, String paramValue) {
		return params.put(paramName, paramValue);
	}

	protected IActionElement getActionElement(String code) {
		for (final IActionElement action : actions) {
			if (action.getCode().equals(code)) {
				return action;
			}
		}
		return null;
	}

	protected void addAction(IActionElement action) {
		actions.add(action);
	}

	protected List<IActionElement> getActions() {
		return actions;
	}

	protected IEditorLoader getLoader() {
		return loader;
	}

	/**
	 * Set the default values.
	 *
	 * @param bean
	 */
	protected void setDefaultValues(IBeanMap bean) {
		for (final Entry<String, Object> e : defaultValues.entrySet()) {
			if (bean.get(e.getKey()) == null) {
				bean.put(e.getKey(), e.getValue());
			}
		}
	}
}
