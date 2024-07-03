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

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import com.arcadsoftware.editor.ElementParameter;
import com.arcadsoftware.editor.implementation.Activator;
import com.arcadsoftware.editor.implementation.EditorEngine;
import com.arcadsoftware.editor.implementation.IProviderFactory;
import com.arcadsoftware.editor.implementation.LayoutElement;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.script.IScriptAction;

public class SWTProviderFactory implements IProviderFactory {

	// private static final String BEGIN_TAG_SLASH = "</"; //$NON-NLS-1$
	// private static final String SLASH_END_TAG = "/>"; //$NON-NLS-1$
	private static final String ELEMENT_PARAMETER = "elementParameter"; //$NON-NLS-1$
	private static final String PARAMETER = "parameter"; //$NON-NLS-1$
	private static final String DEFAULT = "default"; //$NON-NLS-1$
	private static final String TEXT = "text"; //$NON-NLS-1$
	private static final String REQUIRED = "required"; //$NON-NLS-1$
	private static final String USE = "use"; //$NON-NLS-1$
	private static final String CONTAINER = "container"; //$NON-NLS-1$
	private static final String DECORATOR = "decorator"; //$NON-NLS-1$
	private static final String TYPE = "type"; //$NON-NLS-1$
	private static final String NAME = "name"; //$NON-NLS-1$
	private static final String INPUT = "input"; //$NON-NLS-1$
	private static final String CLASS = "class"; //$NON-NLS-1$
	private static final String ACTION = "action"; //$NON-NLS-1$
	private static final String SCRIPTACTION = "scriptAction"; //$NON-NLS-1$
	private static final String REGEX = "regex"; //$NON-NLS-1$

	private static final String EXTENTION_POINT_ID = "com.arcadsoftware.client.editor.swt"; //$NON-NLS-1$

	private final List<IConfigurationElement> inputs = new ArrayList<>();
	private final List<IConfigurationElement> decorators = new ArrayList<>();
	private final List<IConfigurationElement> containers = new ArrayList<>();
	private final List<IConfigurationElement> actions = new ArrayList<>();

	public SWTProviderFactory() {
		final IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
		final IConfigurationElement[] configurationElements = extensionRegistry
				.getConfigurationElementsFor(EXTENTION_POINT_ID);
		for (final IConfigurationElement element : configurationElements) {
			if (INPUT.equals(element.getName())) {
				inputs.add(element);
			} else if (CONTAINER.equals(element.getName())) {
				containers.add(element);
			} else if (DECORATOR.equals(element.getName())) {
				decorators.add(element);
			} else if (SCRIPTACTION.equals(element.getName())) {
				actions.add(element);
			}
		}
	}

	private boolean isCompliant(IConfigurationElement element, String declaredType, String typeToTest) {
		if (typeToTest.equalsIgnoreCase(declaredType)) {
			return true;
		} else {
			final boolean useRegEx = Boolean.valueOf(element.getAttribute(REGEX));
			if (useRegEx) {
				return typeToTest.matches(declaredType);
			} else {
				return false;
			}
		}
	}

	@Override
	public LayoutElement getInput(final String name, String type, Element Element, XmlPullParser xpp)
			throws XmlPullParserException, IOException {
		LayoutElement result = null;
		for (final IConfigurationElement element : inputs) {
			if (isCompliant(element, element.getAttribute(TYPE), type)
					&& ((name == null) || (name.equals(element.getAttribute(NAME))))) {
				LayoutElement layoutElement = null;
				try {
					layoutElement = new LayoutElement(name, element.createExecutableExtension(CLASS), Element);
				} catch (final CoreException e) {
					Activator.getInstance().log(e);
				}
				if (layoutElement != null) {
					String text = null;
					// boolean elementParameterIsRequired = false;
					final Map<String, IConfigurationElement> listElement = new HashMap<>();
					for (final IConfigurationElement parameter : element.getChildren()) {
						if (parameter.getName().equals(PARAMETER)) {
							final String parameterName = parameter.getAttribute(NAME);
							String value = xpp.getAttributeValue(null, parameterName);
							final String use = parameter.getAttribute(USE);
							if (value == null) {
								if (REQUIRED.equals(use)) {
									layoutElement = null;
									break;
								}
								if (TEXT.equals(use)) {
									text = parameterName;
								} else {
									value = parameter.getAttribute(DEFAULT);
									if (value != null) {
										layoutElement.addParameter(parameterName, value);
									}
								}
							} else {
								layoutElement.addParameter(parameterName, value);
							}
						} else if (parameter.getName().equals(ELEMENT_PARAMETER)) {
							listElement.put(parameter.getAttribute(NAME), parameter);
						}
					}
					String textValue = ""; //$NON-NLS-1$
					if (layoutElement != null) {
						if (listElement.size() > 0) {
							int eventType = xpp.getEventType();
							boolean follow = eventType != XmlPullParser.END_DOCUMENT;
							while (follow) {
								switch (eventType) {
								case XmlPullParser.START_TAG:
									String tag = xpp.getName();
									if (tag.equalsIgnoreCase(name)) {
										// If the current tag is the one we
										// do nothing
									} else {
										final IConfigurationElement currentElement = listElement.get(tag);
										if ((layoutElement != null) && (currentElement != null)) {
											final ElementParameter elemParameter = layoutElement
													.addElementParameter(currentElement.getAttribute(NAME));
											for (final IConfigurationElement parameter : currentElement.getChildren()) {
												final String parameterName = parameter.getAttribute(NAME);
												String value = xpp.getAttributeValue(null, parameterName);
												final String use = parameter.getAttribute(USE);
												if (value == null) {
													if (REQUIRED.equals(use)) {
														layoutElement = null;
														break;
													}
													if (TEXT.equals(use)) {
														text = parameterName;
													} else {
														value = parameter.getAttribute(DEFAULT);
														if (value != null) {
															layoutElement.addParameterToElementParameter(elemParameter,
																	parameterName, value);
														}
													}
												} else {
													layoutElement.addParameterToElementParameter(elemParameter,
															parameterName, value);
												}
											}
										}
									}
									break;
								case XmlPullParser.TEXT:
									textValue = textValue + xpp.getText();
									break;
								case XmlPullParser.END_TAG:
									tag = xpp.getName();
									follow = !tag.equalsIgnoreCase(name);
									break;
								case XmlPullParser.END_DOCUMENT:
									follow = false;
								default:
									break;
								}
								eventType = xpp.getEventType();
								if (eventType != XmlPullParser.END_DOCUMENT) {
									eventType = xpp.next();
								}
							}
							// xpp.getText();
							// if (!xpp.getText().contains(SLASH_END_TAG)) {
							// xpp.nextTag();
							// while (true) {
							// xpp.getText();
							// IConfigurationElement currentElement = listElement.get(xpp.getName());
							//
							// if (layoutElement != null && currentElement != null) {
							// ElementParameter elemParameter = layoutElement
							// .addElementParameter(currentElement.getAttribute(NAME));
							// for (IConfigurationElement parameter : currentElement.getChildren()) {
							// String parameterName = parameter.getAttribute(NAME);
							// String value = xpp.getAttributeValue(null, parameterName);
							// String use = parameter.getAttribute(USE);
							// if (value == null) {
							// if (REQUIRED.equals(use)) {
							// layoutElement = null;
							// break;
							// }
							// if (TEXT.equals(use)) {
							// text = parameterName;
							// } else {
							// value = parameter.getAttribute(DEFAULT);
							// if (value != null) {
							// layoutElement.addParameterToElementParameter(elemParameter,
							// parameterName, value);
							// }
							// }
							// } else {
							// layoutElement.addParameterToElementParameter(elemParameter,
							// parameterName, value);
							// }
							// }
							// }
							// xpp.nextTag();
							// xpp.nextTag();
							// xpp.getText();
							// if (xpp.getText().contains(BEGIN_TAG_SLASH)) {
							// xpp.getEventType();
							// break;
							// }
							// }
							// }

						} else {
							// We go to the next tag
							xpp.next();
						}
						// xpp.getText();
						// String textValue = (listElement.size() > 0 && !xpp.getText().contains(SLASH_END_TAG)) ? xpp
						// .getText() : xpp.nextText();
						// xpp.getText();

						if ((text != null) && (layoutElement != null)) {
							layoutElement.addParameter(text, textValue);
						}
						result = layoutElement;
						break;
					}
				}
			}
		}
		return result;
	}

	@Override
	public LayoutElement getDecorator(String name, XmlPullParser xpp) throws XmlPullParserException, IOException {
		for (final IConfigurationElement element : decorators) {
			if (((name == null) || (name.equals(element.getAttribute(NAME))))) {
				try {
					LayoutElement result = new LayoutElement(name, element.createExecutableExtension(CLASS), null);
					String text = null;
					for (final IConfigurationElement parameter : element.getChildren()) {
						final String pn = parameter.getAttribute(NAME);
						String value = xpp.getAttributeValue(null, pn);
						final String usage = parameter.getAttribute(USE);
						if (value == null) {
							if (REQUIRED.equals(usage)) {
								result = null;
								break;
							}
							if (TEXT.equals(usage)) {
								text = pn;
							} else {
								value = parameter.getAttribute(DEFAULT);
								if (value != null) {
									result.addParameter(pn, value);
								}
							}
						} else {
							result.addParameter(pn, value);
						}
					}
					if (result != null) {
						final String textValue = xpp.nextText();
						if (text != null) {
							result.addParameter(text, textValue);
						}
						return result;
					}
				} catch (final CoreException e) {
					Activator.getInstance().log(e);
				}
			}
		}
		return null;
	}

	@Override
	public LayoutElement getContainer(EditorEngine editorEngine, String name, XmlPullParser xpp)
			throws XmlPullParserException, IOException {
		for (final IConfigurationElement element : containers) {
			if (((name == null) || (name.equals(element.getAttribute(NAME))))) {
				try {
					LayoutElement result = new LayoutElement(name, element.createExecutableExtension(CLASS), null);
					for (final IConfigurationElement parameter : element.getChildren()) {
						final String pn = parameter.getAttribute(NAME);
						String value = xpp.getAttributeValue(null, pn);
						final String usage = parameter.getAttribute(USE);
						if (value == null) {
							if (REQUIRED.equals(usage)) {
								result = null;
								break;
							}
							value = parameter.getAttribute(DEFAULT);
							if (value != null) {
								result.addParameter(pn, value);
							}
						} else {
							result.addParameter(pn, value);
						}
					}
					if (result != null) {
						// load children...
						int eventType = xpp.next();
						while ((eventType != XmlPullParser.END_TAG) && (eventType != XmlPullParser.END_DOCUMENT)) {
							if (eventType == XmlPullParser.START_TAG) {
								final String tag = xpp.getName();
								if (ACTION.equals(tag)) {
									editorEngine.parseAction(result.getActions(), xpp);
								} else if (INPUT.equals(tag) || DECORATOR.equals(tag)) {
									editorEngine.parseWidget(result.getContaint(), null, xpp);
								} else if (CONTAINER.equals(tag)) {
									editorEngine.parseContainer(result.getContaint(), xpp);
								} else {
									editorEngine.parseWidget(result.getContaint(), tag, xpp);
								}
							}
							eventType = xpp.next();
							xpp.getText();
						}
						return result;
					}
				} catch (final CoreException e) {
					Activator.getInstance().log(e);
				}
			}
		}
		return null;
	}

	@Override
	public IScriptAction getScriptAction(String name) {
		if ((name != null) && (name.length() > 0)) {
			for (final IConfigurationElement element : actions) {
				if (name.equals(element.getAttribute(NAME))) {
					try {
						final Object o = element.createExecutableExtension(CLASS);
						if (o instanceof IScriptAction) {
							return (IScriptAction) o;
						}
					} catch (final CoreException e) {
						Activator.getInstance().log(e);
					}
				}
			}
		}
		return null;
	}
}
