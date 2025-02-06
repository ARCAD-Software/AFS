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
package com.arcadsoftware.cli.output;

public class OutputNode extends AbstractOutputElement {

	private String text;
	private final OutputAttributes attributes;
	private final OutputNodes childNodes;

	public OutputNode(String name) {
		super(name);
		attributes = new OutputAttributes();
		childNodes = new OutputNodes();
	}

	public void setText(String text) {
		this.text = text;
	}

	public String getText() {
		return text;
	}

	public OutputAttributes getAttributes() {
		return attributes;
	}

	public OutputNodes getChildNodes() {
		return childNodes;
	}

	public OutputNode addNode(String name) {
		final OutputNode n = new OutputNode(name);
		childNodes.add(n);
		return n;
	}

	public void addNode(OutputNode child) {
		childNodes.add(child);
	}

	public OutputNodes getNodes(String name) {
		final OutputNodes result = new OutputNodes();
		for (final OutputNode n : childNodes) {
			if (n.getName().equalsIgnoreCase(name)) {
				result.add(n);
			}
		}
		return result;
	}

	public OutputNode getNode(String name) {
		for (final OutputNode n : childNodes) {
			if (n.getName().equalsIgnoreCase(name)) {
				return n;
			}
		}
		return null;
	}

	public void addAttribute(OutputAttribute attribute) {
		attributes.add(attribute);
	}

	public OutputAttribute addAttribute(String name, String value) {
		final OutputAttribute a = new OutputAttribute(name, value);
		attributes.add(a);
		return a;
	}

	public String getAttributeValue(String attributeKey) {
		for (final OutputAttribute a : attributes) {
			if (a.getName().equalsIgnoreCase(attributeKey)) {
				return a.getValue();
			}
		}
		return ""; //$NON-NLS-1$
	}

	private void printNode(OutputNode n) {
		System.out.println("NODE : " + n.getName());
		for (final OutputAttribute a : n.getAttributes()) {
			System.out.println(a.getName() + " = " + a.getValue()); //$NON-NLS-1$
		}
		for (final OutputNode node : n.getChildNodes()) {
			printNode(node);
		}
	}

	public void print() {
		printNode(this);
	}

}
