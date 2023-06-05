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
package com.arcadsoftware.metadata.registry;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.List;
import java.util.Properties;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;

import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.IEntityRegistry;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataEventHandler;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.UpdateMetaDataEntity;
import com.arcadsoftware.metadata.registry.internal.Messages;
import com.arcadsoftware.metadata.xml.XmlMetaDataStream;
import com.arcadsoftware.osgi.ILoggedPlugin;

/**
 * This class maintain and Entity repository where declaration come from XML files.
 * 
 * <p>
 * Theses file can contain entities declarations or updates. The declarations are versionned.
 * The creation rule is the following: 
 *
 * <ul>
 * <li>The max version of the entity is used as reference.
 * <li>Any update of version superior or equal is added to the reference declaration.
 * </ul>
 * 
 * Creation Date: 18 juil. 2011
 */
public class XmlRegistry implements IEntityRegistry {

	public static final String TAG_ENTITIES = "entities"; //$NON-NLS-1$

	private volatile HashMap<String, MetaDataEntity> entities;
	private HashMap<String, List<MetaDataEntity>> entitiesDeclarations;
	private HashMap<String, List<MetaDataEntity>> containersDeclarations;
	private XmlMetaDataStream xs;
	private ILoggedPlugin activator;
	private BundleContext context;

	public XmlRegistry(ILoggedPlugin activator) {
		this.activator = activator;
		entities = new HashMap<String, MetaDataEntity>();
		entitiesDeclarations = new HashMap<String, List<MetaDataEntity>>();
		containersDeclarations = new HashMap<String, List<MetaDataEntity>>();
		xs = new XmlMetaDataStream(XmlRegistry.class.getClassLoader());
		xs.alias(XmlMetaDataStream.TAG_LIST, ArrayList.class);
		xs.alias(TAG_ENTITIES, ArrayList.class);
	}
	
	public void loadContainer(String name, File file) {
		if (!file.isFile()) {
			activator.error(String.format(Messages.XmlRegistry_NotAFile,file.getAbsolutePath()), null);
			return;
		}
		loadContainer(name, loadXml(file));
	}
	
	public void loadContainer(String name, InputStream is) {
		if (is == null) {
			return;
		}
		try {
			loadContainer(name, loadXml(is));
		} catch (Throwable e) {
			activator.error(e.getLocalizedMessage(), e);
		}
	}
	
	public void loadContainer(String name, String s) {
		try {
			loadContainer(name, loadXml(s));
		} catch (Throwable e) {
			activator.error(e.getLocalizedMessage(), e);
		}
	}
	
	public void loadContainer(String name, List<MetaDataEntity> declarations) {
		if ((declarations == null) || (declarations.size() == 0)) {
			return;
		}
		declarations = preprocessDeclarations(declarations);
		containersDeclarations.put(name, declarations);
		List<String> typesToRegenerate = new ArrayList<String>();
		for (MetaDataEntity declaration: declarations) {
			String type = declaration.getType();
			if (typesToRegenerate.indexOf(type) < 0) {
				typesToRegenerate.add(type);
			}
			List<MetaDataEntity> entdec = entitiesDeclarations.get(type);
			if (entdec != null) {
				entdec.add(declaration);
			} else {
				entdec = new ArrayList<MetaDataEntity>();
				entdec.add(declaration);
				entitiesDeclarations.put(type, entdec);
			}
		}
		for (String type: typesToRegenerate) {
			generate(type);
		}
		return;
	}
	
	private List<MetaDataEntity> preprocessDeclarations(List<MetaDataEntity> declarations) {
		ArrayList<MetaDataEntity> result = new ArrayList<MetaDataEntity>(declarations.size());
		for (MetaDataEntity e: declarations) {
			result.add(e);
			// If this entity is autoLink=true then add update from all other links targeting this entity.
			if (e.getMetadata().contains(MetaDataEntity.METADATA_AUTOLINK)) {
				for (MetaDataEntity ee: entities.values()) {
					for (MetaDataLink l: ee.getLinks().values()) {
						if (e.getType().equals(l.getType())) {
							result.add(createUpdateLink(e.getType(), e.getVersion(), BeanMapList.getListTag(ee.getType()), ee.getType(), MetaDataEntity.METADATA_AUTOLINK, l.getCode()));
							break;
						}
					}
				}				
			}
			// For all Entities with reverseLink = true
			if (e.getMetadata().contains(MetaDataEntity.METADATA_REVERSELINK)) {
				for (MetaDataEntity ee: entities.values()) {
					for (MetaDataAttribute a: ee.getAttributesFromType(e.getType())) {
						result.add(createUpdateLink(e.getType(), e.getVersion(), BeanMapList.getListTag(a.getCode()), ee.getType(), MetaDataEntity.METADATA_REVERSELINK, a.getCode()));
					}
				}
			}
			// For all attributes reverseLink = true, Create an Update for the target entity.
			for (MetaDataAttribute a: e.getAttributes().values()) {
				if (a.getMetadata().contains(MetaDataEntity.METADATA_REVERSELINK)) {
					result.add(createUpdateLink(a.getType(), Integer.MAX_VALUE, BeanMapList.getListTag(e.getType()), e.getType(), MetaDataEntity.METADATA_REVERSELINK, a.getCode()));
				}
			}
			// For all Entities with autolink = true linked to this entity...
			for (MetaDataLink l: e.getLinks().values()) {
				MetaDataEntity ee = entities.get(l.getType());
				if ((ee != null) && e.getMetadata().contains(MetaDataEntity.METADATA_AUTOLINK)) {
					String code = BeanMapList.getListTag(e.getType());
					if (ee.getLink(code) == null) {
						result.add(createUpdateLink(ee.getType(), ee.getVersion(), code, e.getType(), MetaDataEntity.METADATA_AUTOLINK, l.getCode()));
					}
				}
			}
		}
		return result;
	}

	private UpdateMetaDataEntity createUpdateLink(String type, int version, String linkCode, String linkType, String metadata, String value) {
		UpdateMetaDataEntity e = new UpdateMetaDataEntity(type, version);
		MetaDataLink l = new MetaDataLink(e, linkType);
		l.setCode(linkCode);
		l.getMetadata().put(metadata, value);
		e.addLink(l);
		return e;
	}

	public void unloadContainer(String name) {
		List<MetaDataEntity> declarations = containersDeclarations.remove(name);
		if ((declarations == null) || (declarations.size() == 0)) {
			return;
		}
		List<String> typesToRegenerate = new ArrayList<String>();
		for (MetaDataEntity declaration: declarations) {
			String type = declaration.getType();
			if (typesToRegenerate.indexOf(type) < 0) {
				typesToRegenerate.add(type);
			}
			List<MetaDataEntity> entdec = entitiesDeclarations.get(type);
			if (entdec != null) {
				entdec.remove(declaration);
			}
		}
		for (String type: typesToRegenerate) {
			generate(type);
		}
		return;
	}

	public void updateContainer(String name, String s) {
		List<MetaDataEntity> declarations = null;
		try {
			declarations = loadXml(s);
		} catch (Throwable e) {
			activator.error(e.getLocalizedMessage(), e);
		}
		if (declarations != null) {
			updateContainer(name, declarations);
		} else {
			unloadContainer(name);
		}
	}

	public void updateContainer(String name, InputStream is) {
		List<MetaDataEntity> declarations = null;
		try {
			if (is != null) {
				declarations = loadXml(is);
			}
		} catch (Throwable e) {
			activator.error(e.getLocalizedMessage(), e);
		}
		if (declarations != null) {
			updateContainer(name, declarations);
		} else {
			unloadContainer(name);
		}
	}

	public void updateContainer(String name, File file) {
		if (file.isFile()) {
			updateContainer(name, loadXml(file));
		} else {
			unloadContainer(name);
		}
	}
	
	public void updateContainer(String name, List<MetaDataEntity> newDeclarations) {
		List<MetaDataEntity> oldDeclarations = containersDeclarations.remove(name);
		List<String> typesToRegenerate = new ArrayList<String>();
		if ((oldDeclarations != null) && (oldDeclarations.size() > 0)) {
			for(MetaDataEntity declaration: oldDeclarations) {
				String type = declaration.getType();
				if (typesToRegenerate.indexOf(type) < 0) {
					typesToRegenerate.add(type);
				}
				List<MetaDataEntity> entdec = entitiesDeclarations.get(type);
				if (entdec != null) {
					entdec.remove(declaration);
				}
			}
		}
		if ((newDeclarations != null) && (newDeclarations.size() > 0)) {
			for (MetaDataEntity declaration: newDeclarations) {
				String type = declaration.getType();
				if (typesToRegenerate.indexOf(type) < 0) {
					typesToRegenerate.add(type);
				}
				List<MetaDataEntity> entdec = entitiesDeclarations.get(type);
				if (entdec != null) {
					entdec.add(declaration);
				} else {
					entdec = new ArrayList<MetaDataEntity>();
					entdec.add(declaration);
					entitiesDeclarations.put(type, entdec);
				}
			}
		}
		for (String type: typesToRegenerate) {
			generate(type);
		}
	}
	
	protected List<MetaDataEntity> loadXml(File file) {
		// Chargement du fichier (parsing XML).
		FileInputStream fis = null;
		try {
			fis = new FileInputStream(file);
			List<MetaDataEntity> result = loadXml(fis);
			if (result != null) {
				return result;
			}
			activator.error(Messages.XMLRegistry_ErrorDuringLoadingProcess + file.getAbsolutePath() + Messages.XmlRegistry_NoEntitiesDeclared, null);
			return null;
		} catch (Exception e) {
			activator.error(Messages.XMLRegistry_ErrorDuringLoadingProcess + file.getAbsolutePath(), e);
			return null;
		} finally {
			if (fis != null) {
				try {
					fis.close();
				} catch (Exception e) {
					activator.debug(e);
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	protected List<MetaDataEntity> loadXml(InputStream is) {
		Object res = xs.fromXML(is);
		if (res instanceof MetaDataEntity) {
			ArrayList<MetaDataEntity> result = new ArrayList<MetaDataEntity>();
			result.add((MetaDataEntity)res);
			return result;
		} 
		if (res instanceof List<?>) {
			return (List<MetaDataEntity>)res;
		}
		return null;
	}
	
	@SuppressWarnings("unchecked")
	protected List<MetaDataEntity> loadXml(String s) {
		Object res = xs.fromXML(s);
		if (res instanceof MetaDataEntity) {
			ArrayList<MetaDataEntity> result = new ArrayList<MetaDataEntity>();
			result.add((MetaDataEntity) res);
			return result;
		} 
		if (res instanceof List<?>) {
			return (List<MetaDataEntity>) res;
		}
		return null;
	}
	
	protected void generate(String type) {
		List<MetaDataEntity> declarations = entitiesDeclarations.get(type);
		if ((declarations == null) || (declarations.size() == 0)) {
			MetaDataEntity entity = entities.get(type);
			if (entity != null) {
				fireEvent(MetaDataEventHandler.TOPIC_ENTITY_DESTROYED, entity);
			}
			return;
		}
		// Teste si l'entité n'existe pas déjà dans un autre registre...
		MetaDataEntity entity = MetaDataEntity.loadEntity(type);
		if ((entity != null) && (entity.getRegistry() != null) && (entity.getRegistry() != this)) {
			activator.error(String.format(Messages.XmlRegistry_EntityBelongToAnotherRegistry, type, entity.getRegistry().toString()), null);
			return;
		}
		// Construction de l'entité en fonction de ses déclarations...
		// Max des déclarations d'entity
		entity = null;
		boolean fusion = false;
		for (MetaDataEntity e: declarations) {
			if (!(e instanceof UpdateMetaDataEntity)) {
				if ((entity == null) || (e.getVersion() > entity.getVersion())) {
					entity = e;
					fusion = false;
				} else if (e.getVersion() == entity.getVersion()) {
					fusion = true;
				}
			}
		}
		if (entity == null) {
			entity = new MetaDataEntity(type, 1);
		} else if (fusion) {
			entity = new MetaDataEntity(type, entity.getVersion());
			// Fusion des déclaration d'entités de même version.
			for (MetaDataEntity e: declarations) {
				if ((!(e instanceof UpdateMetaDataEntity)) &&
						(e.getVersion() == entity.getVersion())) {
					entity.inject(e, false);
				}
			}
		} else {
			entity = new MetaDataEntity(entity);
		}
		// Ajout de tout les update de version supérieure à la version courante.
		// Ajout des patch dans l'ordre croissant des numéro de version.
		ArrayList<UpdateMetaDataEntity> patchs = new ArrayList<UpdateMetaDataEntity>(declarations.size());
		for (MetaDataEntity e: declarations) {
			if (e instanceof UpdateMetaDataEntity) {
				if (e.getVersion() >= entity.getVersion()) {
					boolean notAdded = true;
					for (int i = patchs.size() - 1; i >= 0; i--) {
						if (e.getVersion() > patchs.get(i).getVersion()) {
							patchs.add(i+1,(UpdateMetaDataEntity) e);
							notAdded = false;
							break;
						}
					}
					if (notAdded) {
						patchs.add(0, (UpdateMetaDataEntity) e);
					}
				}
			}
		}
		for(UpdateMetaDataEntity p:patchs) {
			entity.inject(p, false);
		}
		entity.setType(type);
		entity.setRegistry(this);
		// Si entité déjà en cache...
		MetaDataEntity e = entities.get(type);
		if (e != null) {
			updateEntity(e, entity, true);
		} else {
			entities.put(type, entity);
			fireEvent(MetaDataEventHandler.TOPIC_ENTITY_CREATED, entity);
			fireEvent(MetaDataEventHandler.TOPIC_ENTITY_ATTRIBUTE_CREATED, entity, entity.getAttributeCodes());
			fireEvent(MetaDataEventHandler.TOPIC_ENTITY_LINK_CREATED, entity, entity.getLinkCodes());
		}
		activator.debug(String.format(Messages.XmlRegistry_DEBUG_Generated, entity.getType(), entity.getDomain()));
	}
	
	private void fireEvent(String topic, MetaDataEntity entity) {
		fireEvent(topic, entity, null);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private void fireEvent(String topic, MetaDataEntity entity, String codes) {
		if (getContext() == null) {
			return;
		}
		// Déclenchement des évènements.
		ServiceReference sf = getContext().getServiceReference(EventAdmin.class.getName());
		if (sf == null) {
			activator.log("No Event Service available when throwing event: " + topic);
			return;
		}
		EventAdmin ea = (EventAdmin) getContext().getService(sf);
		if (ea == null) {
			activator.warn("No Event Service available when throwing event: " + topic);
			return;
		}
		Properties properties = new Properties();
		properties.put(MetaDataEventHandler.EVENT_PROP_ENTITY, entity);
		properties.put(MetaDataEventHandler.EVENT_PROP_TYPE, entity.getType());
		properties.put(MetaDataEventHandler.EVENT_PROP_REGISTRY, this);
		if (codes != null) {
			properties.put(MetaDataEventHandler.EVENT_PROP_CODES, codes);
		}
		try {
			ea.postEvent(new Event(topic, (Dictionary) properties));
		} catch (SecurityException e) {
			activator.debug(e);
		}
	}

	public MetaDataEntity getEntity(String type) {
		return entities.get(type);
	}

	public List<MetaDataEntity> getEntities(String domain) {
		if (domain == null) {
			return new ArrayList<MetaDataEntity>();
		}
		ArrayList<MetaDataEntity> result = new ArrayList<MetaDataEntity>();
		for (MetaDataEntity entity:entities.values()) {
			if (domain.equalsIgnoreCase(entity.getDomain())) {
				result.add(entity);
			}
		}
		return result;
	}
	
	public List<MetaDataEntity> getEntities() {
		ArrayList<MetaDataEntity> result = new ArrayList<MetaDataEntity>();
		result.addAll(entities.values());
		return result;
	}

	public MetaDataEntity updateEntity(MetaDataEntity entityPatch) {
		MetaDataEntity entity = entities.get(entityPatch.getType());
		if ((entity != null) && (entity.getVersion() <= entityPatch.getVersion())) {
			updateEntity(entity, entityPatch, false);
		}
		return entity;
	}

	public MetaDataEntity addEntity(MetaDataEntity entity) {
		MetaDataEntity oldEntity = entities.get(entity.getType());
		if (oldEntity == null) {
			entities.put(entity.getType(), entity);
			fireEvent(MetaDataEventHandler.TOPIC_ENTITY_CREATED, entity);
			fireEvent(MetaDataEventHandler.TOPIC_ENTITY_ATTRIBUTE_CREATED, entity, entity.getAttributeCodes());
			fireEvent(MetaDataEventHandler.TOPIC_ENTITY_LINK_CREATED, entity, entity.getLinkCodes());
			return entity;
		}
		if (oldEntity.getVersion() < entity.getVersion()) {
			updateEntity(oldEntity, entity, true);
		}
		return oldEntity;	
	}
	
	private void updateEntity(MetaDataEntity entity, MetaDataEntity entityPatch, boolean purge) {
		StringBuilder modAtt = new StringBuilder();
		StringBuilder newAtt = new StringBuilder();
		for (MetaDataAttribute a: entityPatch.getAttributes().values()) {
			if (entity.getAttribute(a.getCode()) != null) {
				if (modAtt.length() > 0) {
					modAtt.append(' ');
				}
				modAtt.append(a.getCode());
			} else {
				if (newAtt.length() > 0) {
					newAtt.append(' ');
				}
				newAtt.append(a.getCode());
			}
		}
		StringBuilder modLink = new StringBuilder();
		StringBuilder newLink = new StringBuilder();
		for (MetaDataLink l: entityPatch.getLinks().values()) {
			if (entity.getAttribute(l.getCode()) != null) {
				if (modLink.length() > 0) {
					modLink.append(' ');
				}
				modLink.append(l.getCode());
			} else {
				if (newLink.length() > 0) {
					newLink.append(' ');
				}
				newLink.append(l.getCode());
			}
		}
		entity.inject(entityPatch, true);
		fireEvent(MetaDataEventHandler.TOPIC_ENTITY_MODIFIED, entity);
		if (newAtt.length() > 0) {
			fireEvent(MetaDataEventHandler.TOPIC_ENTITY_ATTRIBUTE_CREATED, entity, newAtt.toString());
		}
		if (modAtt.length() > 0) {
			fireEvent(MetaDataEventHandler.TOPIC_ENTITY_ATTRIBUTE_MODIFIED, entity, modAtt.toString());
		}
		if (newLink.length() > 0) {
			fireEvent(MetaDataEventHandler.TOPIC_ENTITY_LINK_CREATED, entity, newLink.toString());
		}
		if (modLink.length() > 0) {
			fireEvent(MetaDataEventHandler.TOPIC_ENTITY_LINK_MODIFIED, entity, modLink.toString());
		}
	}

	public MetaDataEntity removeEntity(MetaDataEntity entity) {
		entity = entities.get(entity.getType());
		if (entity != null) {
			entities.remove(entity.getType());
			entitiesDeclarations.remove(entity.getType());
			fireEvent(MetaDataEventHandler.TOPIC_ENTITY_DESTROYED, entity);
		}
		return entity;
	}

	/**
	 * The Bundle context is used to throw OSGi events.
	 * 
	 * @param context define the Bundle context associated to this service.
	 */
	public void setContext(BundleContext context) {
		this.context = context;
	}

	/**
	 * @return the bundle context associated to this service. May be null.
	 */
	public BundleContext getContext() {
		return context;
	}

	/**
	 * 
	 * @return the Activator used to create this registry.
	 */
	public ILoggedPlugin getActivator() {
		return activator;
	}

}
