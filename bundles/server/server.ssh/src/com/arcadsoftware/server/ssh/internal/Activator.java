package com.arcadsoftware.server.ssh.internal;

import org.osgi.framework.BundleContext;
import org.restlet.Context;
import org.restlet.routing.Router;

import com.arcadsoftware.metadata.IMetaDataDeleteListener;
import com.arcadsoftware.osgi.AbstractActivator;
import com.arcadsoftware.rest.IBranch;
import com.arcadsoftware.rest.RouteList;
import com.arcadsoftware.rest.SimpleBranch;
import com.arcadsoftware.server.ssh.internal.listeners.SSHKeyListener;
import com.arcadsoftware.server.ssh.internal.resources.SSHGenerateKeyResource;
import com.arcadsoftware.server.ssh.internal.resources.SSHGetPublicKeyResource;
import com.arcadsoftware.server.ssh.internal.resources.SSHImportKeyResource;
import com.arcadsoftware.server.ssh.services.SSHService;
import com.arcadsoftware.ssh.model.SSHRoutes;

public class Activator extends AbstractActivator {

	@Override
	public void start(BundleContext bundleContext) throws Exception {
		super.start(bundleContext);
		final SSHService sshs = new SSHService(this);
		registerService(SSHService.class, sshs);
		registerService(IMetaDataDeleteListener.class, new SSHKeyListener(this, sshs));
		registerService(IBranch.class, new SimpleBranch() {
			@Override
			protected RouteList createAttachedResources(final Context context, final Router router) {
				return new RouteList(router.attach(SSHRoutes.GENERATE_KEY, SSHGenerateKeyResource.class),
						router.attach(SSHRoutes.PUBLIC_KEY, SSHGetPublicKeyResource.class),
						router.attach(SSHRoutes.IMPORT_KEY, SSHImportKeyResource.class));
			}
		}, IBranch.URI, IBranch.SECUREDBRANCH);
	}

}
