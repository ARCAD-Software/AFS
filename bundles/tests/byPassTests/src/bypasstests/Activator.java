package bypasstests;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;

import com.arcadsoftware.metadata.IMetaDataDeleteListener;
import com.arcadsoftware.metadata.IMetaDataLinkingListener;
import com.arcadsoftware.metadata.IMetaDataModifyListener;

public class Activator implements BundleActivator {

	private ServiceRegistration<IMetaDataDeleteListener> sr1;
	private ServiceRegistration<IMetaDataModifyListener> sr2;
	private ServiceRegistration<IMetaDataLinkingListener> sr3;

	public void start(BundleContext context) throws Exception {
		
		// Registering theses service without any properties make them global listeners that will be called any time for all entities declared into this platform:
		
		sr1 = context.registerService(IMetaDataDeleteListener.class, new MetaDataByPassDeleteListener(), null);
		sr2 = context.registerService(IMetaDataModifyListener.class, new MetaDataByPassModifyListener(), null);
		sr3 = context.registerService(IMetaDataLinkingListener.class, new MetaDataByPassLinkingListener(), null);
	}

	public void stop(BundleContext context) throws Exception {
		sr1.unregister();
		sr2.unregister();
		sr3.unregister();
	}

}
