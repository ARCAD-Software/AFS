package bypasstests;

import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.IByPassListener;
import com.arcadsoftware.metadata.IMetaDataLinkingListener;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class MetaDataByPassLinkingListener implements IMetaDataLinkingListener, IByPassListener {

	public MetaDataByPassLinkingListener() {}

	@Override
	public boolean testLink(MetaDataLink link, BeanMap sourceItem, BeanMap destItem, IConnectionUserBean user,
			Language language) throws ResourceException {

		System.out.println("Linking of: " + sourceItem.toString() + " --> " + destItem.toString());

		// The modification is accepted:
		return true;
		// returning false will stop the operation. Throwing a ResourceException will also stop the process.
	}

	@Override
	public boolean testUnlink(MetaDataLink link, BeanMap sourceItem, BeanMap destItem, IConnectionUserBean user,
			Language language) throws ResourceException {

		System.out.println("Unlinking of: " + sourceItem.toString() + " --> " + destItem.toString());
		
		// The modification is accepted:
		return true;
		// returning false will stop the operation. Throwing a ResourceException will also stop the process.
	}

}
