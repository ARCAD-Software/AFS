package bypasstests;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.IByPassListener;
import com.arcadsoftware.metadata.IMetaDataDeleteListener;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

/**
 * Implementing the IByPassListener transform this listener into a active step of the deletion...
 * 
 * @author ARCAD Software
 */
public class MetaDataByPassDeleteListener implements IMetaDataDeleteListener, IByPassListener {

	public MetaDataByPassDeleteListener() {}

	@Override
	public boolean testDeletion(MetaDataEntity entity, BeanMap originalItem, IConnectionUserBean user,
			Language language) throws ResourceException {

		// We make the deletion ourselves: ...
		
		System.out.println("Deletion requested for: " + originalItem.toString());
		
		// The modification is accepted:
		return true;
		// returning false will stop the operation. Throwing a ResourceException will also stop the process.
	}

	@Override
	public void postDeletion(MetaDataEntity entity, BeanMap originalItem, IConnectionUserBean user, Language language)
			throws ResourceException {
		// nothing to do here...
	}

}
