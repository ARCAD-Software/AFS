package bypasstests;

import java.util.List;
import java.util.Random;

import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.IByPassListener;
import com.arcadsoftware.metadata.IMetaDataModifyListener;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Implementing the IByPassListener transform this listener into a active step of the creation and update processes...
 * 
 * @author ARCAD Software
 */
public class MetaDataByPassModifyListener implements IMetaDataModifyListener, IByPassListener {

	public MetaDataByPassModifyListener() {}

	@Override
	public boolean testModification(MetaDataEntity entity, BeanMap originalItem, BeanMap modifiedItem,
			List<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) throws ResourceException {
		
		if (originalItem == null) {
			// Note if originalItem is null then this is a creation...
			
			// We must return a result with a valid ID.
			// (We may compute this ID with any tricky way we want !!!) 
			modifiedItem.forceId(new Random().nextInt(10000) + 1);
			
			System.out.println("Creation of: " + modifiedItem.toString());

		} else {
			// If originalItem is not null then this is an update (originalItem does not containt any data except the ID of the modified data !
			
			// Note that modifiedItem does not contain any valid ID (there is no need to set it here) !
			
			System.out.println("Modification of: " + originalItem.toString());

		}
		
		// The modification is accepted:
		return true;
		// returning false will stop the operation. Throwing a ResourceException will also stop the process.
	}

	@Override
	public void postModification(MetaDataEntity entity, BeanMap originalItem, BeanMap modifiedItem,
			List<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) throws ResourceException {
		// Nothing to do here...
	}

}
