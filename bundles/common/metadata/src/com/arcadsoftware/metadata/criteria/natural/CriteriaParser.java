package com.arcadsoftware.metadata.criteria.natural;

import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.metadata.criteria.ISearchCriteria;

public class CriteriaParser extends BaseCriteriaParser {

	public static ISearchCriteria parse(String criteria) throws ResourceException {
		if ((criteria == null) || criteria.isEmpty()) {
			return null;
		}
		try (StringReader sr = new StringReader(criteria)) {
			return new CriteriaParser(sr).criteria();
		} catch (Exception e) {
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, e.getLocalizedMessage());
		}
	}
	
	public CriteriaParser(InputStream stream) {
		super(stream, "UTF-8");
	}

	public CriteriaParser(Reader stream) {
		super(stream);
	}
}
