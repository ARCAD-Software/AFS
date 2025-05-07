package com.arcadsoftware.metadata.criteria;

import java.util.ArrayList;

/**
 * Define criteria which contain a list of sub-criteria...
 * 
 * @author ARCAD Software
 */
public interface ISubCriteria {

	/**
	 * Add a new criteria to the sub-criteria.
	 * 
	 * @param criteria
	 */
	public void add(ISearchCriteria criteria);

	/**
	 * Return true if this criteria is empty.
	 * @return
	 */
	public boolean isEmpty();
	
	/**
	 * Get all the criterias contained in the conjunction.
	 *  
	 * @return Should not return null.
	 */
	public ArrayList<ISearchCriteria> getCriterias();

}
