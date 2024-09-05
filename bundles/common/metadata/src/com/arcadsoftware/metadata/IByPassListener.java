package com.arcadsoftware.metadata;

/**
 * This interface may be used in conjunction with {@link IMetaDataDeleteListener} or {@link IMetaDataModifyListener} 
 * to by pass the default modification implementation.
 * 
 * If the listener return true to any pre-test call then the actual operation (creation, update, deletion) 
 * will be not performed but the Web-Service call will return the correct response.
 * 
 * @author ARCAD Software
 */
public interface IByPassListener {

}
