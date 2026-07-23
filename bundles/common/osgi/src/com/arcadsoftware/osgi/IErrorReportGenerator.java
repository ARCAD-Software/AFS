package com.arcadsoftware.osgi;

/**
 * This OSGi service is dedicated to the generation of report file (kind of logical Dump files).
 * 
 * <p>
 * These report files will contain:
 * 
 * <ul>
 * <li>Some technical information on the system.
 * <li>The OSGi configuration of the application.
 * <li>The latest line of the system log.
 * <li>Any other data the application give during the generation of the Report.
 * </ul>
 * 
 * <p>
 * These reports are stored on the server local file system and may be uploaded through a dedicated web-service.
 * 
 * @author ARCAD Software
 */
public interface IErrorReportGenerator {

	/**
	 * Generate a report file stored on the local file system.
	 * 
	 * @param data Any personnal data that will be added to the report.
	 * @return
	 */
	public long generateReport(String data);
	
}
