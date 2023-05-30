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
package com.arcadsoftware.cli.core.services;


import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Properties;

import com.arcadsoftware.ae.core.utils.Utils;
import com.arcadsoftware.cli.core.ExecutionResult;
import com.arcadsoftware.cli.core.ICoreModifiers;
import com.arcadsoftware.cli.core.help.Parameter;
import com.arcadsoftware.cli.logger.IMessageLogger;
import com.arcadsoftware.cli.logger.LogEntries;
import com.arcadsoftware.cli.logger.LogEntry;
import com.arcadsoftware.cli.logger.ServiceLogger;
import com.arcadsoftware.cli.model.LoggedObject;
import com.arcadsoftware.cli.output.AbstractOutputManager;
import com.arcadsoftware.cli.output.IOutputConsts;
import com.arcadsoftware.cli.output.OutputNode;

 
public abstract class AbstractService extends LoggedObject{
	public static final int EXITCODE_FAILED = 1;
	public static final int EXITCODE_SUCCEED = 0;
	public static final int EXITCODE_UNDEFINED = -1; 
	public static final int EXITCODE_SERVICENOTFOUND = -2;
	public static final int EXITCODE_SERVICECREATIONERROR = -3;
	public static final int EXITCODE_SERVICEFILE_NOTFOUND = -4;
	
	public static final int EXITCODE_MAX = 1;
	
	
	
	
	
	
	protected String serviceName = "";
	protected HashMap<String, ArrayList<String>> optionTable = new HashMap<String, ArrayList<String>> ();	
	protected String[] options;
	protected boolean result = false;	
	protected int exitCode = EXITCODE_UNDEFINED;
	
	protected AbstractOutputManager outputManager = null;

	
	public AbstractService() {
		super();
	}		
	
	public AbstractService(String serviceName) {
		super();
		this.serviceName = serviceName;
	}		
	
	public void initOptions(String[] options) {
		this.options = options;
		setOptions(options);	    
	    
	}

	
	public AbstractService(String serviceName,String[] options) {
		this(serviceName);
		initOptions(options);
	}	

	
	private void readOptionFile(String fileName) {		
        try (final BufferedReader in = new BufferedReader(new FileReader(fileName))){    	            
        	String str;
            while ((str = in.readLine()) != null) {
            	if ((str.startsWith("\"")) && (str.endsWith("\""))) {
            		str = str.substring(1,str.length()-1);
            	}
            	int  pos = str.indexOf('=');                
                if (pos>-1) {
        	        String propertyName =  str.substring(0,pos);
        	        String propertyValue = str.substring(pos+1);                  	              
                	insertOption(propertyName,propertyValue);
                }
            }        	        	    	    	    
        }        
        catch (Exception e) {
        	log(e, IMessageLogger.LOGLVL_FATAL);
        }
	}
	
	private void readOptionFiles() {
	    //Retrieve options file names
	    String[] fileNames = 
	        getOptionValues(ICoreModifiers.CORE_OPT_FILE);	  
	    
		for (String a : fileNames) {
			System.out.println("File : "+a);
		}

	    
	    for (int i=0;i<fileNames.length;i++) {
	    	String fileName =fileNames[i];
	    	fileName = resolveFilenameTemplate(fileName);
	        readOptionFile(fileName);
	    }
	}
	
	/**
	 * M�thode d'ajout d'option
	 * @param modifier
	 * @param value
	 */
	public void insertOption(String modifier,String value){
		//On recherche si l'option n'a pas d�j� �t� 
		//rencontr�e
		ArrayList<String> valueList;
		if (optionTable.containsKey(modifier)) {
		    valueList = optionTable.get(modifier);
		} else {
		    valueList = new ArrayList<String>();
		    optionTable.put(modifier,valueList);
		}
		valueList.add(value);	    
	}	
	

	
	/**
	 * M�thode de gestion des options.<br>
	 * Cette m�thode transfert les options indiqu�es dans la ligne de
	 * commande vers la table de gestion interne des options.<br>
	 * Pour cela, elle consid�re qu'une option est de la forme : <br>
	 * <code>-[switch][value]</code>
	 * o� [switch] repr�sente une seule lettre.
	 * Les options peuvent �tre relues � l'aide de la m�thode 
	 * {@link #getOptionValue(String) getOptions(String)}.<br>
	 * Les cl�s disponibles sont d�finies en tant que constantes dans
	 * l'interface {@link ICoreModifiers IOptionModifiers}.
	 * @param options
	 */
	public void setOptions(String[] options) {
		optionTable.clear();
		for (int i=0;i<options.length;i++){
			String opt = options[i]; 
			//recherche du signe "="
			int pos = opt.indexOf('=');
			if (pos>-1) {
				String modifier = opt.substring(0,pos);
				String value = opt.substring(pos+1);
				insertOption(modifier,value);
			}
		}
		//Update des options par les fichiers
		readOptionFiles();
		
		if (isVerbose()){
			Iterator<String> it = optionTable.keySet().iterator();
			while (it.hasNext()) {
				String id = it.next();
				String values[] = getOptionValues(id);
				System.out.println("-> id : "+id);
				for (int x = 0; x<values.length ;x++) {
					System.out.println("value : "+values[x]);
					//logMessage(id+"="+values[x], IMessageLogger.LOGLVL_INFO);
				}
				
			}		
		}
		
	}
	/**
	 * M�thode de r�cup�ration de la valeur d'une option.<br>
	 * Cette m�thode permet de r�cup�rer la valeur d'une option
	 * qui aurait �t� pass�e en param�tre � la ligne de commande.<br>
	 * Si cette option n'a pa �t� d�finie, cette m�thode renvoit null.<br>
	 * Si cette option a �t� pass�e plusieurs fois, seule la permi�re valeur
	 * est renvoy�e.<br>
	 * Pour r�cup�rer l'ensemble des valeurs pass�es en param�tre pour une
	 * option donn�e, utilisez la m�thode {@link #getOptionValues(String)getOptionsValues(String)}.
	 * @param key String : Cl� d'option telle que d�finie dans l'interface 
	 * 					   {@link ICoreModifiers IOptionModifiers}.
	 * @return String : Value correspondante � la cl� d'option pass�e ou 
	 *                  null si cette option n'a pas �t� d�finie.
	 */
	public String getOptionValue(String key)  {
	    String[] resultOptions = getOptionValues(key);
	    if (resultOptions.length>0)
	        return resultOptions[0];
		return null;
	}
	
	/**
	 * M�thode de r�cup�ration des valeurs d'une option.<br>
	 * Cette m�thode permet de r�cup�rer les valeurs d'une option
	 * qui auraient �t� pass�es en param�tre � la ligne de commande.<br>
	 * Si cette option n'a pa �t� d�finie, cette m�thode renvoit un tableau vide.<br>
	 * @param key String : Cl� d'option 					  
	 * @return String[] : Tableau de valeurs correspondant � la cl� d'option pass�e ou 
	 *                  tableau vide si cette option n'a pas �t� d�finie.
	 */
	public String[] getOptionValues(String key)  {
	    if (optionTable.containsKey(key)) {
	        ArrayList<String> l = optionTable.get(key);
	        int count = l.size();
	        String[] resultOptions = new String[count];	     
	        for (int i=0;i<count;i++){
	        	resultOptions[i] = l.get(i);
	        }	        
	        return resultOptions;
	    }
	    return new String[0];
	}	
	
	public HashMap<String, ArrayList<String>> getOptionTable() {
		return optionTable;
	}
	
	
	/**
	 * M�thode de validation de la d�finition des options.<br>
	 * Cette m�thode permet de contr�ler si toutes les options
	 * n�cessaires au fonctionnement du service ont bien �t� d�finies.<br>
	 * Cette m�thode est appel�e lors de l'ex�cution du service, dans le
	 * processus d'initialisation. Si elle renvoit faux, le service ne 
	 * poursuivra pas son ex�cution.
	 * <div style="margin-bottom:10px">
	 * Les param�tres n�cessaires pour l'ensemble des services sont les suivants : 
	 * <table border ="1">
	 * <tr style="text-align:center;font-weight:bold"><td >Param�tre</td><td>Description</td></tr>
	 * <tr><td>-U</td><td>Utilisateur ARCAD</td></tr>  
	 * <tr><td>-P</td><td>Mot de passe</td></tr>  
	 * <tr><td>-S</td><td>Nom ou adresse IP du serveur</td></tr>		 
	 * </table>
	 * </div>  
	 * <div>Cette m�thode a pour vocation � �tre surcharg�e dans les 
	 *  classes filles.</div> 
	 * @return boolean : true si toutes les options requises ont �t� 
	 *                   d�finies, false sinon.
	 */
	public boolean validateOptions() {
		return true;
	}	
	/**
	 * M�thode de validation des donn�es.<br>
	 * Cette m�thode permet de contr�ler si les valeurs d�finies
	 * par les options sont valides.
	 * Cette m�thode est appel�e lors de l'ex�cution du service, dans le
	 * processus d'initialisation. Si elle renvoit faux, le service ne 
	 * poursuivra pas son ex�cution.<br>
	 * <div>Cette m�thode a pour vocation � �tre surcharg�e dans les 
	 *  classes filles.</div>
	 * @return boolean : true si les donn�es sont valides, false sinon.
	 */	
	public boolean checkData() {
	    return true;
	}		
	/**
	 * M�thode de validation des droits.<br>
	 * Cette m�thode permet de contr�ler l'utilisateur � le droit
	 * d'ex�cuter la partie m�tier du service.
	 * Cette m�thode est appel�e lors de l'ex�cution du service, dans le
	 * processus d'initialisation. Si elle renvoit faux, le service ne 
	 * poursuivra pas son ex�cution. 
	 * <div>Cette m�thode a pour vocation � �tre surcharg�e dans les 
	 *  classes filles.</div>  
	 * @return boolean : true si l'utilisateur � les autorisations, false sinon.
	 */		
	public boolean checkExecutionRights() {
	    return true;
	}		


	
	protected String resolveFilenameTemplate(String filename) {
		if (filename.indexOf("${date}")!=-1) {
			filename = Utils.substitute(filename,"${date}",Utils.computeId());
		}
		if (filename.indexOf("${arcad.home}")!=-1) {
			filename = Utils.substitute(filename,"${arcad.home}",Utils.getHomeDirectory());
		}		
		return filename;
	}
	
	protected boolean checkRequiredOption (String option,String message) {
		if (optionTable.get(option)==null) {
		    log("["+option+"] : "+message, IMessageLogger.LOGLVL_FATAL); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			return false;			    
		}		
		return true;
	}
	
	private boolean initializeProcess(){
		log(getIntializationMessage(), IMessageLogger.LOGLVL_INFO);
		if (validateOptions()){
		    if (initialize()){
		    	log("Check Data in progress...", IMessageLogger.LOGLVL_VERBOSE);
		        return checkData()&&       
		    		checkExecutionRights();
		    } else {
		        return false;
		    }
		}  else {		   
	    	log("Options not valid", IMessageLogger.LOGLVL_FATAL);
			return false;
		}
	}
	
	private boolean finalizeProcess() {
    	log(getFinalizationMessage(),IMessageLogger.LOGLVL_INFO);    
	    return true;
	}	
	
	
	/**
	 * M�thode d'ex�cution du service.<br>
	 * Cette m�thode est la m�thode principale du service. C'est elle
	 * qui est invoqu�e par le {@link com.arcadsoftware.CLI.ServiceProvider ServiceProvider} pour
	 * accomplir les taches propos�es par le service.<br>
	 * Voici son cycle d'ex�cution : 
	 * <ul>
	 * <li>Lancement du processus d'initialisation 
	 * (voir {@link #initializeProcess() initialProcess()})</li>
	 * <li>Lancement du processus d'ex�cution m�tier 
	 * (voir {@link #run() run()}</li>
	 * <li>Lancement du processus de finalisation 
	 * (voir {@link #finalProcess() finalProcess()}</li> 
	 * </ul> 
	 * Il est � noter que le processus de finalisation est syst�matiquement
	 * appel� quelque soit le r�sultat de l'ex�cution du processus m�tier.
	 * @return boolean : true si l'ex�cution a r�ussie, false sinon.
	 */
	public ExecutionResult execute() {
		try {
			result=false;
			try {	    	
			    if (initializeProcess()) {
			    	log("Process is running", IMessageLogger.LOGLVL_VERBOSE);
			    	checkLicence();
			        result = run();

			    }
			} finally {
				saveStatus();				
			    finalizeProcess();
		        createOutput();
			}		
			if (result) {
				exitCode = EXITCODE_SUCCEED;
			}
			return new ExecutionResult(result, getExitCode());
		} catch (RuntimeException e) {
			log(e.getLocalizedMessage()+"\n"+Utils.stackTrace(e), IMessageLogger.LOGLVL_FATAL);
			return new ExecutionResult(result, EXITCODE_UNDEFINED);
		}		
	}
		
	
	public void saveStatus(){

	}
	
	
	
	
	protected void getExtendedResultProperties(Properties props){
		
	}
	
	
	/**
	 * Renvoit l'identifiant du service
	 * @return String : Identifiant du service
	 */
	public String getServiceName() {
		return serviceName;
	}
	/**
	 * Affecte l'identifiant du service
	 * @param serviceName String : Nom du service
	 */
	public void setServiceName(String serviceName) {
		this.serviceName = serviceName;
	}	

	
	public void writeDebugInfo(String information) {

	}
	

	
	
	public boolean isVerbose(){
		return false;
	}
	
	public int getLogLevel(){
		return 0;
	}
	
	protected boolean initialize(){
		return true;
	}
	
	public int getExitCode() {
		return exitCode;
	}
	
	public String getHelp(){
		StringBuilder help = new StringBuilder("Service Description:\n");		
		String description = getDescription();
		help.append("  ").append(description);
		ArrayList<Parameter> parameters = 
				new ArrayList<Parameter>();
		getParameterDescription(parameters);
		help.append("\nParameters:");
		for (Parameter p : parameters) {
			String pHelp = p.displayedHelp();
			help.append("\n").append(pHelp);
		}
		return help.toString();
	}
	
	
	public File getOutPutDir(){
		String outDirectoryName = getOptionValue(ICoreModifiers.CORE_OUTPUT_DIR);
		if ((outDirectoryName!=null)) {
			File dir = new File(outDirectoryName);			
			if (!dir.exists()) {
				dir.mkdirs();
			}
			return dir;
		}
		return null;
	}
	
	public File getOutPutFile(){
		File outPutDir = getOutPutDir();
		String outFileName = getOptionValue(ICoreModifiers.CORE_OUTPUT_FILE);
		if ((outPutDir!=null) &&(outFileName!=null)) {
			return new File(outPutDir,outFileName);
		}
		return null;
	}	
	
	
	private AbstractOutputManager createOutputManager(File outputFile){
		String outputType = getOptionValue(ICoreModifiers.CORE_OUTPUT_TYPE);
		if (outputType==null) {
			outputType="j";
		}
		
		String className = null;
		if (outputType.equalsIgnoreCase("x")) {
			className = IOutputConsts.OUTPUTCLASS_XML;
		} else if (outputType.equalsIgnoreCase("j")) {
			className = IOutputConsts.OUTPUTCLASS_JSON;
		}
		if (className!=null) {
			try {
				AbstractOutputManager outManager =  (AbstractOutputManager) Class.forName(className).newInstance();
				outManager.setService(this);
				outManager.setOutputFile(outputFile);				
				return outManager;
			} catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
				log(e, IMessageLogger.LOGLVL_FATAL);
			}
		}		
		return null;
	}
	
	public void outputAdditionalInfo(OutputNode root) {
		
	}
	
	protected void createOutput(){
		logger.logInfo("Creating the result output file");
		File outputFile = getOutPutFile();
		if (outputFile!=null) {			
			outputManager = createOutputManager(outputFile);
			if (outputManager!=null) {
				outputManager.generateOutput();
				return;
			} else {
				logger.logWarning("Couldn't create the output manager");
			}
		} 

		
		//Write into the standard output
		LogEntries logs = ((ServiceLogger)logger).getLogEntries();
		for (LogEntry le: logs) {
			System.out.println(le.print());
		
		}
		
	}
	
	public boolean getResult(){
		return result;
	}
	
	/**
	 * M�thode impl�mentant le processus m�tier.<br>
	 * Cette m�thode doit impl�menter les actions d�finissant le
	 * processus m�tier du service.  
	 * @return boolean : true si la finalisation a r�ussie, false sinon.
	 */	
	public abstract boolean run();
	
	public abstract String getIntializationMessage();
	public abstract String getFinalizationMessage();
	
	 
	public abstract String getDescription(); 
	public abstract void getParameterDescription(ArrayList<Parameter> parameters);
	
	public abstract boolean checkLicence();
	 
	
}
