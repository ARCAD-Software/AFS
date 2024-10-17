               ARCAD Foundation Configuration Tools 
               ------------------------------------


This project contain the Command Line Interface (CLI) based tools allowing to 
configure, update and test the AFS based application servers.

These tools are provided to facilitate operations which can be proceeded 
manually, or should required external tools. They are not mandatory to the 
installation of the application server, an in some cases a manual intervention 
will still be required.

These tools are absolutely not required to the execution of the application 
server. Moreover as some of them offer some facilities to execute some 
operation related to the security of the installation, it is highly 
recommended to uninstall these tool as soon as the configuration of the 
server is terminated.


#License

The source code of these programs is open-source and is distributed according 
to the Eclipse Public License, version 2.0. A copy of this license is 
provided in the license.txt file. You can access to the source code of these
tool at the following URL: 

  https://github.com/ARCAD-Software/AFS/tree/main/bundles/tools/tool.cli

Please note that this license only apply to the programs contained into
this project, some of the included libraries may also use other king of
open-source license, none of them is a contaminating one.


#Installation

To install these tools you will have to download the latest release of them
from here: 

 https://github.com/ARCAD-Software/AFS/releases/latest
 
 and get the "com.arcadsoftware.tool.cli_X.Y.Z.jar" listed in the assets,
 X, Y and Z number refer the actual version number of this project.

 Move this file in a "tools" folder located under the AFS server installation
 home directory an copy this file in it. Then execute the following command:

 java -jar ./com.arcadsoftware.tool.cli_X.Y.Z.jar
 

A Java Virtual Machine (JVM), with a version higher or equals to 17, is 
required for the execution of these programs, most of the time an Open-source 
version of the JVM is installed in the "jre" folder of the AFS server home 
directory. If not, you will have to manually install it. In that case you will 
have to edit the "tool.conf.sh" or "tool.conf.bat" file to define the actual 
path to your JVM.
