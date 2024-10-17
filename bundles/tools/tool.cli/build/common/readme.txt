               ARCAD Foundation Configuration Tools 
               ------------------------------------


This folder contain the Command Line Interface (CLI) based tools allowing to 
configure, update and test the application server.

These tools are provided to facilitate operations which can be proceeded 
manually, or should required external tools. They are not mandatory to the 
installation of your application, an in some cases they may not fulfill your 
requirement. You should read the application installation guide before to 
execute any of them.

The source code of these programs is open-source and they are distributed 
according to the Eclipse Public License, version 2.0. A copy of this license 
is provided in the license.txt file. You can access to the source code of 
these tool at the following URL: 

  https://github.com/ARCAD-Software/AFS/tree/main/bundles/tools/tool.cli

Please note that this license only apply to the programs contained into
this folder, some of the included libraries may also use other king of
open-source license, none of them is a contaminating one.



These tools are absolutely not required to the execution of the application 
server. Moreover as some of them offer some facilities to execute some 
operation related to the security of your installation, it is highly 
recommended to uninstall these tool as soon as the configuration of the 
server is terminated.

The version number of these tools is equal to the number in the name of the 
file "com.arcadsoftware.tool.cli-X.Y.Z.jar" located in this directory.



Depending on your application, this folder should contain some other files 
(like other ".jar" files and shell script ".sh" or "bat"). If not you can 
install them by typing the following command:

 java -jar ./com.arcadsoftware.tool.cli-X.Y.Z.jar
 
Replace X, Y and Z by the actual number of your version number.

A Java Virtual Machine (JVM), with a version higher or equals to 17, is 
required for the execution of these program, most of the time an Open-source 
version of the JVM is installed in the "jre" folder of the application home 
directory. If not you will have to manually install it. In that case you will 
have to edit the "tool.conf.sh" or "tool.conf.bat" file to define the actual 
path to your JVM. Again please refer to the application installation guide.
