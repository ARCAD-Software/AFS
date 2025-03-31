@rem ***************************************************************************
@rem Copyright (c) 2023 ARCAD Software.
@rem
@rem This program and the accompanying materials
@rem are made available under the terms of the Eclipse Public License 2.0
@rem which accompanies this distribution, and is available at
@rem https://www.eclipse.org/legal/epl-2.0/
@rem
@rem SPDX-License-Identifier: EPL-2.0
@rem
@rem Contributors:
@rem     ARCAD Software - initial API and implementation
@rem ***************************************************************************
@echo off
rem quotes are required for correct handling of path with spaces

rem Default Application home directory
set PRODUCT_HOME=%~dp0..

rem Set the Java command path 
set JAVA_CMD="%PRODUCT_HOME%\jre\bin\java.exe"

if exist %JAVA_CMD% goto javaok

rem Default java exe for running the tools
if "%JAVA_HOME%" == "" (
 rem WARNING: The used JVM must be the same as the one used to run the application.
 rem You can force the java command to be used by replacing this with the path to the desired one.
 set JAVA_CMD="java.exe"
) else (
 set JAVA_CMD="%JAVA_HOME%\bin\java.exe"
)
:javaok

set current=%~dp0.
set tooljar=%current%\tools.target.jar

set CP="%PRODUCT_HOME%\plugins\*;%tooljar%"
set SCP="%tooljar%"
set JAVA_PROPS=-Dorg.ops4j.pax.logging.DefaultServiceLog.level=ERROR

REM show java version
%JAVA_CMD% -version