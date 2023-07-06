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
pushd %~dp0
call tool.conf.bat
cd "%PRODUCT_HOME%"
%JAVA_CMD% %JAVA_PROPS% -cp %SCP% run.Exec cli.GenMasterKey %* -homedir "%PRODUCT_HOME%"
popd
