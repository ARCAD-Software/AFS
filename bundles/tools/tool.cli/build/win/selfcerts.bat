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
set allargs=%*
for /L %%i in (0,1,8) do @shift
set allargs=%allargs% %*
for /L %%i in (0,1,8) do @shift
set allargs=%allargs% %*
for /L %%i in (0,1,8) do @shift
set allargs=%allargs% %*
for /L %%i in (0,1,8) do @shift
%JAVA_CMD% %JAVA_PROPS% -cp %SCP% run.Exec cli.HTTPSSelfCerts %allargs% %* -homedir "%PRODUCT_HOME%"
popd
