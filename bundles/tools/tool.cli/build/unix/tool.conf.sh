#!/usr/bin/env sh
#*******************************************************************************
# Copyright (c) 2023 ARCAD Software.
#
# This program and the accompanying materials
# are made available under the terms of the Eclipse Public License 2.0
# which accompanies this distribution, and is available at
# https://www.eclipse.org/legal/epl-2.0/
#
# SPDX-License-Identifier: EPL-2.0
#
# Contributors:
#     ARCAD Software - initial API and implementation
#*******************************************************************************
# This configuration file define the local parameters
# and will not be updated after first installation.
set -e

# resolve links
current="$(dirname $0)"
cd "$current"
current="$(pwd)"
cd ".."

# Define the home directory of the targeted Product:

PRODUCT_HOME="$(pwd)"

cd "$current"

# Define here the path to the Java command:
# (You can change this variable to match your actual installation.)
# WARNING: The used JVM must be the same as the one used to run the application.

JAVA_CMD=$PRODUCT_HOME/jre/bin/java

if [ ! -f "$JAVA_CMD" ]; then
  JAVA_CMD=java
fi

tooljar="$current/tools.target.jar"

# Check that target executable exists
if [ ! -f "$tooljar" ]; then
  echo "Cannot find $tooljar"
  echo "This file is needed to run this program"
  exit 1
fi

# Define here the path to all required jar:

CP="$PRODUCT_HOME"/plugins/*:"$tooljar"

# Define the path only with tools program:

SCP="$tooljar"

# Define default Java properties

JAVA_PROPS=-Dorg.ops4j.pax.logging.DefaultServiceLog.level=ERROR

# show java version
"$JAVA_CMD" -version

export JAVA_CMD
export JAVA_PROPS
export PRODUCT_HOME
export CP
