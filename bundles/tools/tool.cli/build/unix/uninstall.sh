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
# Launch the Command...

# resolve links - $0 may be a softlink
current="$0"
while [ -h "$current" ] ; do
  ls=`ls -ld "$current"`
  link=`expr "$ls" : '.*-> \(.*\)$'`
  if expr "$link" : '/.*' > /dev/null; then
    current="$link"
  else
    current=`dirname "$current"`/"$link"
  fi
done
current=`dirname "$current"`

# set tools conf parameters
. "$current"/tool.conf.sh

delall() {
shred -f -v "$PRODUCT_HOME"/tools/*
rm -r "$PRODUCT_HOME"/tools
}
trap delall EXIT
cd ".."
echo "Remove all tools from file system..."