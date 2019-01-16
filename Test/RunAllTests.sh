#! /bin/sh
#   _                     _
#  | |__  _ __ ___   ___ | | __
#  | '_ \| '__/ _ \ / _ \| |/ /
#  | |_) | | | (_) | (_) |   <
#  |_.__/|_|  \___/ \___/|_|\_\
#
#  –– microframework which helps to develop web Pascal applications.
#
# Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
#
# This file is part of Brook framework.
#
# Brook framework is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Brook framework is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with Brook framework.  If not, see <http://www.gnu.org/licenses/>.
#

set -e

COUNT=0
for file in ./*; do
    if [ "$file" != "$0" ] && [ -f "$file" ] && [ -x "$file" ] ; then
        echo -n "Running test $file "
        sh -c "$file"
        echo "OK"
        COUNT=$((COUNT+1))
    fi
done
echo "Total of run tests: $COUNT"