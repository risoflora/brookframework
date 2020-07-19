#!/bin/sh

#   _                     _
#  | |__  _ __ ___   ___ | | __
#  | '_ \| '__/ _ \ / _ \| |/ /
#  | |_) | | | (_) | (_) |   <
#  |_.__/|_|  \___/ \___/|_|\_\
#
# Microframework which helps to develop web Pascal applications.
#
# Copyright (c) 2012-2020 Silvio Clecio <silvioprog@gmail.com>
#
# Brook framework is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# Brook framework is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with Brook framework; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#

set -e

out_dir="HTML"
cache_dir="Cache"

cd "$(dirname "$0")"

rm -rf $out_dir && mkdir -p $out_dir

pasdoc \
  --name "Brook" \
  --title "Brook Tardigrade" \
  --verbosity 2 \
  --language en \
  --format html \
  --css pasdoc.css \
  --spell-check \
  --spell-check-ignore-words spell_ignored_words \
  --visible-members public,published \
  --write-uses-list \
  --auto-abstract \
  --use-tipue-search \
  --auto-link \
  --define FPC \
  --define UNIX \
  --define LINUX \
  --define MSWINDOWS \
  --include ../Source \
  ../Source/BrookExtra.pas \
  ../Source/BrookHTTPAuthentication.pas \
  ../Source/BrookHTTPCookies.pas \
  ../Source/BrookHTTPRequest.pas \
  ../Source/BrookHTTPResponse.pas \
  ../Source/BrookHTTPServer.pas \
  ../Source/BrookHTTPUploads.pas \
  ../Source/BrookHandledClasses.pas \
  ../Source/BrookLibraryLoader.pas \
  ../Source/BrookMathExpression.pas \
  ../Source/BrookMediaTypes.pas \
  ../Source/BrookReader.pas \
  ../Source/BrookString.pas \
  ../Source/BrookStringMap.pas \
  ../Source/BrookURLEntryPoints.pas \
  ../Source/BrookURLRouter.pas \
  ../Source/BrookUtility.pas \
  --cache-dir $cache_dir \
  --output $out_dir
