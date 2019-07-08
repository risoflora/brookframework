(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2019 Silvio Clecio <silvioprog@gmail.com>
 *
 * Brook framework is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Brook framework; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

{ Contains useful extra types for the framework. }

unit BrookExtra;

{$I BrookDefines.inc}

interface

const
  { Default post buffer size (in bytes). }
  BROOK_POST_BUFFER_SIZE = {$IFDEF CPUARM}1024{~1Kb}{$ELSE}4096{~4kB}{$ENDIF};
  { Default payload limit (in bytes). }
  BROOK_PAYLOAD_LIMIT = {$IFDEF CPUARM}1048576{~1MB}{$ELSE}4194304{~4MB}{$ENDIF};
  { Deafult upload(s) limit (in bytes). }
  BROOK_UPLOADS_LIMIT = {$IFDEF CPUARM}16777216{~16MB}{$ELSE}67108864{~64MB}{$ENDIF};

  { Default minimal buffer size (in bytes). }
  BROOK_MIN_BUFFER_SIZE = 128;
  { Default buffer size (in bytes). }
  BROOK_BUFFER_SIZE = 4096;
  { Default file rights (under Unix, 438 = &666 = owner/group/others can read/write). }
  BROOK_FILE_RIGHTS = 438;

  { Default content type for text content (text/plain in UTF-8). }
  BROOK_CT_TEXT_PLAIN = 'text/plain; charset=utf-8';
  { Default content type for binary content (octet-stream). }
  BROOK_CT_OCTET_STREAM = 'application/octet-stream';

implementation

end.
