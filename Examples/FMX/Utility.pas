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
 * This file is part of Brook framework.
 *
 * Brook framework is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Brook framework.  If not, see <http://www.gnu.org/licenses/>.
 *)

unit Utility;

interface

{$IFDEF ANDROID}
uses
  System.SysUtils,
  Androidapi.Helpers,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.App,
  Androidapi.JNI.Net;
{$ENDIF}
{$IFDEF MSWINDOWS}
uses
  Winapi.Windows,
  Winapi.ShellAPI;
{$ENDIF}

procedure OpenURL(const AURL: string);

implementation

procedure OpenURL(const AURL: string);
{$IFDEF ANDROID}
var
  VIntent: JIntent;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF ANDROID}
  VIntent := TJIntent.Create;
  VIntent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  VIntent.setData(StrToJURI(AURL));
  TAndroidHelper.Activity.startActivity(VIntent);
{$ENDIF}
end;

end.