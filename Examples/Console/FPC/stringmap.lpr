(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2021 Silvio Clecio <silvioprog@gmail.com>
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

program stringmap;

{$MODE DELPHI}

uses
  SysUtils,
  Classes,
  BrookStringMap;

function map_sort(AData: Pointer; APairA, APairB: TBrookStringPair): Integer;
begin
  Result := CompareStr(APairB.Name, APairA.Name); // desc
end;

function map_iter(AData: Pointer; APair: TBrookStringPair): Integer;
begin
  WriteLn(#9, APair.Name[1], ': ', APair.Name);
  Result := 0;
end;

procedure chat(AMap: TBrookStringMap; const AName, AMsg: string);
var
  VPair: TBrookStringPair;
begin
  AMap.AddOrSet(AName, AMsg);
  if AMap.Find(AName, VPair) then
    WriteLn(VPair.Name[1], ':'#9, VPair.Value);
end;

var
  map: TBrookStringMap;
begin
  map := TBrookStringMap.Create(nil);
  try
    chat(map, 'Clecio', 'Hello!');
    chat(map, 'Paim', 'Hello. How are you?');
    chat(map, 'Clecio', 'I''m fine. And you?');
    chat(map, 'Paim', 'Me too.');
    WriteLn;
    WriteLn('Chatters:');
    map.Sort(map_sort, nil);
    map.Iterate(map_iter, nil);
{$IFDEF MSWINDOWS}
    ReadLn;
{$ENDIF}
  finally
    map.Free;
  end;
end.
