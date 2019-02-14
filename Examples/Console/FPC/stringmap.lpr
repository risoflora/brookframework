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

program stringmap;

{$MODE DELPHI}
{$WARN 5024 OFF}

uses
  SysUtils,
  Classes,
  BrookLibraryLoader,
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
  h: Pointer;
  map: TBrookStringMap;
begin
  if not TBrookLibraryLoader.Load(TBrookLibraryLoader.LIB_NAME) then
  begin
    WriteLn(ErrOutput, 'Library not loaded.');
    Halt(1);
  end;
  map := TBrookStringMap.Create(@h);
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
