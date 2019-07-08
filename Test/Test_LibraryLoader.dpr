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

program Test_LibraryLoader;

{$I Tests.inc}

uses
  SysUtils,
  Classes,
  libsagui,
  BrookLibraryLoader;

type
  TEventHolder = class
  private
    FSender: TObject;
    FPassed: Boolean;
  public
    procedure DoNotifyEvent(ASender: TObject);
    property Sender: TObject read FSender write FSender;
    property Passed: Boolean read FPassed write FPassed;
  end;

procedure TEventHolder.DoNotifyEvent(ASender: TObject);
begin
  FPassed := ASender = FSender;
end;

procedure Test_LibraryLoaderCreate;
var
  LL: TBrookLibraryLoader;
  C: TComponent;
begin
  C := TComponent.Create(nil);
  LL := TBrookLibraryLoader.Create(C);
  try
    Assert(LL.Owner = C);
    Assert(LL.LibraryName = SG_LIB_NAME);
  finally
    LL.Free;
    C.Free;
  end;
end;

procedure Test_LibraryLoaderLoad;
begin
  SgLib.Unload;
  Assert(SgLib.Handle = NilHandle);
  Assert(TBrookLibraryLoader.Load(SG_LIB_NAME));
  Assert(SgLib.Handle <> NilHandle);
  SgLib.Unload;
  Assert(SgLib.Handle = NilHandle);
  Assert(TBrookLibraryLoader.Load);
  Assert(SgLib.Handle <> NilHandle);
end;

procedure Test_LibraryLoaderUnload;
begin
  SgLib.Load(SG_LIB_NAME);
  Assert(SgLib.Handle <> NilHandle);
  Assert(TBrookLibraryLoader.Unload);
  Assert(SgLib.Handle = NilHandle);
end;

procedure Test_LibraryLoaderOpen;
var
  LL: TBrookLibraryLoader;
begin
  LL := TBrookLibraryLoader.Create(nil);
  try
    Assert(not LL.Active);
    LL.Open;
    Assert(LL.Active);
  finally
    LL.Free;
  end;
end;

procedure Test_LibraryLoaderClose;
var
  LL: TBrookLibraryLoader;
begin
  LL := TBrookLibraryLoader.Create(nil);
  try
    LL.Open;
    Assert(LL.Active);
    LL.Close;
    Assert(not LL.Active);
  finally
    LL.Free;
  end;
end;

procedure Test_LibraryLoaderActive;
var
  LL: TBrookLibraryLoader;
begin
  LL := TBrookLibraryLoader.Create(nil);
  try
    Assert(not LL.Active);
    LL.Active := not LL.Active;
    Assert(LL.Active);
  finally
    LL.Free;
  end;
end;

procedure Test_LibraryLoaderLibraryName;
var
  LL: TBrookLibraryLoader;
begin
  LL := TBrookLibraryLoader.Create(nil);
  try
    Assert(LL.LibraryName = LL.LIB_NAME);
    LL.LibraryName := '';
    Assert(LL.LibraryName = LL.LIB_NAME);
    LL.LibraryName := 'test';
    Assert(LL.LibraryName = 'test');
  finally
    LL.Free;
  end;
end;

procedure Test_LibraryLoaderVersion;
var
  LL: TBrookLibraryLoader;
begin
  LL := TBrookLibraryLoader.Create(nil);
  try
    Assert(LL.Version = '');
    LL.Open;
    Assert(LL.Version = Format('%d.%d.%d', [SG_VERSION_MAJOR, SG_VERSION_MINOR,
      SG_VERSION_PATCH]));
  finally
    LL.Free;
  end;
end;

procedure Test_LibraryLoaderOnLoad;
var
  LL: TBrookLibraryLoader;
  EH: TEventHolder;
begin
  LL := TBrookLibraryLoader.Create(nil);
  EH := TEventHolder.Create;
  try
    LL.OnLoad := EH.DoNotifyEvent;
    EH.Sender := LL;
    EH.Passed := False;
    LL.Open;
    Assert(EH.Passed);
    EH.Passed := False;
    LL.Open;
    Assert(not EH.Passed);
  finally
    LL.Free;
    EH.Free;
  end;
end;

procedure Test_LibraryLoaderOnUnload;
var
  LL: TBrookLibraryLoader;
  EH: TEventHolder;
begin
  LL := TBrookLibraryLoader.Create(nil);
  EH := TEventHolder.Create;
  try
    LL.Open;
    LL.OnUnload := EH.DoNotifyEvent;
    EH.Sender := LL;
    EH.Passed := False;
    LL.Close;
    Assert(EH.Passed);
    EH.Passed := False;
    LL.Close;
    Assert(not EH.Passed);
  finally
    LL.Free;
    EH.Free;
  end;
end;

begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Test_LibraryLoaderCreate;
  // Test_LibraryLoaderDestroy - not required
  Test_LibraryLoaderLoad;
  Test_LibraryLoaderUnload;
  Test_LibraryLoaderOpen;
  Test_LibraryLoaderClose;
  // Test_LibraryLoaderDefineProperties - not required
  Test_LibraryLoaderActive;
  Test_LibraryLoaderLibraryName;
  Test_LibraryLoaderVersion;
  Test_LibraryLoaderOnLoad;
  Test_LibraryLoaderOnUnload;
end.
