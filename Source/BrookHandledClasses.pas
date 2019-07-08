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

{ Base types supporting a library handle. }

unit BrookHandledClasses;

{$I BrookDefines.inc}

interface

uses
  Classes;

type
  { The base handled persistent. }
  TBrookHandledPersistent = class abstract(TPersistent)
  protected
    function GetHandle: Pointer; virtual; abstract;
  public
    { Handle of a feature from the loaded library. }
    property Handle: Pointer read GetHandle;
  end;

  { The base handled collection item. }
  TBrookHandledCollectionItem = class abstract(TCollectionItem)
  protected
    function GetHandle: Pointer; virtual; abstract;
  public
    { Handle of a feature from the loaded library. }
    property Handle: Pointer read GetHandle;
  end;

  { The base handled collection. }
  TBrookHandledOwnedCollection = class abstract(TOwnedCollection)
  protected
    function GetHandle: Pointer; virtual; abstract;
  public
    { Handle of a feature from the loaded library. }
    property Handle: Pointer read GetHandle;
  end;

  { The base handled component. }
  TBrookHandledComponent = class abstract(TComponent)
  protected
    function GetHandle: Pointer; virtual; abstract;
  public
    { Handle of a feature from the loaded library. }
    property Handle: Pointer read GetHandle;
  end;

implementation

end.
