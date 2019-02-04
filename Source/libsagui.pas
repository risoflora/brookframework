(*                         _
 *   ___  __ _  __ _ _   _(_)
 *  / __|/ _` |/ _` | | | | |
 *  \__ \ (_| | (_| | |_| | |
 *  |___/\__,_|\__, |\__,_|_|
 *             |___/
 *
 *   –– cross-platform library which helps to develop web servers or frameworks.
 *
 * Copyright (c) 2016-2018 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Sagui library.
 *
 * Sagui library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Sagui library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Sagui library.  If not, see <http://www.gnu.org/licenses/>.
 *)

{ Cross-platform low-level Pascal binding for the Sagui library. }

unit libsagui;

{$IFDEF FPC}
 {$MODE DELPHI}
 {$PACKRECORDS C}
 {$IFDEF VER3_0}
  {$PUSH}{$MACRO ON}
  {$DEFINE MarshaledAString := PAnsiChar}
  {$DEFINE EInvalidOpException := Exception}
  {$IFDEF VER3_0_0}
   {$DEFINE EFileNotFoundException := Exception}
  {$ENDIF}
  {$POP}
 {$ENDIF}
{$ENDIF}

interface

uses
  SysUtils,
  StrUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF FPC}
  DynLibs,
{$ENDIF}
  SyncObjs;

{$IFDEF FPC}
 {$IFDEF VER3_0}
const
  NilHandle = DynLibs.NilHandle;
type
  TLibHandle = DynLibs.TLibHandle;
 {$ENDIF}
{$ELSE}
const
  NilHandle = HMODULE(0);
type
  TLibHandle = HMODULE;
{$ENDIF}

const
  SG_VERSION_MAJOR = 1;

  SG_VERSION_MINOR = 2;

  SG_VERSION_PATCH = 0;

  SG_VERSION_HEX = (SG_VERSION_MAJOR shl 16) or (SG_VERSION_MINOR shl 8) or
    SG_VERSION_PATCH;

  SG_ERR_SIZE = 256;

{$IF (NOT DEFINED(FPC)) OR DEFINED(VER3_0)}
  SharedSuffix =
 {$IF DEFINED(MSWINDOWS)}
    'dll'
 {$ELSEIF DEFINED(MACOS)}
    'dylib'
 {$ELSE}
    'so'
 {$ENDIF};
{$ENDIF}

  SG_LIB_NAME = Concat(
{$IFDEF MSWINDOWS}
    'libsagui-1' // SG_VERSION_MAJOR
{$ELSE}
    'libsagui'
{$ENDIF}, '.', SharedSuffix
{$IFDEF LINUX}
    , '.1' // SG_VERSION_MAJOR
{$ENDIF}
  );

resourcestring
  SSgLibEmptyName = 'Empty library name.';
  SSgLibNotLoaded = 'Library ''%s'' not loaded.';
  SSgLibInvalid = 'Invalid library ''%s''.';
  SSgLibMinVersion = 'Application requires Sagui library ''%d.%d.%d'' or higher.';

type
  cchar = Byte;
  Pcchar = MarshaledAString;
  cbool = Boolean;
  cuint16_t = UInt16;
  cint = Int32;
  cuint = UInt32;
  cuint64_t = UInt64;
  csize_t = NativeUInt;
  cssize_t = NativeInt;
  ctime_t = NativeInt;
  Pcvoid = Pointer;
  PPcvoid = PPointer;
  cenum = cint;
  cva_list = Pointer;

  sg_err_cb = procedure(cls: Pcvoid; const err: Pcchar); cdecl;

  sg_write_cb = function(handle: Pcvoid; offset: cuint64_t; const buf: Pcchar;
    size: csize_t): csize_t; cdecl;

  sg_read_cb = function(handle: Pcvoid; offset: cuint64_t; buf: Pcchar;
    size: csize_t): cssize_t; cdecl;

  sg_free_cb = procedure(handle: Pcvoid); cdecl;

  sg_save_cb = function(handle: Pcvoid; overwritten: cbool): cint; cdecl;

  sg_save_as_cb = function(handle: Pcvoid; const path: Pcchar;
    overwritten: cbool): cint; cdecl;

  sg_get_segments_cb = function(cls: Pcvoid; const segment: Pcchar): cint; cdecl;

  sg_get_vars_cb = function(cls: Pcvoid; const name: Pcchar;
    const val: Pcchar): cint; cdecl;

var
  sg_version: function: cuint; cdecl;

  sg_version_str: function: Pcchar; cdecl;

  sg_alloc: function(size: csize_t): Pcvoid; cdecl;

  sg_realloc: function(ptr: Pcvoid; size: csize_t): Pointer; cdecl;

  sg_free: procedure(ptr: Pcvoid); cdecl;

  sg_strerror: function(errnum: cint; str: Pcchar; len: csize_t): Pcchar; cdecl;

  sg_is_post: function(const method: Pcchar): cbool; cdecl;

  sg_extract_entrypoint: function(const path: Pcchar): Pcchar; cdecl;

  sg_tmpdir: function: Pcchar; cdecl;

type
  Psg_str = ^sg_str;
  sg_str = record
  end;

var
  sg_str_new: function: Psg_str; cdecl;

  sg_str_free: procedure(str: Psg_str); cdecl;

  sg_str_write: function(str: Psg_str; const val: Pcchar;
    len: csize_t): cint; cdecl;

  sg_str_printf_va: function(str: Psg_str; const fmt: Pcchar;
    ap: cva_list): cint; cdecl;

  sg_str_printf: function(str: Psg_str; const fmt: Pcchar): cint; cdecl
{$IFNDEF _FIXINSIGHT_}varargs{$ENDIF};

  sg_str_content: function(str: Psg_str): Pcchar; cdecl;

  sg_str_length: function(str: Psg_str): csize_t; cdecl;

  sg_str_clear: function(str: Psg_str): cint; cdecl;

type
  PPsg_strmap = ^Psg_strmap;
  Psg_strmap = ^sg_strmap;
  sg_strmap = record
  end;

  sg_strmap_iter_cb = function(cls: Pcvoid; pair: Psg_strmap): cint; cdecl;

  sg_strmap_sort_cb = function(cls: Pcvoid; pair_a: Psg_strmap;
    pair_b: Psg_strmap): cint; cdecl;

var
  sg_strmap_name: function(pair: Psg_strmap): Pcchar; cdecl;

  sg_strmap_val: function(pair: Psg_strmap): Pcchar; cdecl;

  sg_strmap_add: function(map: PPsg_strmap; const name: Pcchar;
    const val: Pcchar): cint; cdecl;

  sg_strmap_set: function(map: PPsg_strmap; const name: Pcchar;
    const val: Pcchar): cint; cdecl;

  sg_strmap_find: function(map: Psg_strmap; const name: Pcchar;
    pair: PPsg_strmap): cint; cdecl;

  sg_strmap_get: function(map: Psg_strmap; const name: Pcchar): Pcchar; cdecl;

  sg_strmap_rm: function(map: PPsg_strmap; const name: Pcchar): cint; cdecl;

  sg_strmap_iter: function(map: Psg_strmap; cb: sg_strmap_iter_cb;
    cls: Pcvoid): cint; cdecl;

  sg_strmap_sort: function(map: PPsg_strmap; cb: sg_strmap_sort_cb;
    cls: Pcvoid): cint; cdecl;

  sg_strmap_count: function(map: Psg_strmap): cuint; cdecl;

  sg_strmap_next: function(next: PPsg_strmap): cint; cdecl;

  sg_strmap_cleanup: procedure(map: PPsg_strmap); cdecl;

type
  Psg_httpauth = ^sg_httpauth;
  sg_httpauth = record
  end;

  PPsg_httpupld = ^Psg_httpupld;
  Psg_httpupld = ^sg_httpupld;
  sg_httpupld = record
  end;

  Psg_httpreq = ^sg_httpreq;
  sg_httpreq = record
  end;

  Psg_httpres = ^sg_httpres;
  sg_httpres = record
  end;

  Psg_httpsrv = ^sg_httpsrv;
  sg_httpsrv = record
  end;

type
  sg_httpauth_cb = function(cls: Pcvoid; auth: Psg_httpauth; req: Psg_httpreq;
    res: Psg_httpres): cbool; cdecl;

  sg_httpupld_cb = function(cls: Pcvoid; handle: PPcvoid; const dir: Pcchar;
    const field: Pcchar; const name: Pcchar; const mime: Pcchar;
    const encoding: Pcchar): cint; cdecl;

  sg_httpuplds_iter_cb = function(cls: Pcvoid; upld: Psg_httpupld): cint; cdecl;

  sg_httpreq_cb = procedure(cls: Pcvoid; req: Psg_httpreq;
    res: Psg_httpres); cdecl;

var
  sg_httpauth_set_realm: function(auth: Psg_httpauth;
    const realm: Pcchar): cint; cdecl;

  sg_httpauth_realm: function(auth: Psg_httpauth): pcchar; cdecl;

  sg_httpauth_deny: function(auth: Psg_httpauth; const justification: Pcchar;
    const content_type: Pcchar): cint; cdecl;

  sg_httpauth_cancel: function(auth: Psg_httpauth): cint; cdecl;

  sg_httpauth_usr: function(auth: Psg_httpauth): Pcchar; cdecl;

  sg_httpauth_pwd: function(auth: Psg_httpauth): Pcchar; cdecl;

var
  sg_httpuplds_iter: function(uplds: Psg_httpupld; cb: sg_httpuplds_iter_cb;
    cls: Pcvoid): cint; cdecl;

  sg_httpuplds_next: function(upld: PPsg_httpupld): cint; cdecl;

  sg_httpuplds_count: function(uplds: Psg_httpupld): cuint; cdecl;

var
  sg_httpupld_handle: function(uplds: Psg_httpupld): Pcvoid; cdecl;

  sg_httpupld_dir: function(uplds: Psg_httpupld): Pcchar; cdecl;

  sg_httpupld_field: function(uplds: Psg_httpupld): Pcchar; cdecl;

  sg_httpupld_name: function(uplds: Psg_httpupld): Pcchar; cdecl;

  sg_httpupld_mime: function(uplds: Psg_httpupld): Pcchar; cdecl;

  sg_httpupld_encoding: function(uplds: Psg_httpupld): Pcchar; cdecl;

  sg_httpupld_size: function(uplds: Psg_httpupld): cuint64_t; cdecl;

  sg_httpupld_save: function(upld: Psg_httpupld;
    overwritten: cbool): cint; cdecl;

  sg_httpupld_save_as: function(upld: Psg_httpupld; const path: Pcchar;
    overwritten: cbool): cint; cdecl;

var
  sg_httpreq_headers: function(req: Psg_httpreq): PPsg_strmap; cdecl;

  sg_httpreq_cookies: function(req: Psg_httpreq): PPsg_strmap; cdecl;

  sg_httpreq_params: function(req: Psg_httpreq): PPsg_strmap; cdecl;

  sg_httpreq_fields: function(req: Psg_httpreq): PPsg_strmap; cdecl;

  sg_httpreq_version: function(req: Psg_httpreq): Pcchar; cdecl;

  sg_httpreq_method: function(req: Psg_httpreq): Pcchar; cdecl;

  sg_httpreq_path: function(req: Psg_httpreq): Pcchar; cdecl;

  sg_httpreq_payload: function(req: Psg_httpreq): Psg_str; cdecl;

  sg_httpreq_is_uploading: function(req: Psg_httpreq): cbool; cdecl;

  sg_httpreq_uploads: function(req: Psg_httpreq): Psg_httpupld; cdecl;

  sg_httpreq_tls_session: function(req: Psg_httpreq): Pcvoid; cdecl;

  sg_httpreq_set_user_data: function(req: Psg_httpreq;
    data: Pcvoid): cint; cdecl;

  sg_httpreq_user_data: function(req: Psg_httpreq): Pcvoid; cdecl;

var
  sg_httpres_headers: function(res: Psg_httpres): PPsg_strmap; cdecl;

  sg_httpres_set_cookie: function(res: Psg_httpres; const name: Pcchar;
    const val: Pcchar): cint; cdecl;

function sg_httpres_send(res: Psg_httpres; buf: Pcchar;
  const content_type: Pcchar; status: cuint): cint; cdecl;

var
  sg_httpres_sendbinary: function(res: Psg_httpres; buf: Pcvoid; size: csize_t;
    const content_type: Pcchar; status: cuint): cint; cdecl;

function sg_httpres_download(res: Psg_httpres; const filename: Pcchar;
  status: cuint): cint; cdecl;

function sg_httpres_render(res: Psg_httpres; const filename: Pcchar;
  status: cuint): cint; cdecl;

var
  sg_httpres_sendfile: function(res: Psg_httpres; size: csize_t;
    max_size: cuint64_t; offset: cuint64_t; const filename: Pcchar;
    rendered: cbool; status: cuint): cint; cdecl;

  sg_httpres_sendstream: function(res: Psg_httpres; size: cuint64_t;
    block_size: csize_t; read_cb: sg_read_cb; handle: Pcvoid;
    free_cb: sg_free_cb; status: cuint): cint; cdecl;

  sg_httpres_clear: function(res: Psg_httpres): cint; cdecl;

var
  sg_httpsrv_new2: function(auth_cb: sg_httpauth_cb; req_cb: sg_httpreq_cb;
    err_cb: sg_err_cb; err_cls: Pcvoid): Psg_httpsrv; cdecl;

  sg_httpsrv_new: function(cb: sg_httpreq_cb; cls: Pcvoid): Psg_httpsrv; cdecl;

  sg_httpsrv_free: procedure(srv: Psg_httpsrv); cdecl;

  sg_httpsrv_tls_listen2: function(srv: Psg_httpsrv; const key: Pcchar;
    const pwd: Pcchar; const cert: Pcchar; const trust: Pcchar;
    const dhparams: Pcchar; port: cuint16_t; threaded: cbool): cbool; cdecl;

  sg_httpsrv_tls_listen: function(srv: Psg_httpsrv; const key: Pcchar;
    const cert: Pcchar; port: cuint16_t; threaded: cbool): cbool; cdecl;

  sg_httpsrv_listen: function(srv: Psg_httpsrv; port: cuint16_t;
    threaded: cbool): cbool; cdecl;

  sg_httpsrv_shutdown: function(srv: Psg_httpsrv): cint; cdecl;

  sg_httpsrv_port: function(srv: Psg_httpsrv): cuint16_t; cdecl;

  sg_httpsrv_is_threaded: function(srv: Psg_httpsrv): cbool; cdecl;

  sg_httpsrv_set_upld_cbs: function(srv: Psg_httpsrv; cb: sg_httpupld_cb;
    cls: Pcvoid; write_cb: sg_write_cb; free_cb: sg_free_cb;
    save_cb: sg_save_cb; save_as_cb: sg_save_as_cb): cint; cdecl;

  sg_httpsrv_set_upld_dir: function(srv: Psg_httpsrv;
    const dir: Pcchar): cint; cdecl;

  sg_httpsrv_upld_dir: function(srv: Psg_httpsrv): Pcchar; cdecl;

  sg_httpsrv_set_post_buf_size: function(srv: Psg_httpsrv;
    size: csize_t): cint; cdecl;

  sg_httpsrv_post_buf_size: function(srv: Psg_httpsrv): csize_t; cdecl;

  sg_httpsrv_set_payld_limit: function(srv: Psg_httpsrv;
    limit: csize_t): cint; cdecl;

  sg_httpsrv_payld_limit: function(srv: Psg_httpsrv): csize_t; cdecl;

  sg_httpsrv_set_uplds_limit: function(srv: Psg_httpsrv;
    limit: cuint64_t): cint; cdecl;

  sg_httpsrv_uplds_limit: function(srv: Psg_httpsrv): cuint64_t; cdecl;

  sg_httpsrv_set_thr_pool_size: function(srv: Psg_httpsrv;
    size: cuint): cint; cdecl;

  sg_httpsrv_thr_pool_size: function(srv: Psg_httpsrv): cuint; cdecl;

  sg_httpsrv_set_con_timeout: function(srv: Psg_httpsrv;
    timeout: cuint): cint; cdecl;

  sg_httpsrv_con_timeout: function(srv: Psg_httpsrv): cuint; cdecl;

  sg_httpsrv_set_con_limit: function(srv: Psg_httpsrv;
    limit: cuint): cint; cdecl;

  sg_httpsrv_con_limit: function(srv: Psg_httpsrv): cuint; cdecl;

var
  sg_httpread_end: function(err: cbool): cssize_t; cdecl;

type
  PPsg_route = ^Psg_route;
  Psg_route = ^sg_route;
  sg_route = record
  end;

  sg_route_cb = procedure(cls: Pcvoid; route: Psg_route); cdecl;

  sg_routes_iter_cb = function(cls: Pcvoid; route: Psg_route): cint; cdecl;

var
  sg_route_handle: function(route: Psg_route): Pcvoid; cdecl;

  sg_route_match: function(route: Psg_route): Pcvoid; cdecl;

  sg_route_rawpattern: function(route: Psg_route): Pcchar; cdecl;

  sg_route_pattern: function(route: Psg_route): Pcchar; cdecl;

  sg_route_path: function(route: Psg_route): Pcchar; cdecl;

  sg_route_get_segments: function(route: Psg_route; cb: sg_get_segments_cb;
    cls: Pcvoid): cint; cdecl;

  sg_route_get_vars: function(route: Psg_route; cb: sg_get_vars_cb;
    cls: Pcvoid): cint; cdecl;

  sg_route_user_data: function(route: Psg_route): Pcvoid; cdecl;

var
  sg_routes_add2: function(routes: PPsg_route; route: PPsg_route;
    const pattern: Pcchar; errmsg: Pcchar; errlen: csize_t;
    cb: sg_route_cb; cls: Pcvoid): cint; cdecl;

  sg_routes_add: function(routes: Psg_route; const pattern: Pcchar;
    cb: sg_route_cb; cls: Pcvoid): cint; cdecl;

  sg_routes_rm: function(routes: PPsg_route;
    const pattern: Pcchar): cint; cdecl;

  sg_routes_iter: function(routes: Psg_route; cb: sg_routes_iter_cb;
    cls: Pcvoid): cint; cdecl;

  sg_routes_next: function(route: PPsg_route): cint; cdecl;

  sg_routes_count: function(routes: Psg_route): cuint; cdecl;

  sg_routes_cleanup: function(routes: PPsg_route): cint; cdecl;

type
  Psg_router = ^sg_router;
  sg_router = record
  end;

  sg_router_dispatch_cb = function(cls: Pcvoid; const path: Pcchar;
    route: Psg_route): cint; cdecl;

  sg_router_match_cb = function(cls: Pcvoid; route: Psg_route): cint; cdecl;

var
  sg_router_new: function(routes: Psg_route): Psg_router; cdecl;

  sg_router_free: procedure(router: Psg_router); cdecl;

  sg_router_dispatch2: function(router: Psg_router; const path: Pcchar;
    user_data: Pcvoid; dispatch_cb: sg_router_dispatch_cb; cls: Pcvoid;
    match_cb: sg_router_match_cb): cint; cdecl;

  sg_router_dispatch: function(router: Psg_router; const path: Pcchar;
    user_data: Pcvoid): cint; cdecl;

type
  PPsg_entrypoint = ^Psg_entrypoint;
  Psg_entrypoint = ^sg_entrypoint;
  sg_entrypoint = record
  end;

var
  sg_entrypoint_name: function(entrypoint: Psg_entrypoint): Pcchar; cdecl;

  sg_entrypoint_set_user_data: function(entrypoint: Psg_entrypoint;
    user_data: Pcvoid): cint; cdecl;

  sg_entrypoint_user_data: function(entrypoint: Psg_entrypoint): Pcvoid; cdecl;

type
  Psg_entrypoints = ^sg_entrypoints;
  sg_entrypoints = record
  end;

  sg_entrypoints_iter_cb = function(cls: Pcvoid;
    entrypoint: Psg_entrypoint): cint; cdecl;

var
  sg_entrypoints_new: function: Psg_entrypoints; cdecl;

  sg_entrypoints_free: procedure(entrypoints: Psg_entrypoints); cdecl;

  sg_entrypoints_add: function(entrypoints: Psg_entrypoints; const path: Pcchar;
    user_data: Pcvoid): cint; cdecl;

  sg_entrypoints_rm: function(entrypoints: Psg_entrypoints;
    const path: Pcchar): cint; cdecl;

  sg_entrypoints_iter: function(entrypoints: Psg_entrypoints;
    cb: sg_entrypoints_iter_cb; cls: Pcvoid): cint; cdecl;

  sg_entrypoints_clear: function(entrypoints: Psg_entrypoints): cint; cdecl;

  sg_entrypoints_find: function(entrypoints: Psg_entrypoints;
    entrypoint: PPsg_entrypoint; const path: Pcchar): cint; cdecl;

type
  ESgLibNotLoaded = class(EFileNotFoundException);

  TSgLibUnloadCb = procedure(ACls: Pointer); cdecl;

  PSgLibUnloadCbItem = ^TSgLibUnloadCbItem;
  TSgLibUnloadCbItem = record
    Next: PSgLibUnloadCbItem;
    Cb: TSgLibUnloadCb;
    Cls: Pointer;
  end;

  SgLib = record
  private class var
    GCS: TCriticalSection;
    GUnloadCbs: PSgLibUnloadCbItem;
    GLastName: TFileName;
    GHandle: TLibHandle;
  private
    class procedure FreeUnloadCbs; static; inline;
    class procedure CallUnloadCbs; static; inline;
  public
    class procedure Init; static; inline;
    class procedure Done; static; inline;
    class function GetLastName: string; static; inline;
    class procedure CheckVersion; static; inline;
    class procedure CheckLastError(ALastError: Integer); static; inline;
    class function Load(const AName: TFileName): TLibHandle; static;
    class function Unload: TLibHandle; static;
    class function IsLoaded: Boolean; static;
    class procedure Check; static;
    class procedure AddUnloadCb(ACb: TSgLibUnloadCb; ACls: Pointer); static;
    class procedure RmUnloadCb(ACb: TSgLibUnloadCb); static;
    class property Handle: TLibHandle read GHandle;
  end;

implementation

function sg_httpres_send(res: Psg_httpres; buf: Pcchar;
  const content_type: Pcchar; status: cuint): cint;
var
  len: csize_t;
begin
  if Assigned(buf) then
    len := Length(buf)
  else
    len := 0;
  Result := sg_httpres_sendbinary(res, buf, len, content_type, status);
end;

function sg_httpres_download(res: Psg_httpres; const filename: Pcchar;
  status: cuint): cint;
begin
  Result := sg_httpres_sendfile(res, 0, 0, 0, filename, False, status);
end;

function sg_httpres_render(res: Psg_httpres; const filename: Pcchar;
  status: cuint): cint;
begin
  Result := sg_httpres_sendfile(res, 0, 0, 0, filename, True, status);
end;

class procedure SgLib.Init;
begin
  GCS := TCriticalSection.Create;
  GUnloadCbs := nil;
end;

class procedure SgLib.FreeUnloadCbs;
var
  P: PSgLibUnloadCbItem;
begin
  while Assigned(GUnloadCbs) do
  begin
    P := GUnloadCbs;
    GUnloadCbs := P^.Next;
    Dispose(P);
  end;
end;

class procedure SgLib.Done;
begin
  GCS.Acquire;
  try
    FreeUnloadCbs;
  finally
    GCS.Release;
    GCS.Free;
  end;
end;

class procedure SgLib.CallUnloadCbs;
var
  P: PSgLibUnloadCbItem;
begin
  P := GUnloadCbs;
  while Assigned(P) do
  begin
    P^.Cb(P^.Cls);
    P := P^.Next;
  end;
end;

class function SgLib.GetLastName: string;
begin
  Result := GLastName;
end;

class procedure SgLib.CheckVersion;
begin
  try
    if not Assigned(sg_version) then
      raise EInvalidOpException.CreateFmt(SSgLibInvalid, [GetLastName]);
    if sg_version < SG_VERSION_HEX then
      raise EInvalidOpException.CreateFmt(SSgLibMinVersion, [
        SG_VERSION_MAJOR, SG_VERSION_MINOR, SG_VERSION_PATCH]);
  except
    Unload;
    raise;
  end;
end;

class procedure SgLib.CheckLastError(ALastError: Integer);
var
  P: array[0..SG_ERR_SIZE-1] of cchar;
  S: string;
begin
  if (ALastError = 0) or (not Assigned(sg_strerror)) then
    Exit;
  sg_strerror(ALastError, @P[0], SG_ERR_SIZE);
{$IFDEF FPC}
  SetString(S, @P[0], Length(Pcchar(@P[0])));
  SetCodePage(RawByteString(S), CP_UTF8, False);
{$ELSE}
  S := TMarshal.ReadStringAsUtf8(TPtrWrapper.Create(@P[0]));
{$ENDIF}
  raise EOSError.Create(S);
end;

class function SgLib.Load(const AName: TFileName): TLibHandle;
begin //FI:C101
  GCS.Acquire;
  try
    if AName = '' then
      raise EArgumentException.Create(SSgLibEmptyName);
    GHandle := SafeLoadLibrary(AName);
    if GHandle = NilHandle then
    begin
{$IFDEF MSWINDOWS}
      if GetLastError = ERROR_BAD_EXE_FORMAT then
        raise ESgLibNotLoaded.CreateFmt(SSgLibInvalid, [AName]);
{$ENDIF}
      raise ESgLibNotLoaded.CreateFmt(SSgLibNotLoaded, [AName])
    end;
    GLastName := AName;

    sg_version := GetProcAddress(GHandle, 'sg_version');
    sg_version_str := GetProcAddress(GHandle, 'sg_version_str');

    CheckVersion;

    sg_alloc := GetProcAddress(GHandle, 'sg_alloc');
    sg_realloc := GetProcAddress(GHandle, 'sg_realloc');
    sg_free := GetProcAddress(GHandle, 'sg_free');
    sg_strerror := GetProcAddress(GHandle, 'sg_strerror');
    sg_is_post := GetProcAddress(GHandle, 'sg_is_post');
    sg_extract_entrypoint := GetProcAddress(GHandle, 'sg_extract_entrypoint');
    sg_tmpdir := GetProcAddress(GHandle, 'sg_tmpdir');

    sg_str_new := GetProcAddress(GHandle, 'sg_str_new');
    sg_str_free := GetProcAddress(GHandle, 'sg_str_free');
    sg_str_write := GetProcAddress(GHandle, 'sg_str_write');
    sg_str_printf_va := GetProcAddress(GHandle, 'sg_str_printf_va');
    sg_str_printf := GetProcAddress(GHandle, 'sg_str_printf');
    sg_str_content := GetProcAddress(GHandle, 'sg_str_content');
    sg_str_length := GetProcAddress(GHandle, 'sg_str_length');
    sg_str_clear := GetProcAddress(GHandle, 'sg_str_clear');

    sg_strmap_name := GetProcAddress(GHandle, 'sg_strmap_name');
    sg_strmap_val := GetProcAddress(GHandle, 'sg_strmap_val');
    sg_strmap_add := GetProcAddress(GHandle, 'sg_strmap_add');
    sg_strmap_set := GetProcAddress(GHandle, 'sg_strmap_set');
    sg_strmap_find := GetProcAddress(GHandle, 'sg_strmap_find');
    sg_strmap_get := GetProcAddress(GHandle, 'sg_strmap_get');
    sg_strmap_rm := GetProcAddress(GHandle, 'sg_strmap_rm');
    sg_strmap_iter := GetProcAddress(GHandle, 'sg_strmap_iter');
    sg_strmap_sort := GetProcAddress(GHandle, 'sg_strmap_sort');
    sg_strmap_count := GetProcAddress(GHandle, 'sg_strmap_count');
    sg_strmap_next := GetProcAddress(GHandle, 'sg_strmap_next');
    sg_strmap_cleanup := GetProcAddress(GHandle, 'sg_strmap_cleanup');

    sg_httpauth_set_realm := GetProcAddress(GHandle, 'sg_httpauth_set_realm');
    sg_httpauth_realm := GetProcAddress(GHandle, 'sg_httpauth_realm');
    sg_httpauth_deny := GetProcAddress(GHandle, 'sg_httpauth_deny');
    sg_httpauth_cancel := GetProcAddress(GHandle, 'sg_httpauth_cancel');
    sg_httpauth_usr := GetProcAddress(GHandle, 'sg_httpauth_usr');
    sg_httpauth_pwd := GetProcAddress(GHandle, 'sg_httpauth_pwd');

    sg_httpuplds_iter := GetProcAddress(GHandle, 'sg_httpuplds_iter');
    sg_httpuplds_next := GetProcAddress(GHandle, 'sg_httpuplds_next');
    sg_httpuplds_count := GetProcAddress(GHandle, 'sg_httpuplds_count');

    sg_httpupld_handle := GetProcAddress(GHandle, 'sg_httpupld_handle');
    sg_httpupld_dir := GetProcAddress(GHandle, 'sg_httpupld_dir');
    sg_httpupld_field := GetProcAddress(GHandle, 'sg_httpupld_field');
    sg_httpupld_name := GetProcAddress(GHandle, 'sg_httpupld_name');
    sg_httpupld_mime := GetProcAddress(GHandle, 'sg_httpupld_mime');
    sg_httpupld_encoding := GetProcAddress(GHandle, 'sg_httpupld_encoding');
    sg_httpupld_size := GetProcAddress(GHandle, 'sg_httpupld_size');
    sg_httpupld_save := GetProcAddress(GHandle, 'sg_httpupld_save');
    sg_httpupld_save_as := GetProcAddress(GHandle, 'sg_httpupld_save_as');

    sg_httpreq_headers := GetProcAddress(GHandle, 'sg_httpreq_headers');
    sg_httpreq_cookies := GetProcAddress(GHandle, 'sg_httpreq_cookies');
    sg_httpreq_params := GetProcAddress(GHandle, 'sg_httpreq_params');
    sg_httpreq_fields := GetProcAddress(GHandle, 'sg_httpreq_fields');
    sg_httpreq_version := GetProcAddress(GHandle, 'sg_httpreq_version');
    sg_httpreq_method := GetProcAddress(GHandle, 'sg_httpreq_method');
    sg_httpreq_path := GetProcAddress(GHandle, 'sg_httpreq_path');
    sg_httpreq_payload := GetProcAddress(GHandle, 'sg_httpreq_payload');
    sg_httpreq_is_uploading := GetProcAddress(GHandle, 'sg_httpreq_is_uploading');
    sg_httpreq_uploads := GetProcAddress(GHandle, 'sg_httpreq_uploads');
    sg_httpreq_tls_session := GetProcAddress(GHandle, 'sg_httpreq_tls_session');
    sg_httpreq_set_user_data := GetProcAddress(GHandle, 'sg_httpreq_set_user_data');
    sg_httpreq_user_data := GetProcAddress(GHandle, 'sg_httpreq_user_data');

    sg_httpres_headers := GetProcAddress(GHandle, 'sg_httpres_headers');
    sg_httpres_set_cookie := GetProcAddress(GHandle, 'sg_httpres_set_cookie');
    sg_httpres_sendbinary := GetProcAddress(GHandle, 'sg_httpres_sendbinary');
    sg_httpres_sendfile := GetProcAddress(GHandle, 'sg_httpres_sendfile');
    sg_httpres_sendstream := GetProcAddress(GHandle, 'sg_httpres_sendstream');
    sg_httpres_clear := GetProcAddress(GHandle, 'sg_httpres_clear');

    sg_httpsrv_new2 := GetProcAddress(GHandle, 'sg_httpsrv_new2');
    sg_httpsrv_new := GetProcAddress(GHandle, 'sg_httpsrv_new');
    sg_httpsrv_free := GetProcAddress(GHandle, 'sg_httpsrv_free');
    sg_httpsrv_tls_listen2 := GetProcAddress(GHandle, 'sg_httpsrv_tls_listen2');
    sg_httpsrv_tls_listen := GetProcAddress(GHandle, 'sg_httpsrv_tls_listen');
    sg_httpsrv_listen := GetProcAddress(GHandle, 'sg_httpsrv_listen');
    sg_httpsrv_shutdown := GetProcAddress(GHandle, 'sg_httpsrv_shutdown');
    sg_httpsrv_port := GetProcAddress(GHandle, 'sg_httpsrv_port');
    sg_httpsrv_is_threaded := GetProcAddress(GHandle, 'sg_httpsrv_is_threaded');
    sg_httpsrv_set_upld_cbs := GetProcAddress(GHandle, 'sg_httpsrv_set_upld_cbs');
    sg_httpsrv_set_upld_dir := GetProcAddress(GHandle, 'sg_httpsrv_set_upld_dir');
    sg_httpsrv_upld_dir := GetProcAddress(GHandle, 'sg_httpsrv_upld_dir');
    sg_httpsrv_set_post_buf_size := GetProcAddress(GHandle, 'sg_httpsrv_set_post_buf_size');
    sg_httpsrv_post_buf_size := GetProcAddress(GHandle, 'sg_httpsrv_post_buf_size');
    sg_httpsrv_set_payld_limit := GetProcAddress(GHandle, 'sg_httpsrv_set_payld_limit');
    sg_httpsrv_payld_limit := GetProcAddress(GHandle, 'sg_httpsrv_payld_limit');
    sg_httpsrv_set_uplds_limit := GetProcAddress(GHandle, 'sg_httpsrv_set_uplds_limit');
    sg_httpsrv_uplds_limit := GetProcAddress(GHandle, 'sg_httpsrv_uplds_limit');
    sg_httpsrv_set_thr_pool_size := GetProcAddress(GHandle, 'sg_httpsrv_set_thr_pool_size');
    sg_httpsrv_thr_pool_size := GetProcAddress(GHandle, 'sg_httpsrv_thr_pool_size');
    sg_httpsrv_set_con_timeout := GetProcAddress(GHandle, 'sg_httpsrv_set_con_timeout');
    sg_httpsrv_con_timeout := GetProcAddress(GHandle, 'sg_httpsrv_con_timeout');
    sg_httpsrv_set_con_limit := GetProcAddress(GHandle, 'sg_httpsrv_set_con_limit');
    sg_httpsrv_con_limit := GetProcAddress(GHandle, 'sg_httpsrv_con_limit');

    sg_httpread_end := GetProcAddress(GHandle, 'sg_httpread_end');

    sg_route_handle := GetProcAddress(GHandle, 'sg_route_handle');
    sg_route_match := GetProcAddress(GHandle, 'sg_route_match');
    sg_route_rawpattern := GetProcAddress(GHandle, 'sg_route_rawpattern');
    sg_route_pattern := GetProcAddress(GHandle, 'sg_route_pattern');
    sg_route_path := GetProcAddress(GHandle, 'sg_route_path');
    sg_route_get_segments := GetProcAddress(GHandle, 'sg_route_get_segments');
    sg_route_get_vars := GetProcAddress(GHandle, 'sg_route_get_vars');
    sg_route_user_data := GetProcAddress(GHandle, 'sg_route_user_data');

    sg_routes_add2 := GetProcAddress(GHandle, 'sg_routes_add2');
    sg_routes_add := GetProcAddress(GHandle, 'sg_routes_add');
    sg_routes_rm := GetProcAddress(GHandle, 'sg_routes_rm');
    sg_routes_iter := GetProcAddress(GHandle, 'sg_routes_iter');
    sg_routes_next := GetProcAddress(GHandle, 'sg_routes_next');
    sg_routes_count := GetProcAddress(GHandle, 'sg_routes_count');
    sg_routes_cleanup := GetProcAddress(GHandle, 'sg_routes_cleanup');

    sg_router_new := GetProcAddress(GHandle, 'sg_router_new');
    sg_router_free := GetProcAddress(GHandle, 'sg_router_free');
    sg_router_dispatch2 := GetProcAddress(GHandle, 'sg_router_dispatch2');
    sg_router_dispatch := GetProcAddress(GHandle, 'sg_router_dispatch');

    sg_entrypoint_name := GetProcAddress(GHandle, 'sg_entrypoint_name');
    sg_entrypoint_set_user_data := GetProcAddress(GHandle, 'sg_entrypoint_set_user_data');
    sg_entrypoint_user_data := GetProcAddress(GHandle, 'sg_entrypoint_user_data');

    sg_entrypoints_new := GetProcAddress(GHandle, 'sg_entrypoints_new');
    sg_entrypoints_free := GetProcAddress(GHandle, 'sg_entrypoints_free');
    sg_entrypoints_add := GetProcAddress(GHandle, 'sg_entrypoints_add');
    sg_entrypoints_rm := GetProcAddress(GHandle, 'sg_entrypoints_rm');
    sg_entrypoints_iter := GetProcAddress(GHandle, 'sg_entrypoints_iter');
    sg_entrypoints_clear := GetProcAddress(GHandle, 'sg_entrypoints_clear');
    sg_entrypoints_find := GetProcAddress(GHandle, 'sg_entrypoints_find');

    Result := GHandle;
  finally
    GCS.Release;
  end;
end;

class function SgLib.Unload: TLibHandle;
begin //FI:C101
  GCS.Acquire;
  try
    if GHandle = NilHandle then
      Exit(NilHandle);
    CallUnloadCbs;
    if not FreeLibrary(GHandle) then
      Exit(GHandle);
    GHandle := NilHandle;
    GLastName := '';

    sg_version := nil;
    sg_version_str := nil;
    sg_alloc := nil;
    sg_realloc := nil;
    sg_free := nil;
    sg_strerror := nil;
    sg_is_post := nil;
    sg_extract_entrypoint := nil;
    sg_tmpdir := nil;

    sg_str_new := nil;
    sg_str_free := nil;
    sg_str_write := nil;
    sg_str_printf_va := nil;
    sg_str_printf := nil;
    sg_str_content := nil;
    sg_str_length := nil;
    sg_str_clear := nil;

    sg_strmap_name := nil;
    sg_strmap_val := nil;
    sg_strmap_add := nil;
    sg_strmap_set := nil;
    sg_strmap_find := nil;
    sg_strmap_get := nil;
    sg_strmap_rm := nil;
    sg_strmap_iter := nil;
    sg_strmap_sort := nil;
    sg_strmap_count := nil;
    sg_strmap_next := nil;
    sg_strmap_cleanup := nil;

    sg_httpauth_set_realm := nil;
    sg_httpauth_realm := nil;
    sg_httpauth_deny := nil;
    sg_httpauth_cancel := nil;
    sg_httpauth_usr := nil;
    sg_httpauth_pwd := nil;

    sg_httpuplds_iter := nil;
    sg_httpuplds_next := nil;
    sg_httpuplds_count := nil;

    sg_httpupld_handle := nil;
    sg_httpupld_dir := nil;
    sg_httpupld_field := nil;
    sg_httpupld_name := nil;
    sg_httpupld_mime := nil;
    sg_httpupld_encoding := nil;
    sg_httpupld_size := nil;
    sg_httpupld_save := nil;
    sg_httpupld_save_as := nil;

    sg_httpreq_headers := nil;
    sg_httpreq_cookies := nil;
    sg_httpreq_params := nil;
    sg_httpreq_fields := nil;
    sg_httpreq_version := nil;
    sg_httpreq_method := nil;
    sg_httpreq_path := nil;
    sg_httpreq_payload := nil;
    sg_httpreq_is_uploading := nil;
    sg_httpreq_uploads := nil;
    sg_httpreq_tls_session := nil;
    sg_httpreq_set_user_data := nil;
    sg_httpreq_user_data := nil;

    sg_httpres_headers := nil;
    sg_httpres_set_cookie := nil;
    sg_httpres_sendbinary := nil;
    sg_httpres_sendfile := nil;
    sg_httpres_sendstream := nil;
    sg_httpres_clear := nil;

    sg_httpsrv_new2 := nil;
    sg_httpsrv_new := nil;
    sg_httpsrv_free := nil;
    sg_httpsrv_tls_listen2 := nil;
    sg_httpsrv_tls_listen := nil;
    sg_httpsrv_listen := nil;
    sg_httpsrv_shutdown := nil;
    sg_httpsrv_port := nil;
    sg_httpsrv_is_threaded := nil;
    sg_httpsrv_set_upld_cbs := nil;
    sg_httpsrv_set_upld_dir := nil;
    sg_httpsrv_upld_dir := nil;
    sg_httpsrv_set_post_buf_size := nil;
    sg_httpsrv_post_buf_size := nil;
    sg_httpsrv_set_payld_limit := nil;
    sg_httpsrv_payld_limit := nil;
    sg_httpsrv_set_uplds_limit := nil;
    sg_httpsrv_uplds_limit := nil;
    sg_httpsrv_set_thr_pool_size := nil;
    sg_httpsrv_thr_pool_size := nil;
    sg_httpsrv_set_con_timeout := nil;
    sg_httpsrv_con_timeout := nil;
    sg_httpsrv_set_con_limit := nil;
    sg_httpsrv_con_limit := nil;

    sg_httpread_end := nil;

    sg_route_handle := nil;
    sg_route_match := nil;
    sg_route_rawpattern := nil;
    sg_route_pattern := nil;
    sg_route_path := nil;
    sg_route_get_segments := nil;
    sg_route_get_vars := nil;
    sg_route_user_data := nil;
    sg_routes_add2 := nil;
    sg_routes_add := nil;
    sg_routes_rm := nil;
    sg_routes_iter := nil;
    sg_routes_next := nil;
    sg_routes_count := nil;
    sg_routes_cleanup := nil;

    sg_router_new := nil;
    sg_router_free := nil;
    sg_router_dispatch2 := nil;
    sg_router_dispatch := nil;

    sg_entrypoint_name := nil;
    sg_entrypoint_set_user_data := nil;
    sg_entrypoint_user_data := nil;

    sg_entrypoints_new := nil;
    sg_entrypoints_free := nil;
    sg_entrypoints_add := nil;
    sg_entrypoints_rm := nil;
    sg_entrypoints_iter := nil;
    sg_entrypoints_clear := nil;
    sg_entrypoints_find := nil;

    Result := NilHandle;
  finally
    GCS.Release;
  end;
end;

class function SgLib.IsLoaded: Boolean;
begin
  GCS.Acquire;
  try
    Result := GHandle <> NilHandle;
  finally
    GCS.Release;
  end;
end;

class procedure SgLib.Check;
begin
  if GHandle = NilHandle then
    raise ESgLibNotLoaded.CreateFmt(SSgLibNotLoaded,
      [IfThen(GLastName = '', SG_LIB_NAME, GLastName)]);
end;

class procedure SgLib.AddUnloadCb(ACb: TSgLibUnloadCb; ACls: Pointer);
var
  P: PSgLibUnloadCbItem;
begin
  GCS.Acquire;
  try
    New(P);
    P^.Next := GUnloadCbs;
    P^.Cb := ACb;
    P^.Cls := ACls;
    GUnloadCbs := P;
  finally
    GCS.Release;
  end;
end;

class procedure SgLib.RmUnloadCb(ACb: TSgLibUnloadCb);
var
  D: PSgLibUnloadCbItem;
  P: ^PSgLibUnloadCbItem;
begin
  GCS.Acquire;
  try
    P := @GUnloadCbs;
    while Assigned(P^) and (@P^^.Cb <> @ACb) do
      P := @P^.Next;
    if Assigned(P^) then
    begin
      D := P^;
      P^ := D^.Next;
      Dispose(D);
    end;
  finally
    GCS.Release;
  end;
end;

initialization
  SgLib.Init;

finalization
  SgLib.Done;

end.
