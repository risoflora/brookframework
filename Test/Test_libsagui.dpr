(*                         _
 *   ___  __ _  __ _ _   _(_)
 *  / __|/ _` |/ _` | | | | |
 *  \__ \ (_| | (_| | |_| | |
 *  |___/\__,_|\__, |\__,_|_|
 *             |___/
 *
 * Cross-platform library which helps to develop web servers or frameworks.
 *
 * Copyright (c) 2016-2019 Silvio Clecio <silvioprog@gmail.com>
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

program Test_libsagui;

{$I Tests.inc}

uses
  RTLConsts,
  SysUtils,
  Platform,
  libsagui;

procedure DoLibNotifier1(AClosure: Pointer); cdecl;
begin
  PInteger(AClosure)^ := 123;
end;

procedure DoLibNotifier2(AClosure: Pointer); cdecl;
begin
  PInteger(AClosure)^ := 456;
end;

procedure DoLibNotifier3(AClosure: Pointer); cdecl;
begin
  PInteger(AClosure)^ := 789;
end;

procedure Test_SgLibAddNotifier;
var
  I1, I2, I3: Integer;
  OK: Boolean;
begin
  SgLib.ClearNotifiers;

  OK := False;
  try
    SgLib.AddNotifier(nil, Pointer(1));
  except
    on E: Exception do
      OK := (E.ClassType = EArgumentNilException) and
        (E.Message = Format(SParamIsNil, ['ANotifier']));
  end;
  Assert(OK);
  SgLib.AddNotifier(@DoLibNotifier1, nil);
  SgLib.RemoveNotifier(@DoLibNotifier1);

  SgLib.Load(SG_LIB_NAME);
  I1 := 0;
  I2 := 0;
  I3 := 0;
  SgLib.AddNotifier(@DoLibNotifier1, @I1);
  SgLib.AddNotifier(@DoLibNotifier2, @I2);
  SgLib.AddNotifier(@DoLibNotifier3, @I3);
  SgLib.Unload;
  Assert(I1 = 123);
  Assert(I2 = 456);
  Assert(I3 = 789);
  SgLib.Load(SG_LIB_NAME);
  I1 := 0;
  I2 := 0;
  I3 := 0;
  SgLib.Unload;
  Assert(I1 = 123);
  Assert(I2 = 456);
  Assert(I3 = 789);
end;

procedure Test_SgLibRemoveNotifier;
var
  I1, I2, I3: Integer;
  OK: Boolean;
begin
  SgLib.ClearNotifiers;

  OK := False;
  try
    SgLib.RemoveNotifier(nil);
  except
    on E: Exception do
      OK := (E.ClassType = EArgumentNilException) and
        (E.Message = Format(SParamIsNil, ['ANotifier']));
  end;
  Assert(OK);

  SgLib.Load(SG_LIB_NAME);
  I1 := 0;
  I2 := 0;
  I3 := 0;
  SgLib.AddNotifier(@DoLibNotifier1, @I1);
  SgLib.AddNotifier(@DoLibNotifier2, @I2);
  SgLib.AddNotifier(@DoLibNotifier3, @I3);
  SgLib.RemoveNotifier(@DoLibNotifier2);
  SgLib.Unload;
  Assert(I1 = 123);
  Assert(I2 = 0);
  Assert(I3 = 789);
end;

procedure Test_SgLibClearNotifiers;
var
  I1, I2, I3: Integer;
begin
  SgLib.Load(SG_LIB_NAME);
  I1 := 0;
  I2 := 0;
  I3 := 0;
  SgLib.AddNotifier(@DoLibNotifier1, @I1);
  SgLib.AddNotifier(@DoLibNotifier2, @I2);
  SgLib.AddNotifier(@DoLibNotifier3, @I3);
  SgLib.Unload;
  Assert(I1 = 123);
  Assert(I2 = 456);
  Assert(I3 = 789);
  SgLib.Load(SG_LIB_NAME);
  I1 := 0;
  I2 := 0;
  I3 := 0;
  SgLib.ClearNotifiers;
  SgLib.Unload;
  Assert(I1 = 0);
  Assert(I2 = 0);
  Assert(I3 = 0);

  SgLib.ClearNotifiers;
end;

procedure Test_SgLibGetLastName;
begin
  SgLib.Unload;
  Assert(SgLib.GetLastName = '');
  SgLib.Load(SG_LIB_NAME);
  Assert(SgLib.GetLastName = SG_LIB_NAME);
end;

procedure Test_SgLibCheckVersion;
var
  OK: Boolean;
begin
  SgLib.Unload;
  SgLib.Load(SG_LIB_NAME);
  OK := False;
  try
    SgLib.CheckVersion((Pred(SG_VERSION_MAJOR) shl 16) or
      (SG_VERSION_MINOR shl 8) or SG_VERSION_PATCH);
  except
    on E: Exception do
      OK := (E.ClassType = EInvalidOpException) and
        (E.Message = Format(SSgLibVersion, [SG_VERSION_MAJOR, SG_VERSION_MINOR,
          SG_VERSION_PATCH]));
  end;
  Assert(OK);
  SgLib.Load(SG_LIB_NAME);
  OK := False;
  try
    SgLib.CheckVersion((Succ(SG_VERSION_MAJOR) shl 16) or
      (SG_VERSION_MINOR shl 8) or SG_VERSION_PATCH);
  except
    on E: Exception do
      OK := (E.ClassType = EInvalidOpException) and
        (E.Message = Format(SSgLibVersion, [SG_VERSION_MAJOR, SG_VERSION_MINOR,
          SG_VERSION_PATCH]));
  end;
  Assert(OK);

  SgLib.Load(SG_LIB_NAME);
  OK := False;
  try
    SgLib.CheckVersion((SG_VERSION_MAJOR shl 16) or
      (Pred(SG_VERSION_MINOR) shl 8) or SG_VERSION_PATCH);
  except
    on E: Exception do
      OK := (E.ClassType = EInvalidOpException) and
        (E.Message = Format(SSgLibVersion, [SG_VERSION_MAJOR, SG_VERSION_MINOR,
          SG_VERSION_PATCH]));
  end;
  Assert(OK);
  SgLib.Load(SG_LIB_NAME);
  SgLib.CheckVersion((SG_VERSION_MAJOR shl 16) or
    (Succ(SG_VERSION_MINOR) shl 8) or SG_VERSION_PATCH);

  OK := False;
  try
    SgLib.CheckVersion((SG_VERSION_MAJOR shl 16) or
      (SG_VERSION_MINOR shl 8) or Pred(SG_VERSION_PATCH));
  except
    on E: Exception do
      OK := (E.ClassType = EInvalidOpException) and
        (E.Message = Format(SSgLibVersion, [SG_VERSION_MAJOR, SG_VERSION_MINOR,
          SG_VERSION_PATCH]));
  end;
  Assert(OK);
  SgLib.Load(SG_LIB_NAME);
  SgLib.CheckVersion((SG_VERSION_MAJOR shl 16) or
    (Succ(SG_VERSION_MINOR) shl 8) or 0);

  SgLib.CheckVersion;
end;

procedure Test_SgLibCheckLastError;
var
  F: Pointer;
  OK: Boolean;
begin
  SgLib.CheckLastError(0);
  F := @sg_strerror;
  sg_strerror := nil;
  SgLib.CheckLastError(123);
  sg_strerror := F;

  OK := False;
  try
    SgLib.CheckLastError(EINVAL);
  except
    on E: Exception do
      OK := (E.ClassType = EOSError) and (EOSError(E).ErrorCode = EINVAL) and
        (E.Message = SysErrorMessage(EINVAL));
  end;
  Assert(OK);
  OK := False;
  try
    SgLib.CheckLastError(456);
  except
    on E: Exception do
      OK := (E.ClassType = EOSError) and (EOSError(E).ErrorCode = 456) and
        (E.Message = 'Unknown error 456');
  end;
  Assert(OK);
end;

procedure Test_SgLibLoad;
var
  OK: Boolean;
begin
  SgLib.Unload;
  OK := False;
  try
    Assert(SgLib.Load('') = NilHandle);
  except
    on E: Exception do
      OK := (E.ClassType = EArgumentException) and (E.Message = SSgLibEmptyName);
  end;
  Assert(OK);

  OK := False;
  try
    Assert(SgLib.Load('abc') = NilHandle);
  except
    on E: Exception do
      OK := (E.ClassType = ESgLibNotLoaded) and
        (E.Message = Format(SSgLibNotLoaded, ['abc']));
  end;
  Assert(OK);

  Assert(SgLib.Load(SG_LIB_NAME) <> NilHandle);
end;

procedure Test_SgLibUnload;
begin
  Assert(SgLib.Load(SG_LIB_NAME) <> NilHandle);
  Assert(SgLib.Unload = NilHandle);
end;

procedure Test_SgLibIsLoaded;
begin
  SgLib.Unload;
  Assert(not SgLib.IsLoaded);
  SgLib.Load(SG_LIB_NAME);
  Assert(SgLib.IsLoaded);
end;

procedure Test_SgLibCheck;
var
  OK: Boolean;
begin
  OK := False;
  try
    SgLib.Unload;
    SgLib.Check;
  except
    on E: Exception do
      OK := (E.ClassType = ESgLibNotLoaded) and
        (E.Message = Format(SSgLibNotLoaded, [SG_LIB_NAME]));
  end;
  Assert(OK);
end;

procedure Test_SgLibHandle;
begin
  SgLib.Unload;
  Assert(SgLib.Handle = NilHandle);
  SgLib.Load(SG_LIB_NAME);
  Assert(SgLib.Handle <> NilHandle);
end;

procedure Test_SgLibBinding;
begin
  SgLib.Unload;

  Assert(not Assigned(sg_version));
  Assert(not Assigned(sg_version_str));

  Assert(not Assigned(sg_malloc));
  Assert(not Assigned(sg_alloc));
  Assert(not Assigned(sg_realloc));
  Assert(not Assigned(sg_free));
  Assert(not Assigned(sg_strerror));
  Assert(not Assigned(sg_is_post));
  Assert(not Assigned(sg_extract_entrypoint));
  Assert(not Assigned(sg_tmpdir));
  Assert(not Assigned(sg_eor));

  Assert(not Assigned(sg_str_new));
  Assert(not Assigned(sg_str_free));
  Assert(not Assigned(sg_str_write));
  Assert(not Assigned(sg_str_printf_va));
  Assert(not Assigned(sg_str_printf));
  Assert(not Assigned(sg_str_content));
  Assert(not Assigned(sg_str_length));
  Assert(not Assigned(sg_str_clear));

  Assert(not Assigned(sg_strmap_name));
  Assert(not Assigned(sg_strmap_val));
  Assert(not Assigned(sg_strmap_add));
  Assert(not Assigned(sg_strmap_set));
  Assert(not Assigned(sg_strmap_find));
  Assert(not Assigned(sg_strmap_get));
  Assert(not Assigned(sg_strmap_rm));
  Assert(not Assigned(sg_strmap_iter));
  Assert(not Assigned(sg_strmap_sort));
  Assert(not Assigned(sg_strmap_count));
  Assert(not Assigned(sg_strmap_next));
  Assert(not Assigned(sg_strmap_cleanup));

  Assert(not Assigned(sg_httpauth_set_realm));
  Assert(not Assigned(sg_httpauth_realm));
  Assert(not Assigned(sg_httpauth_deny));
  Assert(not Assigned(sg_httpauth_cancel));
  Assert(not Assigned(sg_httpauth_usr));
  Assert(not Assigned(sg_httpauth_pwd));

  Assert(not Assigned(sg_httpuplds_iter));
  Assert(not Assigned(sg_httpuplds_next));
  Assert(not Assigned(sg_httpuplds_count));

  Assert(not Assigned(sg_httpupld_handle));
  Assert(not Assigned(sg_httpupld_dir));
  Assert(not Assigned(sg_httpupld_field));
  Assert(not Assigned(sg_httpupld_name));
  Assert(not Assigned(sg_httpupld_mime));
  Assert(not Assigned(sg_httpupld_encoding));
  Assert(not Assigned(sg_httpupld_size));
  Assert(not Assigned(sg_httpupld_save));
  Assert(not Assigned(sg_httpupld_save_as));

  Assert(not Assigned(sg_httpreq_headers));
  Assert(not Assigned(sg_httpreq_cookies));
  Assert(not Assigned(sg_httpreq_params));
  Assert(not Assigned(sg_httpreq_fields));
  Assert(not Assigned(sg_httpreq_version));
  Assert(not Assigned(sg_httpreq_method));
  Assert(not Assigned(sg_httpreq_path));
  Assert(not Assigned(sg_httpreq_payload));
  Assert(not Assigned(sg_httpreq_is_uploading));
  Assert(not Assigned(sg_httpreq_uploads));
  Assert(not Assigned(sg_httpreq_tls_session));
  Assert(not Assigned(sg_httpreq_set_user_data));
  Assert(not Assigned(sg_httpreq_user_data));

  Assert(not Assigned(sg_httpres_headers));
  Assert(not Assigned(sg_httpres_set_cookie));
  Assert(not Assigned(sg_httpres_sendbinary));
  Assert(not Assigned(sg_httpres_sendfile2));
  Assert(not Assigned(sg_httpres_sendfile));
  Assert(not Assigned(sg_httpres_sendstream));
  Assert(not Assigned(sg_httpres_zsendbinary2));
  Assert(not Assigned(sg_httpres_zsendbinary));
  Assert(not Assigned(sg_httpres_zsendstream));
  Assert(not Assigned(sg_httpres_zsendstream2));
  Assert(not Assigned(sg_httpres_zsendfile2));
  Assert(not Assigned(sg_httpres_zsendfile));
  Assert(not Assigned(sg_httpres_clear));

  Assert(not Assigned(sg_httpsrv_new2));
  Assert(not Assigned(sg_httpsrv_new));
  Assert(not Assigned(sg_httpsrv_free));
  Assert(not Assigned(sg_httpsrv_tls_listen2));
  Assert(not Assigned(sg_httpsrv_tls_listen));
  Assert(not Assigned(sg_httpsrv_listen));
  Assert(not Assigned(sg_httpsrv_shutdown));
  Assert(not Assigned(sg_httpsrv_port));
  Assert(not Assigned(sg_httpsrv_is_threaded));
  Assert(not Assigned(sg_httpsrv_set_upld_cbs));
  Assert(not Assigned(sg_httpsrv_set_upld_dir));
  Assert(not Assigned(sg_httpsrv_upld_dir));
  Assert(not Assigned(sg_httpsrv_set_post_buf_size));
  Assert(not Assigned(sg_httpsrv_post_buf_size));
  Assert(not Assigned(sg_httpsrv_set_payld_limit));
  Assert(not Assigned(sg_httpsrv_payld_limit));
  Assert(not Assigned(sg_httpsrv_set_uplds_limit));
  Assert(not Assigned(sg_httpsrv_uplds_limit));
  Assert(not Assigned(sg_httpsrv_set_thr_pool_size));
  Assert(not Assigned(sg_httpsrv_thr_pool_size));
  Assert(not Assigned(sg_httpsrv_set_con_timeout));
  Assert(not Assigned(sg_httpsrv_con_timeout));
  Assert(not Assigned(sg_httpsrv_set_con_limit));
  Assert(not Assigned(sg_httpsrv_con_limit));

  Assert(not Assigned(sg_entrypoint_name));
  Assert(not Assigned(sg_entrypoint_set_user_data));
  Assert(not Assigned(sg_entrypoint_user_data));

  Assert(not Assigned(sg_entrypoints_new));
  Assert(not Assigned(sg_entrypoints_free));
  Assert(not Assigned(sg_entrypoints_add));
  Assert(not Assigned(sg_entrypoints_rm));
  Assert(not Assigned(sg_entrypoints_iter));
  Assert(not Assigned(sg_entrypoints_clear));
  Assert(not Assigned(sg_entrypoints_find));

  Assert(not Assigned(sg_route_handle));
  Assert(not Assigned(sg_route_match));
  Assert(not Assigned(sg_route_rawpattern));
  Assert(not Assigned(sg_route_pattern));
  Assert(not Assigned(sg_route_path));
  Assert(not Assigned(sg_route_segments_iter));
  Assert(not Assigned(sg_route_vars_iter));
  Assert(not Assigned(sg_route_user_data));

  Assert(not Assigned(sg_routes_add2));
  Assert(not Assigned(sg_routes_add));
  Assert(not Assigned(sg_routes_rm));
  Assert(not Assigned(sg_routes_iter));
  Assert(not Assigned(sg_routes_next));
  Assert(not Assigned(sg_routes_count));
  Assert(not Assigned(sg_routes_cleanup));

  Assert(not Assigned(sg_router_new));
  Assert(not Assigned(sg_router_free));
  Assert(not Assigned(sg_router_dispatch2));
  Assert(not Assigned(sg_router_dispatch));

  SgLib.Load(SG_LIB_NAME);

  Assert(Assigned(sg_version));
  Assert(Assigned(sg_version_str));

  Assert(Assigned(sg_malloc));
  Assert(Assigned(sg_alloc));
  Assert(Assigned(sg_realloc));
  Assert(Assigned(sg_free));
  Assert(Assigned(sg_strerror));
  Assert(Assigned(sg_is_post));
  Assert(Assigned(sg_extract_entrypoint));
  Assert(Assigned(sg_tmpdir));
  Assert(Assigned(sg_eor));

  Assert(Assigned(sg_str_new));
  Assert(Assigned(sg_str_free));
  Assert(Assigned(sg_str_write));
  Assert(Assigned(sg_str_printf_va));
  Assert(Assigned(sg_str_printf));
  Assert(Assigned(sg_str_content));
  Assert(Assigned(sg_str_length));
  Assert(Assigned(sg_str_clear));

  Assert(Assigned(sg_strmap_name));
  Assert(Assigned(sg_strmap_val));
  Assert(Assigned(sg_strmap_add));
  Assert(Assigned(sg_strmap_set));
  Assert(Assigned(sg_strmap_find));
  Assert(Assigned(sg_strmap_get));
  Assert(Assigned(sg_strmap_rm));
  Assert(Assigned(sg_strmap_iter));
  Assert(Assigned(sg_strmap_sort));
  Assert(Assigned(sg_strmap_count));
  Assert(Assigned(sg_strmap_next));
  Assert(Assigned(sg_strmap_cleanup));

  Assert(Assigned(sg_httpauth_set_realm));
  Assert(Assigned(sg_httpauth_realm));
  Assert(Assigned(sg_httpauth_deny));
  Assert(Assigned(sg_httpauth_cancel));
  Assert(Assigned(sg_httpauth_usr));
  Assert(Assigned(sg_httpauth_pwd));

  Assert(Assigned(sg_httpuplds_iter));
  Assert(Assigned(sg_httpuplds_next));
  Assert(Assigned(sg_httpuplds_count));

  Assert(Assigned(sg_httpupld_handle));
  Assert(Assigned(sg_httpupld_dir));
  Assert(Assigned(sg_httpupld_field));
  Assert(Assigned(sg_httpupld_name));
  Assert(Assigned(sg_httpupld_mime));
  Assert(Assigned(sg_httpupld_encoding));
  Assert(Assigned(sg_httpupld_size));
  Assert(Assigned(sg_httpupld_save));
  Assert(Assigned(sg_httpupld_save_as));

  Assert(Assigned(sg_httpreq_headers));
  Assert(Assigned(sg_httpreq_cookies));
  Assert(Assigned(sg_httpreq_params));
  Assert(Assigned(sg_httpreq_fields));
  Assert(Assigned(sg_httpreq_version));
  Assert(Assigned(sg_httpreq_method));
  Assert(Assigned(sg_httpreq_path));
  Assert(Assigned(sg_httpreq_payload));
  Assert(Assigned(sg_httpreq_is_uploading));
  Assert(Assigned(sg_httpreq_uploads));
  Assert(Assigned(sg_httpreq_tls_session));
  Assert(Assigned(sg_httpreq_set_user_data));
  Assert(Assigned(sg_httpreq_user_data));

  Assert(Assigned(sg_httpres_headers));
  Assert(Assigned(sg_httpres_set_cookie));
  Assert(Assigned(sg_httpres_sendbinary));
  Assert(Assigned(sg_httpres_sendfile2));
  Assert(Assigned(sg_httpres_sendfile));
  Assert(Assigned(sg_httpres_sendstream));
  Assert(Assigned(sg_httpres_zsendbinary2));
  Assert(Assigned(sg_httpres_zsendbinary));
  Assert(Assigned(sg_httpres_zsendstream));
  Assert(Assigned(sg_httpres_zsendstream2));
  Assert(Assigned(sg_httpres_zsendfile2));
  Assert(Assigned(sg_httpres_zsendfile));
  Assert(Assigned(sg_httpres_clear));

  Assert(Assigned(sg_httpsrv_new2));
  Assert(Assigned(sg_httpsrv_new));
  Assert(Assigned(sg_httpsrv_free));
  Assert(Assigned(sg_httpsrv_tls_listen2));
  Assert(Assigned(sg_httpsrv_tls_listen));
  Assert(Assigned(sg_httpsrv_listen));
  Assert(Assigned(sg_httpsrv_shutdown));
  Assert(Assigned(sg_httpsrv_port));
  Assert(Assigned(sg_httpsrv_is_threaded));
  Assert(Assigned(sg_httpsrv_set_upld_cbs));
  Assert(Assigned(sg_httpsrv_set_upld_dir));
  Assert(Assigned(sg_httpsrv_upld_dir));
  Assert(Assigned(sg_httpsrv_set_post_buf_size));
  Assert(Assigned(sg_httpsrv_post_buf_size));
  Assert(Assigned(sg_httpsrv_set_payld_limit));
  Assert(Assigned(sg_httpsrv_payld_limit));
  Assert(Assigned(sg_httpsrv_set_uplds_limit));
  Assert(Assigned(sg_httpsrv_uplds_limit));
  Assert(Assigned(sg_httpsrv_set_thr_pool_size));
  Assert(Assigned(sg_httpsrv_thr_pool_size));
  Assert(Assigned(sg_httpsrv_set_con_timeout));
  Assert(Assigned(sg_httpsrv_con_timeout));
  Assert(Assigned(sg_httpsrv_set_con_limit));
  Assert(Assigned(sg_httpsrv_con_limit));

  Assert(Assigned(sg_entrypoint_name));
  Assert(Assigned(sg_entrypoint_set_user_data));
  Assert(Assigned(sg_entrypoint_user_data));

  Assert(Assigned(sg_entrypoints_new));
  Assert(Assigned(sg_entrypoints_free));
  Assert(Assigned(sg_entrypoints_add));
  Assert(Assigned(sg_entrypoints_rm));
  Assert(Assigned(sg_entrypoints_iter));
  Assert(Assigned(sg_entrypoints_clear));
  Assert(Assigned(sg_entrypoints_find));

  Assert(Assigned(sg_route_handle));
  Assert(Assigned(sg_route_match));
  Assert(Assigned(sg_route_rawpattern));
  Assert(Assigned(sg_route_pattern));
  Assert(Assigned(sg_route_path));
  Assert(Assigned(sg_route_segments_iter));
  Assert(Assigned(sg_route_vars_iter));
  Assert(Assigned(sg_route_user_data));

  Assert(Assigned(sg_routes_add2));
  Assert(Assigned(sg_routes_add));
  Assert(Assigned(sg_routes_rm));
  Assert(Assigned(sg_routes_iter));
  Assert(Assigned(sg_routes_next));
  Assert(Assigned(sg_routes_count));
  Assert(Assigned(sg_routes_cleanup));

  Assert(Assigned(sg_router_new));
  Assert(Assigned(sg_router_free));
  Assert(Assigned(sg_router_dispatch2));
  Assert(Assigned(sg_router_dispatch));
end;

begin
  Test_SgLibAddNotifier;
  Test_SgLibRemoveNotifier;
  Test_SgLibClearNotifiers;
  Test_SgLibGetLastName;
  Test_SgLibCheckVersion;
  Test_SgLibCheckLastError;
  Test_SgLibLoad;
  Test_SgLibUnload;
  Test_SgLibIsLoaded;
  Test_SgLibCheck;
  Test_SgLibHandle;
  Test_SgLibBinding;
end.
