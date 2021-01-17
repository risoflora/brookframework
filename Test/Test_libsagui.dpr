(*                         _
 *   ___  __ _  __ _ _   _(_)
 *  / __|/ _` |/ _` | | | | |
 *  \__ \ (_| | (_| | |_| | |
 *  |___/\__,_|\__, |\__,_|_|
 *             |___/
 *
 * Cross-platform library which helps to develop web servers or frameworks.
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

program Test_libsagui;

{$I Tests.inc}

{.$DEFINE TLS_SUPPORT}

uses
  RTLConsts,
  SysUtils,
  Classes,
  Platform,
  libsagui,
  Test;

type
  TFakeObject = class
  public
    procedure DoAddUnloadEvent1(ASender: TObject);
    procedure DoAddUnloadEvent2(ASender: TObject);
    procedure DoAddUnloadEvent3(ASender: TObject);
  end;

procedure TFakeObject.DoAddUnloadEvent1(ASender: TObject);
begin
  TComponent(ASender).Name := 'abc';
end;

procedure TFakeObject.DoAddUnloadEvent2(ASender: TObject);
begin
  TComponent(ASender).Name := 'def';
end;

procedure TFakeObject.DoAddUnloadEvent3(ASender: TObject);
begin
  TComponent(ASender).Name := 'ghi';
end;

procedure DoAddUnloadEvent(const AArgs: array of const);
begin
  SgLib.UnloadEvents.Add(nil, AArgs[0].VObject);
end;

procedure Test_AddUnloadEvent;
var
  O: TFakeObject;
  C1, C2, C3: TComponent;
begin
  O := TFakeObject.Create;
  C1 := TComponent.Create(nil);
  C2 := TComponent.Create(nil);
  C3 := TComponent.Create(nil);
  try
    AssertExcept(DoAddUnloadEvent, EArgumentNilException,
      Format(SParamIsNil, ['AEvent']), [O]);

    SgLib.UnloadEvents.Clear;
    SgLib.UnloadEvents.Add(O.DoAddUnloadEvent1, nil);
    SgLib.UnloadEvents.Remove(O.DoAddUnloadEvent1);

    SgLib.Load(SG_LIB_NAME);
    SgLib.UnloadEvents.Add(O.DoAddUnloadEvent1, C1);
    SgLib.UnloadEvents.Add(O.DoAddUnloadEvent2, C2);
    SgLib.UnloadEvents.Add(O.DoAddUnloadEvent3, C3);
    Assert(C1.Name = '');
    Assert(C2.Name = '');
    Assert(C3.Name = '');
    SgLib.Unload;
    Assert(C1.Name = 'abc');
    Assert(C2.Name = 'def');
    Assert(C3.Name = 'ghi');
    SgLib.UnloadEvents.Clear;
  finally
    O.Free;
    C3.Free;
    C2.Free;
    C1.Free;
  end;
end;

procedure DoRemoveUnloadEvent;
begin
  SgLib.UnloadEvents.Remove(nil);
end;

procedure Test_RemoveNotifier;
var
  O: TFakeObject;
  C1, C2, C3: TComponent;
begin
  AssertExcept(DoRemoveUnloadEvent, EArgumentNilException,
    Format(SParamIsNil, ['AEvent']));

  O := TFakeObject.Create;
  C1 := TComponent.Create(nil);
  C2 := TComponent.Create(nil);
  C3 := TComponent.Create(nil);
  try
    SgLib.UnloadEvents.Clear;
    SgLib.Load(SG_LIB_NAME);
    SgLib.UnloadEvents.Add(O.DoAddUnloadEvent1, C1);
    SgLib.UnloadEvents.Add(O.DoAddUnloadEvent2, C2);
    SgLib.UnloadEvents.Add(O.DoAddUnloadEvent3, C3);
    SgLib.UnloadEvents.Remove(O.DoAddUnloadEvent2);
    SgLib.Unload;
    Assert(C1.Name = 'abc');
    Assert(C2.Name = '');
    Assert(C3.Name = 'ghi');
    SgLib.UnloadEvents.Clear;
  finally
    C3.Free;
    C2.Free;
    C1.Free;
    O.Free;
  end;
end;

procedure Test_GetLastName;
begin
  SgLib.Unload;
  Assert(SgLib.GetLastName = '');
  SgLib.Load(SG_LIB_NAME);
  Assert(SgLib.GetLastName = SG_LIB_NAME);
end;

procedure DoSgLibCheckVersion1;
begin
  SgLib.CheckVersion((Pred(SG_VERSION_MAJOR) shl 16) or
    (SG_VERSION_MINOR shl 8) or SG_VERSION_PATCH);
end;

procedure DoSgLibCheckVersion2;
begin
  SgLib.CheckVersion((Succ(SG_VERSION_MAJOR) shl 16) or
    (SG_VERSION_MINOR shl 8) or SG_VERSION_PATCH);
end;

procedure DoSgLibCheckVersion3;
begin
  SgLib.CheckVersion((SG_VERSION_MAJOR shl 16) or
    (Pred(SG_VERSION_MINOR) shl 8) or SG_VERSION_PATCH);
end;

procedure DoSgLibCheckVersion4;
begin
  SgLib.CheckVersion((SG_VERSION_MAJOR shl 16) or
    (SG_VERSION_MINOR shl 8) or Pred(SG_VERSION_PATCH));
end;

procedure Test_CheckVersion;
begin
  SgLib.Unload;
  SgLib.Load(SG_LIB_NAME);

  AssertExcept(DoSgLibCheckVersion1, EInvalidOpException,
    Format(SSgLibVersion, [SG_VERSION_MAJOR, SG_VERSION_MINOR, SG_VERSION_PATCH]));
  SgLib.Load(SG_LIB_NAME);
  AssertExcept(DoSgLibCheckVersion2, EInvalidOpException,
    Format(SSgLibVersion, [SG_VERSION_MAJOR, SG_VERSION_MINOR, SG_VERSION_PATCH]));
  SgLib.Load(SG_LIB_NAME);
  AssertExcept(DoSgLibCheckVersion3, EInvalidOpException,
    Format(SSgLibVersion, [SG_VERSION_MAJOR, SG_VERSION_MINOR, SG_VERSION_PATCH]));
  SgLib.Load(SG_LIB_NAME);
  SgLib.CheckVersion((SG_VERSION_MAJOR shl 16) or
    (Succ(SG_VERSION_MINOR) shl 8) or SG_VERSION_PATCH);

  AssertExcept(DoSgLibCheckVersion4, EInvalidOpException,
    Format(SSgLibVersion, [SG_VERSION_MAJOR, SG_VERSION_MINOR, SG_VERSION_PATCH]));
  SgLib.Load(SG_LIB_NAME);
  SgLib.CheckVersion((SG_VERSION_MAJOR shl 16) or
    (Succ(SG_VERSION_MINOR) shl 8) or 0);

  SgLib.CheckVersion;
end;

procedure DoSgLibCheckLastError1;
begin
  SgLib.CheckLastError(EINVAL);
end;

procedure DoSgLibCheckLastError2;
begin
  SgLib.CheckLastError(456);
end;

procedure Test_CheckLastError;

  function strerror(ALastError: Integer): string;
  var
    P: array[0..SG_ERR_SIZE-1] of cchar;
  begin
    sg_strerror(ALastError, @P[0], SG_ERR_SIZE);
  {$IFDEF FPC}
    SetString(Result, @P[0], Length(Pcchar(@P[0])));
    SetCodePage(RawByteString(Result), CP_UTF8, False);
  {$ELSE}
    Result := TMarshal.ReadStringAsUtf8(TPtrWrapper.Create(@P[0]));
  {$ENDIF}
  end;

var
  F: Pointer;
begin
  SgLib.CheckLastError(0);
  F := @sg_strerror;
  sg_strerror := nil;
  SgLib.CheckLastError(123);
  sg_strerror := F;

  AssertOSExcept(DoSgLibCheckLastError1, strerror(EINVAL), EINVAL);
  AssertOSExcept(DoSgLibCheckLastError2, strerror(456), 456);
end;

procedure DoSgLibLoad1;
begin
  Assert(SgLib.Load('') = NilHandle);
end;

procedure DoSgLibLoad2;
begin
  Assert(SgLib.Load('abc') = NilHandle);
end;

procedure Test_Load;
begin
  SgLib.Unload;
  AssertExcept(DoSgLibLoad1, EArgumentException, SSgLibEmptyName);
  AssertExcept(DoSgLibLoad2, ESgLibNotLoaded, Format(SSgLibNotLoaded, ['abc']));

  Assert(SgLib.Load(SG_LIB_NAME) <> NilHandle);
end;

procedure Test_Unload;
begin
  Assert(SgLib.Load(SG_LIB_NAME) <> NilHandle);
  Assert(SgLib.Unload = NilHandle);
end;

procedure Test_IsLoaded;
begin
  SgLib.Unload;
  Assert(not SgLib.IsLoaded);
  SgLib.Load(SG_LIB_NAME);
  Assert(SgLib.IsLoaded);
end;

procedure DoSgLibCheck;
begin
  SgLib.Unload;
  SgLib.Check;
end;

procedure Test_Check;
begin
  AssertExcept(DoSgLibCheck, ESgLibNotLoaded,
    Format(SSgLibNotLoaded, [SG_LIB_NAME]));
end;

procedure Test_Handle;
begin
  SgLib.Unload;
  Assert(SgLib.Handle = NilHandle);
  SgLib.Load(SG_LIB_NAME);
  Assert(SgLib.Handle <> NilHandle);
end;

procedure Test_Binding;
begin
  SgLib.Unload;

  Assert(not Assigned(sg_version));
  Assert(not Assigned(sg_version_str));

  Assert(not Assigned(sg_mm_set));
  Assert(not Assigned(sg_malloc));
  Assert(not Assigned(sg_alloc));
  Assert(not Assigned(sg_realloc));
  Assert(not Assigned(sg_free));
  Assert(not Assigned(sg_math_set));
  Assert(not Assigned(sg_strerror));
  Assert(not Assigned(sg_is_post));
  Assert(not Assigned(sg_extract_entrypoint));
  Assert(not Assigned(sg_tmpdir));
  Assert(not Assigned(sg_eor));
  Assert(not Assigned(sg_ip));

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
  Assert(not Assigned(sg_httpauth_deny2));
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

  Assert(not Assigned(sg_httpreq_srv));
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
  Assert(not Assigned(sg_httpreq_client));
{$IFDEF TLS_SUPPORT}
  Assert(not Assigned(sg_httpreq_tls_session));
{$ENDIF}
  Assert(not Assigned(sg_httpreq_isolate));
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
  Assert(not Assigned(sg_httpres_zsendstream2));
  Assert(not Assigned(sg_httpres_zsendstream));
  Assert(not Assigned(sg_httpres_zsendfile2));
  Assert(not Assigned(sg_httpres_zsendfile));
  Assert(not Assigned(sg_httpres_clear));
  Assert(not Assigned(sg_httpres_is_empty));

  Assert(not Assigned(sg_httpsrv_new2));
  Assert(not Assigned(sg_httpsrv_new));
  Assert(not Assigned(sg_httpsrv_free));
{$IFDEF TLS_SUPPORT}
  Assert(not Assigned(sg_httpsrv_tls_listen2));
  Assert(not Assigned(sg_httpsrv_tls_listen));
{$ENDIF}
  Assert(not Assigned(sg_httpsrv_listen));
  Assert(not Assigned(sg_httpsrv_shutdown));
  Assert(not Assigned(sg_httpsrv_port));
  Assert(not Assigned(sg_httpsrv_is_threaded));
  Assert(not Assigned(sg_httpsrv_set_cli_cb));
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
  Assert(not Assigned(sg_httpsrv_handle));

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

  Assert(not Assigned(sg_expr_new));
  Assert(not Assigned(sg_expr_free));
  Assert(not Assigned(sg_expr_compile));
  Assert(not Assigned(sg_expr_clear));
  Assert(not Assigned(sg_expr_eval));
  Assert(not Assigned(sg_expr_var));
  Assert(not Assigned(sg_expr_set_var));
  Assert(not Assigned(sg_expr_arg));
  Assert(not Assigned(sg_expr_near));
  Assert(not Assigned(sg_expr_err));
  Assert(not Assigned(sg_expr_strerror));
  Assert(not Assigned(sg_expr_calc));

  SgLib.Load(SG_LIB_NAME);

  Assert(Assigned(sg_version));
  Assert(Assigned(sg_version_str));

  Assert(Assigned(sg_mm_set));
  Assert(Assigned(sg_malloc));
  Assert(Assigned(sg_alloc));
  Assert(Assigned(sg_realloc));
  Assert(Assigned(sg_free));
  Assert(Assigned(sg_math_set));
  Assert(Assigned(sg_strerror));
  Assert(Assigned(sg_is_post));
  Assert(Assigned(sg_extract_entrypoint));
  Assert(Assigned(sg_tmpdir));
  Assert(Assigned(sg_eor));
  Assert(Assigned(sg_ip));

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
  Assert(Assigned(sg_httpauth_deny2));
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

  Assert(Assigned(sg_httpreq_srv));
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
  Assert(Assigned(sg_httpreq_client));
{$IFDEF TLS_SUPPORT}
  Assert(Assigned(sg_httpreq_tls_session));
{$ENDIF}
  Assert(Assigned(sg_httpreq_isolate));
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
  Assert(Assigned(sg_httpres_zsendstream2));
  Assert(Assigned(sg_httpres_zsendstream));
  Assert(Assigned(sg_httpres_zsendfile2));
  Assert(Assigned(sg_httpres_zsendfile));
  Assert(Assigned(sg_httpres_clear));
  Assert(Assigned(sg_httpres_is_empty));

  Assert(Assigned(sg_httpsrv_new2));
  Assert(Assigned(sg_httpsrv_new));
  Assert(Assigned(sg_httpsrv_free));
{$IFDEF TLS_SUPPORT}
  Assert(Assigned(sg_httpsrv_tls_listen2));
  Assert(Assigned(sg_httpsrv_tls_listen));
{$ENDIF}
  Assert(Assigned(sg_httpsrv_listen));
  Assert(Assigned(sg_httpsrv_shutdown));
  Assert(Assigned(sg_httpsrv_port));
  Assert(Assigned(sg_httpsrv_is_threaded));
  Assert(Assigned(sg_httpsrv_set_cli_cb));
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
  Assert(Assigned(sg_httpsrv_handle));

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

  Assert(Assigned(sg_expr_new));
  Assert(Assigned(sg_expr_free));
  Assert(Assigned(sg_expr_compile));
  Assert(Assigned(sg_expr_clear));
  Assert(Assigned(sg_expr_eval));
  Assert(Assigned(sg_expr_var));
  Assert(Assigned(sg_expr_set_var));
  Assert(Assigned(sg_expr_arg));
  Assert(Assigned(sg_expr_near));
  Assert(Assigned(sg_expr_err));
  Assert(Assigned(sg_expr_strerror));
  Assert(Assigned(sg_expr_calc));
end;

begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Test_AddUnloadEvent;
  Test_RemoveNotifier;
  Test_GetLastName;
  Test_CheckVersion;
  Test_CheckLastError;
  Test_Load;
  Test_Unload;
  Test_IsLoaded;
  Test_Check;
  Test_Handle;
  Test_Binding;
end.
