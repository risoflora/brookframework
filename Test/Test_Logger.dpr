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

program Test_Logger;

{$I Tests.inc}

uses
  RTLConsts,
  SysUtils,
  Classes,
{$IFNDEF FPC}
  IOUtils,
{$ENDIF}
  BrookLibraryLoader,
  BrookLogger,
  Test;

const
  FAKE_LOG_FILE_NAME = 'FakeOutput.log';
  FAKE_ERR_LOG_FILE_NAME = 'FakeErrOutput.log';

type
  TFakeLoggerOutput = class(TBrookLoggerOutput)
  public
    FakeLevel: string;
    FakeMessage: string;
    class function GetName: string; override;
    procedure Log(const ALevel, AMessage: string); override;
    procedure Fail(const ALevel: string; AException: Exception); override;
  end;

  TFakeLoggerOutputFile = class(TBrookLoggerOutputFile)
  public
    property LastDate;
  end;

class function TFakeLoggerOutput.GetName: string;
begin
  Result := 'FakeOutput';
end;

procedure TFakeLoggerOutput.Log(const ALevel, AMessage: string);
begin
  Assert(not ALevel.IsEmpty);
  Assert(not AMessage.IsEmpty);
  FakeLevel := ALevel;
  FakeMessage := AMessage;
end;

procedure TFakeLoggerOutput.Fail(const ALevel: string; AException: Exception);
begin
  Assert(not ALevel.IsEmpty);
  Assert(Assigned(AException));
  FakeLevel := ALevel;
  FakeMessage := AException.Message;
end;

var
  FakeComponent: TComponent;
  FakeFilters, FakeOptions: TStringList;

procedure DoLoggerOutputCreateParamIsNilFilters;
begin
  TFakeLoggerOutput.Create(nil, FakeOptions);
end;

procedure DoLoggerOutputCreateParamIsNilOptions;
begin
  TFakeLoggerOutput.Create(FakeFilters, nil);
end;

procedure Test_LoggerOutputCreate;
var
  O: TBrookLoggerOutput;
begin
  FakeFilters.Clear;
  FakeOptions.Clear;
  O := TFakeLoggerOutput.Create(FakeFilters, FakeOptions);
  try
    Assert(O.Filters = FakeFilters);
    Assert(O.Options = FakeOptions);

    AssertExcept(DoLoggerOutputCreateParamIsNilFilters, EArgumentNilException,
      Format(SParamIsNil, ['AFilters']));
    AssertExcept(DoLoggerOutputCreateParamIsNilOptions, EArgumentNilException,
      Format(SParamIsNil, ['AOptions']));
  finally
    O.Free;
  end;
end;

procedure Test_LoggerOutputGetRegisterAlias;
begin
  Assert(TFakeLoggerOutput.GetRegisterAlias = 'BrookLogger_FakeOutput');
end;

procedure Test_LoggerOutputGetName;
begin
  Assert(TFakeLoggerOutput.GetName = 'FakeOutput');
end;

procedure Test_LoggerOutputIsFiltered;
var
  O: TBrookLoggerOutput;
begin
  FakeFilters.Clear;
  FakeOptions.Clear;
  O := TFakeLoggerOutput.Create(FakeFilters, FakeOptions);
  try
    Assert(not O.IsFiltered(''));
    Assert(not O.IsFiltered('abc'));
    FakeFilters.Add('');
    FakeFilters.Add('abc');
    FakeFilters.Add('def');
    FakeFilters.Add('ghi');
    Assert(O.IsFiltered(''));
    Assert(O.IsFiltered('def'));
  finally
    O.Free;
  end;
end;

procedure Test_LoggerOutputLog;
var
  O: TFakeLoggerOutput;
begin
  FakeFilters.Clear;
  FakeOptions.Clear;
  O := TFakeLoggerOutput.Create(FakeFilters, FakeOptions);
  try
    O.FakeLevel := '';
    O.FakeMessage := '';
    Assert(O.FakeLevel = '');
    Assert(O.FakeMessage = '');
    O.Log('foo', 'bar');
    Assert(O.FakeLevel = 'foo');
    Assert(O.FakeMessage = 'bar');
  finally
    O.Free;
  end;
end;

procedure Test_LoggerOutputFail;
var
  O: TFakeLoggerOutput;
  E: Exception;
begin
  FakeFilters.Clear;
  FakeOptions.Clear;
  O := TFakeLoggerOutput.Create(FakeFilters, FakeOptions);
  E := Exception.Create('bar');
  try
    O.FakeLevel := '';
    O.FakeMessage := '';
    Assert(O.FakeLevel = '');
    Assert(O.FakeMessage = '');
    O.Fail('foo', E);
    Assert(O.FakeLevel = 'foo');
    Assert(O.FakeMessage = 'bar');
  finally
    E.Free;
    O.Free;
  end;
end;

procedure Test_LoggerOutputFilters;
var
  O: TBrookLoggerOutput;
begin
  O := TFakeLoggerOutput.Create(FakeFilters, FakeOptions);
  try
    Assert(O.Filters = FakeFilters);
  finally
    O.Free;
  end;
end;

procedure Test_LoggerOutputOptions;
var
  O: TBrookLoggerOutput;
begin
  O := TFakeLoggerOutput.Create(FakeFilters, FakeOptions);
  try
    Assert(O.Options = FakeOptions);
  finally
    O.Free;
  end;
end;

procedure Test_LoggerOutputConsoleGetName;
begin
  Assert(TBrookLoggerOutputConsole.GetName = 'Console');
end;

procedure Test_LoggerOutputConsoleLog;
var
  O: TBrookLoggerOutput;
  S: string;
begin
  FakeFilters.Clear;
  FakeOptions.Clear;
  FakeFilters.Add('test');
  O := TBrookLoggerOutputConsole.Create(FakeFilters, FakeOptions);
  try
    DeleteFile(FAKE_LOG_FILE_NAME);
    AssignFile(Output, FAKE_LOG_FILE_NAME);
    Rewrite(Output);
    O.Log('foo', 'bar');
    O.Log('bar', 'foo');
    O.Log('test', 'ok');
    Flush(Output);
    CloseFile(Output);

    Reset(Output);
    S := '';
    ReadLn(Output, S);
    Assert(S.SubString(24) = 'foo: bar');
    S := '';
    ReadLn(Output, S);
    Assert(S.SubString(24) = 'bar: foo');
    S := '';
    ReadLn(Output, S);
    Assert(S = '');
    CloseFile(Output);
  finally
    O.Free;
  end;
end;

procedure Test_LoggerOutputConsoleFail;
var
  O: TBrookLoggerOutput;
  E: Exception;
  S: string;
begin
  FakeFilters.Clear;
  FakeOptions.Clear;
  O := TBrookLoggerOutputConsole.Create(FakeFilters, FakeOptions);
  E := Exception.Create('bar');
  try
    DeleteFile(FAKE_ERR_LOG_FILE_NAME);
    AssignFile(ErrOutput, FAKE_ERR_LOG_FILE_NAME);
    Rewrite(ErrOutput);
    E.Message := 'bar';
    O.Fail('foo', E);
    E.Message := 'foo';
    O.Fail('bar', E);
    Flush(ErrOutput);
    CloseFile(ErrOutput);

    Reset(ErrOutput);
    S := '';
    ReadLn(ErrOutput, S);
    Assert(S.SubString(24) = 'foo: Exception: bar');
    S := '';
    ReadLn(ErrOutput, S);
    Assert(S.SubString(24) = 'bar: Exception: foo');
    S := '';
    ReadLn(ErrOutput, S);
    Assert(S = '');
    CloseFile(ErrOutput);
  finally
    E.Free;
    O.Free;
  end;
end;

procedure Test_LoggerOutputFileAfterConstruction;
var
  O: TBrookLoggerOutputFile;
begin
  FakeOptions.Clear;
  FakeOptions.Clear;
  O := TBrookLoggerOutputFile.Create(FakeFilters, FakeOptions);
  try
    Assert(O.Directory = {$IFDEF FPC}GetUserDir{$ELSE}TPath.GetHomePath{$ENDIF});
  finally
    O.Free;
  end;
  FakeOptions.Clear;
  FakeOptions.Clear;
  FakeOptions.Add('directory=foobar');
  O := TBrookLoggerOutputFile.Create(FakeFilters, FakeOptions);
  try
    Assert(O.Directory = 'foobar');
  finally
    O.Free;
  end;
end;

procedure Test_LoggerOutputFileGetName;
begin
  Assert(TBrookLoggerOutputFile.GetName = 'File');
end;

procedure Test_LoggerOutputFileLog;
var
  O: TBrookLoggerOutputFile;
  F: TextFile;
  FN: TFileName;
  S: string;
begin
  FakeOptions.Clear;
  FakeOptions.Clear;
  FakeFilters.Add('test');
  O := TBrookLoggerOutputFile.Create(FakeFilters, FakeOptions);
  try
    FN := O.FileName;
    DeleteFile(FN);
    O.Log('foo', 'bar');
    O.Log('bar', 'foo');
  finally
    O.Free;
  end;
  AssignFile(F, FN);
  Reset(F);
  S := '';
  ReadLn(F, S);
  Assert(S.SubString(24) = 'foo: bar');
  S := '';
  ReadLn(F, S);
  Assert(S.SubString(24) = 'bar: foo');
  S := '';
  ReadLn(F, S);
  Assert(S = '');
  CloseFile(F);
end;

procedure Test_LoggerOutputFileFail;
var
  O: TBrookLoggerOutputFile;
  E: Exception;
  F: TextFile;
  FN: TFileName;
  S: string;
begin
  FakeOptions.Clear;
  FakeOptions.Clear;
  O := TBrookLoggerOutputFile.Create(FakeFilters, FakeOptions);
  E := Exception.Create('bar');
  try
    FN := O.FileName;
    DeleteFile(FN);
    E.Message := 'bar';
    O.Fail('foo', E);
    E.Message := 'foo';
    O.Fail('bar', E);
  finally
    E.Free;
    O.Free;
  end;
  AssignFile(F, FN);
  Reset(F);
  S := '';
  ReadLn(F, S);
  Assert(S.SubString(24) = 'foo: Exception: bar');
  S := '';
  ReadLn(F, S);
  Assert(S.SubString(24) = 'bar: Exception: foo');
  CloseFile(F);
end;

procedure Test_LoggerOutputFileDirectory;
var
  O: TFakeLoggerOutputFile;
begin
  FakeFilters.Clear;
  FakeOptions.Clear;
  O := TFakeLoggerOutputFile.Create(FakeFilters, FakeOptions);
  try
    O.Log('foo', 'bar');
    Assert(O.LastDate > 0);
    O.Directory := 'foo';
    Assert(O.Directory = 'foo');
    Assert(O.LastDate = 0);
  finally
    O.Free;
  end;
end;

procedure Test_LoggerOutputFileFileName;
var
  O: TFakeLoggerOutputFile;
begin
  FakeFilters.Clear;
  FakeOptions.Clear;
  O := TFakeLoggerOutputFile.Create(FakeFilters, FakeOptions);
  try
    Assert(O.FileName = Concat(IncludeTrailingPathDelimiter(O.Directory),
      ChangeFileExt(ExtractFileName(ParamStr(0)), ''), '_',
        FormatDateTime('yyyymmdd', Date), '.log'));
  finally
    O.Free;
  end;
end;

procedure Test_LoggerLevelsCreate;
var
  L: TBrookLoggerLevels;
begin
  L := TBrookLoggerLevels.Create;
  try
    Assert(L.Info = SBrookLevelInfo);
    Assert(L.Hint = SBrookLevelHint);
    Assert(L.Warn = SBrookLevelWarn);
    Assert(L.Debug = SBrookLevelDebug);
    Assert(L.Error = SBrookLevelError);
  finally
    L.Free;
  end;
end;

procedure Test_LoggerLevelsAssign;
var
  S, D: TBrookLoggerLevels;
begin
  S := TBrookLoggerLevels.Create;
  D := TBrookLoggerLevels.Create;
  try
    Assert(D.Info = SBrookLevelInfo);
    Assert(D.Hint = SBrookLevelHint);
    Assert(D.Warn = SBrookLevelWarn);
    Assert(D.Debug = SBrookLevelDebug);
    Assert(D.Error = SBrookLevelError);
    S.Info := 'FakeInfo';
    S.Hint := 'FakeHint';
    S.Warn := 'FakeWarn';
    S.Debug := 'FakeDebug';
    S.Error := 'FakeError';
    D.Assign(S);
    Assert(D.Info = 'FakeInfo');
    Assert(D.Hint = 'FakeHint');
    Assert(D.Warn = 'FakeWarn');
    Assert(D.Debug = 'FakeDebug');
    Assert(D.Error = 'FakeError');
  finally
    S.Free;
    D.Free;
  end;
end;

procedure Test_LoggerCreate;
var
  L: TBrookLogger;
begin
  L := TBrookLogger.Create(nil);
  try
    Assert(not Assigned(L.Owner));
  finally
    L.Free;
  end;
  L := TBrookLogger.Create(FakeComponent);
  try
    Assert(L.Owner = FakeComponent);
    Assert(Assigned(L.Levels));
    Assert(Assigned(L.Filters));
    Assert(Assigned(L.Options));
    Assert(L.OutputName = BROOK_LOGGER_OUTPUT_NAME);
  finally
    L.Free;
  end;
end;

procedure DoLoggerGetOutputClassUnknownOutputName(const AArgs: array of const);
begin
  TBrookLogger(AArgs[0].VObject).GetOutputClass;
end;

procedure DoLoggerGetOutputClassInvalidOutputClass(const AArgs: array of const);
begin
  TBrookLogger(AArgs[0].VObject).GetOutputClass;
end;

procedure Test_LoggerGetOutputClass;
var
  L: TBrookLogger;
begin
  L := TBrookLogger.Create(nil);
  try
    Assert(L.GetOutputClass = TBrookLoggerOutputConsole);

    L.OutputName := 'test';
    AssertExcept(DoLoggerGetOutputClassUnknownOutputName, EClassNotFound,
      Format(SBrookUnknownOutputName, ['test']), [L]);

    RegisterClassAlias(TBrookLoggerLevels, Concat(BROOK_LOGGER_TAG, 'test'));
    AssertExcept(DoLoggerGetOutputClassInvalidOutputClass, EInvalidCast,
      Format(SBrookInvalidOutputClass, [TBrookLoggerLevels.ClassName]), [L]);
  finally
    L.Free;
  end;
end;

procedure Test_LoggerOpen;
var
  L: TBrookLogger;
begin
  L := TBrookLogger.Create(nil);
  try
    Assert(not L.Active);
    L.Open;
    Assert(L.Active);
    L.Open;
  finally
    L.Free;
  end;
end;

procedure Test_LoggerClose;
var
  L: TBrookLogger;
begin
  L := TBrookLogger.Create(nil);
  try
    L.Open;
    Assert(L.Active);
    L.Close;
    Assert(not L.Active);
    L.Close;
  finally
    L.Free;
  end;
end;

procedure Test_LoggerLog;
var
  L: TBrookLogger;
  S: string;
begin
  L := TBrookLogger.Create(nil);
  try
    DeleteFile(FAKE_LOG_FILE_NAME);
    AssignFile(Output, FAKE_LOG_FILE_NAME);
    Rewrite(Output);
    L.Log('test', 'ok');
    L.Open;
    L.Filters.Add('test');
    L.Log('foo', 'bar');
    L.Log('bar', 'foo');
    L.Log('test', 'ok');
    Flush(Output);
    CloseFile(Output);

    Reset(Output);
    S := '';
    ReadLn(Output, S);
    Assert(S.SubString(24) = 'foo: bar');
    S := '';
    ReadLn(Output, S);
    Assert(S.SubString(24) = 'bar: foo');
    S := '';
    ReadLn(Output, S);
    Assert(S = '');
    CloseFile(Output);
  finally
    L.Free;
  end;
end;

procedure Test_LoggerFail;
var
  L: TBrookLogger;
  E: Exception;
  S: string;
begin
  FakeFilters.Clear;
  FakeOptions.Clear;
  L := TBrookLogger.Create(nil);
  E := Exception.Create('bar');
  try
    DeleteFile(FAKE_ERR_LOG_FILE_NAME);
    AssignFile(ErrOutput, FAKE_ERR_LOG_FILE_NAME);
    Rewrite(ErrOutput);
    E.Message := 'ok';
    L.Fail('test', E);
    L.Open;
    E.Message := 'bar';
    L.Fail('foo', E);
    E.Message := 'foo';
    L.Fail('bar', E);
    Flush(ErrOutput);
    CloseFile(ErrOutput);

    Reset(ErrOutput);
    S := '';
    ReadLn(ErrOutput, S);
    Assert(S.SubString(24) = 'foo: Exception: bar');
    S := '';
    ReadLn(ErrOutput, S);
    Assert(S.SubString(24) = 'bar: Exception: foo');
    S := '';
    ReadLn(ErrOutput, S);
    Assert(S = '');
    CloseFile(ErrOutput);
  finally
    E.Free;
    L.Free;
  end;
end;

procedure Test_LoggerInfo;
var
  L: TBrookLogger;
  S: string;
begin
  L := TBrookLogger.Create(nil);
  try
    L.Open;
    DeleteFile(FAKE_LOG_FILE_NAME);
    AssignFile(Output, FAKE_LOG_FILE_NAME);
    Rewrite(Output);
    L.Open;
    L.Info('test');
    L.Filters.Clear;
    L.Close;
    L.Info('test');
    L.Open;
    L.Info('foo');
    L.Info('bar');
    L.Filters.Add(SBrookLevelInfo);
    L.Info('test');
    Flush(Output);
    CloseFile(Output);

    Reset(Output);
    S := '';
    ReadLn(Output, S);
    Assert(S.SubString(24) = Concat(SBrookLevelInfo, ': foo'));
    S := '';
    ReadLn(Output, S);
    Assert(S.SubString(24) = Concat(SBrookLevelInfo, ': bar'));
    S := '';
    ReadLn(Output, S);
    Assert(S = '');
    CloseFile(Output);
  finally
    L.Free;
  end;
end;

procedure Test_LoggerHint;
var
  L: TBrookLogger;
  S: string;
begin
  L := TBrookLogger.Create(nil);
  try
    L.Open;
    DeleteFile(FAKE_LOG_FILE_NAME);
    AssignFile(Output, FAKE_LOG_FILE_NAME);
    Rewrite(Output);
    L.Hint('test');
    L.Filters.Clear;
    L.Close;
    L.Hint('test');
    L.Open;
    L.Hint('foo');
    L.Hint('bar');
    L.Filters.Add(SBrookLevelHint);
    L.Hint('test');
    Flush(Output);
    CloseFile(Output);

    Reset(Output);
    S := '';
    ReadLn(Output, S);
    Assert(S.SubString(24) = Concat(SBrookLevelHint, ': foo'));
    S := '';
    ReadLn(Output, S);
    Assert(S.SubString(24) = Concat(SBrookLevelHint, ': bar'));
    S := '';
    ReadLn(Output, S);
    Assert(S = '');
    CloseFile(Output);
  finally
    L.Free;
  end;
end;

procedure Test_LoggerWarn;
var
  L: TBrookLogger;
  S: string;
begin
  L := TBrookLogger.Create(nil);
  try
    L.Open;
    DeleteFile(FAKE_LOG_FILE_NAME);
    AssignFile(Output, FAKE_LOG_FILE_NAME);
    Rewrite(Output);
    L.Filters.Clear;
    L.Close;
    L.Warn('test');
    L.Open;
    L.Warn('foo');
    L.Warn('bar');
    L.Filters.Add(SBrookLevelWarn);
    L.Warn('test');
    Flush(Output);
    CloseFile(Output);

    Reset(Output);
    S := '';
    ReadLn(Output, S);
    Assert(S.SubString(24) = Concat(SBrookLevelWarn, ': foo'));
    S := '';
    ReadLn(Output, S);
    Assert(S.SubString(24) = Concat(SBrookLevelWarn, ': bar'));
    S := '';
    ReadLn(Output, S);
    Assert(S = '');
    CloseFile(Output);
  finally
    L.Free;
  end;
end;

procedure Test_LoggerDebug;
var
  L: TBrookLogger;
  S: string;
begin
  L := TBrookLogger.Create(nil);
  try
    L.Open;
    DeleteFile(FAKE_LOG_FILE_NAME);
    AssignFile(Output, FAKE_LOG_FILE_NAME);
    Rewrite(Output);
    L.Debug('test');
    L.Filters.Clear;
    L.Close;
    L.Debug('test');
    L.Open;
    L.Debug('foo');
    L.Debug('bar');
    L.Filters.Add(SBrookLevelDebug);
    L.Debug('test');
    Flush(Output);
    CloseFile(Output);

    Reset(Output);
    S := '';
    ReadLn(Output, S);
    Assert(S.SubString(24) = Concat(SBrookLevelDebug, ': foo'));
    S := '';
    ReadLn(Output, S);
    Assert(S.SubString(24) = Concat(SBrookLevelDebug, ': bar'));
    S := '';
    ReadLn(Output, S);
    Assert(S = '');
    CloseFile(Output);
  finally
    L.Free;
  end;
end;

procedure Test_LoggerError;
var
  L: TBrookLogger;
  E1, E2, E3: Exception;
  S: string;
begin
  L := TBrookLogger.Create(nil);
  E1 := Exception.Create('test');
  E2 := Exception.Create('foo');
  E3 := Exception.Create('bar');
  try
    L.Open;
    DeleteFile(FAKE_ERR_LOG_FILE_NAME);
    AssignFile(ErrOutput, FAKE_ERR_LOG_FILE_NAME);
    Rewrite(ErrOutput);
    L.Filters.Clear;
    L.Close;
    L.Error(E1);
    L.Open;
    L.Error(E2);
    L.Error(E3);
    L.Filters.Add(SBrookLevelError);
    L.Error(E1);
    Flush(ErrOutput);
    CloseFile(ErrOutput);

    Reset(ErrOutput);
    S := '';
    ReadLn(ErrOutput, S);
    Assert(S.SubString(24) = Concat(SBrookLevelError, ': Exception: foo'));
    S := '';
    ReadLn(ErrOutput, S);
    Assert(S.SubString(24) = Concat(SBrookLevelError, ': Exception: bar'));
    S := '';
    ReadLn(ErrOutput, S);
    Assert(S = '');
    CloseFile(ErrOutput);
  finally
    E3.Free;
    E2.Free;
    E1.Free;
    L.Free;
  end;
end;

procedure DoLoggerOutputInactiveOutput(const AArgs: array of const);
begin
  TBrookLogger(AArgs[0].VObject).Output;
end;

procedure Test_LoggerOutput;
var
  L: TBrookLogger;
begin
  L := TBrookLogger.Create(nil);
  try
    L.Open;
    Assert(Assigned(L.Output));

    L.Close;
    AssertExcept(DoLoggerOutputInactiveOutput, EInvalidOpException,
      SBrookInactiveOutput, [L]);
  finally
    L.Free;
  end;
end;

procedure Test_LoggerActive;
var
  L: TBrookLogger;
begin
  L := TBrookLogger.Create(nil);
  try
    Assert(not L.Active);
    L.Active := not L.Active;
    Assert(L.Active);
  finally
    L.Free;
  end;
end;

procedure Test_LoggerLevels;
var
  L: TBrookLogger;
begin
  L := TBrookLogger.Create(nil);
  try
    Assert(Assigned(L.Levels));
  finally
    L.Free;
  end;
end;

procedure Test_LoggerOutputName;
var
  L: TBrookLogger;
begin
  L := TBrookLogger.Create(nil);
  try
    Assert(L.OutputName = 'Console');
    L.OutputName := 'File';
    Assert(L.OutputName = 'File');
    L.OutputName := '';
    Assert(L.OutputName = 'Console');
  finally
    L.Free;
  end;
end;

procedure Test_LoggerFilters;
var
  L: TBrookLogger;
begin
  L := TBrookLogger.Create(nil);
  try
    Assert(Assigned(L.Filters));
    Assert(L.Filters.Count = 3);
    Assert(L.Filters.IndexOf(SBrookLevelInfo) > -1);
    Assert(L.Filters.IndexOf(SBrookLevelHint) > -1);
    Assert(L.Filters.IndexOf(SBrookLevelDebug) > -1);
  finally
    L.Free;
  end;
end;

procedure Test_LoggerOptions;
var
  L: TBrookLogger;
begin
  L := TBrookLogger.Create(nil);
  try
    Assert(Assigned(L.Options));
    Assert(L.Options.Count = 0);
  finally
    L.Free;
  end;
end;

begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  TBrookLibraryLoader.Load;
  FakeComponent := TComponent.Create(nil);
  FakeFilters := TStringList.Create;
  FakeOptions := TStringList.Create;
  try
    Test_LoggerOutputCreate;
    Test_LoggerOutputGetRegisterAlias;
    Test_LoggerOutputGetName;
    Test_LoggerOutputIsFiltered;
    Test_LoggerOutputLog;
    Test_LoggerOutputFail;
    Test_LoggerOutputFilters;
    Test_LoggerOutputOptions;
    Test_LoggerOutputConsoleGetName;
    Test_LoggerOutputConsoleLog;
    Test_LoggerOutputConsoleFail;
    Test_LoggerOutputFileAfterConstruction;
    // Test_LoggerOutputFileDestroy - not required
    Test_LoggerOutputFileGetName;
    Test_LoggerOutputFileLog;
    Test_LoggerOutputFileFail;
    Test_LoggerOutputFileDirectory;
    Test_LoggerOutputFileFileName;
    Test_LoggerLevelsCreate;
    Test_LoggerLevelsAssign;
    // Test_LoggerDestroy - not required
    Test_LoggerCreate;
    Test_LoggerGetOutputClass;
    Test_LoggerOpen;
    Test_LoggerClose;
    Test_LoggerLog;
    Test_LoggerFail;
    Test_LoggerInfo;
    Test_LoggerHint;
    Test_LoggerWarn;
    Test_LoggerDebug;
    Test_LoggerError;
    Test_LoggerOutput;
    Test_LoggerActive;
    Test_LoggerLevels;
    Test_LoggerOutputName;
    Test_LoggerFilters;
    Test_LoggerOptions;
  finally
    FakeFilters.Free;
    FakeOptions.Free;
    FakeComponent.Free;
    TBrookLibraryLoader.Unload;
  end;
end.
