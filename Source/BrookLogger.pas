(*  _                     _
 * | |__  _ __ ___   ___ | | __
 * | '_ \| '__/ _ \ / _ \| |/ /
 * | |_) | | | (_) | (_) |   <
 * |_.__/|_|  \___/ \___/|_|\_\
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)


{ Contains classes for basic logging. }

unit BrookLogger;

{$I BrookDefines.inc}

interface

uses
  RTLConsts,
{$IFDEF FPC}
  Math,
{$ELSE}
  Types,
  IOUtils,
{$ENDIF}
  SysUtils,
  DateUtils,
  Classes,
  BrookExtra;

const
  { Default logger name. }
  BROOK_LOGGER_OUTPUT_NAME = 'Console';
  { Default logger tag. }
  BROOK_LOGGER_TAG = 'BrookLogger_';

resourcestring
  { Error message @code('Empty output name.'). }
  SBrookEmptyOutputName = 'Empty output name.';
  { Error message @code('Active output log.'). }
  SBrookActiveOutput = 'Active output log.';
  { Error message @code('Inactive output log.'). }
  SBrookInactiveOutput = 'Inactive output log.';
  { Error message @code('Invalid output class: <class-name>.'). }
  SBrookInvalidOutputClass = 'Invalid output class: %s.';
  { Error message @code('Unknown output name: <output-name>.'). }
  SBrookUnknownOutputName = 'Unknown output name: %s.';
  { Name for information log level. }
  SBrookLevelInfo = 'INFO';
  { Name for hint log level. }
  SBrookLevelHint = 'HINT';
  { Name for warning log level. }
  SBrookLevelWarn = 'WARN';
  { Name for debug log level. }
  SBrookLevelDebug = 'DEBUG';
  { Name for error log level. }
  SBrookLevelError = 'ERROR';

type
  { Abstract class for logger output. }
  TBrookLoggerOutput = class abstract(TPersistent)
  private
    FFilters: TStringList;
    FOptions: TStringList;
    class function InternalFormat(const ALevel,
      AMessage: string): string; inline;
  protected
    class function FormatLog(const ALevel, AMessage: string): string; virtual;
    class function FormatFail(const ALevel: string;
      AException: Exception): string; virtual;
  public
    { Creates an instance of @code(TBrookLoggerOutput).
      @param(AFilters[in] Filters to be assigned to the logger instance.)
      @param(AOptions[in] Options to be assigned to the logger instance.) }
    constructor Create(AFilters, AOptions: TStringList); virtual;
    { Returns the alias name for output source.
      @returns(Output source alias.) }
    class function GetRegisterAlias: string; virtual;
    { Returns the name for output source.
      @returns(Output source name.) }
    class function GetName: string; virtual; abstract;
    { Returns @True if a certain log level is filtered.
      @param(ALevel[in] Log level.) }
    function IsFiltered(const ALevel: string): Boolean; virtual;
    { Appends a message to the output log.
      @param(ALevel[in] Log level.)
      @param(AMessage[in] Log message.) }
    procedure Log(const ALevel, AMessage: string); virtual; abstract;
    { Appends an exception message to the output log.
      @param(ALevel[in] Log level.)
      @param(AException[in] Log exception.) }
    procedure Fail(const ALevel: string;
      AException: Exception); virtual; abstract;
    { List containing the filtered log levels. }
    property Filters: TStringList read FFilters;
    { List containing additional options to the output. }
    property Options: TStringList read FOptions;
  end;

  { Class-reference for @code(TBrookLoggerOutput). }
  TBrookLoggerOutputClass = class of TBrookLoggerOutput;

  { Class for console logger output. }
  TBrookLoggerOutputConsole = class(TBrookLoggerOutput)
  public
    { Returns the name for output source.
      @returns(Output source name.) }
    class function GetName: string; override;
    { Appends a message to the output log.
      @param(ALevel[in] Log level.)
      @param(AMessage[in] Log message.) }
    procedure Log(const ALevel, AMessage: string); override;
    { Appends an exception message to the output log.
      @param(ALevel[in] Log level.)
      @param(AException[in] Log exception.) }
    procedure Fail(const ALevel: string; AException: Exception); override;
  end;

  { Class for file logger output. }
  TBrookLoggerOutputFile = class(TBrookLoggerOutput)
  private
    FHandle: TFileStream;
    FEncoding: TEncoding;
    FLastDate: TDate;
    FDirectory: string;
    FFileName: TFileName;
    procedure SetDirectory(const AValue: string);
  protected
    function CreateFile(AEncoding: TEncoding;
      const AFileName: TFileName): TFileStream; overload; virtual;
    function CreateFile(
      const AFileName: TFileName): TFileStream; overload; virtual;
    function RecreateFile(const AFileName: TFileName): TFileStream; virtual;
    procedure UpgradeFileName; virtual;
    procedure UpgradeFile; virtual;
    procedure WriteLog(const AMsg: string); inline;
    property LastDate: TDate read FLastDate;
    property Handle: TFileStream read FHandle;
  public
    { Method triggered after the constructor is called. }
    procedure AfterConstruction; override;
    { Destroys an instance of @code(TBrookLoggerOutputFile). }
    destructor Destroy; override;
    { Returns the name for output source.
      @returns(Output source name.) }
    class function GetName: string; override;
    { Appends a message to the output log.
      @param(ALevel[in] Log level.)
      @param(AMessage[in] Log message.) }
    procedure Log(const ALevel, AMessage: string); override;
    { Appends an exception message to the output log.
      @param(ALevel[in] Log level.)
      @param(AException[in] Log exception.) }
    procedure Fail(const ALevel: string; AException: Exception); override;
    { Specifies the output directory containing the logs. }
    property Directory: string read FDirectory write SetDirectory;
    { Generated absolute filename for the log. }
    property FileName: TFileName read FFileName;
  end;

  { Class that retains the log levels. }
  TBrookLoggerLevels = class(TPersistent)
  private
    FInfo: string;
    FHint: string;
    FWarn: string;
    FDebug: string;
    FError: string;
    function IsInfoStored: Boolean;
    function IsHintStored: Boolean;
    function IsWarnStored: Boolean;
    function IsDebugStored: Boolean;
    function IsErrorStored: Boolean;
  public
    { Creates an instance of @code(TBrookLoggerLevels). }
    constructor Create; virtual;
    { Copies the properties of the source levels.
      @param(ASource[in] Levels source to be copied.) }
    procedure Assign(ASource: TPersistent); override;
  published
    { Level message for information log. }
    property Info: string read FInfo write FInfo stored IsInfoStored;
    { Level message for hint log. }
    property Hint: string read FHint write FHint stored IsHintStored;
    { Level message for warning log. }
    property Warn: string read FWarn write FWarn stored IsWarnStored;
    { Level message for debug log. }
    property Debug: string read FDebug write FDebug stored IsDebugStored;
    { Level message for error log. }
    property Error: string read FError write FError stored IsErrorStored;
  end;

  { Component that writes log to a predefined output type. }
  TBrookLogger = class(TComponent)
  private
    FOutput: TBrookLoggerOutput;
    FFilters: TStringList;
    FOptions: TStringList;
    FLevels: TBrookLoggerLevels;
    FOutputName: string;
    FStreamedActive: Boolean;
    FActive: Boolean;
    function GetOutput: TBrookLoggerOutput;
    procedure SetActive(AValue: Boolean);
    procedure SetOutputName(const AValue: string);
    function IsActiveStored: Boolean;
    function IsOutputNameStored: Boolean;
    procedure SetFilters(AValue: TStringList);
    procedure SetOptions(AValue: TStringList);
  protected
    procedure Loaded; override;
    function CreateFilters: TStringList; virtual;
    function CreateOptions: TStringList; virtual;
    function CreateOutput(AFilters,
      AOptions: TStringList): TBrookLoggerOutput; virtual;
    function CreateLevels: TBrookLoggerLevels; virtual;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    procedure CheckActive; inline;
    procedure CheckInactive; inline;
    procedure CheckOutputName; inline;
  public
    { Creates an instance of @code(TBrookLogger).
      @param(AOwner[in] Owner component.) }
    constructor Create(AOwner: TComponent); override;
    { Destroys an instance of @code(TBrookLogger). }
    destructor Destroy; override;
    { Gets an output log class from the classes register. }
    function GetOutputClass: TBrookLoggerOutputClass; inline;
    { Enabled the logger component. }
    procedure Open;
    { Disables the logger component. }
    procedure Close;
    { Appends a message to the output log.
      @param(ALevel[in] Log level.)
      @param(AMessage[in] Log message.) }
    procedure Log(const ALevel, AMessage: string); inline;
    { Appends an exception message to the output log.
      @param(ALevel[in] Log level.)
      @param(AException[in] Log exception.) }
    procedure Fail(const ALevel: string; AException: Exception); inline;
    { Appends a message to the output log as information level.
      @param(AMessage[in] Log message.) }
    procedure Info(const AMessage: string); inline;
    { Appends a message to the output log as hint level.
      @param(AMessage[in] Log message.) }
    procedure Hint(const AMessage: string); inline;
    { Appends a message to the output log as warning level.
      @param(AMessage[in] Log message.) }
    procedure Warn(const AMessage: string); inline;
    { Appends a message to the output log as debug level.
      @param(AMessage[in] Log message.) }
    procedure Debug(const AMessage: string); inline;
    { Appends a message to the output log as error level.
      @param(AMessage[in] Log message.) }
    procedure Error(AException: Exception); inline;
    { Current active log output. }
    property Output: TBrookLoggerOutput read GetOutput;
  published
    { Activates the logger component. }
    property Active: Boolean read FActive write SetActive stored IsActiveStored;
    { Retains the log levels. }
    property Levels: TBrookLoggerLevels read FLevels write FLevels;
    { Name of the chosen output type. }
    property OutputName: string read FOutputName write SetOutputName
      stored IsOutputNameStored;
    { List containing the filtered log levels. }
    property Filters: TStringList read FFilters write SetFilters;
    { List containing additional options to the chosen output. }
    property Options: TStringList read FOptions write SetOptions;
  end;

implementation

{ TBrookLoggerOutput }

constructor TBrookLoggerOutput.Create(AFilters, AOptions: TStringList);
begin
  inherited Create;
  if not Assigned(AFilters) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AFilters']);
  if not Assigned(AOptions) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AOptions']);
  FFilters := AFilters;
  FOptions := AOptions;
end;

class function TBrookLoggerOutput.GetRegisterAlias: string;
begin
  Result := Concat(BROOK_LOGGER_TAG, GetName);
end;

class function TBrookLoggerOutput.InternalFormat(const ALevel,
  AMessage: string): string;
begin
  Result := Concat(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), ' ', ALevel,
    ': ', AMessage.TrimRight);
end;

class function TBrookLoggerOutput.FormatLog(const ALevel,
  AMessage: string): string;
begin
  Result := InternalFormat(ALevel, AMessage);
end;

class function TBrookLoggerOutput.FormatFail(const ALevel: string;
  AException: Exception): string;
begin
  if not Assigned(AException) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AException']);
  Result := FormatLog(ALevel, Concat(AException.ClassName, ': ',
    AException.Message));
end;

function TBrookLoggerOutput.IsFiltered(const ALevel: string): Boolean;
begin
  Result := FFilters.IndexOf(ALevel) > -1;
end;

{ TBrookLoggerOutputConsole }

class function TBrookLoggerOutputConsole.GetName: string;
begin
  Result := 'Console';
end;

procedure TBrookLoggerOutputConsole.Log(const ALevel, AMessage: string);
begin
  if IsConsole and not IsFiltered(ALevel) then
    WriteLn(Output, FormatLog(ALevel, AMessage));
end;

procedure TBrookLoggerOutputConsole.Fail(const ALevel: string;
  AException: Exception);
begin
  if IsConsole and not IsFiltered(ALevel) then
    WriteLn(ErrOutput, FormatFail(ALevel, AException));
end;

{ TBrookLoggerOutputFile }

procedure TBrookLoggerOutputFile.AfterConstruction;
begin
  inherited AfterConstruction;
  if not FDirectory.IsEmpty then
    Exit;
  FDirectory := FOptions.Values['Directory'];
  if FDirectory.IsEmpty then
    FDirectory :=
  {$IFDEF FPC}
      GetUserDir
  {$ELSE}
      TPath.GetHomePath
  {$ENDIF};
  UpgradeFileName;
end;

destructor TBrookLoggerOutputFile.Destroy;
begin
  FHandle.Free;
  inherited Destroy;
end;

function TBrookLoggerOutputFile.CreateFile(AEncoding: TEncoding;
  const AFileName: TFileName): TFileStream;
var
  VMode: Word;
begin
  FEncoding := AEncoding;
  if not Assigned(FEncoding) then
    FEncoding := TEncoding.UTF8;
  if FileExists(AFileName) then
    VMode := fmOpenReadWrite
  else
    VMode := fmCreate;
  VMode := VMode or fmShareDenyWrite;
  Result := TFileStream.Create(AFileName, VMode, BROOK_FILE_RIGHTS);
end;

function TBrookLoggerOutputFile.CreateFile(
  const AFileName: TFileName): TFileStream;
begin
  Result := CreateFile(TEncoding.UTF8, AFileName);
end;

function TBrookLoggerOutputFile.RecreateFile(
  const AFileName: TFileName): TFileStream;
begin
  FHandle.Free;
  Result := CreateFile(FEncoding, AFileName);
end;

procedure TBrookLoggerOutputFile.UpgradeFileName;
var
  VDate: TDate;
begin
  VDate := Date;
  if CompareDateTime(IncDay(FLastDate), VDate) <> LessThanValue then
    Exit;
  FLastDate := VDate;
  FFileName := Concat(IncludeTrailingPathDelimiter(FDirectory),
    ChangeFileExt(ExtractFileName(ParamStr(0)), ''), '_',
    FormatDateTime('yyyymmdd', FLastDate), '.log');
end;

procedure TBrookLoggerOutputFile.UpgradeFile;
begin
  if not FDirectory.IsEmpty then
    ForceDirectories(FDirectory);
  UpgradeFileName;
  FHandle := RecreateFile(FFileName);
end;

procedure TBrookLoggerOutputFile.SetDirectory(const AValue: string);
begin
  if AValue = FDirectory then
    Exit;
  FDirectory := AValue;
  FLastDate := 0;
end;

procedure TBrookLoggerOutputFile.WriteLog(const AMsg: string);
var
  VBuffer: TBytes;
begin
  if not Assigned(FHandle) then
    Exit;
  VBuffer := FEncoding.GetBytes(
{$IFDEF FPC}UnicodeString({$ENDIF}Concat(AMsg, sLineBreak){$IFDEF FPC}){$ENDIF});
  FHandle.Seek(0, TSeekOrigin.soEnd);
  FHandle.WriteBuffer(VBuffer[0], FEncoding.GetCharCount(VBuffer));
end;

class function TBrookLoggerOutputFile.GetName: string;
begin
  Result := 'File';
end;

procedure TBrookLoggerOutputFile.Log(const ALevel, AMessage: string);
begin
  if IsFiltered(ALevel) then
    Exit;
  UpgradeFile;
  WriteLog(FormatLog(ALevel, AMessage));
end;

procedure TBrookLoggerOutputFile.Fail(const ALevel: string;
  AException: Exception);
begin
  if IsFiltered(ALevel) then
    Exit;
  UpgradeFile;
  WriteLog(FormatFail(ALevel, AException));
end;

{ TBrookLoggerLevels }

constructor TBrookLoggerLevels.Create;
begin
  inherited Create;
  FInfo := SBrookLevelInfo;
  FHint := SBrookLevelHint;
  FWarn := SBrookLevelWarn;
  FDebug := SBrookLevelDebug;
  FError := SBrookLevelError;
end;

procedure TBrookLoggerLevels.Assign(ASource: TPersistent);
var
  VSrc: TBrookLoggerLevels;
begin
  if ASource is TBrookLoggerLevels then
  begin
    VSrc := ASource as TBrookLoggerLevels;
    FInfo := VSrc.Info;
    FHint := VSrc.Hint;
    FWarn := VSrc.Warn;
    FDebug := VSrc.Debug;
    FError := VSrc.Error;
  end
  else
    inherited Assign(ASource);
end;

function TBrookLoggerLevels.IsInfoStored: Boolean;
begin
  Result := FInfo <> SBrookLevelInfo;
end;

function TBrookLoggerLevels.IsHintStored: Boolean;
begin
  Result := FHint <> SBrookLevelHint;
end;

function TBrookLoggerLevels.IsWarnStored: Boolean;
begin
  Result := FWarn <> SBrookLevelWarn;
end;

function TBrookLoggerLevels.IsDebugStored: Boolean;
begin
  Result := FDebug <> SBrookLevelDebug;
end;

function TBrookLoggerLevels.IsErrorStored: Boolean;
begin
  Result := FError <> SBrookLevelError;
end;

{ TBrookLogger }

constructor TBrookLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLevels := CreateLevels;
  FFilters := CreateFilters;
  FOptions := CreateOptions;
  FOutputName := BROOK_LOGGER_OUTPUT_NAME;
end;

destructor TBrookLogger.Destroy;
begin
  SetActive(False);
  FLevels.Free;
  FOptions.Free;
  FFilters.Free;
  inherited Destroy;
end;

function TBrookLogger.CreateFilters: TStringList;
begin
  Result := TStringList.Create;
  Result.Add(FLevels.Info);
  Result.Add(FLevels.Hint);
  Result.Add(FLevels.Debug);
end;

function TBrookLogger.CreateOptions: TStringList;
begin
  Result := TStringList.Create;
end;

function TBrookLogger.GetOutputClass: TBrookLoggerOutputClass;
var
  C: TPersistentClass;
  N: string;
begin
  N := Concat(BROOK_LOGGER_TAG, FOutputName);
  C := Classes.GetClass(N);
  if not Assigned(C) then
    raise EClassNotFound.CreateFmt(SBrookUnknownOutputName, [FOutputName]);
  if not C.InheritsFrom(TBrookLoggerOutput) then
    raise EInvalidCast.CreateFmt(SBrookInvalidOutputClass, [C.ClassName]);
  Result := TBrookLoggerOutputClass(C);
end;

function TBrookLogger.CreateOutput(AFilters,
  AOptions: TStringList): TBrookLoggerOutput;
begin
  Result := GetOutputClass.Create(AFilters, AOptions);
end;

function TBrookLogger.CreateLevels: TBrookLoggerLevels;
begin
  Result := TBrookLoggerLevels.Create;
end;

procedure TBrookLogger.CheckOutputName;
begin
  if FOutputName.IsEmpty then
    raise EArgumentException.Create(SBrookEmptyOutputName);
end;

procedure TBrookLogger.CheckActive;
begin
  if not Active then
    raise EInvalidOpException.Create(SBrookInactiveOutput);
end;

procedure TBrookLogger.CheckInactive;
begin
  if (not (csLoading in ComponentState)) and Active then
    raise EInvalidOpException.Create(SBrookActiveOutput);
end;

procedure TBrookLogger.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then
      SetActive(True);
  except
    if csDesigning in ComponentState then
    begin
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr);
    end
    else
      raise;
  end;
end;

procedure TBrookLogger.DoOpen;
begin
  if Assigned(FOutput) then
    Exit;
  CheckOutputName;
  FOutput := CreateOutput(FFilters, FOptions);
  FActive := True;
end;

procedure TBrookLogger.DoClose;
begin
  if not Assigned(FOutput) then
    Exit;
  FOutput.Destroy;
  FOutput := nil;
  FActive := False;
end;

function TBrookLogger.IsActiveStored: Boolean;
begin
  Result := FActive;
end;

function TBrookLogger.GetOutput: TBrookLoggerOutput;
begin
  CheckActive;
  Result := FOutput;
end;

function TBrookLogger.IsOutputNameStored: Boolean;
begin
  Result := FOutputName <> BROOK_LOGGER_OUTPUT_NAME;
end;

procedure TBrookLogger.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csDesigning in ComponentState then
    FActive := AValue
  else
    if AValue then
    begin
      if csReading in ComponentState then
        FStreamedActive := True
      else
        DoOpen;
    end
    else
      DoClose;
end;

procedure TBrookLogger.SetFilters(AValue: TStringList);
begin
  FFilters.Assign(AValue);
end;

procedure TBrookLogger.SetOptions(AValue: TStringList);
begin
  FOptions.Assign(AValue);
end;

procedure TBrookLogger.SetOutputName(const AValue: string);
begin
  if FOutputName = AValue then
    Exit;
  if not FStreamedActive then
    CheckInactive;
  FOutputName := AValue;
  if FOutputName.IsEmpty then
    FOutputName := BROOK_LOGGER_OUTPUT_NAME;
end;

procedure TBrookLogger.Open;
begin
  SetActive(True);
end;

procedure TBrookLogger.Close;
begin
  SetActive(False);
end;

procedure TBrookLogger.Log(const ALevel, AMessage: string);
begin
  if Active then
    Output.Log(ALevel, AMessage);
end;

procedure TBrookLogger.Fail(const ALevel: string; AException: Exception);
begin
  if Active then
    Output.Fail(ALevel, AException);
end;

procedure TBrookLogger.Info(const AMessage: string);
begin
  Log(FLevels.Info, AMessage);
end;

procedure TBrookLogger.Hint(const AMessage: string);
begin
  Log(FLevels.Hint, AMessage);
end;

procedure TBrookLogger.Warn(const AMessage: string);
begin
  Log(FLevels.Warn, AMessage);
end;

procedure TBrookLogger.Debug(const AMessage: string);
begin
  Log(FLevels.Debug, AMessage);
end;

procedure TBrookLogger.Error(AException: Exception);
begin
  Fail(FLevels.Error, AException);
end;

initialization
  RegisterClassAlias(TBrookLoggerOutputConsole,
    TBrookLoggerOutputConsole.GetRegisterAlias);
  RegisterClassAlias(TBrookLoggerOutputFile,
    TBrookLoggerOutputFile.GetRegisterAlias);

finalization
  UnregisterClass(TBrookLoggerOutputConsole);
  UnregisterClass(TBrookLoggerOutputFile);

end.
