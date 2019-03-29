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

unit BrookMediaTypes;

{$I BrookDefines.inc}

interface

uses
  RTLConsts,
  SysUtils,
  Classes,
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  libsagui,
  BrookExtra,
  BrookReader,
  BrookHandledClasses,
  BrookStringMap;

const
  BROOK_MIME_FILE = 'mime.types';
  BROOK_MIME_TAG = 'BrookMediaTypes_';

resourcestring
  SBrookInvalidMediaType = 'Invalid media type: %s.';
  SBrookInvalidMediaExt = 'Invalid media extension: %s.';
  SBrookEmptyMediaType = 'Empty media type.';
  SBrookEmptyMediaExt = 'Empty media extension.';
  SBrookActiveMIMETypes = 'Active MIME types.';
  SBrookInactiveMIMETypes = 'Inactive MIME types.';
  SBrookEmptyMIMEProvider = 'Empty MIME provider.';
  SBrookInvalidMIMEProviderClass = 'Invalid MIME provider class: %s.';
  SBrookUnknownMIMEProvider = 'Unknown MIME provider: %s.';

type
  EBrookMediaTypes = class(Exception);

  TBrookMediaTypes = class abstract(TBrookHandledPersistent)
  private
    FCache: TBrookStringMap;
    FDefaultType: string;
    FHandle: Psg_strmap;
    procedure SetDefaultType(const AValue: string);
  protected
    class function GetRegisterAlias: string; virtual;
    function CreateCache: TBrookStringMap; virtual;
    function IsPrepared: Boolean; virtual; abstract;
    function GetHandle: Pointer; override;
    procedure CheckExt(const AExt: string); inline;
    procedure CheckType(const AType: string); inline;
    procedure CheckPrepared; inline;
    property Cache: TBrookStringMap read FCache;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function GetDescription: string; virtual; abstract;
    class function IsValid(const AType: string): Boolean; static; inline;
    class function IsText(const AType: string): Boolean; static; inline;
    class function IsExt(const AExt: string): Boolean; static; inline;
    class function NormalizeExt(const AExt: string): string; static; inline;
    procedure Prepare; virtual; abstract;
    procedure Add(const AExt, AType: string); virtual;
    procedure Remove(const AExt: string); virtual;
    function TryType(const AExt: string; out AType: string): Boolean; virtual;
    function Find(const AExt, ADefType: string): string; overload; virtual;
    function Find(const AExt: string): string; overload; virtual;
    function Count: Integer; virtual;
    procedure Clear; virtual;
    property DefaultType: string read FDefaultType write SetDefaultType;
    property Prepared: Boolean read IsPrepared;
  end;

  TBrookMediaTypesClass = class of TBrookMediaTypes;

  TBrookMediaTypesParser = class(TPersistent)
  private
    FReader: TBrookTextReader;
    FTypes: TBrookMediaTypes;
  public
    constructor Create(AReader: TBrookTextReader;
      ATypes: TBrookMediaTypes); virtual;
    procedure Parse; virtual;
    property Reader: TBrookTextReader read FReader;
    property Types: TBrookMediaTypes read FTypes;
  end;

  TBrookMediaTypesParserNginx = class(TBrookMediaTypesParser)
  public
    procedure Parse; override;
  end;

  TBrookMediaTypesPath = class(TBrookMediaTypes)
  private
    FParser: TBrookMediaTypesParser;
    FReader: TBrookTextReader;
    FFileName: string;
    FPrepared: Boolean;
  protected
    function CreateReader: TBrookTextReader; virtual;
    function CreateParser: TBrookMediaTypesParser; virtual;
    function IsPrepared: Boolean; override;
  public
    constructor Create(const AFileName: string); reintroduce; overload; virtual;
    constructor Create; overload; override;
    destructor Destroy; override;
    class function GetDescription: string; override;
    procedure Prepare; override;
    procedure Clear; override;
    property Reader: TBrookTextReader read FReader;
    property Parser: TBrookMediaTypesParser read FParser;
    property FileName: string read FFileName;
  end;

  TBrookMediaTypesPathClass = class of TBrookMediaTypesPath;

  TBrookMediaTypesApache = class(TBrookMediaTypesPath)
  public
    class function GetDescription: string; override;
  end;

  TBrookMediaTypesNginx = class(TBrookMediaTypesPath)
  protected
    function CreateParser: TBrookMediaTypesParser; override;
  public
    constructor Create; override;
    class function GetDescription: string; override;
  end;

  TBrookMediaTypesWindows = class(TBrookMediaTypesPath)
  public
    class function GetDescription: string; override;
  end;

  TBrookMediaTypesUnix = class(TBrookMediaTypesPath)
  public
    constructor Create; override;
    class function GetDescription: string; override;
  end;

  { TBrookMIME }

  TBrookMIME = class(TBrookHandledComponent)
  private
    FDefaultType: string;
    FFileName: TFileName;
    FTypes: TBrookMediaTypes;
    FActive: Boolean;
    FStreamedActive: Boolean;
    FProvider: string;
    function GetTypes: TBrookMediaTypes;
    function IsActive: Boolean;
    function IsDefaultType: Boolean;
    function IsFileName: Boolean;
    function IsProvider: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetDefaultType(const AValue: string);
    procedure SetFileName(AValue: TFileName);
    procedure SetProvider(const AValue: string);
  protected
    class procedure LibNotifier(AClosure: Pointer); static; cdecl;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    procedure CheckProvider; inline;
    procedure CheckActive; inline;
    procedure CheckInactive; inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetProviderClass: TBrookMediaTypesClass; inline;
    procedure Open;
    procedure Close;
    property Types: TBrookMediaTypes read GetTypes;
  published
    property Active: Boolean read FActive write SetActive stored IsActive;
    property DefaultType: string read FDefaultType write SetDefaultType stored
      IsDefaultType;
    property FileName: TFileName read FFileName write SetFileName
      stored IsFileName;
    property Provider: string read FProvider write SetProvider stored IsProvider;
  end;

implementation

{ TBrookMediaTypes }

constructor TBrookMediaTypes.Create;
begin
  inherited Create;
  FCache := CreateCache;
  FDefaultType := BROOK_CT_OCTET_STREAM;
end;

destructor TBrookMediaTypes.Destroy;
begin
  FCache.Free;
  inherited Destroy;
end;

procedure TBrookMediaTypes.SetDefaultType(const AValue: string);
begin
  if FDefaultType = AValue then
    Exit;
  FDefaultType := AValue;
  if FDefaultType.IsEmpty then
    FDefaultType := BROOK_CT_OCTET_STREAM;
end;

class function TBrookMediaTypes.GetRegisterAlias: string;
begin
  Result := Concat(BROOK_MIME_TAG + GetDescription);
end;

function TBrookMediaTypes.CreateCache: TBrookStringMap;
begin
  Result := TBrookStringMap.Create(@FHandle);
end;

class function TBrookMediaTypes.IsValid(const AType: string): Boolean;
begin
  Result := (Length(AType) > 0) and
    (Length(AType.Split(['/'], TStringSplitOptions.ExcludeEmpty)) > 1);
end;

class function TBrookMediaTypes.IsText(const AType: string): Boolean;
begin
  Result := IsValid(AType) and AType.StartsWith('text/');
end;

class function TBrookMediaTypes.IsExt(const AExt: string): Boolean;
begin
  Result := (Length(AExt) > 0) and (AExt <> '.') and (AExt <> '..');
end;

procedure TBrookMediaTypes.CheckExt(const AExt: string);
begin
  if AExt.IsEmpty then
    raise EArgumentException.Create(SBrookEmptyMediaExt);
  if not IsExt(AExt) then
    raise EBrookMediaTypes.CreateFmt(SBrookInvalidMediaExt, [AExt]);
end;

procedure TBrookMediaTypes.CheckPrepared;
begin
  if not IsPrepared then
    Prepare;
end;

procedure TBrookMediaTypes.CheckType(const AType: string);
begin
  if AType.IsEmpty then
    raise EArgumentException.Create(SBrookEmptyMediaType);
  if not IsValid(AType) then
    raise EBrookMediaTypes.CreateResFmt(@SBrookInvalidMediaType, [AType]);
end;

function TBrookMediaTypes.GetHandle: Pointer;
begin
  Result := FHandle;
end;

class function TBrookMediaTypes.NormalizeExt(const AExt: string): string;
begin
  Result := AExt;
  if (Length(AExt) > 0) and (AExt[1] <> '.') then
    Result := Concat('.', Result);
end;

procedure TBrookMediaTypes.Add(const AExt, AType: string);
begin
  CheckExt(AExt);
  CheckType(AType);
  FCache.AddOrSet(NormalizeExt(AExt), AType);
end;

procedure TBrookMediaTypes.Remove(const AExt: string);
begin
  CheckExt(AExt);
  FCache.Remove(NormalizeExt(AExt));
end;

function TBrookMediaTypes.TryType(const AExt: string;
  out AType: string): Boolean;
begin
  CheckExt(AExt);
  CheckPrepared;
  Result := FCache.TryValue(NormalizeExt(AExt), AType);
end;

function TBrookMediaTypes.Find(const AExt, ADefType: string): string;
begin
  CheckExt(AExt);
  if not ADefType.IsEmpty then
    CheckType(ADefType);
  CheckPrepared;
  if not FCache.TryValue(NormalizeExt(AExt), Result) then
    Result := ADefType;
end;

function TBrookMediaTypes.Find(const AExt: string): string;
begin
  Result := Find(AExt, FDefaultType);
end;

function TBrookMediaTypes.Count: Integer;
begin
  Result := FCache.Count;
end;

procedure TBrookMediaTypes.Clear;
begin
  FCache.Clear;
end;

{ TBrookMediaTypesParser }

constructor TBrookMediaTypesParser.Create(AReader: TBrookTextReader;
  ATypes: TBrookMediaTypes);
begin
  inherited Create;
  if not Assigned(AReader) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AReader']);
  if not Assigned(ATypes) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['ATypes']);
  FReader := AReader;
  FTypes := ATypes;
end;

procedure TBrookMediaTypesParser.Parse;
var
  I: Integer;
  VLine, VMediaType: string;
  VPair, VExtensions: TArray<string>;
begin
  FTypes.Cache.Clear;
  while not FReader.EOF do
  begin
    VLine := FReader.Read;
    if (Length(VLine) > 0) and (VLine[1] <> '#') then
    begin
      VPair := VLine.Split([#9], TStringSplitOptions.ExcludeEmpty);
      if Length(VPair) > 1 then
      begin
        VMediaType := VPair[0];
        VExtensions := VPair[1].Split([' '], TStringSplitOptions.ExcludeEmpty);
        for I := Low(VExtensions) to High(VExtensions) do
          FTypes.Add(VExtensions[I], VMediaType);
      end;
    end;
  end;
end;

{ TBrookMediaTypesParserNginx }

procedure TBrookMediaTypesParserNginx.Parse;
var
  I: Integer;
  VPair: TArray<string>;
  VLine, VMediaType: string;
begin
  while not FReader.EOF do
  begin
    VLine := Trim(FReader.Read);
    Delete(VLine, Length(VLine), Length(';'));
    if (Length(VLine) > 0) and (VLine[1] <> '#') and (VLine <> 'types ') then
    begin
      VPair := VLine.Split([' '], TStringSplitOptions.ExcludeEmpty);
      if Length(VPair) > 1 then
      begin
        VMediaType := VPair[0];
        for I := 1 to High(VPair) do
          FTypes.Add(VPair[I], VMediaType);
      end;
    end;
  end;
end;

{ TBrookMediaTypesPath }

constructor TBrookMediaTypesPath.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FReader := CreateReader;
  FParser := CreateParser;
end;

constructor TBrookMediaTypesPath.Create;
begin
  Create(Concat(
{$IFDEF UNIX}'/etc/'{$ELSE}ExtractFilePath(ParamStr(0)){$ENDIF},
    BROOK_MIME_FILE));
end;

destructor TBrookMediaTypesPath.Destroy;
begin
  FReader.Free;
  FParser.Free;
  inherited Destroy;
end;

function TBrookMediaTypesPath.CreateReader: TBrookTextReader;
begin
  Result := TBrookFileReader.Create(FFileName);
end;

function TBrookMediaTypesPath.CreateParser: TBrookMediaTypesParser;
begin
  Result := TBrookMediaTypesParser.Create(FReader, Self);
end;

class function TBrookMediaTypesPath.GetDescription: string;
begin
  Result := 'Default';
end;

function TBrookMediaTypesPath.IsPrepared: Boolean;
begin
  Result := FPrepared;
end;

procedure TBrookMediaTypesPath.Prepare;
begin
  if FPrepared then
    Exit;
  FParser.Parse;
  FPrepared := True;
end;

procedure TBrookMediaTypesPath.Clear;
begin
  if not FPrepared then
    Exit;
  inherited Clear;
  FPrepared := False;
end;

{ TBrookMediaTypesApache }

class function TBrookMediaTypesApache.GetDescription: string;
begin
  Result := 'Apache';
end;

{ TBrookMediaTypesNginx }

constructor TBrookMediaTypesNginx.Create;
begin
  inherited Create(Concat(
{$IFDEF UNIX}'/etc/nginx/'{$ELSE}ExtractFilePath(ParamStr(0)){$ENDIF},
    BROOK_MIME_FILE));
end;

function TBrookMediaTypesNginx.CreateParser: TBrookMediaTypesParser;
begin
  Result := TBrookMediaTypesParserNginx.Create(FReader, Self);
end;

class function TBrookMediaTypesNginx.GetDescription: string;
begin
  Result := 'Nginx';
end;

{ TBrookMediaTypesWindows }

class function TBrookMediaTypesWindows.GetDescription: string;
begin
  Result := 'Windows';
end;

{ TBrookMediaTypesUnix }

constructor TBrookMediaTypesUnix.Create;
var
  FNs: TArray<TFileName>;
  FN: TFileName;
begin
  FNs := TArray<TFileName>.Create(
    Concat('/etc/', BROOK_MIME_FILE)
    // Put other 'mime.types' paths here...
  );
  for FN in FNs do
    if FileExists(FN) then
    begin
      inherited Create(FN);
      Exit;
    end;
  if Length(FN) = 0 then
    FN := BROOK_MIME_FILE;
  inherited Create(FN);
end;

class function TBrookMediaTypesUnix.GetDescription: string;
begin
  Result := 'Unix';
end;

{ TBrookMIME }

constructor TBrookMIME.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SgLib.AddNotifier({$IFNDEF VER3_0}@{$ENDIF}LibNotifier, Self);
  FDefaultType := BROOK_CT_OCTET_STREAM;
  FFileName := BROOK_MIME_FILE;
  FProvider := 'Default';
end;

destructor TBrookMIME.Destroy;
begin
  try
    SetActive(False);
    SgLib.RemoveNotifier({$IFNDEF VER3_0}@{$ENDIF}LibNotifier);
  finally
    inherited Destroy;
  end;
end;

procedure TBrookMIME.CheckProvider;
begin
  if FProvider.IsEmpty then
    raise EArgumentException.Create(SBrookEmptyMIMEProvider);
end;

procedure TBrookMIME.CheckActive;
begin
  if not Active then
    raise EInvalidOpException.Create(SBrookInactiveMIMETypes);
end;

procedure TBrookMIME.CheckInactive;
begin
  if (not (csLoading in ComponentState)) and Active then
    raise EInvalidOpException.Create(SBrookActiveMIMETypes);
end;

procedure TBrookMIME.Loaded;
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

class procedure TBrookMIME.LibNotifier(AClosure: Pointer);
begin
  TBrookMIME(AClosure).Close;
end;

function TBrookMIME.GetHandle: Pointer;
begin
  Result := FTypes.Handle;
end;

function TBrookMIME.GetProviderClass: TBrookMediaTypesClass;
var
  D: string;
  C: TPersistentClass;
begin
  D := Concat(BROOK_MIME_TAG, FProvider);
  C := Classes.GetClass(D);
  if Assigned(C) and (not C.InheritsFrom(TBrookMediaTypes)) then
    raise EInvalidCast.CreateFmt(SBrookInvalidMIMEProviderClass, [C.ClassName]);
  Result := TBrookMediaTypesClass(C);
  if not Assigned(Result) then
    raise EClassNotFound.CreateFmt(SBrookUnknownMIMEProvider, [FProvider]);
end;

procedure TBrookMIME.DoOpen;
var
  T: TBrookMediaTypesClass;
begin
  if Assigned(FTypes) then
    Exit;
  CheckProvider;
  T := GetProviderClass;
  if T.InheritsFrom(TBrookMediaTypesPath) then
    FTypes := TBrookMediaTypesPathClass(T).Create(FFileName)
  else
    FTypes := T.Create;
  FTypes.DefaultType := FDefaultType;
  FActive := True;
end;

procedure TBrookMIME.DoClose;
begin
  if not Assigned(FTypes) then
    Exit;
  FTypes.Destroy;
  FTypes := nil;
  FActive := False;
end;

function TBrookMIME.IsActive: Boolean;
begin
  Result := FActive;
end;

function TBrookMIME.GetTypes: TBrookMediaTypes;
begin
  CheckActive;
  Result := FTypes;
end;

function TBrookMIME.IsDefaultType: Boolean;
begin
  Result := FDefaultType <> BROOK_CT_OCTET_STREAM;
end;

function TBrookMIME.IsFileName: Boolean;
begin
  Result := FFileName <> BROOK_MIME_FILE;
end;

function TBrookMIME.IsProvider: Boolean;
begin
  Result := FProvider <> 'Default';
end;

procedure TBrookMIME.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csDesigning in ComponentState then
  begin
    if not (csLoading in ComponentState) then
      SgLib.Check;
    FActive := AValue;
  end
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

procedure TBrookMIME.SetDefaultType(const AValue: string);
begin
  if FDefaultType = AValue then
    Exit;
  if not FStreamedActive then
    CheckInactive;
  FDefaultType := AValue;
  if FDefaultType.IsEmpty then
    FDefaultType := BROOK_CT_OCTET_STREAM;
end;

procedure TBrookMIME.SetFileName(AValue: TFileName);
begin
  if FFileName = AValue then
    Exit;
  if not FStreamedActive then
    CheckInactive;
  FFileName := AValue;
  if FFileName.IsEmpty then
    FFileName := BROOK_MIME_FILE;
end;

procedure TBrookMIME.SetProvider(const AValue: string);
begin
  if FProvider = AValue then
    Exit;
  if not FStreamedActive then
    CheckInactive;
  FProvider := AValue;
  if FProvider.IsEmpty then
    FProvider := 'Default';
end;

procedure TBrookMIME.Open;
begin
  SetActive(True);
end;

procedure TBrookMIME.Close;
begin
  SetActive(False);
end;

initialization
  RegisterClassAlias(TBrookMediaTypesPath, TBrookMediaTypesPath.GetRegisterAlias);
  RegisterClassAlias(TBrookMediaTypesApache, TBrookMediaTypesApache.GetRegisterAlias);
  RegisterClassAlias(TBrookMediaTypesNginx, TBrookMediaTypesNginx.GetRegisterAlias);
  RegisterClassAlias(TBrookMediaTypesWindows, TBrookMediaTypesWindows.GetRegisterAlias);
  RegisterClassAlias(TBrookMediaTypesUnix, TBrookMediaTypesUnix.GetRegisterAlias);

finalization
  UnRegisterClass(TBrookMediaTypesPath);
  UnRegisterClass(TBrookMediaTypesApache);
  UnRegisterClass(TBrookMediaTypesNginx);
  UnRegisterClass(TBrookMediaTypesWindows);
  UnRegisterClass(TBrookMediaTypesUnix);

end.
