(*  _                     _
 * | |__  _ __ ___   ___ | | __
 * | '_ \| '__/ _ \ / _ \| |/ /
 * | |_) | | | (_) | (_) |   <
 * |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2020 Silvio Clecio <silvioprog@gmail.com>
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

unit BrookMathExpression;

{$I BrookDefines.inc}

interface

uses
  SysUtils,
  Math,
{$IFNDEF FPC}
  AnsiStrings,
{$ENDIF}
  Classes,
  Marshalling,
  libsagui,
  BrookHandledClasses;

resourcestring
  SBrookInactiveMathExpression = 'Inactive math expression.';

type
  (* experimental *)
  TBrookMathExpressionErrorKind = (ekNone, ekUnknown, ekUnexpectedNumber,
    ekUnexpectedWord, ekUnexpectedParens, ekMissingOperand, ekUnknownOperator,
    ekInvalidFuncName, ekBadParens, ekTooFewFuncArgs, ekFirstArgIsNotVar,
    ekBadVariableName, ekBadAssignment);

  (* experimental *)
  TBrookMathExpressionError = packed record
  private
    FHandle: Psg_expr;
    function GetNear: Integer;
    function GetKind: TBrookMathExpressionErrorKind;
    function GetMessage: string;
    function GetHandle: Pointer;
  public
    constructor Create(AHandle: Pointer);
    property Near: Integer read GetNear;
    property Kind: TBrookMathExpressionErrorKind read GetKind;
    property Message: string read GetMessage;
    property Handle: Pointer read GetHandle;
  end;

  (* experimental *)
  TBrookMathExpressionErrorEvent = procedure(ASender: TObject;
    AError: TBrookMathExpressionError) of object;

  (* experimental *)
  TBrookMathExpressionExtension = packed record
  private
    FHandle: Psg_expr_argument;
    FIdent: MarshaledAString;
    function GetHasArgs: Boolean;
    function GetArg(AIndex: Integer): Double;
    function GetIdent: string;
    function GetHandle: Pointer;
  public
    constructor Create(AHandle: Pointer; const AIdent: MarshaledAString);
    property HasArgs: Boolean read GetHasArgs;
    property Args[AIndex: Integer]: Double read GetArg; default;
    property Ident: string read GetIdent;
    property Handle: Pointer read GetHandle;
  end;

  (* experimental *)
  TBrookMathExpressionExtensionEvent = function(ASender: TObject;
    AExtension: TBrookMathExpressionExtension): Double of object;

  (* experimental *)
  TBrookMathExpression = class(TBrookHandledComponent)
  private
    FExtensions: TStringList;
    FExtensionsHandle: array of sg_expr_extension;
    FOnExtension: TBrookMathExpressionExtensionEvent;
    FOnError: TBrookMathExpressionErrorEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FHandle: Psg_expr;
    FExpression: string;
    FActive: Boolean;
    FStreamedActive: Boolean;
    FCompiled: Boolean;
    procedure DoExtensionsChange(Sender: TObject);
    function IsActiveStored: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetExtensions(AValue: TStringList);
    procedure SetExpression(const AValue: string);
    procedure InternalLibUnloadEvent(ASender: TObject);
  protected
    function CreateExtensions: TStringList; virtual;
    class function DoExprFunc(Acls: Pcvoid; Aargs: Psg_expr_argument;
      const Aidentifier: Pcchar): cdouble; cdecl; static;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
    function DoExtension(ASender: TObject;
      AExtension: TBrookMathExpressionExtension): Double; virtual;
    procedure DoError(ASender: TObject;
      AError: TBrookMathExpressionError); virtual;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    procedure CheckActive; inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function Compile(const AExpression: string;
      out AError: TBrookMathExpressionError): Boolean; overload; virtual;
    function Compile(const AExpression: string): Boolean; overload; virtual;
    procedure Clear; virtual;
    function Evaluate: Double; virtual;
    function GetVariable(const AName: string): Double; virtual;
    procedure SetVariable(const AName: string; const AValue: Double); virtual;
    property Compiled: Boolean read FCompiled;
    property Variables[const AName: string]: Double read GetVariable
      write SetVariable; default;
  published
    property Active: Boolean read FActive write SetActive stored IsActiveStored;
    property Expression: string read FExpression write SetExpression;
    property Extensions: TStringList read FExtensions write SetExtensions;
    property OnExtension: TBrookMathExpressionExtensionEvent read FOnExtension
      write FOnExtension;
    property OnError: TBrookMathExpressionErrorEvent read FOnError
      write FOnError;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
  end;

function Evaluate(const AExpression: string): Double;

implementation

function Evaluate(const AExpression: string): Double;
var
  M: TMarshaller;
begin
  SgLib.Check;
  Result := sg_expr_calc(M.ToCString(AExpression), Length(AExpression));
end;

{ TBrookMathExpressionError }

constructor TBrookMathExpressionError.Create(AHandle: Pointer);
begin
  FHandle := AHandle;
end;

function TBrookMathExpressionError.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookMathExpressionError.GetNear: Integer;
begin
  SgLib.Check;
  Result := sg_expr_near(FHandle);
end;

function TBrookMathExpressionError.GetKind: TBrookMathExpressionErrorKind;
begin
  SgLib.Check;
  Result := TBrookMathExpressionErrorKind(Succ(sg_expr_err(FHandle)));
end;

function TBrookMathExpressionError.GetMessage: string;
begin
  SgLib.Check;
  Result := TMarshal.ToString(sg_expr_strerror(FHandle));
end;

{ TBrookMathExpressionExtension }

constructor TBrookMathExpressionExtension.Create(AHandle: Pointer;
  const AIdent: MarshaledAString);
begin
  FHandle := AHandle;
  FIdent := AIdent;
end;

function TBrookMathExpressionExtension.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookMathExpressionExtension.GetHasArgs: Boolean;
begin
  Result := Assigned(FHandle);
end;

function TBrookMathExpressionExtension.GetArg(AIndex: Integer): Double;
begin
  if not Assigned(FHandle) then
    Exit(NaN);
  SgLib.Check;
  Result := sg_expr_arg(FHandle, AIndex);
end;

function TBrookMathExpressionExtension.GetIdent: string;
begin
  Result := TMarshal.ToString(FIdent);
end;

{ TBrookMathExpression }

constructor TBrookMathExpression.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExtensions := CreateExtensions;
  FExtensions.OnChange := DoExtensionsChange;
  SgLib.UnloadEvents.Add(InternalLibUnloadEvent, Self);
end;

destructor TBrookMathExpression.Destroy;
begin
  SetActive(False);
  FExtensions.Free;
  SgLib.UnloadEvents.Remove(InternalLibUnloadEvent);
  inherited Destroy;
end;

class function TBrookMathExpression.DoExprFunc(Acls: Pcvoid;
  Aargs: Psg_expr_argument; const Aidentifier: Pcchar): cdouble;
begin
  Result := TBrookMathExpression(Acls).DoExtension(Acls,
    TBrookMathExpressionExtension.Create(Aargs, Aidentifier));
end;

function TBrookMathExpression.CreateExtensions: TStringList;
begin
  Result := TStringList.Create;
end;

procedure TBrookMathExpression.CheckActive;
begin
  if (not (csLoading in ComponentState)) and (not Active) then
    raise EInvalidOpException.Create(SBrookInactiveMathExpression);
end;

procedure TBrookMathExpression.Loaded;
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

function TBrookMathExpression.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookMathExpression.InternalLibUnloadEvent(ASender: TObject);
begin
  if Assigned(ASender) then
    TBrookMathExpression(ASender).Close;
end;

procedure TBrookMathExpression.DoExtensionsChange(Sender: TObject);
begin
  Clear;
end;

function TBrookMathExpression.DoExtension(ASender: TObject;
  AExtension: TBrookMathExpressionExtension): Double;
begin
  if Assigned(FOnExtension) then
    Exit(FOnExtension(ASender, AExtension));
  Result := NaN;
end;

procedure TBrookMathExpression.DoError(ASender: TObject;
  AError: TBrookMathExpressionError);
begin
  if Assigned(FOnError) then
    FOnError(ASender, AError);
end;

procedure TBrookMathExpression.SetExpression(const AValue: string);
begin
  if AValue = FExpression then
    Exit;
  FExpression := AValue;
  Clear;
end;

procedure TBrookMathExpression.SetExtensions(AValue: TStringList);
begin
  if Assigned(AValue) then
    FExtensions.Assign(AValue)
  else
    FExtensions.Clear;
end;

procedure TBrookMathExpression.DoOpen;
begin
  if Assigned(FHandle) then
    Exit;
  SgLib.Check;
  FHandle := sg_expr_new;
  FActive := Assigned(FHandle);
  if Assigned(FOnActivate) then
    FOnActivate(Self);
end;

procedure TBrookMathExpression.DoClose;
begin
  Clear;
  if not Assigned(FHandle) then
    Exit;
  SgLib.Check;
  sg_expr_free(FHandle);
  FHandle := nil;
  FActive := False;
  FCompiled := False;
  if Assigned(FOnDeactivate) then
    FOnDeactivate(Self);
end;

function TBrookMathExpression.IsActiveStored: Boolean;
begin
  Result := FActive;
end;

procedure TBrookMathExpression.SetActive(AValue: Boolean);
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

function TBrookMathExpression.GetVariable(const AName: string): Double;
var
  M: TMarshaller;
begin
  CheckActive;
  SgLib.Check;
  Result := sg_expr_var(FHandle, M.ToCString(AName), Length(AName));
end;

procedure TBrookMathExpression.SetVariable(const AName: string;
  const AValue: Double);
var
  M: TMarshaller;
begin
  CheckActive;
  SgLib.Check;
  SgLib.CheckLastError(sg_expr_set_var(FHandle, M.ToCString(AName),
    Length(AName), AValue));
end;

function TBrookMathExpression.Compile(const AExpression: string;
  out AError: TBrookMathExpressionError): Boolean;
var
  EX: sg_expr_extension;
  M: TMarshaller;
  I: Integer;
  R: cint;
  S: string;
begin
  if FCompiled then
    Exit(True);
  CheckActive;
  SetLength(FExtensionsHandle, Succ(FExtensions.Count));
  for I := 0 to Pred(FExtensions.Count) do
  begin
    S := FExtensions[I];
    if S.Trim.IsEmpty then
      Continue;
    EX.func := DoExprFunc;
    EX.identifier :=
{$IFNDEF FPC}
      AnsiStrings.StrNew(
{$ENDIF}
        M.ToCString(S)
{$IFNDEF FPC}
      )
{$ENDIF};
    EX.cls := Self;
    FExtensionsHandle[I] := EX;
  end;
  FExtensionsHandle[FExtensions.Count] := Default(sg_expr_extension);
  R := sg_expr_compile(FHandle, M.ToCString(AExpression), Length(AExpression),
    @FExtensionsHandle[0]);
  FCompiled := R = 0;
  if not FCompiled then
    AError := TBrookMathExpressionError.Create(FHandle);
  Result := FCompiled;
end;

function TBrookMathExpression.Compile(const AExpression: string): Boolean;
var
  E: TBrookMathExpressionError;
begin
  Result := Compile(AExpression, E);
  if not Result then
    DoError(Self, E);
end;

procedure TBrookMathExpression.Clear;
{$IFNDEF FPC}
var
  I: Integer;
{$ENDIF}
begin
  if not Assigned(FHandle) then
    Exit;
  SgLib.Check;
  SgLib.CheckLastError(sg_expr_clear(FHandle));
{$IFNDEF FPC}
  for I := Low(FExtensionsHandle) to Pred(High(FExtensionsHandle)) do
    AnsiStrings.StrDispose(FExtensionsHandle[I].identifier);
{$ENDIF}
  FExtensionsHandle := nil;
  FCompiled := False;
end;

function TBrookMathExpression.Evaluate: Double;
begin
  if not FCompiled then
    Compile(FExpression);
  Result := sg_expr_eval(FHandle);
end;

procedure TBrookMathExpression.Open;
begin
  SetActive(True);
end;

procedure TBrookMathExpression.Close;
begin
  SetActive(False);
end;

end.
