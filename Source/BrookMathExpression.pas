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

{ Contains class to evaluate mathematical expressions. }

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
  { Error message @code('Inactive math expression.'). }
  SBrookInactiveMathExpression = 'Inactive math expression.';

type
  { Possible error types returned by the mathematical expression evaluator. }
  TBrookMathExpressionErrorKind = (
    { None error. }
    ekNone,
    { Error not related to evaluation. }
    ekUnknown,
    { Unexpected number, e.g. '(1+2)3'. }
    ekUnexpectedNumber,
    { Unexpected word, e.g. '(1+2)x'. }
    ekUnexpectedWord,
    { Unexpected parenthesis, e.g. '1(2+3)'. }
    ekUnexpectedParens,
    { Missing expected operand, e.g. '0^+1'. }
    ekMissingOperand,
    { Unknown operator, e.g. '(1+2).'. }
    ekUnknownOperator,
    { Invalid function name, e.g. 'unknownfunc()'. }
    ekInvalidFuncName,
    { Bad parenthesis, e.g. '(1+2'. }
    ekBadParens,
    { Too few arguments passed to a macro, e.g. '$()'. }
    ekTooFewFuncArgs,
    { First macro argument is not variable, e.g. '$(1)'. }
    ekFirstArgIsNotVar,
    { Bad variable name, e.g. '2.3.4'. }
    ekBadVariableName,
    { Bad assignment, e.g. '2=3'. }
    ekBadAssignment
  );

  { Structured type which holds the mathematical expression errors. }
  TBrookMathExpressionError = packed record
  private
    FHandle: Psg_expr;
    function GetNear: Integer;
    function GetKind: TBrookMathExpressionErrorKind;
    function GetMessage: string;
    function GetHandle: Pointer;
  public
    { Creates an instance of @code(TBrookMathExpressionError).
      @param(AHandle[in] Math expression error handle.) }
    constructor Create(AHandle: Pointer);
    { Nearby position of an error in the mathematical expression. }
    property Near: Integer read GetNear;
    { Kind of an error in the mathematical expression. }
    property Kind: TBrookMathExpressionErrorKind read GetKind;
    { Description of an error in the mathematical expression. }
    property Message: string read GetMessage;
    { Math expression error handle. }
    property Handle: Pointer read GetHandle;
  end;

  { Event signature used to handle errors in a mathematical expression. }
  TBrookMathExpressionErrorEvent = procedure(ASender: TObject;
    AError: TBrookMathExpressionError) of object;

  { Structured type which holds the properties of a math expression extension. }
  TBrookMathExpressionExtension = packed record
  private
    FHandle: Psg_expr_argument;
    FIdent: MarshaledAString;
    function GetHasArgs: Boolean;
    function GetArg(AIndex: Integer): Double;
    function GetIdent: string;
    function GetHandle: Pointer;
  public
    { Creates an instance of @code(TBrookMathExpressionExtension).
      @param(AHandle[in] Math expression extension handle.)
      @param(AIdent[in] Function identifier.) }
    constructor Create(AHandle: Pointer; const AIdent: MarshaledAString);
    { Indicates that extension contains arguments. }
    property HasArgs: Boolean read GetHasArgs;
    { Function argument by its index. }
    property Args[AIndex: Integer]: Double read GetArg; default;
    { Function identifier. }
    property Ident: string read GetIdent;
    { Extension handle. }
    property Handle: Pointer read GetHandle;
  end;

  { Event signature used to handle extension in a mathematical expression. }
  TBrookMathExpressionExtensionEvent = function(ASender: TObject;
    AExtension: TBrookMathExpressionExtension): Double of object;

  { Mathematical expression evaluator. }
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
    { Creates an instance of @code(TBrookMathExpression).
      @param(AOwner[in] Owner component.) }
    constructor Create(AOwner: TComponent); override;
    { Destroys an instance of @code(TBrookMathExpression). }
    destructor Destroy; override;
    { Opens the math expression. }
    procedure Open;
    { Closes the math expression. }
    procedure Close;
    { Compiles a mathematical expression returning the errors if it does not
      succeed.
      @param(AExpression[in] Mathematical expression.)
      @param(AError[out] Mathematical expression error.)
      @returns(@True if compilation succeeds.) }
    function Compile(const AExpression: string;
      out AError: TBrookMathExpressionError): Boolean; overload; virtual;
    { Compiles a mathematical expression indicating if it succeeds.
      @param(AExpression[in] Mathematical expression.)
      @returns(@True if compilation succeeds.) }
    function Compile(const AExpression: string): Boolean; overload; virtual;
    { Clears a mathematical expression instance. }
    procedure Clear; virtual;
    { Evaluates a compiled mathematical expression.
      @returns(Evaluated mathematical expression) }
    function Evaluate: Double; virtual;
    { Gets the value of a declared variable.
      @param(AName[in] Name of the declared variable.)
      @returns(Value of a declared variable.) }
    function GetVariable(const AName: string): Double; virtual;
    { Sets a variable to the mathematical expression.
      @param(AName[in] Name for the variable.)
      @param(AValue[in] Value for the variable.) }
    procedure SetVariable(const AName: string; const AValue: Double); virtual;
    { Indicates if a mathematical expression has been successfully compiled. }
    property Compiled: Boolean read FCompiled;
    { Gets or sets a mathematical expression variable. }
    property Variables[const AName: string]: Double read GetVariable
      write SetVariable; default;
  published
    { Activates the mathematical expression evaluator. }
    property Active: Boolean read FActive write SetActive stored IsActiveStored;
    { Declares the mathematical expression. }
    property Expression: string read FExpression write SetExpression;
    { Declares the list of mathematical expression extensions. }
    property Extensions: TStringList read FExtensions write SetExtensions;
    { Event triggered when an extensions is called in the mathematical
      expression. }
    property OnExtension: TBrookMathExpressionExtensionEvent read FOnExtension
      write FOnExtension;
    { Event triggered when a mathematical expression compilation fails. }
    property OnError: TBrookMathExpressionErrorEvent read FOnError
      write FOnError;
    { Event triggered when the math expression component is enabled. }
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    { Event triggered when the math expression component is disabled. }
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
  end;

{ Evaluates a mathematical expression. }
function Evaluate(const AExpression: string): Double;

implementation

function Evaluate(const AExpression: string): Double;
var
  M: TMarshaller;
begin
  SgLib.Check;
  Result := sg_expr_calc(M.ToCString(AExpression), M.Length(AExpression));
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
  R := sg_expr_compile(FHandle, M.ToCString(AExpression), M.Length(AExpression),
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

function TBrookMathExpression.GetVariable(const AName: string): Double;
var
  M: TMarshaller;
begin
  CheckActive;
  Result := sg_expr_var(FHandle, M.ToCString(AName), M.Length(AName));
end;

procedure TBrookMathExpression.SetVariable(const AName: string;
  const AValue: Double);
var
  M: TMarshaller;
begin
  CheckActive;
  SgLib.CheckLastError(sg_expr_set_var(FHandle, M.ToCString(AName),
    M.Length(AName), AValue));
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
