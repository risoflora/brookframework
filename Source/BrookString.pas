(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 *  –– microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
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

{ String type used to represent a HTML body, POST payload and more. }

unit BrookString;

{$I BrookDefines.inc}

interface

uses
  RTLConsts,
  SysUtils,
  Classes,
  libsagui,
  Marshalling,
  BrookHandledClasses;

{ TODO: TBrookString.Assign() }

type
  { String class and its related methods. }
  TBrookString = class(TBrookHandledPersistent)
  private
    FHandle: Psg_str;
    FOwnsHandle: Boolean;
    function GetContent: TBytes;
    function GetLength: NativeUInt;
    procedure SetText(const AValue: string);
    function GetText: string; inline;
  protected
    class procedure CheckEncoding(AEncoding: TEncoding); static; inline;
    function GetHandle: Pointer; override;
  public
    { Creates an instance of @link(TBrookString).

      @param(AHandle[in] String handle.) }
    constructor Create(AHandle: Pointer); virtual;
    { Frees an instance of @link(TBrookString). }
    destructor Destroy; override;
    { Determines if the handle is freed on the class destruction. }
    property OwnsHandle: Boolean read FOwnsHandle write FOwnsHandle;
    { Write a string buffer to the string handle. All strings previously
      written are kept.

      @param(ASource[in] String buffer source to be written.)
      @param(ALength[in] Length of the string buffer being written.)

      @returns(Length of the written string buffer.) }
    function WriteBytes(const ASource: TBytes;
      ALength: NativeUInt): NativeUInt; virtual;
    { Writes a string to the string handle. All strings previously written are
      kept.

      @param(ASource[in] String to be written.)
      @param(AEncoding[in] Determines the encoding of the string being written.) }
    procedure Write(const ASource: string;
      AEncoding: TEncoding); overload; virtual;
    { Writes a string to the string handle. All strings previously written are
      kept.

      @param(ASource[in] String to be written.) }
    procedure Write(const ASource: string); overload; virtual;
    { Gets the string from the string handle. }
    function ToString: string; override;
    { Cleans all the content present in the string handle. }
    procedure Clear; virtual;
    { Gets the content buffer from the string handle. }
    property Content: TBytes read GetContent;
    { Gets the content length from the string handle. }
    property Length: NativeUInt read GetLength;
    { Gets or sets a string from or to the string handle. }
    property Text: string read GetText write SetText;
  end;

implementation

constructor TBrookString.Create(AHandle: Pointer);
begin
  inherited Create;
  FOwnsHandle := not Assigned(AHandle);
  if FOwnsHandle then
  begin
    SgLib.Check;
    FHandle := sg_str_new;
  end
  else
    FHandle := AHandle;
end;

destructor TBrookString.Destroy;
begin
  try
    if FOwnsHandle then
    begin
      SgLib.Check;
      sg_str_free(FHandle);
      FHandle := nil;
    end;
  finally
    inherited Destroy;
  end;
end;

class procedure TBrookString.CheckEncoding(AEncoding: TEncoding);
begin
  if not Assigned(AEncoding) then
    raise EArgumentNilException.CreateResFmt(@SParamIsNil, ['AEncoding']);
end;

function TBrookString.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookString.GetText: string;
begin
  Result :=
{$IFDEF FPC}string({$ENDIF}TEncoding.UTF8.GetString(GetContent){$IFDEF FPC}){$ENDIF};
end;

function TBrookString.WriteBytes(const ASource: TBytes;
  ALength: NativeUInt): NativeUInt;
begin
  SgLib.Check;
  Result := ALength;
  SgLib.CheckLastError(sg_str_write(FHandle, @ASource[0], Result));
end;

procedure TBrookString.Write(const ASource: string; AEncoding: TEncoding);
var
  VBytes: TBytes;
begin
  CheckEncoding(AEncoding);
  VBytes := AEncoding.GetBytes(
{$IFDEF FPC}UnicodeString({$ENDIF}ASource{$IFDEF FPC}){$ENDIF});
  WriteBytes(VBytes, System.Length(VBytes));
end;

procedure TBrookString.Write(const ASource: string);
begin
  Write(ASource, TEncoding.UTF8);
end;

procedure TBrookString.Clear;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_str_clear(FHandle));
end;

function TBrookString.GetLength: NativeUInt;
begin
  SgLib.Check;
  Result := sg_str_length(FHandle);
end;

procedure TBrookString.SetText(const AValue: string);
begin
  Clear;
  Write(AValue);
end;

function TBrookString.GetContent: TBytes;
begin
  SgLib.Check;
  Result := TMarshal.ToBytes(sg_str_content(FHandle), GetLength);
end;

function TBrookString.ToString: string;
begin
  Result := GetText;
end;

end.
