unit FPC300Fixes;

{$MODE DELPHI}

interface

uses
  SysUtils;

type
  TArray<T> = array of T;

  TStringArray = array of string;

  TStringSplitOptions = (None, ExcludeEmpty);

  { TIntegerHelper }

  TIntegerHelper = record helper for Integer
  public
    function ToString: string; inline;
  end;

  { TStringHelper }

  TStringHelper = record helper for string
  private
    function GetLength: SizeInt;
  public
    class function Equals(const A: string;
      const B: string): Boolean; overload; static;
    class function ToInteger(const S: string): Integer; overload; static; inline;
    function CompareTo(const B: string): Integer;
    function Contains(const AValue: string): Boolean;
    function EndsWith(const AValue: string): Boolean; overload; inline;
    function EndsWith(const AValue: string;
      AIgnoreCase: Boolean): Boolean; overload;
    function Equals(const AValue: string): Boolean;
    function IndexOf(const AValue: string; StartIndex: SizeInt;
      ACount: SizeInt): SizeInt;
    function IndexOfUnQuoted(const AValue: string; AStartQuote, AEndQuote: Char;
      AStartIndex: SizeInt = 0): SizeInt;
    function IndexOfAny(const AAnyOf: array of Char;
      AStartIndex: SizeInt): SizeInt; overload;
    function IndexOfAny(const AAnyOf: array of Char; AStartIndex: SizeInt;
      ACount: SizeInt): SizeInt; overload;
    function IndexOfAnyUnquoted(const AAnyOf: array of Char;
      AStartQuote, AEndQuote: Char; AStartIndex: SizeInt): SizeInt; overload;
    function IndexOfAnyUnquoted(const AAnyOf: array of Char; AStartQuote,
      AEndQuote: Char; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    function IsEmpty: Boolean;
    function Split(const ASeparators: array of Char;
      AOptions: TStringSplitOptions): TStringArray; overload;
    function Split(const ASeparators: array of Char; ACount: SizeInt;
      AOptions: TStringSplitOptions): TStringArray; overload;
    function Split(const ASeparators: array of Char; AQuoteStart,
      AQuoteEnd: Char; ACount: SizeInt;
      AOptions: TStringSplitOptions): TStringArray; overload;
    function StartsWith(const AValue: string): Boolean; overload; inline;
    function StartsWith(const AValue: string;
      AIgnoreCase: Boolean): Boolean; overload;
    function SubString(AStartIndex: SizeInt): string; overload;
    function SubString(AStartIndex: SizeInt; ALen: SizeInt): string; overload;
    function ToInteger: Integer; inline;
    function ToLower: string; inline;
    function ToUpper: string; inline;
    function TrimLeft: string;
    function TrimRight: string;
    function Trim: string;
    property Length: SizeInt read GetLength;
  end;

function SameStr(const S1, S2: string): Boolean; inline;

implementation

function SameStr(const S1, S2: string): Boolean;
begin
  Result := CompareStr(S1, S2) = 0;
end;

{ TIntegerHelper }

function TIntegerHelper.ToString: string;
begin
  Result := IntToStr(Self);
end;

{ TStringHelper }

function HaveChar(AChar: Char; const AList: array of Char): Boolean;
var
  I: SizeInt;
begin
  I := 0;
  Result := False;
  while (not Result) and (I < Length(AList)) do
  begin
    Result := (AList[I] = AChar);
    Inc(I);
  end;
end;

function TStringHelper.GetLength: SizeInt;
begin
  Result := System.Length(Self);
end;

class function TStringHelper.Equals(const A: string; const B: string): Boolean;
begin
  Result := A = B;
end;

class function TStringHelper.ToInteger(const S: string): Integer;
begin
  Result := StrToInt(S);
end;

function TStringHelper.CompareTo(const B: string): Integer;
begin
  Result := SysUtils.StrComp(PChar(Self), PChar(B));
end;

function TStringHelper.Contains(const AValue: string): Boolean;
begin
  Result := Pos(AValue, Self) > 0;
end;

function TStringHelper.EndsWith(const AValue: string): Boolean;
begin
  Result := EndsWith(AValue, False);
end;

function TStringHelper.EndsWith(const AValue: string;
  AIgnoreCase: Boolean): Boolean;
var
  L: SizeInt;
  S: string;
begin
  L := System.Length(AVAlue);
  Result := L = 0;
  if not Result then
  begin
    S := System.Copy(Self, Length - L + 1, L);
    Result := System.Length(S) = L;
    if Result then
      if AIgnoreCase then
        Result := CompareText(S, AValue) = 0
      else
        Result := S = AValue;
  end;
end;

function TStringHelper.Equals(const AValue: string): Boolean;
begin
  Result := (Self = AValue);
end;

function TStringHelper.IndexOf(const AValue: string; StartIndex: SizeInt;
  ACount: SizeInt): SizeInt;
var
  S: string;
begin
  S := System.Copy(Self, StartIndex + 1, ACount);
  Result := Pos(AValue, S) - 1;
  if Result <> -1 then
    Result := Result + StartIndex;
end;

function TStringHelper.IndexOfUnQuoted(const AValue: string;
  AStartQuote, AEndQuote: Char; AStartIndex: SizeInt = 0): SizeInt;

var
  LV: SizeInt;

  function MatchAt(I: SizeInt): Boolean; inline;
  var
    J: SizeInt;
  begin
    J := 1;
    repeat
      Result := (Self[I + J - 1] = AValue[J]);
      Inc(J);
    until (not Result) or (J > LV);
  end;

var
  I, L, Q: SizeInt;
begin
  Result := -1;
  LV := System.Length(AValue);
  L := Length - LV + 1;
  if L < 0 then
    L := 0;
  I := AStartIndex + 1;
  Q := 0;
  if AStartQuote = AEndQuote then
  begin
    while (Result = -1) and (I <= L) do
    begin
      if (Self[I] = AStartQuote) then
        Q := 1 - Q;
      if (Q = 0) and MatchAt(I) then
        Result := I - 1;
      Inc(I);
    end;
  end
  else
  begin
    while (Result = -1) and (I <= L) do
    begin
      if Self[I] = AStartQuote then
        Inc(Q)
      else if (Self[I] = AEndQuote) and (Q > 0) then
        Dec(Q);
      if (Q = 0) and MatchAt(I) then
        Result := I - 1;
      Inc(I);
    end;
  end;
end;

function TStringHelper.IndexOfAny(const AAnyOf: array of Char;
  AStartIndex: SizeInt): SizeInt;
begin
  Result := IndexOfAny(AAnyOf, AStartIndex, Length);
end;

function TStringHelper.IndexOfAny(const AAnyOf: array of Char;
  AStartIndex: SizeInt; ACount: SizeInt): SizeInt;
var
  I, L: SizeInt;
begin
  I := AStartIndex + 1;
  L := I + ACount - 1;
  if L > Length then
    L := Length;
  Result := -1;
  while (Result = -1) and (I <= L) do
  begin
    if HaveChar(Self[I], AAnyOf) then
      Result := I - 1;
    Inc(I);
  end;
end;

function TStringHelper.IndexOfAnyUnquoted(const AAnyOf: array of Char;
  AStartQuote, AEndQuote: Char; AStartIndex: SizeInt): SizeInt;
begin
  Result := IndexOfAnyUnquoted(AAnyOf, AStartQuote, AEndQuote,
    AStartIndex, Length);
end;

function TStringHelper.IndexOfAnyUnquoted(const AAnyOf: array of Char;
  AStartQuote, AEndQuote: Char; AStartIndex: SizeInt; ACount: SizeInt): SizeInt;
var
  I, L: SizeInt;
  Q: SizeInt;
begin
  Result := -1;
  L := AStartIndex + ACount - 1;
  if L > Length then
    L := Length;
  I := AStartIndex + 1;
  Q := 0;
  if AStartQuote = AEndQuote then
    while (Result = -1) and (I <= L) do
    begin
      if (Self[I] = AStartQuote) then
        Q := 1 - Q;
      if (Q = 0) and HaveChar(Self[I], AAnyOf) then
        Result := I - 1;
      Inc(I);
    end
  else
    while (Result = -1) and (I <= L) do
    begin
      if Self[I] = AStartQuote then
        Inc(Q)
      else if (Self[I] = AEndQuote) and (Q > 0) then
        Dec(Q);
      if (Q = 0) and HaveChar(Self[I], AAnyOf) then
        Result := I - 1;
      Inc(I);
    end;
end;

function TStringHelper.IsEmpty: Boolean;
begin
  Result := (Length = 0);
end;

function TStringHelper.Split(const ASeparators: array of Char;
  AOptions: TStringSplitOptions): TStringArray;
begin
  Result := Split(ASeparators, Length, AOptions);
end;

function TStringHelper.Split(const ASeparators: array of Char;
  ACount: SizeInt; AOptions: TStringSplitOptions): TStringArray;
begin
  Result := Split(ASeparators, #0, #0, ACount, AOptions);
end;

function TStringHelper.Split(const ASeparators: array of Char;
  AQuoteStart, AQuoteEnd: Char; ACount: SizeInt;
  AOptions: TStringSplitOptions): TStringArray;

const
  BlockSize = 10;

  function NextSep(AStartIndex: SizeInt): SizeInt;
  begin
    if (AQuoteStart <> #0) then
      Result := Self.IndexOfAnyUnQuoted(ASeparators, AQuoteStart, AQuoteEnd,
        AStartIndex)
    else
      Result := Self.IndexOfAny(ASeparators, AStartIndex);
  end;

  procedure MaybeGrow(ACurLen: SizeInt);
  begin
    if System.Length(Result) <= ACurLen then
      SetLength(Result, System.Length(Result) + BlockSize);
  end;

var
  Sep, LastSep, Len: SizeInt;
  T: string;
begin
  SetLength(Result, BlockSize);
  Len := 0;
  LastSep := 0;
  Sep := NextSep(0);
  while (Sep <> -1) and ((ACount = 0) or (Len < ACount)) do
  begin
    T := SubString(LastSep, Sep - LastSep);
    if (T <> '') or (not (TStringSplitOptions.ExcludeEmpty = AOptions)) then
    begin
      MaybeGrow(Len);
      Result[Len] := T;
      Inc(Len);
    end;
    LastSep := Sep + 1;
    Sep := NextSep(LastSep);
  end;
  if (LastSep < Length) and ((ACount = 0) or (Len < ACount)) then
  begin
    T := SubString(LastSep);
    MaybeGrow(Len);
    Result[Len] := T;
    Inc(Len);
  end;
  SetLength(Result, Len);
end;

function TStringHelper.StartsWith(const AValue: string): Boolean;
begin
  Result := StartsWith(AValue, False);
end;

function TStringHelper.StartsWith(const AValue: string;
  AIgnoreCase: Boolean): Boolean;
var
  L: SizeInt;
  S: string;
begin
  L := System.Length(AValue);
  Result := L <= 0;
  if not Result then
  begin
    S := System.Copy(Self, 1, L);
    Result := (System.Length(S) = L);
    if Result then
      if AIgnoreCase then
        Result := SameText(S, aValue)
      else
        Result := SameStr(S, AValue);
  end;
end;

function TStringHelper.SubString(AStartIndex: SizeInt): string;
begin
  Result := Self.SubString(AStartIndex, Self.Length - AStartIndex);
end;

function TStringHelper.SubString(AStartIndex: SizeInt; ALen: SizeInt): string;
begin
  Result := System.Copy(Self, AStartIndex + 1, ALen);
end;

function TStringHelper.ToInteger: Integer;
begin
  Result := StrToInt(Self);
end;

function TStringHelper.ToLower: string;
begin
  Result := LowerCase(Self);
end;

function TStringHelper.ToUpper: string;
begin
  Result := UpperCase(Self);
end;

function TStringHelper.Trim: string;
begin
  Result := Self.TrimLeft.TrimRight;
end;

function TStringHelper.TrimLeft: string;
var
  I, Len: SizeInt;
begin
  I := 1;
  Len := Self.Length;
  while (I <= Len) and HaveChar(Self[I], TArray<Char>(Self)) do
    Inc(I);
  if I = 1 then
    Result := Self
  else if I > Len then
    Result := ''
  else
    Result := System.Copy(Self, I, Len - I + 1);
end;

function TStringHelper.TrimRight: string;
var
  I, Len: SizeInt;
begin
  Len := Self.Length;
  I := Len;
  while (I >= 1) and HaveChar(Self[I], TArray<Char>(Self)) do
    Dec(I);
  if I < 1 then
    Result := ''
  else if I = Len then
    Result := Self
  else
    Result := System.Copy(Self, 1, I);
end;

end.
