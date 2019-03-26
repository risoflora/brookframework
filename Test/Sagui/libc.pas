unit libc;

interface

uses
{$IF DEFINED(MSWINDOWS)}
  Windows
{$ELSEIF DEFINED(FPC) AND DEFINED(UNIX)}
  BaseUnix
{$ELSEIF DEFINED(POSIX)}
  Posix.Errno
{$ENDIF},
  libsagui;

const
{$IF DEFINED(FPC) AND DEFINED(UNIX)}
  EINTR = ESysEINTR;
{$ELSEIF DEFINED(POSIX)}
  EINTR = Posix.Errno.EINTR;
{$ELSEIF DEFINED(MSWINDOWS)}
  EINTR = 4;
{$ENDIF}

const
  LIB_NAME = {$IFDEF MSWINDOWS}'msvcrt'{$ELSE}'c'{$ENDIF};

function strerror(errnum: cint): Pcchar; cdecl; external LIB_NAME;

function sprintf(s: Pcchar; const format: Pcchar): cint; cdecl; varargs; external LIB_NAME;

function strcmp(const s1: Pcchar; const s2: Pcchar): cint; cdecl; external LIB_NAME;

function strlen(const s: Pcchar): cint; cdecl; external LIB_NAME;

function strcpy(dest: Pcchar; const src: Pcchar): Pcchar; cdecl; external LIB_NAME;

function strcat (dest: Pcchar; const src: Pcchar): Pcchar; cdecl; external LIB_NAME;

procedure memset(s: Pcvoid; c: cint; n: csize_t); cdecl; external LIB_NAME;

function errno: cint; inline;

procedure seterrno(errnum: cint); inline;

function _errno: PLongInt; cdecl;
  external LIB_NAME name {$IFDEF LINUX}'__errno_location'{$ELSE}'_errno'{$ENDIF};

implementation

function errno: cint;
begin
  Result := _errno()^;
end;

procedure seterrno(errnum: cint);
begin
  _errno()^ := errnum;
end;

end.
