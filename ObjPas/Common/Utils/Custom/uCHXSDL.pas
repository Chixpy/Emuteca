unit uCHXSDL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  sdl;

resourcestring
  rseSDLVideoInit = 'InitSDL: SDL_Init Error: %s';

  rseSDLInvalidTSDLRect = '"%s" is an invalid TSDLRect.';

type
  ESDLError = Class(Exception);

// TSDL_Rect related operators
// ---------------------------
operator = (const a, b: TSDL_Rect): boolean;
{< Comparison between TSDL_Rect }
operator := (const Value : string) : TSDL_Rect;
{< Converting to string }
operator := (const Value : TSDL_Rect) : string;
{< Converting from string }

function CHXSDLInit(aWidth, aHeight, aBPP: integer; video_flags: UInt32): PSDL_Surface;
{< SDL initialization }
procedure CHXSDLQuit;
{< SDL quit }


implementation

operator = (const a, b: TSDL_Rect): boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y) and
    (a.w = b.w) and (a.h = b.h);
end;

operator := (const Value: string): TSDL_Rect;
  var
    Position: integer;
    tmpStr: string;
  begin
    try
      tmpStr := Value;
      Position := Pos(',', tmpStr);
      Result.x := StrToInt(Trim(Copy(tmpStr, 1, Position - 1)));

      tmpStr := Trim(Copy(tmpStr, Position + 1, MaxInt));
      Position := Pos(',', tmpStr);
      Result.y := StrToInt(Trim(Copy(tmpStr, 1, Position - 1)));

      tmpStr := Trim(Copy(tmpStr, Position + 1, MaxInt));
      Position := Pos(',', tmpStr);
      Result.w := StrToInt(Trim(Copy(tmpStr, 1, Position - 1)));

      Result.h := StrToInt(Trim(Copy(tmpStr, Position + 1, MaxInt)));
    except
      raise EConvertError.CreateFmt(rseSDLInvalidTSDLRect, [Value]);
    end;
end;

operator := (const Value: TSDL_Rect): string;
begin
  Result := IntToStr(Value.x) + ',' + IntToStr(Value.y) + ',' +
    IntToStr(Value.w) + ',' + IntToStr(Value.h);
end;

function CHXSDLInit(aWidth, aHeight, aBPP: integer; video_flags: UInt32): PSDL_Surface;
begin
  Result := nil;
  video_flags := video_flags or SDL_HWSURFACE or SDL_ANYFORMAT or SDL_DOUBLEBUF;
  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
    raise ESDLError.CreateFmt(rseSDLVideoInit, [SDL_GetError]);

  Result := SDL_SetVideoMode(aWidth, aHeight, aBPP, video_flags);

  if (Result = nil) then
    raise ESDLError.CreateFmt(rseSDLVideoInit, [SDL_GetError]);
end;

procedure CHXSDLQuit;
begin
  SDL_Quit;
end;

finalization
  CHXSDLQuit;

end.

