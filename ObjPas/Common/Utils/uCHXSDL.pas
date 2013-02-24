unit uCHXSDL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  sdl;
type
  ESDLError = Class(Exception);

function InitSDL(aWidth, aHeight, aBPP: integer; video_flags: UInt32): PSDL_Surface;
procedure QuitSDL;


implementation

function InitSDL(aWidth, aHeight, aBPP: integer; video_flags: UInt32): PSDL_Surface;
begin
  Result := nil;
  video_flags := video_flags or SDL_HWSURFACE or SDL_ANYFORMAT or SDL_DOUBLEBUF;
  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
    raise ESDLError.CreateFmt('InitSDL: SDL_Init Error: %s', [SDL_GetError]);

  Result := SDL_SetVideoMode(aWidth, aHeight, aBPP, video_flags);

  if (Result = nil) then
    raise ESDLError.CreateFmt('InitSDL: SDL_SetVideoMode Error: %s', [SDL_GetError]);
end;

procedure QuitSDL;
begin
  SDL_Quit;
end;

finalization
  QuitSDL;

end.

