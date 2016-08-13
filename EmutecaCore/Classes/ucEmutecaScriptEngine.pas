unit ucEmutecaScriptEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ucEmutecaEmulator,
  ucCHXScriptEngine,

  uPSI_ucEmutecaEmulator;

type

  { cEmutecaScriptEngine }

  cEmutecaScriptEngine = class(cCHXScriptEngine)
  public
    {
  property GameManager: cEmutecaGameManager;
    property Game: cEmutecaGameVersion;
    property GameGroup: cEmutecaGameFamily;
        }
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cEmutecaScriptEngine }

constructor cEmutecaScriptEngine.Create;
begin
  inherited;
end;

destructor cEmutecaScriptEngine.Destroy;
begin
  inherited;
end;

end.

