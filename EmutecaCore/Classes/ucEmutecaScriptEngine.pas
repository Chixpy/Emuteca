unit ucEmutecaScriptEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ucCHXScriptEngine, uPSI_ucEmutecaEmulator;

type

  { cEmutecaScriptEngine }

  cEmutecaScriptEngine = class(cCHXScriptEngine)
  public
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

