unit uScriptEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCustomUtils;

type

  { cScriptEngine }

  cScriptEngine = class (TObject)

  private
    FScriptFile: string;
    FScriptText: TStringList;
    procedure SetScriptFile(AValue: string);
    procedure SetScriptText(AValue: TStringList);
  public
    property ScriptFile: string read FScriptFile write SetScriptFile;
    property ScriptText: TStringList read FScriptText write SetScriptText;

    procedure Run;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cScriptEngine }

procedure cScriptEngine.SetScriptFile(AValue: string);
begin
  FScriptFile:=SetAsFile(AValue);
end;

procedure cScriptEngine.SetScriptText(AValue: TStringList);
begin
  if FScriptText=AValue then Exit;
  FScriptText:=AValue;
end;

procedure cScriptEngine.Run;
begin

end;

constructor cScriptEngine.Create;
begin
  FScriptText := TStringList.Create;
end;

destructor cScriptEngine.Destroy;
begin
  FreeAndNil(FScriptText);
  inherited Destroy;
end;

end.

