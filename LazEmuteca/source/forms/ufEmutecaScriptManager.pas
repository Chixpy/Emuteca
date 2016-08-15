unit ufEmutecaScriptManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ufCHXScriptManager, ucEmutecaScriptEngine,
  ucEmuteca;

type

  { TfrmEmutecaScriptManager }

  TfrmEmutecaScriptManager = class(TfrmCHXScriptManager)
    procedure FormCreate(Sender: TObject);
  private
    FEmuteca: cEmuteca;
    procedure SetEmuteca(AValue: cEmuteca);
    { private declarations }
  public
    { public declarations }
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
  end;

var
  frmEmutecaScriptManager: TfrmEmutecaScriptManager;

implementation

{$R *.lfm}

{ TfrmEmutecaScriptManager }

procedure TfrmEmutecaScriptManager.FormCreate(Sender: TObject);
var
  aScriptEngine: cEmutecaScriptEngine;
begin
  // Creating custom script engine
  // freed automatically
  aScriptEngine := cEmutecaScriptEngine.Create;
  aScriptEngine.Emuteca := Emuteca;
  ScriptEngine := aScriptEngine;
end;

procedure TfrmEmutecaScriptManager.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca=AValue then Exit;
  FEmuteca:=AValue;

  if assigned(ScriptEngine) then
    cEmutecaScriptEngine(ScriptEngine).Emuteca := Emuteca;
end;

end.
