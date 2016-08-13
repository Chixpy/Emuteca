unit ufEmutecaScriptManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ufCHXScriptManager, ucEmutecaScriptEngine;

type

  { TfrmEmutecaScriptManager }

  TfrmEmutecaScriptManager = class(TfrmCHXScriptManager)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
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
  ScriptEngine := aScriptEngine;
end;

end.
