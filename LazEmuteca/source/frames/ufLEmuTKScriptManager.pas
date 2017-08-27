unit ufLEmuTKScriptManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ufCHXScriptManager, ucEmuteca, uEmutecaCommon, ufCHXForm,
  ucEmutecaScriptEngine;

type

  { TfmLEmuTKScriptManager }

  TfmLEmuTKScriptManager = class(TfmCHXScriptManager)
  private
    FEmuteca: cEmuteca;
    procedure SetEmuteca(AValue: cEmuteca);

  protected
    procedure ClearFrameData; override;
    procedure LoadFrameData; override;
  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    // Creates a form with Script Manager.
    class function SimpleForm(aEmuteca: cEmuteca; aBaseFolder: string;
      aGUIIconsIni: string; aGUIConfigIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKScriptManager }

procedure TfmLEmuTKScriptManager.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  LoadFrameData;
end;

procedure TfmLEmuTKScriptManager.ClearFrameData;
begin

end;

procedure TfmLEmuTKScriptManager.LoadFrameData;
begin
  Enabled := Assigned(Emuteca);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  if assigned(ScriptEngine) then
    cEmutecaScriptEngine(ScriptEngine).Emuteca := Emuteca;

end;

class function TfmLEmuTKScriptManager.SimpleForm(aEmuteca: cEmuteca;
  aBaseFolder: string; aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmLEmuTKScriptManager;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmLEmuTKScriptManager';
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, 'Script Manager']);

    aFrame := TfmLEmuTKScriptManager.Create(aForm);
    aFrame.Align := alClient;

    aFrame.SetBaseFolder(aBaseFolder);
    aFrame.Emuteca := aEmuteca;

    aForm.GUIConfigIni := aGUIConfigIni;
    aForm.GUIIconsIni := aGUIIconsIni;
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

constructor TfmLEmuTKScriptManager.Create(TheOwner: TComponent);
var
  aScriptEngine: cEmutecaScriptEngine;
begin
  inherited Create(TheOwner);

  slvGeneral.Mask := krsFileMaskScript;

  // Creating custom script engine
  // freed automatically
  aScriptEngine := cEmutecaScriptEngine.Create;
  aScriptEngine.Emuteca := Emuteca;
  ScriptEngine := aScriptEngine;
end;

destructor TfmLEmuTKScriptManager.Destroy;
begin
  inherited Destroy;
end;

end.
