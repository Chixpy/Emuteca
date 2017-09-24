unit ufLEmuTKScriptManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  uCHXStrUtils,
  ufCHXForm, ufCHXScriptManager,
  ucEmuteca, uEmutecaCommon,
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

    procedure CreateCustomEngine; override;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure SetBaseFolder(const aFolder: string); override;

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

procedure TfmLEmuTKScriptManager.CreateCustomEngine;
var
  aScriptEngine: cEmutecaScriptEngine;
begin
  // Setting before inherited call
  aScriptEngine := cEmutecaScriptEngine.Create;
  aScriptEngine.Emuteca := Emuteca;
  ScriptEngine := aScriptEngine;

  inherited CreateCustomEngine;
end;

procedure TfmLEmuTKScriptManager.SetBaseFolder(const aFolder: string);
begin
  inherited SetBaseFolder(aFolder);
  ScriptEngine.CommonUnitFolder := SetAsFolder(aFolder) + 'Units';
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
begin
  inherited Create(TheOwner);

  slvGeneral.Mask := krsFileMaskScript;
end;

destructor TfmLEmuTKScriptManager.Destroy;
begin
  inherited Destroy;
end;

end.
