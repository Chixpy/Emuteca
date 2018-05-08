unit ufETKGUIScriptManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  uCHXStrUtils,
  ufrCHXForm, ufCHXScriptManager,
  ucEmuteca, uEmutecaCommon,
  ucEmutecaScriptEngine;

type

  { TfmLEmuTKScriptManager }

  TfmLEmuTKScriptManager = class(TfmCHXScriptManager)
  private
    FEmuteca: cEmuteca;
    procedure SetEmuteca(AValue: cEmuteca);

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;

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

procedure TfmLEmuTKScriptManager.DoClearFrameData;
begin

end;

procedure TfmLEmuTKScriptManager.DoLoadFrameData;
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

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

constructor TfmLEmuTKScriptManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  slvFiles.Mask := krsFileMaskScript;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
end;

destructor TfmLEmuTKScriptManager.Destroy;
begin
  inherited Destroy;
end;

end.
