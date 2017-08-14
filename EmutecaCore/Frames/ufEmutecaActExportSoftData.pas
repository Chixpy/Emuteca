unit ufEmutecaActExportSoftData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, EditBtn,
  uCHXDlgUtils,
  ufCHXForm, ufCHXPropEditor, ufCHXProgressBar,
  uEmutecaCommon,
  uaEmutecaCustomSystem,
  ucEmuteca, ucEmutecaSystem,
  ufEmutecaSystemCBX;

type

  { TfmActExportSoftData }

  TfmActExportSoftData = class(TfmCHXPropEditor)
    eExportFile: TFileNameEdit;
    eSoftIDType: TEdit;
    gbxExportFile: TGroupBox;
    gbxSystemInfo: TGroupBox;
    lSoftIDType: TLabel;
    pSelectSystem: TPanel;
    procedure eExportFileButtonClick(Sender: TObject);

  private
    FEmuteca: cEmuteca;
    FfmSystemCBX: TfmEmutecaSystemCBX;
    FSystem: cEmutecaSystem;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    property fmSystemCBX: TfmEmutecaSystemCBX read FfmSystemCBX;

    property System: cEmutecaSystem read FSystem write SetSystem;

    function SelectSystem(aSystem: cEmutecaSystem): boolean;


    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure SaveFrameData; override;

    // Creates a form with AddFolder frame.
    class function SimpleForm(aEmuteca: cEmuteca; aGUIIconsIni: string;
      aGUIConfigIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmActExportSoftData }

procedure TfmActExportSoftData.eExportFileButtonClick(Sender: TObject);
begin
  SetFileEditInitialDir(eExportFile, ProgramDirectory);
end;

procedure TfmActExportSoftData.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  if assigned(Emuteca) then
    fmSystemCBX.SystemList := Emuteca.SystemManager.EnabledList
  else
    fmSystemCBX.SystemList := nil;
  fmSystemCBX.SelectedSystem := nil;

  LoadFrameData;
end;

procedure TfmActExportSoftData.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;
end;

function TfmActExportSoftData.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := True;

  System := aSystem;

  if Assigned(System) then
  begin
    eSoftIDType.Text := EmutecaSoftExportKey2StrK(System.SoftExportKey);
  end
  else
  begin
    eSoftIDType.Clear;
  end;
end;

procedure TfmActExportSoftData.ClearFrameData;
begin
  eSoftIDType.Clear;
end;

procedure TfmActExportSoftData.LoadFrameData;
begin
  Enabled := Assigned(Emuteca);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmActExportSoftData.SaveFrameData;
var
  PCBSoft, PCBGroup: TEmutecaProgressCallBack;
begin
  if (eExportFile.FileName = '') or (not assigned(System)) then
    Exit;

  PCBSoft := System.SoftManager.ProgressCallBack;
  PCBGroup := System.GroupManager.ProgressCallBack;
  System.SoftManager.ProgressCallBack := @(frmCHXProgressBar.UpdTextAndBar);
  System.GroupManager.ProgressCallBack := @(frmCHXProgressBar.UpdTextAndBar);

  System.SaveLists(ChangeFileExt(eExportFile.FileName, ''), True);

  System.SoftManager.ProgressCallBack := PCBSoft;
  System.GroupManager.ProgressCallBack := PCBGroup;
end;

class function TfmActExportSoftData.SimpleForm(aEmuteca: cEmuteca;
  aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmActExportSoftData;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmEmutecaActExportSoftData';
    aForm.Caption := Format(rsFmtWindowCaption,
      [Application.Title, 'Export soft data']);

    aFrame := TfmActExportSoftData.Create(aForm);
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.Emuteca := aEmuteca;

    aForm.GUIConfigIni := aGUIConfigIni;
    aForm.GUIIconsIni := aGUIIconsIni;
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

constructor TfmActExportSoftData.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmSystemCBX := TfmEmutecaSystemCBX.Create(pSelectSystem);
    fmSystemCBX.Align := alTop;
    fmSystemCBX.FirstItem := ETKSysCBXFISelect;
    fmSystemCBX.OnSelectSystem := @SelectSystem;
    fmSystemCBX.Parent := pSelectSystem;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;

  // If frmCHXProgressBar is not created...
  if not Assigned(frmCHXProgressBar) then
    Application.CreateForm(TfrmCHXProgressBar, frmCHXProgressBar);

  eExportFile.Filter := rsEmutecaSoftFileMaskDesc + '|' + krsEmutecaSoftFileMask;
end;

destructor TfmActExportSoftData.Destroy;
begin
  inherited Destroy;
end;

end.
