unit ufEmutecaActImportSoftData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, EditBtn, ufCHXPropEditor, ufCHXForm,
  ufCHXProgressBar, ucEmuteca, uCHXDlgUtils, uEmutecaCommon,
  ucEmutecaSystem, ufEmutecaSystemCBX;

type

  { TfmEmutecaActImportSoftData }

  TfmEmutecaActImportSoftData = class(TfmCHXPropEditor)
    eImportFile: TFileNameEdit;
    eSoftIDType: TEdit;
    gbxImportFile: TGroupBox;
    gbxSystemInfo: TGroupBox;
    lSoftIDType: TLabel;
    lWarning: TLabel;
    pSelectSystem: TPanel;
    procedure eImportFileButtonClick(Sender: TObject);
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

{ TfmEmutecaActImportSoftData }

procedure TfmEmutecaActImportSoftData.SetSystem(AValue: cEmutecaSystem);
var
  IsCached: Boolean;
  i: Integer;
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  if Assigned(System) then
  begin
    eSoftIDType.Text := SoftExportKey2StrK(System.SoftExportKey);

    // Testing if all files have SHA1 cached
    IsCached := True;
    if System.SoftExportKey = TEFKSHA1 then
    begin
      i := 0;
      while IsCached and (i < System.SoftManager.FullList.Count) do
      begin
        IsCached := not System.SoftManager.FullList[i].SHA1IsEmpty;
        Inc(i);
      end;
    end;

    if not IsCached then
      lWarning.Caption := 'Warning: Some info could not be imported because some file haven''t got SHA1 cached.'
    else
      lWarning.Caption := '';

    eImportFile.Enabled := True;
  end
  else
  begin
    eSoftIDType.Clear;
    lWarning.Caption := '';
    eImportFile.Enabled := False;
  end;

end;

procedure TfmEmutecaActImportSoftData.eImportFileButtonClick(Sender: TObject);
begin
  SetFileEditInitialDir(eImportFile, ProgramDirectory);
end;

procedure TfmEmutecaActImportSoftData.SetEmuteca(AValue: cEmuteca);
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

procedure TfmEmutecaActImportSoftData.ClearFrameData;
begin

end;

procedure TfmEmutecaActImportSoftData.LoadFrameData;
begin
  Enabled := Assigned(Emuteca);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

function TfmEmutecaActImportSoftData.SelectSystem(aSystem:
  cEmutecaSystem): boolean;
begin
  Result := True;

  System := aSystem;
end;

procedure TfmEmutecaActImportSoftData.SaveFrameData;
var
  PCB: TEmutecaProgressCallBack;
begin
  if (eImportFile.FileName = '') or (not assigned(System)) then
    Exit;

  PCB := System.ProgressCallBack;
  System.ProgressCallBack := @(frmCHXProgressBar.UpdTextAndBar);

  System.ImportSoftGroupLists(ChangeFileExt(eImportFile.FileName, ''));

  System.ProgressCallBack := PCB;
end;

class function TfmEmutecaActImportSoftData.SimpleForm(aEmuteca: cEmuteca;
  aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmEmutecaActImportSoftData;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmEmutecaActImportSoftData';
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, 'Import soft data...']);

    aFrame := TfmEmutecaActImportSoftData.Create(aForm);
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

constructor TfmEmutecaActImportSoftData.Create(TheOwner: TComponent);

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

  // Add
  eImportFile.Filter := rsFileMaskDescSoft + '|' + krsFileMaskSoft;

  // If frmCHXProgressBar is not created...
  if not Assigned(frmCHXProgressBar) then
    Application.CreateForm(TfrmCHXProgressBar, frmCHXProgressBar);
end;

destructor TfmEmutecaActImportSoftData.Destroy;
begin
  inherited Destroy;
end;

end.
