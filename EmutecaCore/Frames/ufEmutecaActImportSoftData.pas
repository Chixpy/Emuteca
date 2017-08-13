unit ufEmutecaActImportSoftData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, EditBtn, ufCHXPropEditor, ufCHXForm,
  ufCHXProgressBar, uCHXStrUtils, ucEmuteca, uCHXDlgUtils, uEmutecaCommon,
  ucEmutecaSystem, ufEmutecaSystemCBX;

type

  { TfmEmutecaActImportSoftData }

  TfmEmutecaActImportSoftData = class(TfmCHXPropEditor)
    eImportFile: TFileNameEdit;
    eSoftIDType: TEdit;
    gbxImportFile: TGroupBox;
    gbxSystemInfo: TGroupBox;
    lSoftIDType: TLabel;
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
begin
  if FSystem = AValue then Exit;
  FSystem := AValue;
end;

procedure TfmEmutecaActImportSoftData.eImportFileButtonClick(Sender: TObject);
begin
    SetFileEditInitialDir(eImportFile, ProgramDirectory);
end;

procedure TfmEmutecaActImportSoftData.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then Exit;
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

function TfmEmutecaActImportSoftData.SelectSystem(aSystem: cEmutecaSystem
  ): boolean;
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

procedure TfmEmutecaActImportSoftData.SaveFrameData;
var
  PrevCallBack: TEmutecaProgressCallBack;
begin
    if (eImportFile.FileName = '') or (not assigned(System)) then
    Exit;

  PrevCallBack := System.SoftManager.ProgressCallBack;
  System.SoftManager.ProgressCallBack := @(frmCHXProgressBar.UpdTextAndBar);
  if SupportedExtCT(eImportFile.FileName, krsEmutecaINIFileExt) then
    System.SoftManager.ImportFromFileIni(eImportFile.FileName)
  else if SupportedExtCT(eImportFile.FileName, krsEmutecaSoftFileExt) then
    System.SoftManager.ImportFromFileStrLst(eImportFile.FileName);
  System.SoftManager.ProgressCallBack := PrevCallBack;

  // TODO: Import groups data
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
    aForm.Caption := Format(rsFmtWindowCaption,
      [Application.Title, 'Export soft data']);

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
  eImportFile.Filter := rsEmutecaINIFileMaskDesc + '|' +
        krsEmutecaINIFileMask + ';' + krsEmutecaSoftFileMask;

  // If frmCHXProgressBar is not created...
  if not Assigned(frmCHXProgressBar) then
    Application.CreateForm(TfrmCHXProgressBar, frmCHXProgressBar);
end;

destructor TfmEmutecaActImportSoftData.Destroy;
begin
  inherited Destroy;
end;

end.

