unit ufEmutecaActExportSoftData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, EditBtn, ComCtrls,
  uCHXDlgUtils,
  ufrCHXForm,
  ufCHXPropEditor, ufCHXProgressBar,
  uEmutecaCommon,
  uaEmutecaCustomSystem,
  ucEmuteca, ucEmutecaSystem, ucEmutecaSoftware,
  ufEmutecaSystemCBX;

type

  { TfmActExportSoftData }

  TfmActExportSoftData = class(TfmCHXPropEditor)
    actSoftDataCheckAll: TAction;
    bSoftDataCheckAll: TToolButton;
    cgbSoftData: TCheckGroup;
    eExportFile: TFileNameEdit;
    eSoftIDType: TEdit;
    gbxExportFile: TGroupBox;
    gbxSystemInfo: TGroupBox;
    lSoftIDType: TLabel;
    lWarning: TLabel;
    pSelectSystem: TPanel;
    tbSoftData: TToolBar;
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

    procedure CheckAllSoftData;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

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
var
  aSoft: cEmutecaSoftware;
  IsCached: boolean;
  i: integer;
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
        aSoft := System.SoftManager.FullList[i];
        IsCached := not aSoft.SHA1IsEmpty;
        Inc(i);
      end;
    end;

    eExportFile.Enabled := IsCached;

    if not IsCached then
    begin
      lWarning.Caption :=  Format(rsExportingNoSHA1,
         [aSoft.Folder, aSoft.FileName, i, System.SoftManager.FullList.Count]);
      eExportFile.FileName := '';
    end
    else
    begin
      lWarning.Caption := '';
      eExportFile.FileName :=
        ExtractFilePath(eExportFile.FileName) + System.FileName +
        krsFileExtSoft;
    end;
  end
  else
  begin
    eSoftIDType.Clear;
    lWarning.Caption := '';
    eExportFile.Enabled := False;
  end;
end;

function TfmActExportSoftData.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := True;

  System := aSystem;
end;

procedure TfmActExportSoftData.CheckAllSoftData;
var
  i: Integer;
begin
  i := 0;
  while i < cgbSoftData.Items.Count do
  begin
    cgbSoftData.Checked[i] := True;
    inc(i);
  end;
end;

procedure TfmActExportSoftData.DoClearFrameData;
begin
  eSoftIDType.Clear;
  lWarning.Caption := '';
end;

procedure TfmActExportSoftData.DoLoadFrameData;
begin
  Enabled := Assigned(Emuteca);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmActExportSoftData.DoSaveFrameData;
var
  PCB: TEmutecaProgressCallBack;
begin
  if (eExportFile.FileName = '') or (not assigned(System)) then
    Exit;

  PCB := System.ProgressCallBack;
  System.ProgressCallBack := @(frmCHXProgressBar.UpdTextAndBar);

  System.SaveSoftGroupLists(ChangeFileExt(eExportFile.FileName, ''), True);

  System.ProgressCallBack := PCB;
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
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, 'Export soft data']);

    aFrame := TfmActExportSoftData.Create(aForm);
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.Emuteca := aEmuteca;

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
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

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;

  // If frmCHXProgressBar is not created...
  if not Assigned(frmCHXProgressBar) then
    Application.CreateForm(TfrmCHXProgressBar, frmCHXProgressBar);

  eExportFile.Filter := rsFileMaskDescSoft + '|' + krsFileMaskSoft;

  CheckAllSoftData;
end;

destructor TfmActExportSoftData.Destroy;
begin
  inherited Destroy;
end;

end.
