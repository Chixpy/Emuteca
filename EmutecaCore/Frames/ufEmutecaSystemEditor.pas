unit ufEmutecaSystemEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, CheckLst, EditBtn, LazFileUtils,
  uCHXStrUtils, uCHXDlgUtils,
  ufCHXPropEditor,
  ucEmutecaSystem, ucEmutecaEmulator,
  ucEmutecaEmulatorManager, uaEmutecaCustomSystem;

resourcestring
  rsSelectEmulator = 'Select a System';

type

  { TfmEmutecaSystemEditor }

  TfmEmutecaSystemEditor = class(TfmCHXPropEditor)
    cbxMainEmulator: TComboBox;
    chkExtractAllFiles: TCheckBox;
    clbOtherEmulators: TCheckListBox;
    eBaseFolder: TDirectoryEdit;
    eExtraInfoFilename: TEdit;
    eWorkingFolder: TDirectoryEdit;
    eTitle: TEdit;
    gbxBasicInfo: TGroupBox;
    gbxEmulators: TGroupBox;
    gbxFiles: TGroupBox;
    lBaseFolder: TLabel;
    lFileExtensions: TLabel;
    lFileName: TLabel;
    lMainEmulator: TLabel;
    lOtherEmulators: TLabel;
    lWorkingFolder: TLabel;
    lTitle: TLabel;
    mExtensions: TMemo;
    Panel2: TPanel;
    rgbGameKey: TRadioGroup;
    Splitter1: TSplitter;
    procedure eBaseFolderButtonClick(Sender: TObject);
    procedure eWorkingFolderButtonClick(Sender: TObject);

  private
    FEmuManager: cEmutecaEmulatorManager;
    FSystem: cEmutecaSystem;
    procedure SetEmuManager(AValue: cEmutecaEmulatorManager);
    procedure SetSystem(AValue: cEmutecaSystem);

    procedure UpdateLists;

  protected

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

  public
    property System: cEmutecaSystem read FSystem write SetSystem;

    property EmuManager: cEmutecaEmulatorManager
      read FEmuManager write SetEmuManager;

    procedure SaveFrameData; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemEditor }

procedure TfmEmutecaSystemEditor.eBaseFolderButtonClick(Sender: TObject);
begin
  SetDirEditInitialDir(eBaseFolder, ProgramDirectory);
end;

procedure TfmEmutecaSystemEditor.eWorkingFolderButtonClick(Sender: TObject);
begin
  SetDirEditInitialDir(eWorkingFolder, ProgramDirectory);
end;

procedure TfmEmutecaSystemEditor.SetEmuManager(AValue:
  cEmutecaEmulatorManager);
begin
  if FEmuManager = AValue then
    Exit;
  FEmuManager := AValue;
  UpdateLists;
  Enabled := Assigned(System) and Assigned(EmuManager);
end;

procedure TfmEmutecaSystemEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;
  LoadFrameData;
  Enabled := Assigned(System) and Assigned(EmuManager);
end;

procedure TfmEmutecaSystemEditor.UpdateLists;
begin
  clbOtherEmulators.Clear;
  cbxMainEmulator.Clear;

  if not assigned(EmuManager) then
    exit;

  EmuManager.AssingEnabledTo(clbOtherEmulators.Items);
  cbxMainEmulator.Items.Assign(clbOtherEmulators.Items);
end;

procedure TfmEmutecaSystemEditor.ClearFrameData;
begin
  eExtraInfoFilename.Clear;
  eTitle.Clear;
  cbxMainEmulator.ItemIndex := -1;
  clbOtherEmulators.CheckAll(cbUnchecked);
  eBaseFolder.Clear;
  eWorkingFolder.Clear;
  rgbGameKey.ItemIndex := 0;
  chkExtractAllFiles.Checked := False;
  mExtensions.Clear;
end;

procedure TfmEmutecaSystemEditor.SaveFrameData;
var
  i, j: integer;
begin
  System.FileName := eExtraInfoFilename.Text;
  System.Title := eTitle.Text;

  if cbxMainEmulator.ItemIndex <> -1 then
    System.MainEmulator :=
      cEmutecaEmulator(cbxMainEmulator.Items.Objects[
      cbxMainEmulator.ItemIndex]).ID;

  // Adding/Removing other emulators,
  // but keeping not listed ones...
  i := 0;
  while i < clbOtherEmulators.Count do
  begin
    if clbOtherEmulators.Checked[i] then
      AddToStringList(System.OtherEmulators,
        cEmutecaEmulator(clbOtherEmulators.Items.Objects[i]).ID)
    else
    begin
      j := System.OtherEmulators.IndexOf(
        cEmutecaEmulator(clbOtherEmulators.Items.Objects[i]).ID);
      if j <> -1 then
        System.OtherEmulators.Delete(j);
    end;
    Inc(i);
  end;

  System.BaseFolder := eBaseFolder.Text;
  System.WorkingFolder := eWorkingFolder.Text;
  system.ExtractAll := chkExtractAllFiles.Checked;

  case rgbGameKey.ItemIndex of
    1: System.GameKey := TEFKCRC32;
    2: System.GameKey := TEFKFileName;
    3: System.GameKey := TEFKCustom;
    else  // SHA1 by default
      System.GameKey := TEFKSHA1;
  end;

  System.Extensions.Assign(mExtensions.Lines);
end;

procedure TfmEmutecaSystemEditor.LoadFrameData;
var
  aEmulator: cEmutecaEmulator;
  i: integer;
begin
  ClearFrameData;

  if (not assigned(System)) or (not assigned(EmuManager)) then
    Exit;

  eTitle.Text := System.Title;
  eExtraInfoFilename.Text := System.FileName;

  aEmulator := EmuManager.FullList.ItemById(System.MainEmulator);
  cbxMainEmulator.ItemIndex := cbxMainEmulator.Items.IndexOfObject(aEmulator);

  i := 0;
  while i < clbOtherEmulators.Count do
  begin
    aEmulator := cEmutecaEmulator(clbOtherEmulators.Items.Objects[i]);
    if System.OtherEmulators.IndexOf(aEmulator.ID) <> -1 then
      clbOtherEmulators.Checked[i] := True
    else
      clbOtherEmulators.Checked[i] := False;
    Inc(i);
  end;

  eBaseFolder.Text := SysPath(System.BaseFolder);
  eWorkingFolder.Text := SysPath(System.WorkingFolder);

  case System.GameKey of
    TEFKCRC32: rgbGameKey.ItemIndex := 1;
    TEFKFileName: rgbGameKey.ItemIndex := 2;
    TEFKCustom: rgbGameKey.ItemIndex := 3;
    else  // SHA1 by default
      rgbGameKey.ItemIndex := 0;
  end;

  chkExtractAllFiles.Checked := System.ExtractAll;

  mExtensions.Lines.Assign(System.Extensions);
end;

end.
