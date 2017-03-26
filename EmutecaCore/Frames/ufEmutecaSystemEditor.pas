unit ufEmutecaSystemEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, CheckLst, EditBtn, LazFileUtils,
  uCHXStrUtils, uCHXDlgUtils,
  ufCHXPropEditor,
  ucEmutecaConfig, ucEmutecaSystem, ucEmutecaEmulator,
  ucEmutecaEmulatorManager;

resourcestring
  rsSelectEmulator = 'Select a System';

type

  { TfmEmutecaSystemEditor }

  TfmEmutecaSystemEditor = class(TfmCHXPropEditor)
    bCreateSubdirs: TButton;
    cbxMainEmulator: TComboBox;
    chkExtractAllFiles: TCheckBox;
    clbOtherEmulators: TCheckListBox;
    eBaseFolder: TDirectoryEdit;
    eExtraInfoFilename: TEdit;
    eTempFolder: TDirectoryEdit;
    eTitle: TEdit;
    gbxBasicInfo: TGroupBox;
    gbxEmulators: TGroupBox;
    gbxFiles: TGroupBox;
    lBaseFolder: TLabel;
    lFileExtensions: TLabel;
    lFileName: TLabel;
    lMainEmulator: TLabel;
    lOtherEmulators: TLabel;
    lTempFolder: TLabel;
    lTitle: TLabel;
    mExtensions: TMemo;
    Panel2: TPanel;
    rgbGameKey: TRadioGroup;
    Splitter1: TSplitter;
    procedure bCreateSubdirsClick(Sender: TObject);
    procedure eBaseFolderButtonClick(Sender: TObject);
    procedure eTempFolderButtonClick(Sender: TObject);
  private
    FConfig: cEmutecaConfig;
    FEmuManager: cEmutecaEmulatorManager;
    FSystem: cEmutecaSystem;
    procedure SetConfig(AValue: cEmutecaConfig);
    procedure SetEmuManager(AValue: cEmutecaEmulatorManager);
    procedure SetSystem(AValue: cEmutecaSystem);

    procedure UpdateLists;

  protected
    procedure ClearData; override;

  public
    property System: cEmutecaSystem read FSystem write SetSystem;

    property EmuManager: cEmutecaEmulatorManager
      read FEmuManager write SetEmuManager;
    property Config: cEmutecaConfig read FConfig write SetConfig;

    procedure SaveData; override;
    procedure LoadData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemEditor }

procedure TfmEmutecaSystemEditor.bCreateSubdirsClick(Sender: TObject);

  procedure CreateFolder(aLine: TStringList);
  var
    aFolder: string;
    aTitle: string;
  begin
    if aLine.Count = 0 then
      Exit;

    if aLine[0] = '' then
      Exit;

    aFolder := SetAsFolder(eBaseFolder.Text) + SetAsFolder(aLine[0]);
    ForceDirectoriesUTF8(aFolder);

    if aLine.Count = 1 then
      Exit;

    if aLine[1] = '' then
      Exit;

    if (aLine.Count = 2) or (aLine[2] = '') then
      aTitle := ExtractFileNameOnly(ExcludeTrailingPathDelimiter(aLine[0]))
    else
      aTitle := aLine[2];

    if aLine[1] = 'i' then
    begin
      System.ImageFolders.Add(aFolder);
      System.ImageCaptions.Add(aTitle);
    end
    else if aLine[1] = 't' then
    begin
      System.TextFolders.Add(aFolder);
      System.TextCaptions.Add(aTitle);
    end
    else if aLine[1] = 'c' then
    begin
      System.IconFolder := aFolder;
    end;

    { TODO : Add folders of music and video }
  end;

var
  FolderList, aLine: TStringList;
  i: integer;
begin
  if not assigned(Config) then
    Exit;
  if not Assigned(System) then
    Exit;

  if (eBaseFolder.Text = '') or not DirectoryExistsUTF8(eBaseFolder.Text) then
    { TODO : Exception :-P }
    Exit;
  if not FileExistsUTF8(Config.AutoSysFolder) then
    { TODO : Exception :-P }
    Exit;

  aLine := TStringList.Create;
  FolderList := TStringList.Create;
  try
    FolderList.LoadFromFile(Config.AutoSysFolder);
    i := 1; //Skip header
    while i < FolderList.Count do
    begin
      aLine.Clear;
      aLine.CommaText := FolderList[i];
      CreateFolder(aLine);

      Inc(i);
    end;

  finally
    FreeAndNil(FolderList);
    FreeAndNil(aLine);
  end;
end;

procedure TfmEmutecaSystemEditor.eBaseFolderButtonClick(Sender: TObject);
begin
  SetDirEditInitialDir(eBaseFolder, ProgramDirectory);
end;

procedure TfmEmutecaSystemEditor.eTempFolderButtonClick(Sender: TObject);
begin
  SetDirEditInitialDir(eTempFolder, ProgramDirectory);
end;

procedure TfmEmutecaSystemEditor.SetConfig(AValue: cEmutecaConfig);
begin
  if FConfig = AValue then
    Exit;
  FConfig := AValue;
  self.Enabled := Assigned(System) and Assigned(EmuManager) and Assigned(Config);
end;

procedure TfmEmutecaSystemEditor.SetEmuManager(AValue:
  cEmutecaEmulatorManager);
begin
  if FEmuManager = AValue then
    Exit;
  FEmuManager := AValue;
  UpdateLists;
  self.Enabled := Assigned(System) and Assigned(EmuManager) and Assigned(Config);
end;

procedure TfmEmutecaSystemEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;
  LoadData;
  self.Enabled := Assigned(System) and Assigned(EmuManager) and Assigned(Config);
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

procedure TfmEmutecaSystemEditor.ClearData;
begin
  eExtraInfoFilename.Clear;
  eTitle.Clear;
  cbxMainEmulator.ItemIndex := -1;
  clbOtherEmulators.CheckAll(cbUnchecked);
  eBaseFolder.Clear;
  eTempFolder.Clear;
  rgbGameKey.ItemIndex := 0;
  chkExtractAllFiles.Checked := False;
  mExtensions.Clear;
end;

procedure TfmEmutecaSystemEditor.SaveData;
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
  System.TempFolder := eTempFolder.Text;
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

procedure TfmEmutecaSystemEditor.LoadData;
var
  aEmulator: cEmutecaEmulator;
  i: integer;
begin
  ClearData;

  if (not assigned(System)) or (not assigned(EmuManager)) then
    Exit;

  eTitle.Text := System.Title;
  eExtraInfoFilename.Text := System.FileName;

  aEmulator := EmuManager.ItemById(System.MainEmulator);
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
  eTempFolder.Text := SysPath(System.TempFolder);

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

constructor TfmEmutecaSystemEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmutecaSystemEditor.Destroy;
begin
  inherited Destroy;
end;

end.
