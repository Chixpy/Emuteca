unit ufEmutecaActAddSoft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, ActnList, StdCtrls, EditBtn, LazFileUtils,
  u7zWrapper,
  uCHXStrUtils, uCHXFileUtils,
  ufCHXPropEditor,
  uEmutecaCommon, ucEmuteca,
  ucEmutecaSystem, ucEmutecaSoftware, ufEmutecaSoftEditor, ufEmutecaSystemCBX;

type

  { TfmEmutecaActAddSoft }

  TfmEmutecaActAddSoft = class(TfmCHXPropEditor)
    cbxInnerFile: TComboBox;
    chkOpenAsArchive: TCheckBox;
    eFile: TFileNameEdit;
    eVersionKey: TEdit;
    gbxFileSelection: TGroupBox;
    gbxDuplicates: TGroupBox;
    gbxSelectSystem: TGroupBox;
    gbxSoftInfo: TGroupBox;
    lDupFile: TLabel;
    lSystemInfo: TLabel;
    pSelectFile: TPanel;
    rgbSoftKey: TRadioGroup;
    Splitter1: TSplitter;
    procedure bTestClick(Sender: TObject);
    procedure cbxInnerFileChange(Sender: TObject);
    procedure chkOpenAsArchiveChange(Sender: TObject);
    procedure eFileEditingDone(Sender: TObject);
    procedure eVersionKeyEditingDone(Sender: TObject);
    procedure rgbSoftKeySelectionChanged(Sender: TObject);

  private
    FcbxSystem: TfmEmutecaSystemCBX;
    FSoftEditor: TfmEmutecaSoftEditor;

  private
    FEmuteca: cEmuteca;
    FSoftware: cEmutecaSoftware;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property SoftEditor: TfmEmutecaSoftEditor read FSoftEditor;
    property cbxSystem: TfmEmutecaSystemCBX read FcbxSystem;

    property Software: cEmutecaSoftware read FSoftware write SetSoftware;

    procedure ClearData; override;

    procedure UpdateFileData;
    procedure UpdateInnerFileData;
    procedure UpdateSoftKey;
    procedure UpdateDupInfo;

    function SelectSystem(aSystem: cEmutecaSystem): boolean;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure LoadData; override;
    procedure SaveData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaActAddSoft }

procedure TfmEmutecaActAddSoft.ClearData;
begin
  cbxSystem.SelectedSystem := nil;
  rgbSoftKey.ItemIndex := 0;
  eFile.Clear;
  chkOpenAsArchive.Checked := False;
  cbxInnerFile.ItemIndex := -1;
  eVersionKey.Clear;
  lDupFile.Caption := ' ';
end;

procedure TfmEmutecaActAddSoft.UpdateFileData;
begin
  if not Assigned(Software) then
    Exit;
  Software.Folder := ExtractFileDir(eFile.FileName);
  Software.FileName := ExtractFileName(eFile.FileName);
  Software.SHA1 := kEmuTKSHA1Empty;

  Software.GroupKey := ExtractFileNameOnly(
    ExcludeTrailingPathDelimiter(Software.Folder));
  Software.Title := Software.FileName;

  UpdateSoftKey;
  UpdateDupInfo;
  SoftEditor.LoadData;
end;

procedure TfmEmutecaActAddSoft.UpdateInnerFileData;
begin
  if not Assigned(Software) then
    Exit;
  Software.Folder := eFile.FileName;
  Software.FileName := cbxInnerFile.Text;
  Software.SHA1 := kEmuTKSHA1Empty;

  Software.GroupKey := ExtractFileNameOnly(
    ExcludeTrailingPathDelimiter(Software.Folder));
  Software.Title := Software.FileName;

  UpdateSoftKey;
  UpdateDupInfo;
  SoftEditor.LoadData;
end;

procedure TfmEmutecaActAddSoft.UpdateSoftKey;
var
  aFile: string;
begin
  // We use selected rgbSoftKey, not system default
  if not chkOpenAsArchive.Checked then
  begin // Non compressed file
    aFile := Software.Folder + Software.FileName;
    if not FileExistsUTF8(aFile) then
      Exit;

    case rgbSoftKey.ItemIndex of
      0: // TEFKSHA1
        Software.ID := '';
      1: // TEFKCRC32
        Software.ID := CRC32FileStr(aFile);

      2, 3: // TEFKCustom and TEFKFileName
        Software.ID := ExtractFileNameOnly(Software.FileName);
      else // By default TEFKSHA1
      begin
        Software.ID := '';
      end;
    end;
  end
  else  // Compressed file
  begin
    aFile := ExcludeTrailingPathDelimiter(Software.Folder);
    if not FileExistsUTF8(aFile) then
      Exit;

    case rgbSoftKey.ItemIndex of
      0: //TEFKSHA1
        Software.ID := '';
      1: // TEFKCRC32
      begin
        Software.ID := w7zCRC32InnerFileStr(aFile, Software.FileName, '');
      end;

      2, 3: // TEFKCustom and TEFKFileName
        Software.ID := ExtractFileNameOnly(Software.FileName);

      else // By default TEFKSHA1
      begin
        Software.ID := '';
      end;
    end;
  end;

  eVersionKey.Text := Software.ID;
  eVersionKey.Enabled := rgbSoftKey.ItemIndex = 2;
end;

procedure TfmEmutecaActAddSoft.UpdateDupInfo;
var
  aSoft: cEmutecaSoftware;
  i: integer;
  FoundFile, SameSystem: boolean;
begin
  FoundFile := False;
  SameSystem := False;
  i := 0;
  while (not FoundFile) and (i < Emuteca.SoftManager.FullList.Count) do
  begin
    aSoft := Emuteca.SoftManager.FullList[i];
    if Software.MatchMFile(aSoft) then
    begin
      FoundFile := True;
      SameSystem := aSoft.System = Software.System;
    end;
    Inc(i);
  end;

  if FoundFile then
  begin
    lDupFile.Caption := 'This file is already added.';
    if not SameSystem then
      lDupFile.Caption := lDupFile.Caption + ' But in another System.';
  end;
end;

function TfmEmutecaActAddSoft.SelectSystem(aSystem: cEmutecaSystem): boolean;
var
  ExtFilter: string;
begin
  Result := False;

  Software.System := aSystem;

  gbxFileSelection.Enabled := assigned(Software.System);
  rgbSoftKey.Enabled := gbxFileSelection.Enabled;

  SoftEditor.LoadData;

  if not assigned(Software.System) then
    Exit;

  // Autoselecting Key Type
  case Software.System.GameKey of
    TEFKSHA1: rgbSoftKey.ItemIndex := 0;
    TEFKCRC32: rgbSoftKey.ItemIndex := 1;
    TEFKCustom: rgbSoftKey.ItemIndex := 2;
    TEFKFileName: rgbSoftKey.ItemIndex := 3;
    else  // SHA1 by default
      rgbSoftKey.ItemIndex := 0;
  end;

  lSystemInfo.Caption := Software.System.Extensions.CommaText;

  ExtFilter := 'All suported files' + '|' +
    FileMaskFromCommaText(w7zGetFileExts);
  ExtFilter := ExtFilter + '|' + 'All files' + '|' + AllFilesMask;
  eFile.Filter := ExtFilter;

  eFile.InitialDir := CreateAbsolutePath(Software.System.BaseFolder,
    ProgramDirectory);

  UpdateSoftKey;

  Result := True;
end;

procedure TfmEmutecaActAddSoft.cbxInnerFileChange(Sender: TObject);
begin
  UpdateInnerFileData;
end;

procedure TfmEmutecaActAddSoft.bTestClick(Sender: TObject);
begin
  UpdateDupInfo;
end;

procedure TfmEmutecaActAddSoft.chkOpenAsArchiveChange(Sender: TObject);
begin

  if chkOpenAsArchive.Checked then
  begin
    if not SupportedExt(eFile.FileName,
      Emuteca.Config.CompressedExtensions) then
    begin
      chkOpenAsArchive.Checked := False;
      chkOpenAsArchive.Enabled := False;
      Exit;
    end;
    cbxInnerFile.Clear;
    if not FileExistsUTF8(eFile.FileName) then
      Exit;
    w7zListFiles(eFile.FileName, cbxInnerFile.Items, True, True, '');
  end
  else
  begin
    // Reverting to normal file data
    cbxInnerFile.ItemIndex := -1;
    UpdateFileData;
  end;
end;

procedure TfmEmutecaActAddSoft.eFileEditingDone(Sender: TObject);
begin
  chkOpenAsArchive.Checked := False;
  cbxInnerFile.Clear;

  UpdateFileData;

  // Recognized ext of an archive (from cEmutecaConfig, not u7zWrapper)
  chkOpenAsArchive.Enabled :=
    SupportedExt(eFile.FileName, Emuteca.Config.CompressedExtensions);
  cbxInnerFile.Enabled := chkOpenAsArchive.Enabled;
end;

procedure TfmEmutecaActAddSoft.eVersionKeyEditingDone(Sender: TObject);
begin
  Software.ID := eVersionKey.Text;
end;

procedure TfmEmutecaActAddSoft.rgbSoftKeySelectionChanged(Sender: TObject);
begin
  UpdateSoftKey;
end;

procedure TfmEmutecaActAddSoft.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  LoadData;
end;

procedure TfmEmutecaActAddSoft.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;
  LoadData;
end;

procedure TfmEmutecaActAddSoft.LoadData;
begin
  if not assigned(Software) or not Assigned(Emuteca) then
  begin
    ClearData;
    Self.Enabled := False;
    Exit;
  end;

  Self.Enabled := True;

  if not assigned(Emuteca) then
    cbxSystem.SystemList := nil
  else
  begin
    cbxSystem.SystemList := Emuteca.SystemManager.EnabledList;
    // TODO: HACK: Changing "all systems" option...
    if cbxSystem.cbxSystem.Items.Count > 0 then
      cbxSystem.cbxSystem.Items[0] := rsSelectSystem;
  end;

  SoftEditor.Software := Software;
  SoftEditor.LoadData;
end;

procedure TfmEmutecaActAddSoft.SaveData;
begin
  SoftEditor.SaveData;

  Emuteca.SoftManager.FullList.Add(Software);

  Emuteca.SystemManager.SaveToFileIni('', False);
  Emuteca.SoftManager.SaveSoftOfSystem(Software.System, False);

  // If we don't close then prepare to add a new software
  if ButtonClose then
    Software := nil
  else
    FSoftware := cEmutecaSoftware.Create(nil);

  SoftEditor.Software := Software;
end;

constructor TfmEmutecaActAddSoft.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FcbxSystem := TfmEmutecaSystemCBX.Create(gbxSelectSystem);
    cbxSystem.Align := alTop;
    cbxSystem.OnSelectSystem := @SelectSystem;
    cbxSystem.Parent := gbxSelectSystem;

    FSoftEditor := TfmEmutecaSoftEditor.Create(gbxSoftInfo);
    SoftEditor.SaveButtons := False;
    SoftEditor.ButtonClose := False;
    SoftEditor.Parent := gbxSoftInfo;
  end;

begin
  inherited Create(TheOwner);

  Self.Enabled := False;

  CreateFrames;

  FSoftware := cEmutecaSoftware.Create(nil);
end;

destructor TfmEmutecaActAddSoft.Destroy;
begin
  FreeAndNil(FSoftware);
  inherited Destroy;
end;

end.
