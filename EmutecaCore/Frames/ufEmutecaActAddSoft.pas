unit ufEmutecaActAddSoft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, ActnList, StdCtrls, EditBtn, LazFileUtils,
  u7zWrapper,
  uCHXStrUtils, uCHXFileUtils,
  ufCHXPropEditor,
  uEmutecaCommon, ucEmuteca, uaEmutecaCustomSystem,
  ucEmutecaSystem, ucEmutecaSoftList, ucEmutecaSoftware, ufEmutecaSoftEditor, ufEmutecaSystemCBXOld;

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
    lCompressedError: TLabel;
    lDupFile: TLabel;
    lSystemInfo: TLabel;
    pSelectFile: TPanel;
    rgbSoftKey: TRadioGroup;
    Splitter1: TSplitter;
    procedure cbxInnerFileChange(Sender: TObject);
    procedure chkOpenAsArchiveChange(Sender: TObject);
    procedure eFileAcceptFileName(Sender: TObject; var Value: String);
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

    procedure SelectFile;
    procedure UpdateFileData;
    procedure UpdateInnerFileData;
    procedure UpdateSoftKey;
    procedure UpdateDupInfo;

    function SelectSystem(aSystem: cEmutecaSystem): boolean;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure ClearData; override;
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
  lCompressedError.Caption := ' ';
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
  Software.Title := ExtractFileNameOnly(Software.FileName);

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
  Software.Title := ExtractFileNameOnly(Software.FileName);

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
  aSoftList: cEmutecaSoftList;
  i: integer;
  FoundFile: boolean;
begin
  lDupFile.Caption := '';

  if not assigned (cbxSystem.SelectedSystem) then
    Exit;

  aSoftList := cbxSystem.SelectedSystem.SoftManager.FullList;

  i := 0;
  FoundFile := False;
  while (not FoundFile) and (i < aSoftList.Count) do
  begin
    if Software.MatchMFile(aSoftList[i]) then
      FoundFile := True;

    Inc(i);
  end;

  if FoundFile then
    lDupFile.Caption := 'This file is already added.';
end;

function TfmEmutecaActAddSoft.SelectSystem(aSystem: cEmutecaSystem): boolean;
var
  ExtFilter: string;
begin
  Result := False;

  Software.CachedSystem := aSystem;

  gbxFileSelection.Enabled := assigned(Software.CachedSystem);
  rgbSoftKey.Enabled := gbxFileSelection.Enabled;

  SoftEditor.LoadData;

  if not assigned(Software.CachedSystem) then
    Exit;

  // Autoselecting Key Type
  case Software.CachedSystem.GameKey of
    TEFKSHA1: rgbSoftKey.ItemIndex := 0;
    TEFKCRC32: rgbSoftKey.ItemIndex := 1;
    TEFKCustom: rgbSoftKey.ItemIndex := 2;
    TEFKFileName: rgbSoftKey.ItemIndex := 3;
    else  // SHA1 by default
      rgbSoftKey.ItemIndex := 0;
  end;

  lSystemInfo.Caption := Software.CachedSystem.Extensions.CommaText;

  ExtFilter := 'All suported files' + '|' +
    FileMaskFromCommaText(w7zGetFileExts);
  ExtFilter := ExtFilter + '|' + 'All files' + '|' + AllFilesMask;
  eFile.Filter := ExtFilter;

  eFile.InitialDir := CreateAbsolutePath(Software.CachedSystem.BaseFolder,
    ProgramDirectory);

  UpdateSoftKey;

  Result := True;
end;

procedure TfmEmutecaActAddSoft.cbxInnerFileChange(Sender: TObject);
begin
  UpdateInnerFileData;
end;

procedure TfmEmutecaActAddSoft.chkOpenAsArchiveChange(Sender: TObject);
  procedure AnError(aText: string);
  begin
    lCompressedError.Caption := aText;
    chkOpenAsArchive.Checked := False;
    cbxInnerFile.ItemIndex := -1;
    cbxInnerFile.Enabled := False;
  end;

begin
  cbxInnerFile.Clear;

  if chkOpenAsArchive.Checked then
  begin
    if not SupportedExt(eFile.FileName,
      Emuteca.Config.CompressedExtensions) then
    begin
      AnError('Not a compressed file.');
      Exit;
    end;

    if not FileExistsUTF8(eFile.FileName) then
    begin
      AnError('Compressed file not found.');
      Exit;
    end;

    w7zListFiles(eFile.FileName, cbxInnerFile.Items, True, True, '');

    if cbxInnerFile.Items.Count = 0 then
        begin
      AnError('No files not found.');
      Exit;
    end;

    lCompressedError.Caption := format('%0:d files found.', [cbxInnerFile.Items.Count]);
    cbxInnerFile.Enabled := True;
  end
  else
  begin
    // Reverting to normal file data
    lCompressedError.Caption := ' ';
    cbxInnerFile.ItemIndex := -1;
    cbxInnerFile.Enabled := False;
    UpdateFileData;
  end;
end;

procedure TfmEmutecaActAddSoft.eFileAcceptFileName(Sender: TObject;
  var Value: String);
begin
  // It's called before Text is updated
  eFile.Text := Value;

  SelectFile;
end;

procedure TfmEmutecaActAddSoft.eFileEditingDone(Sender: TObject);
begin
  SelectFile;
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

procedure TfmEmutecaActAddSoft.SelectFile;
begin
  chkOpenAsArchive.Checked := False;
  cbxInnerFile.Enabled := False;
  cbxInnerFile.Clear;

  UpdateFileData;

  // Recognized ext of an archive (from cEmutecaConfig, not u7zWrapper)
  chkOpenAsArchive.Enabled :=
    SupportedExt(eFile.FileName, Emuteca.Config.CompressedExtensions);
end;

procedure TfmEmutecaActAddSoft.LoadData;
begin
  if not assigned(Software) or not Assigned(Emuteca) then
  begin
    ClearData;
    Enabled := False;
    Exit;
  end;

  Enabled := True;

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
var
  aSystem: cEmutecaSystem;
begin
  SoftEditor.SaveData;

  aSystem := cEmutecaSystem(Software.CachedSystem);

  if not assigned(aSystem) then Exit;

  aSystem.SoftManager.FullList.Add(Software);
  aSystem.CacheData;

  // If we don't close then prepare to add a new software
  //   if we close, it will be freed on destroy
  FSoftware := cEmutecaSoftware.Create(nil);
  ClearData;

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

  Enabled := False;

  CreateFrames;

  FSoftware := cEmutecaSoftware.Create(nil);
end;

destructor TfmEmutecaActAddSoft.Destroy;
begin
  FreeAndNil(FSoftware);
  inherited Destroy;
end;

end.
