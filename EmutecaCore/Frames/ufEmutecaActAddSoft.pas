unit ufEmutecaActAddSoft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, ActnList, StdCtrls, EditBtn, LazFileUtils, sha1,
  u7zWrapper, uCHXStrUtils, uCHXFileUtils, ufCHXPropEditor, ucEmuteca,
  ucEmutecaSystem, ucEmutecaSoftware, ufEmutecaSoftEditor, ufEmutecaSystemCBX;

type

  { TfmEmutecaActAddSoft }

  TfmEmutecaActAddSoft = class(TfmCHXPropEditor)
    bTest: TButton;
    cbxInnerFile: TComboBox;
    chkOpenAsArchive: TCheckBox;
    eFile: TFileNameEdit;
    eVersionKey: TEdit;
    gbxFileSelection: TGroupBox;
    gbxDuplicates: TGroupBox;
    gbxSelectSystem: TGroupBox;
    gbxSoftInfo: TGroupBox;
    lDupFile: TLabel;
    lDupSHA1: TLabel;
    lSystemInfo: TLabel;
    pSelectFile: TPanel;
    rgbSoftKey: TRadioGroup;
    Splitter1: TSplitter;
    procedure bTestClick(Sender: TObject);
    procedure cbxInnerFileChange(Sender: TObject);
    procedure chkOpenAsArchiveChange(Sender: TObject);
    procedure eFileAcceptFileName(Sender: TObject; var Value: string);
    procedure eVersionKeyEditingDone(Sender: TObject);
    procedure rgbGroupSelectionChanged(Sender: TObject);
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

    procedure UpdateGroup;
    procedure UpdateSoftKey;
    procedure UpdateInfo;

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
  // Not used...
  cbxSystem.SelectedSystem := nil;
  rgbSoftKey.ItemIndex := 0;
  eFile.Clear;
  chkOpenAsArchive.Checked := False;
  cbxInnerFile.ItemIndex := -1;
  eVersionKey.Clear;
end;

procedure TfmEmutecaActAddSoft.UpdateGroup;
begin
  //case rgbGroup.ItemIndex of
  //  1: // Filename;
  //    SoftEditor.SelectGroupByID(Software.Info.Title);
  //  else // Folder
  //    SoftEditor.SelectGroupByID(ExtractFileNameOnly(
  //      ExcludeTrailingPathDelimiter(Software.Folder)));
  //end;
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
      begin
        Software.ID := SHA1Print(Software.SHA1);
      end;
      1: // TEFKCRC32
        Software.ID := CRC32FileStr(aFile);
      2: // TEFKCustom
        ;
      3: // TEFKFileName
        Software.ID := ExtractFileNameOnly(Software.FileName);
      else // TEFKSHA1 by default
      begin
        Software.ID := SHA1Print(Software.SHA1);
      end;
    end;
  end
  else  // Compressed file
  begin
    aFile := ExcludeTrailingPathDelimiter(Software.Folder);
    if not FileExistsUTF8(aFile) then
      Exit;

    case rgbSoftKey.ItemIndex of
      0: // TEFKSHA1
        Software.ID := SHA1Print(Software.SHA1);
      1: // TEFKCRC32
      begin
        { TODO : There is faster way for CRC32 in compressed files
            with w7zWrapper. }
        w7zExtractFile(aFile, Software.FileName, Emuteca.TempFolder +
          'Temp', False, '');

        aFile := Emuteca.TempFolder + 'Temp\' + Software.FileName;

        if not FileExistsUTF8(aFile) then
          Exit;

        Software.ID := CRC32FileStr(aFile);
        DeleteFileUTF8(aFile);
      end;

      2: // TEFKCustom
        ;
      3: // TEFKFileName
        Software.ID := Software.FileName;
      else // TEFKSHA1 by default
      begin
        Software.ID := SHA1Print(Software.SHA1);
      end;
    end;
  end;

  eVersionKey.Text := Software.ID;
  eVersionKey.Enabled := rgbSoftKey.ItemIndex = 2;
end;

procedure TfmEmutecaActAddSoft.UpdateInfo;
var
  i, FoundSHA1, FoundFile: integer;
  aSoft: cEmutecaSoftware;
begin
  FoundSHA1 := -1;
  FoundFile := -1;
  i := Emuteca.SoftManager.FullList.Count - 1;
  while ((FoundSHA1 = -1) or (FoundFile = -1)) and (i >= 0) do
  begin
    aSoft := Emuteca.SoftManager.FullList[i];

    if FoundSHA1 = -1 then
      if SHA1Match(Software.SHA1, aSoft.SHA1) then
        FoundSHA1 := i;

    if FoundFile = -1 then
      if (CompareFilenames(Software.FileName, aSoft.FileName) = 0) and
        (CompareFilenames(Software.Folder, aSoft.Folder) = 0) then
        FoundFile := i;

    Dec(i);
  end;

  if FoundSHA1 <> -1 then
  lDupSHA1.Caption := 'A file with this SHA1 already added'
  else
  lDupSHA1.Caption := ' ';

   if FoundFile <> -1 then
  lDupFile.Caption := 'This file is already added'
  else
  lDupFile.Caption := ' ';
end;

function TfmEmutecaActAddSoft.SelectSystem(aSystem: cEmutecaSystem): boolean;
var
  ExtFilter: string;
begin
  Result := False;

  Software.System := aSystem;

  gbxFileSelection.Enabled := assigned(Software.System);
  rgbSoftKey.Enabled := gbxFileSelection.Enabled;

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

  ExtFilter := 'All suported files' + '|';
  ExtFilter := ExtFilter + '*.' +
    UTF8TextReplace(lSystemInfo.Caption, ',', ';*.');
  ExtFilter := ExtFilter + ';*.' + UTF8TextReplace(w7zGetFileExts, ',', ';*.');
  ExtFilter := ExtFilter + '|' + 'All files' + '|' + AllFilesMask;
  eFile.Filter := ExtFilter;
  eFile.InitialDir := CreateAbsolutePath(Software.System.BaseFolder,
    ProgramDirectory);

  UpdateSoftKey;
  SoftEditor.LoadData;

  Result := True;
end;

procedure TfmEmutecaActAddSoft.eFileAcceptFileName(Sender: TObject;
  var Value: string);
begin
  chkOpenAsArchive.Checked := False;
  cbxInnerFile.Clear;

  Software.Folder := ExtractFileDir(Value);
  Software.FileName := ExtractFileName(Value);
  Software.SHA1 := SHA1File(Value);

  UpdateSoftKey;
  SoftEditor.LoadData;

  // Recognized ext of an archive (from cEmutecaConfig, not u7zWrapper)
  chkOpenAsArchive.Enabled :=
    SupportedExt(Value, Emuteca.Config.CompressedExtensions);
  cbxInnerFile.Enabled := chkOpenAsArchive.Enabled;
end;

procedure TfmEmutecaActAddSoft.cbxInnerFileChange(Sender: TObject);
var
  aFile: string;
begin
  Software.Folder := eFile.Text;
  Software.FileName := cbxInnerFile.Text;

  // Decompressing file to search SHA1...
  aFile := Emuteca.TempFolder + 'Temp';
  w7zExtractFile(ExcludeTrailingPathDelimiter(Software.Folder),
    Software.FileName, aFile, False, '');

  aFile := SetAsFolder(aFile) + Software.FileName;

  if not FileExistsUTF8(aFile) then
    Exit;

  Software.SHA1 := SHA1File(aFile);
  DeleteFileUTF8(aFile);

  UpdateSoftKey;
  SoftEditor.LoadData;
end;

procedure TfmEmutecaActAddSoft.bTestClick(Sender: TObject);
begin
  UpdateInfo;
end;

procedure TfmEmutecaActAddSoft.chkOpenAsArchiveChange(Sender: TObject);
begin

  if chkOpenAsArchive.Checked then
  begin
    if not SupportedExt(eFile.Text, Emuteca.Config.CompressedExtensions) then
      Exit;
    cbxInnerFile.Clear;
    if not FileExistsUTF8(eFile.Text) then
      Exit;
    w7zListFiles(eFile.Text, cbxInnerFile.Items, True);
  end
  else
  begin
    // Reverting to normal file data
    cbxInnerFile.ItemIndex := -1;
    Software.FileName := ExtractFileName(
      ExcludeTrailingPathDelimiter(Software.Folder));
    Software.Folder := ExtractFileDir(ExcludeTrailingPathDelimiter(
      Software.Folder));
    Software.SHA1 := SHA1File(Software.Folder + Software.FileName);
    UpdateSoftKey;
  end;
end;

procedure TfmEmutecaActAddSoft.eVersionKeyEditingDone(Sender: TObject);
begin
  Software.ID := eVersionKey.Text;
end;

procedure TfmEmutecaActAddSoft.rgbGroupSelectionChanged(Sender: TObject);
begin
  UpdateGroup;
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

  if not assigned(Emuteca) then
    cbxSystem.SystemList := nil
  else
  begin
    cbxSystem.SystemList := Emuteca.SystemManager.EnabledList;
    // TODO: HACK: Changing "all systems" option...
    if cbxSystem.cbxSystem.Items.Count > 0 then
      cbxSystem.cbxSystem.Items[0] := rsSelectSystem;
  end;

  Self.Enabled := Assigned(Emuteca) and Assigned(Software);
end;

procedure TfmEmutecaActAddSoft.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;
  self.Enabled := Assigned(Emuteca) and Assigned(Software);
end;

procedure TfmEmutecaActAddSoft.LoadData;
begin
  SoftEditor.LoadData;
end;

procedure TfmEmutecaActAddSoft.SaveData;
begin
  SoftEditor.SaveData;
  Emuteca.SoftManager.FullList.Add(Software);

  // If we don't close then prepare to add a new software
  if ButtonClose then
    Software := nil
  else
    FSoftware := cEmutecaSoftware.Create(nil);

  SoftEditor.Software := nil;
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
  SoftEditor.Software := nil;
end;

destructor TfmEmutecaActAddSoft.Destroy;
begin
  FreeAndNil(FSoftware);
  inherited Destroy;
end;

end.
