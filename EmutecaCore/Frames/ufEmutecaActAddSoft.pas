unit ufEmutecaActAddSoft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, EditBtn, LazFileUtils,
  u7zWrapper, uCHXStrUtils, uCHXFileUtils,
  ufCHXPropEditor,
  ucEmuteca, ucEmutecaSystem, ucEmutecaSoftware,
  ufEmutecaSoftEditor, ufEmutecaSystemCBX;

type

  { TfmEmutecaActAddSoft }

  TfmEmutecaActAddSoft = class(TfmCHXPropEditor)
    cbxInnerFile: TComboBox;
    chkOpenAsArchive: TCheckBox;
    eFile: TFileNameEdit;
    eVersionKey: TEdit;
    gbxFileSelection: TGroupBox;
    gbxSelectSystem: TGroupBox;
    gbxSoftInfo: TGroupBox;
    lSystemInfo: TLabel;
    pSelectFile: TPanel;
    rgbGroup: TRadioGroup;
    rgbVersionKey: TRadioGroup;
    Splitter1: TSplitter;
    procedure cbxInnerFileChange(Sender: TObject);
    procedure chkOpenAsArchiveChange(Sender: TObject);
    procedure eFileAcceptFileName(Sender: TObject; var Value: string);
    procedure eFileButtonClick(Sender: TObject);
    procedure rgbGroupSelectionChanged(Sender: TObject);
    procedure rgbVersionKeySelectionChanged(Sender: TObject);

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

end;

procedure TfmEmutecaActAddSoft.UpdateGroup;
begin
  case rgbGroup.ItemIndex of
    1: // Filename;
      SoftEditor.SelectGroupByID(Software.Title);
    else // Folder
      SoftEditor.SelectGroupByID(Software.Folder);
  end;
end;

procedure TfmEmutecaActAddSoft.UpdateSoftKey;
var
  aFile: string;
begin
  // We use selected rgbVersionKey, not system default
  if not chkOpenAsArchive.Checked then
  begin // Non compressed file
    aFile := Software.Folder + Software.FileName;
    if not FileExistsUTF8(aFile) then
      Exit;

    case rgbVersionKey.ItemIndex of
      1: {TEFKCRC32}
        eVersionKey.Text := CRC32FileStr(aFile);
      2: {TEFKCustom};
      3: {TEFKFileName}
        eVersionKey.Text := Software.FileName;
      else // TEFKSHA1 by default
        eVersionKey.Text := SHA1FileStr(aFile);
    end;
  end
  else  // Compressed file
  begin
    aFile := ExcludeTrailingPathDelimiter(Software.Folder);
    if not FileExistsUTF8(aFile) then
      Exit;

    case rgbVersionKey.ItemIndex of
      1: {TEFKCRC32}
      begin
        w7zExtractFile(aFile, Software.FileName, Emuteca.TempFolder +
          'Temp', False, '');

        aFile := Emuteca.TempFolder + 'Temp\' + Software.FileName;
        if not FileExistsUTF8(aFile) then
          Exit;

        eVersionKey.Text := CRC32FileStr(aFile);
        { TODO : Delete extracted file only... }
        DeleteDirectory(Emuteca.TempFolder + 'Temp', False);
      end;

      2: {TEFKCustom};
      3: {TEFKFileName}
        eVersionKey.Text := Software.FileName;
      else // TEFKSHA1 by default
      begin
        w7zExtractFile(aFile, Software.FileName, Emuteca.TempFolder +
          'Temp', False, '');

        aFile := Emuteca.TempFolder + 'Temp\' + Software.FileName;
        if not FileExistsUTF8(aFile) then
          Exit;

        eVersionKey.Text := SHA1FileStr(aFile);
        { TODO : Delete extracted file only... }
        DeleteDirectory(Emuteca.TempFolder + 'Temp', False);
      end;
    end;
  end;
  Software.ID := eVersionKey.Text;
end;

function TfmEmutecaActAddSoft.SelectSystem(aSystem: cEmutecaSystem): boolean;
var
  ExtFilter: String;
begin
  Result := False;

  Software.System := aSystem;

  gbxFileSelection.Enabled := assigned(Software.System);
  rgbGroup.Enabled := gbxFileSelection.Enabled;
  rgbVersionKey.Enabled := gbxFileSelection.Enabled;

  if not assigned(Software.System) then
    Exit;

  // Autoselecting Key Type
  case Software.System.GameKey of
    TEFKSHA1: rgbVersionKey.ItemIndex := 0;
    TEFKCRC32: rgbVersionKey.ItemIndex := 1;
    TEFKCustom: rgbVersionKey.ItemIndex := 2;
    TEFKFileName: rgbVersionKey.ItemIndex := 3;
    else  // SHA1 by default
      rgbVersionKey.ItemIndex := 0;
  end;
  UpdateSoftKey;

  lSystemInfo.Caption := Software.System.Extensions.CommaText;

  ExtFilter := 'All suported files|';
  ExtFilter := ExtFilter + '*.' +
    UTF8TextReplace(lSystemInfo.Caption, ',', ';*.');
  ExtFilter := ExtFilter + ';*.' + UTF8TextReplace(w7zFileExts, ',', ';*.');
  ExtFilter := ExtFilter + '|All files|' + AllFilesMask;
  eFile.Filter := ExtFilter;

  SoftEditor.LoadData;

  Result := True;

end;

procedure TfmEmutecaActAddSoft.eFileAcceptFileName(Sender: TObject;
  var Value: string);
begin
    chkOpenAsArchive.Checked := False;
  cbxInnerFile.Clear;

  // Updating SoftEditor
  Software.Folder := ExtractFileDir(Value);
  Software.FileName := ExtractFileName(Value);
  Software.Title := RemoveFromBrackets(ExtractFileNameOnly(Software.FileName));
  Software.Version := CopyFromBrackets(ExtractFileNameOnly(Software.FileName));
  UpdateGroup;
  UpdateSoftKey;
  SoftEditor.LoadData;

  // Recognized ext of an archive (from cEmutecaConfig, not u7zWrapper)
  chkOpenAsArchive.Enabled :=
    SupportedExt(Value, Emuteca.Config.CompressedExtensions);
  cbxInnerFile.Enabled := chkOpenAsArchive.Enabled;
end;

procedure TfmEmutecaActAddSoft.cbxInnerFileChange(Sender: TObject);
begin
    Software.Folder := eFile.Text;
  Software.FileName := cbxInnerFile.Text;
  Software.Title := RemoveFromBrackets(ExtractFileNameOnly(cbxInnerFile.Text));
  Software.Version := CopyFromBrackets(ExtractFileNameOnly(cbxInnerFile.Text));
  UpdateGroup;
  UpdateSoftKey;
  SoftEditor.LoadData;
end;

procedure TfmEmutecaActAddSoft.chkOpenAsArchiveChange(Sender: TObject);
begin
    if not chkOpenAsArchive.Enabled then
    Exit;

  if not SupportedExt(eFile.Text, Emuteca.Config.CompressedExtensions) then
    Exit;

  cbxInnerFile.Clear;

  if not FileExistsUTF8(eFile.Text) then
    Exit;

  w7zListFiles(eFile.Text, cbxInnerFile.Items, True);
end;

procedure TfmEmutecaActAddSoft.eFileButtonClick(Sender: TObject);
var
  aEFN: TFileNameEdit;
begin
  aEFN := TFileNameEdit(Sender);
  if FilenameIsAbsolute(aEFN.FileName) then
  begin
    aEFN.InitialDir := ExtractFileDir(SysPath(aEFN.FileName));
  end
  else
  begin
    // Open from system base folder
    if Assigned(cbxSystem.SelectedSystem) then
      aEFN.InitialDir := ExtractFileDir(
        TrimFilename(cbxSystem.SelectedSystem.BaseFolder + aEFN.FileName));
  end;
end;

procedure TfmEmutecaActAddSoft.rgbGroupSelectionChanged(Sender: TObject);
begin
  UpdateGroup;
end;

procedure TfmEmutecaActAddSoft.rgbVersionKeySelectionChanged(Sender: TObject);
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
    cbxSystem.SystemList := Emuteca.SystemManager.VisibleList;
    // TODO: HACK: Changing "all systems" option...
    if cbxSystem.cbxSystem.Items.Count > 0 then
      cbxSystem.cbxSystem.Items[0] := reSelectSystem;
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
  Software := nil;
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

  CreateFrames;

  FSoftware := cEmutecaSoftware.Create(nil);
  SoftEditor.Software := self.Software;
end;

destructor TfmEmutecaActAddSoft.Destroy;
begin
  FreeAndNil(FSoftware);
  inherited Destroy;
end;

end.
