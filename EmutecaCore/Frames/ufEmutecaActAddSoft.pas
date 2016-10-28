unit ufEmutecaActAddSoft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, FileUtil, strutils, Forms, Controls,
  StdCtrls, EditBtn, ActnList, ExtCtrls, Buttons,
  u7zWrapper, uCHXStrUtils, uCHXFileUtils,
  ucEmuteca, ucEmutecaSoftware, ucEmutecaSystem,
  ufEmutecaSoftEditor, ufEmutecaSystemCBX;

type

  { TfmActAddSoft }

  TfmActAddSoft = class(TFrame)
    ActionList1: TActionList;
    bAccept: TBitBtn;
    bCancel: TBitBtn;
    gbxSystem: TGroupBox;
    cbxInnerFile: TComboBox;
    chkOpenAsArchive: TCheckBox;
    eFile: TFileNameEdit;
    eVersionKey: TEdit;
    gbxFileSelection: TGroupBox;
    gbxVersionInfo: TGroupBox;
    ilActions: TImageList;
    lSystemInfo: TLabel;
    pBottom: TPanel;
    pLeft: TPanel;
    rgbVersionKey: TRadioGroup;
    Splitter1: TSplitter;
    procedure bAcceptClick(Sender: TObject);
    procedure cbxInnerFileChange(Sender: TObject);
    procedure chkOpenAsArchiveChange(Sender: TObject);
    procedure eFileAcceptFileName(Sender: TObject; var Value: string);
    procedure eFileButtonClick(Sender: TObject);
    procedure rgbVersionKeyClick(Sender: TObject);

  private
    FcbxSystem: TfmEmutecaSystemCBX;
    FEmuteca: cEmuteca;
    FIconsIni: string;
    FSoftware: cEmutecaSoftware;
    FSoftEditor: TfmEmutecaSoftEditor;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetIconsIni(AValue: string);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property SoftEditor: TfmEmutecaSoftEditor read FSoftEditor;
    property cbxSystem: TfmEmutecaSystemCBX read FcbxSystem;

    procedure UpdateVersionKey;
    procedure UpdateLists;

    function SelectSystem(aSystem: cEmutecaSystem): boolean;

  public
    { public declarations }
    property IconsIni: string read FIconsIni write SetIconsIni;
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmActAddSoft }

procedure TfmActAddSoft.eFileAcceptFileName(Sender: TObject;
  var Value: string);
begin
  // Updating SoftEditor
  Software.Folder := ExtractFileDir(Value);
  Software.FileName := ExtractFileName(Value);
  Software.ParentKey := RemoveFromBrackets(ExtractFileNameOnly(
    Software.FileName));
  Software.Title := Software.ParentKey;
  Software.Version :=
    CopyFromBrackets(ExtractFileNameOnly(Software.FileName));
  SoftEditor.UpdateData;

  chkOpenAsArchive.Checked := False;
  cbxInnerFile.Clear;
  UpdateVersionKey;

  // Recognized ext of an archive (from cEmutecaConfig, not u7zWrapper)
  if not assigned(Emuteca) then
    chkOpenAsArchive.Enabled := False
  else
    chkOpenAsArchive.Enabled :=
      SupportedExt(Value, Emuteca.Config.CompressedExtensions);
  cbxInnerFile.Enabled := chkOpenAsArchive.Enabled;
end;

procedure TfmActAddSoft.eFileButtonClick(Sender: TObject);
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
    if Assigned(Emuteca.CurrentSystem) then
      aEFN.InitialDir := ExtractFileDir(
        TrimFilename(Emuteca.CurrentSystem.BaseFolder + aEFN.FileName));
  end;
end;

procedure TfmActAddSoft.rgbVersionKeyClick(Sender: TObject);
begin
  UpdateVersionKey;
end;

procedure TfmActAddSoft.bAcceptClick(Sender: TObject);
begin
  SoftEditor.SaveData;
  Emuteca.SoftManager.FullList.Add(Software);
  // HACK: Created a new Software, so we can free it on Destroy.
  FSoftware := cEmutecaSoftware.Create(nil);
end;

procedure TfmActAddSoft.cbxInnerFileChange(Sender: TObject);
begin
  Software.Folder := eFile.Text;
  Software.FileName := cbxInnerFile.Text;
  Software.ParentKey := RemoveFromBrackets(ExtractFileNameOnly(eFile.Text));
  Software.Title := RemoveFromBrackets(ExtractFileNameOnly(
    cbxInnerFile.Text));
  Software.Version :=
    CopyFromBrackets(ExtractFileNameOnly(cbxInnerFile.Text));
  SoftEditor.UpdateData;
  UpdateVersionKey;
end;

procedure TfmActAddSoft.chkOpenAsArchiveChange(Sender: TObject);
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

procedure TfmActAddSoft.SetIconsIni(AValue: string);
begin
  if FIconsIni = AValue then
    Exit;
  FIconsIni := AValue;
  //ReadActionsIcons(IconsIni, Self.Name, '', ilActions, ActionList1);
end;

procedure TfmActAddSoft.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  UpdateLists;
end;

procedure TfmActAddSoft.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;
end;

procedure TfmActAddSoft.UpdateVersionKey;
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
        eVersionKey.Text := SetAsID(Software.FileName);

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
        eVersionKey.Text := SetAsID(Software.FileName);

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

procedure TfmActAddSoft.UpdateLists;
begin
  SoftEditor.Emuteca := Emuteca;

  if not assigned(Emuteca) then
    cbxSystem.SystemList := nil
  else
    cbxSystem.SystemList := Emuteca.SystemManager.VisibleList;
end;

function TfmActAddSoft.SelectSystem(aSystem: cEmutecaSystem): boolean;
var
  ExtFilter: string;
begin
  Result := False;
  Emuteca.CurrentSystem := aSystem;

  if Emuteca.CurrentSystem = nil then
    Exit;

  Software.SystemKey := Emuteca.CurrentSystem.ID;

  // Autoselecting Key Type
  case Emuteca.CurrentSystem.GameKey of
    TEFKCRC32: rgbVersionKey.ItemIndex := 1;
    TEFKCustom: rgbVersionKey.ItemIndex := 2;
    TEFKFileName: rgbVersionKey.ItemIndex := 3;
    else  // SHA1 by default
      rgbVersionKey.ItemIndex := 0;
  end;
  UpdateVersionKey;

  lSystemInfo.Caption := Emuteca.CurrentSystem.Extensions.CommaText;

  ExtFilter := 'All suported files|';
  ExtFilter := ExtFilter + '*.' +
    AnsiReplaceText(lSystemInfo.Caption, ',', ';*.');
  ExtFilter := ExtFilter + ';*.' + AnsiReplaceText(w7zFileExts, ',', ';*.');
  ExtFilter := ExtFilter + '|All files|' + AllFilesMask;
  eFile.Filter := ExtFilter;

  SoftEditor.UpdateData;

  Result := True;
end;

constructor TfmActAddSoft.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FcbxSystem := TfmEmutecaSystemCBX.Create(gbxSystem);
    cbxSystem.Parent := gbxSystem;
    cbxSystem.Align := alTop;
    cbxSystem.OnSelectSystem := @SelectSystem;
    // HACK: Removing "all systems" option
    if cbxSystem.cbxSystem.Items.Count > 0 then
      cbxSystem.cbxSystem.Items.Delete(0);

    FSoftEditor := TfmEmutecaSoftEditor.Create(gbxVersionInfo);
    SoftEditor.Parent := gbxVersionInfo;
    SoftEditor.Align := alClient;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;

  FSoftware := cEmutecaSoftware.Create(nil);
  SoftEditor.Software := self.Software;
end;

destructor TfmActAddSoft.Destroy;
begin
  FreeAndNil(FSoftware);
  inherited Destroy;
end;

end.
