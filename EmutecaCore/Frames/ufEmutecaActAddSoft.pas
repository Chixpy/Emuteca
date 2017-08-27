unit ufEmutecaActAddSoft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, ActnList, StdCtrls, EditBtn, LazFileUtils,
  u7zWrapper,
  uCHXStrUtils, uCHXFileUtils, ufCHXForm,
  ufCHXPropEditor,
  uEmutecaCommon,
  uaEmutecaCustomSystem,
  ucEmuteca,
  ucEmutecaSystem, ucEmutecaSoftList, ucEmutecaSoftware,
  ufEmutecaSoftEditor, ufEmutecaSystemCBX;

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
    procedure eFileAcceptFileName(Sender: TObject; var Value: string);
    procedure eFileEditingDone(Sender: TObject);
    procedure eVersionKeyEditingDone(Sender: TObject);
    procedure rgbSoftKeySelectionChanged(Sender: TObject);

  private
    FfmSystemCBX: TfmEmutecaSystemCBX;
    FfmSoftEditor: TfmEmutecaSoftEditor;

  private
    FEmuteca: cEmuteca;
    FSoftware: cEmutecaSoftware;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property fmSoftEditor: TfmEmutecaSoftEditor read FfmSoftEditor;
    property fmSystemCBX: TfmEmutecaSystemCBX read FfmSystemCBX;

    property Software: cEmutecaSoftware read FSoftware write SetSoftware;

    procedure SelectFile;
    procedure UpdateFileData;
    procedure UpdateInnerFileData;
    procedure UpdateSoftKey;
    procedure UpdateDupInfo;

    function SelectSystem(aSystem: cEmutecaSystem): boolean;

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure SaveFrameData; override;

    // Creates a form with AddSoft frame.
    class function SimpleForm(aEmuteca: cEmuteca; aGUIIconsIni: string;
      aGUIConfigIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaActAddSoft }

procedure TfmEmutecaActAddSoft.ClearFrameData;
begin
  fmSystemCBX.SelectedSystem := nil;
  fmSoftEditor.Software := nil;

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
  fmSoftEditor.Software := nil;

  if not Assigned(Software) then
    Exit;

  Software.Folder := ExtractFileDir(eFile.FileName);
  Software.FileName := ExtractFileName(eFile.FileName);
  Software.SHA1 := kCHXSHA1Empty;

  Software.GroupKey := ExtractFileNameOnly(
    ExcludeTrailingPathDelimiter(Software.Folder));
  Software.Title := ExtractFileNameOnly(Software.FileName);

  UpdateSoftKey;
  UpdateDupInfo;

  fmSoftEditor.Software := Software;
end;

procedure TfmEmutecaActAddSoft.UpdateInnerFileData;
begin
  fmSoftEditor.Software := nil;

  if not Assigned(Software) then
    Exit;

  Software.Folder := eFile.FileName;
  Software.FileName := cbxInnerFile.Text;
  Software.SHA1 := kCHXSHA1Empty;

  Software.GroupKey := ExtractFileNameOnly(
    ExcludeTrailingPathDelimiter(Software.Folder));
  Software.Title := ExtractFileNameOnly(Software.FileName);

  UpdateSoftKey;
  UpdateDupInfo;

  fmSoftEditor.Software := Software;
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

  if not assigned(fmSystemCBX.SelectedSystem) then
    Exit;

  aSoftList := fmSystemCBX.SelectedSystem.SoftManager.FullList;

  i := 0;
  FoundFile := False;
  while (not FoundFile) and (i < aSoftList.Count) do
  begin
    if Software.MatchFile(aSoftList[i].Folder, aSoftList[i].FileName) then
      FoundFile := True;

    Inc(i);
  end;

  if FoundFile then
    lDupFile.Caption := rsFileAlreadyAdded;
end;

function TfmEmutecaActAddSoft.SelectSystem(aSystem: cEmutecaSystem): boolean;
var
  ExtFilter: string;
begin
  Result := False;

  fmSoftEditor.Software := nil;

  Software.CachedSystem := aSystem;

  gbxFileSelection.Enabled := assigned(Software.CachedSystem);
  rgbSoftKey.Enabled := gbxFileSelection.Enabled;

  fmSoftEditor.Software := Software;

  if not assigned(Software.CachedSystem) then
    Exit;

  // Autoselecting Key Type
  case Software.CachedSystem.SoftExportKey of
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
    if not SupportedExtSL(eFile.FileName,
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

    lCompressedError.Caption :=
      format('%0:d files found.', [cbxInnerFile.Items.Count]);
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
  var Value: string);
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

  if assigned(Emuteca) then
    fmSystemCBX.SystemList := Emuteca.SystemManager.EnabledList
  else
    fmSystemCBX.SystemList := nil;
  fmSystemCBX.SelectedSystem := nil;

  fmSoftEditor.Software := Software;

  LoadFrameData;
end;

procedure TfmEmutecaActAddSoft.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;

  LoadFrameData;
end;

procedure TfmEmutecaActAddSoft.SelectFile;
begin
  chkOpenAsArchive.Checked := False;
  cbxInnerFile.Enabled := False;
  cbxInnerFile.Clear;

  UpdateFileData;

  // Recognized ext of an archive (from cEmutecaConfig, not u7zWrapper)
  chkOpenAsArchive.Enabled :=
    SupportedExtSL(eFile.FileName, Emuteca.Config.CompressedExtensions);
end;

procedure TfmEmutecaActAddSoft.LoadFrameData;
begin
  Enabled := Assigned(Software) and Assigned(Emuteca);
  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmEmutecaActAddSoft.SaveFrameData;
var
  aSystem: cEmutecaSystem;
begin
  fmSoftEditor.SaveFrameData;

  aSystem := cEmutecaSystem(Software.CachedSystem);

  if not assigned(aSystem) then
    Exit;

  aSystem.AddSoft(Software);

  // If we don't close then prepare to add a new software
  //   if we close, it will be freed on destroy
  FSoftware := cEmutecaSoftware.Create(nil);
  ClearFrameData;

  fmSoftEditor.Software := Software;
end;

class function TfmEmutecaActAddSoft.SimpleForm(aEmuteca: cEmuteca;
  aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmEmutecaActAddSoft;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmEmutecaActAddSoft';
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, 'Add Software...']);
    aForm.AutoSize := True;

    aFrame := TfmEmutecaActAddSoft.Create(aForm);
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

constructor TfmEmutecaActAddSoft.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmSystemCBX := TfmEmutecaSystemCBX.Create(gbxSelectSystem);
    fmSystemCBX.Align := alTop;
    fmSystemCBX.OnSelectSystem := @SelectSystem;
    fmSystemCBX.FirstItem := ETKSysCBXFISelect;
    fmSystemCBX.Parent := gbxSelectSystem;

    FfmSoftEditor := TfmEmutecaSoftEditor.Create(gbxSoftInfo);
    fmSoftEditor.SaveButtons := False;
    fmSoftEditor.ButtonClose := False;
    fmSoftEditor.Parent := gbxSoftInfo;
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
