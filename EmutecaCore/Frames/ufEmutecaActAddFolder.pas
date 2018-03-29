unit ufEmutecaActAddFolder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, EditBtn, LazFileUtils, LazUTF8,
  u7zWrapper,
  uCHXFileUtils, uCHXStrUtils, uCHXDlgUtils,
  ufrCHXForm, ufCHXPropEditor,
  uEmutecaCommon,
  ucEmuteca, uaEmutecaCustomSystem, ucEmutecaSystem,
  ucEmutecaSoftList, ucEmutecaSoftware,
  ufEmutecaSystemCBX;

type

  { TfmEmutecaActAddFolder }

  TfmEmutecaActAddFolder = class(TfmCHXPropEditor)
    chkNoZip: TCheckBox;
    chkSubfolders: TCheckBox;
    eFolder: TDirectoryEdit;
    eSystemExportKey: TEdit;
    eSystemExtensions: TEdit;
    gbxFolder: TGroupBox;
    gbxSelectSystem: TGroupBox;
    gbxSysInfo: TGroupBox;
    lSystemExportKey: TLabel;
    lSystemExtensions: TLabel;
    rgbFilename: TRadioGroup;
    rgbGroup: TRadioGroup;
  private
    FfmSystemCBX: TfmEmutecaSystemCBX;
    FEmuteca: cEmuteca;
    procedure SetEmuteca(AValue: cEmuteca);

  protected
    property fmSystemCBX: TfmEmutecaSystemCBX read FfmSystemCBX;

    function SelectSystem(aSystem: cEmutecaSystem): boolean;

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

{ TfmEmutecaActAddFolder }

procedure TfmEmutecaActAddFolder.SetEmuteca(AValue: cEmuteca);
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

function TfmEmutecaActAddFolder.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := False;

  eSystemExtensions.Text := '';
  eSystemExportKey.Text := '';
  gbxFolder.Enabled := Assigned(aSystem);

  if not Assigned(aSystem) then
    Exit;

  eSystemExtensions.Text := aSystem.Extensions.CommaText;
  eSystemExportKey.Text := SoftExportKey2StrK(aSystem.SoftExportKey);
  SetDirEditInitialDir(eFolder, aSystem.BaseFolder);
end;

procedure TfmEmutecaActAddFolder.DoLoadFrameData;
begin
  Enabled := Assigned(Emuteca);

  //if not Enabled then
  //begin
  //  ClearFrameData;
  //  Exit;
  //end;
end;

procedure TfmEmutecaActAddFolder.DoSaveFrameData;

  procedure AddFile(aFolder, aFile: string; aSystem: cEmutecaSystem;
    aCacheSoftList: cEmutecaSoftList);
  var
    aSoft: cEmutecaSoftware;
    Found: boolean;
    j: integer;
  begin
    // Search if file is already added to system list
    aSoft := nil;
    Found := False;
    j := 0;
    while (j < aCacheSoftList.Count) and (not Found) do
    begin
      aSoft := aCacheSoftList[j];
      // Same file
      if aSoft.MatchFile(aFolder, aFile) then
      begin
        Found := True;

        case rgbFilename.ItemIndex of
          0: // Ignore  (set to nil and Found)
            aSoft := nil;
          else // Update SHA
            ;
        end;
        aCacheSoftList.Delete(j); // Speeds up following searchs
      end;
      Inc(j);
    end;

    if not Found then // Create soft
    begin
      aSoft := cEmutecaSoftware.Create(nil);
      aSoft.Folder := aFolder;
      aSoft.FileName := aFile;
    end;

    // if not assigned -> Found and Ignored
    if Assigned(aSoft) then
    begin
      // SHA1 = 0
      // It's updated in background when caching
      aSoft.SHA1 := kCHXSHA1Empty;

      // ID
      case aSystem.SoftExportKey of
        TEFKSHA1:
          aSoft.ID := '';

        TEFKCRC32:
        begin
          // Check if it's a compressed file
          if FileExistsUTF8(ExcludeTrailingPathDelimiter(aSoft.Folder)) then
          begin
            aSoft.ID := w7zCRC32InnerFileStr(aSoft.Folder,
              aSoft.FileName, '');
          end
          else
          begin
            aSoft.ID := CRC32FileStr(aSoft.Folder + aSoft.FileName);
          end;
        end;

        TEFKCustom, TEFKFileName:
          aSoft.ID := ExtractFileNameOnly(aSoft.FileName);

        else  // TEFKSHA1 by default
          aSoft.ID := '';
      end;

      aSoft.Title := RemoveFromBrackets(ExtractFileNameOnly(aSoft.FileName));
      aSoft.Version := CopyFromBrackets(ExtractFileNameOnly(aSoft.FileName));

      case rgbGroup.ItemIndex of
        1: // Group by filename
          aSoft.GroupKey :=
            RemoveFromBrackets(ExtractFileNameOnly(aSoft.FileName))
        else
          aSoft.GroupKey :=
            RemoveFromBrackets(ExtractFileNameOnly(
            ExcludeTrailingPathDelimiter(aSoft.Folder)));
      end;

      // We don't want to do whole articles thing here, but...
      aSoft.Title := Trim(UTF8TextReplace(aSoft.Title, ' - ', ': '));
      aSoft.GroupKey := Trim(UTF8TextReplace(aSoft.GroupKey, ' - ', ': '));

      // Add it if not found before
      if not Found then
        aSystem.AddSoft(aSoft);
    end;
  end;

var
  aSystem: cEmutecaSystem;
  FileList, ComprFileList: TStringList;
  aFileMask: string;
  aFile, aFolder: string;
  i, j: integer;
  CacheSoftList: cEmutecaSoftList;
  Continue: Boolean;
begin
  if not assigned(Emuteca) then
    Exit;

  if not DirectoryExistsUTF8(eFolder.Text) then
    Exit;

  aSystem := fmSystemCBX.SelectedSystem;
  if not assigned(aSystem) then
    Exit;

  Self.Enabled := False;

  // Loading data if not already loaded
  if not aSystem.SoftGroupLoaded then
    Emuteca.SystemManager.LoadSystemData(aSystem);

  // Copy soft
  CacheSoftList := cEmutecaSoftList.Create(False);
  CacheSoftList.Assign(aSystem.SoftManager.FullList);
  CacheSoftList.Sort(@EmutecaCompareSoftByFileName);

  FileList := TStringList.Create;
  ComprFileList := TStringList.Create;
  try
    if assigned(Emuteca.ProgressCallBack) then
      Continue := Emuteca.ProgressCallBack('Making list of all files', Format('This can take a while. Searching for: %0:s',
        [aSystem.Extensions.CommaText]), 1, 100, True);

    // 1.- Straight search of all files
    aFileMask := FileMaskFromStringList(aSystem.Extensions);
    if not chkNoZip.Checked then
      aFileMask := aFileMask + ';' + FileMaskFromStringList(Emuteca.Config.CompressedExtensions);

    FileList.BeginUpdate;
    FileList.Sorted := False;
    FindAllFiles(FileList, eFolder.Text, aFileMask, chkSubfolders.Checked);
    FileList.Sorted := True;
    FileList.EndUpdate;

    i := 0;
    Continue := True;
    while Continue and (i < FileList.Count) do
    begin
      aFolder := SetAsFolder(ExtractFilePath(FileList[i]));
      aFile := SetAsFile(ExtractFileName(FileList[i]));

      if assigned(Emuteca.ProgressCallBack) then
        Continue := Emuteca.ProgressCallBack('Adding files', FileList[i],
          i, FileList.Count, True);

      if SupportedExtSL(aFile, aSystem.Extensions) then
      begin // it's a supported file
        AddFile(aFolder, aFile, aSystem, CacheSoftList);
      end
      else if (not chkNoZip.Checked) and SupportedExtSL(aFile,
        Emuteca.Config.CompressedExtensions) then
      begin // It´s a compressed archive (not supported by system)

        ComprFileList.BeginUpdate;
        ComprFileList.Clear;
        ComprFileList.Sorted := False;
        w7zListFiles(aFolder + aFile, ComprFileList, True, '');
        ComprFileList.Sorted := True;
        ComprFileList.EndUpdate;
        j := 0;
        while j < ComprFileList.Count do
        begin
          if SupportedExtSL(ComprFileList[j], aSystem.Extensions) then
            AddFile(aFolder + aFile, ComprFileList[j], aSystem, CacheSoftList);
          Inc(j);
        end;
      end;

      Inc(i);
    end;

  finally

    Emuteca.CacheData;

    if assigned(Emuteca.ProgressCallBack) then
      Emuteca.ProgressCallBack('', '',  0, 0, False);

    ComprFileList.Free;
    FileList.Free;
    CacheSoftList.Free;

    Self.Enabled := True;
  end;
end;

class function TfmEmutecaActAddFolder.SimpleForm(aEmuteca: cEmuteca;
  aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmEmutecaActAddFolder;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmEmutecaActAddFolder';
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, 'Add Folder']);

    aFrame := TfmEmutecaActAddFolder.Create(aForm);
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

constructor TfmEmutecaActAddFolder.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmSystemCBX := TfmEmutecaSystemCBX.Create(gbxSelectSystem);
    fmSystemCBX.Align := alTop;
    fmSystemCBX.FirstItem := ETKSysCBXFISelect;
    fmSystemCBX.OnSelectSystem := @SelectSystem;
    fmSystemCBX.Parent := gbxSelectSystem;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;

  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmEmutecaActAddFolder.Destroy;
begin
  inherited Destroy;
end;

end.
