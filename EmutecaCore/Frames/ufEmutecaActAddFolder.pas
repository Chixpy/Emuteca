unit ufEmutecaActAddFolder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, EditBtn, LazFileUtils, LazUTF8,
  u7zWrapper,
  uCHXFileUtils, uCHXStrUtils,
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
    eSystemExtensions: TEdit;
    gbxFolder: TGroupBox;
    gbxSelectSystem: TGroupBox;
    gbxSysInfo: TGroupBox;
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
  gbxFolder.Enabled := Assigned(aSystem);

  if not Assigned(aSystem) then
    Exit;

  eSystemExtensions.Text := aSystem.Extensions.CommaText;
  eFolder.RootDir := CreateAbsolutePath(aSystem.BaseFolder,
    ProgramDirectory);
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
var
  aSystem: cEmutecaSystem;
  FolderList, FileList: TStrings;
  aSoft: cEmutecaSoftware;
  i, j: integer;
  Found, IsCompressed: boolean;
  SoftSysList: cEmutecaSoftList;
begin
  // TODO: Make it faaaster!!
  if not assigned(Emuteca) then
    Exit;

  if not DirectoryExistsUTF8(eFolder.Text) then
    Exit;
  aSystem := fmSystemCBX.SelectedSystem;
  if not assigned(aSystem) then
    Exit;

  SoftSysList := cEmutecaSoftList.Create(False);
  SoftSysList.Assign(aSystem.SoftManager.FullList);

  FolderList := TStringList.Create;
  FolderList.BeginUpdate;
  FileList := TStringList.Create;
  FileList.BeginUpdate;

  try
    if assigned(Emuteca.ProgressCallBack) then
      Emuteca.ProgressCallBack('Adding files', 'Searching for: ' +
        aSystem.Extensions.CommaText, 'This can take a while', 1, 20);

    // Searching files
    if chkNoZip.Checked then
    begin
      // 1.- Straight search
      FindAllFiles(FileList, eFolder.Text,
        FileMaskFromStringList(aSystem.Extensions), True);

      // 1.1.- Splitting Folders and Filenames
      i := 0;
      while i < FileList.Count do
      begin
        FolderList.Add(SetAsFolder(ExtractFilePath(FileList[i])));
        FileList[i] := SetAsFile(ExtractFileName(FileList[i]));
        Inc(i);
      end;
    end
    else
      w7zFilesByExt(FolderList, FileList, eFolder.Text,
        aSystem.Extensions, True);

    // For every file found
    i := 0;
    while i < FileList.Count do
    begin

      if assigned(Emuteca.ProgressCallBack) then
        Emuteca.ProgressCallBack('Adding files', FolderList[i],
          FileList[i], i, FileList.Count);

      aSoft := nil;
      Found := False;
      j := 0;
      while (j < SoftSysList.Count) and (not Found) do
      begin
        aSoft := SoftSysList[j];
        // Same file
        if aSoft.MatchFile(FolderList[i], FileList[i]) then
        begin
          Found := True;

          case rgbFilename.ItemIndex of
            1: // Ignore
              aSoft := nil;
            else
              ;
          end;

          SoftSysList.Delete(j); // Speeds up following searchs
        end;
        Inc(j);
      end;

      if not Found then
      begin
        aSoft := cEmutecaSoftware.Create(nil);
        aSoft.Folder := FolderList[i];
        aSoft.FileName := FileList[i];
      end;

      if assigned(aSoft) then
      begin
        IsCompressed := FileExistsUTF8(
          ExcludeTrailingPathDelimiter(aSoft.Folder));

        // SHA1 = 0
        // It's updated in background when caching
        aSoft.SHA1 := kCHXSHA1Empty;

        // ID
        case aSystem.SoftExportKey of
          TEFKSHA1:
            aSoft.ID := '';

          TEFKCRC32:
          begin
            // Is a compressed file
            if IsCompressed then
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

        if not Found then
          aSystem.AddSoft(aSoft);
      end;

      Inc(i);
    end;

    FolderList.EndUpdate;
    FileList.EndUpdate;

  finally

    if assigned(Emuteca.ProgressCallBack) then
      Emuteca.ProgressCallBack('', '', '', 0, 0);

    Emuteca.CacheData;

    FreeAndNil(FolderList);
    FreeAndNil(FileList);
    FreeAndNil(SoftSysList);
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
