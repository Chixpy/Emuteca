unit ufEmutecaActAddFolder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls,
  StdCtrls, EditBtn, Buttons, ExtCtrls,
  ucEmuteca, ucEmutecaSystem, ucEmutecaSoftware,
  uCHXFileUtils, uCHXStrUtils,
  u7zWrapper;

type

  { TfmEmutecaActAddFolder }

  TfmEmutecaActAddFolder = class(TFrame)
    bRun: TBitBtn;
    cbxSystem: TComboBox;
    chkIncSubfolders: TCheckBox;
    eExtensions: TEdit;
    eFolder: TDirectoryEdit;
    lExtensions: TLabel;
    lFolder: TLabel;
    lSystem: TLabel;
    Panel1: TPanel;
    procedure bRunClick(Sender: TObject);
    procedure cbxSystemChange(Sender: TObject);
    procedure eFolderButtonClick(Sender: TObject);
  private
    FEmuteca: cEmuteca;
    procedure SetEmuteca(AValue: cEmuteca);
    { private declarations }
  public
    { public declarations }
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaActAddFolder }

procedure TfmEmutecaActAddFolder.cbxSystemChange(Sender: TObject);
var
  aSystem: cEmutecaSystem;
begin
  eExtensions.Text := '';

  if not assigned(Emuteca) then
    Exit;
  if cbxSystem.ItemIndex = -1 then
    Exit;

  aSystem := cEmutecaSystem(cbxSystem.Items.Objects[cbxSystem.ItemIndex]);

  eExtensions.Text := aSystem.Extensions.CommaText;

  eFolder.Directory := CreateAbsolutePath(aSystem.BaseFolder,
    ProgramDirectory);
end;

procedure TfmEmutecaActAddFolder.eFolderButtonClick(Sender: TObject);
var
  aEFN: TDirectoryEdit;
  aSystem: cEmutecaSystem;
begin
  if cbxSystem.ItemIndex <> -1 then
    aSystem := cEmutecaSystem(cbxSystem.Items.Objects[cbxSystem.ItemIndex]);

  aEFN := TDirectoryEdit(Sender);
  aEFN.RootDir := aEFN.Directory;

  if (aEFN.RootDir = '') and Assigned(aSystem) then
    aEFN.RootDir := aSystem.BaseFolder;
end;

procedure TfmEmutecaActAddFolder.bRunClick(Sender: TObject);
var
  aSystem: cEmutecaSystem;
  FolderList, FileList: TStrings;
  aVersion: cEmutecaSoftware;
  i: integer;
begin
  { TODO : Must be in cEmuteca }
  if not assigned(Emuteca) then
    Exit;
  if cbxSystem.ItemIndex = -1 then
    Exit;
  if not DirectoryExistsUTF8(eFolder.Text) then
    Exit;

  aSystem := cEmutecaSystem(cbxSystem.Items.Objects[cbxSystem.ItemIndex]);
  if not assigned(aSystem) then
    Exit;

  FolderList := TStringList.Create;
  FileList := TStringList.Create;
  try
    // TODO: Maybe this must be an Emuteca procedure?
    if assigned(Emuteca.ProgressCallBack) then
      Emuteca.ProgressCallBack('Adding files', 'Searching for: ' +
        aSystem.Extensions.CommaText, 'This can take a while', 1, 1000);

    Search7ZFilesByExt(FolderList, FileList, eFolder.Text,
      aSystem.Extensions);

    i := 0;
    while i < FileList.Count do
    begin
      aVersion := cEmutecaSoftware.Create(nil);
      aVersion.Folder := FolderList[i];
      aVersion.FileName := FileList[i];
      aVersion.System := aSystem.ID;
      if FileExistsUTF8(FolderList[i]) then
      begin // it's a compressed archive
        { TODO 1 : Extract IDs... }
        aversion.ID := '';
        case aSystem.GameKey of
          TEFKCRC32:
            { TODO : We can know CRC32 without extracting... }
          begin
            w7zExtractFile(FolderList[i], FileList[i],
              Emuteca.TempFolder + 'Temp', False, '');
            aversion.ID :=
              CRC32FileStr(Emuteca.TempFolder + 'Temp\' + FileList[i]);
            DeleteDirectory(Emuteca.TempFolder + 'Temp', False);
          end;

          TEFKCustom: aversion.ID :=
              SetAsID(ExtractFileNameOnly(FileList[i]));

          TEFKFileName: aversion.ID :=
              SetAsID(ExtractFileNameOnly(FileList[i]));

          else  // TEFKSHA1 by default
          begin
            w7zExtractFile(FolderList[i], FileList[i],
              Emuteca.TempFolder + 'Temp', False, '');
            aversion.ID :=
              SHA1FileStr(Emuteca.TempFolder + 'Temp\' + FileList[i]);
            DeleteDirectory(Emuteca.TempFolder + 'Temp', False);
          end;
        end;

        aVersion.Parent :=
          RemoveFromBrackets(ExtractFileNameOnly(FolderList[i]));
        aVersion.Title :=
          RemoveFromBrackets(ExtractFileNameOnly(FileList[i]));
        aVersion.Description :=
          CopyFromBrackets(ExtractFileNameOnly(FileList[i]));
      end
      else
      begin  // it's a file

        aversion.ID := '';
        case aSystem.GameKey of
          TEFKCRC32: aversion.ID :=
              CRC32FileStr(aVersion.Folder + aVersion.FileName);
          TEFKCustom: aversion.ID := SetAsID(aVersion.FileName);
          TEFKFileName: aversion.ID := SetAsID(aVersion.FileName);
          else  // TEFKSHA1 by default
            aversion.ID := SHA1FileStr(aVersion.Folder + aVersion.FileName);
        end;

        aVersion.Parent :=
          RemoveFromBrackets(ExtractFileNameOnly(aVersion.FileName));
        aVersion.Title := aVersion.Parent;
        aVersion.Description :=
          CopyFromBrackets(ExtractFileNameOnly(aVersion.FileName));
      end;

      Emuteca.SoftManager.FullList.Add(aVersion);

      if assigned(Emuteca.ProgressCallBack) then
        Emuteca.ProgressCallBack('Adding files', FolderList[i],
          FileList[i], i + 1, FolderList.Count);
      Inc(i);
    end;
    // Closing ProgressBar...
    if assigned(Emuteca.ProgressCallBack) then
      Emuteca.ProgressCallBack('', '', '', 0, 0);

  finally
    FreeAndNil(FolderList);
    FreeAndNil(FileList);
  end;
end;

procedure TfmEmutecaActAddFolder.SetEmuteca(AValue: cEmuteca);
begin
  FEmuteca := AValue;

  cbxSystem.Clear;

  if not assigned(Emuteca) then
    Exit;

  Emuteca.SystemManager.AssingEnabledTo(cbxSystem.Items);
end;

end.
