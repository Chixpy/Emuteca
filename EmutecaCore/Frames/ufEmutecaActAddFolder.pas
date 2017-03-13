unit ufEmutecaActAddFolder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, EditBtn, LazFileUtils, LazUTF8,
  u7zWrapper,
  uCHXFileUtils, uCHXStrUtils,
  ufCHXPropEditor,
  uEmutecaCommon,
  ucEmuteca, ucEmutecaSystem, ucEmutecaSoftware,
  ufEmutecaSystemCBX;

type

  { TfmEmutecaActAddFolder }

  TfmEmutecaActAddFolder = class(TfmCHXPropEditor)
    chkNoZip: TCheckBox;
    chkSubfolders: TCheckBox;
    eFolder: TDirectoryEdit;
    gbxFolder: TGroupBox;
    gbxSelectSystem: TGroupBox;
    lSystemInfo: TLabel;
    rgbFilename: TRadioGroup;
    rgbGroup: TRadioGroup;
  private
    FcbxSystem: TfmEmutecaSystemCBX;
    FEmuteca: cEmuteca;
    procedure SetEmuteca(AValue: cEmuteca);

  protected
    property cbxSystem: TfmEmutecaSystemCBX read FcbxSystem;

    procedure ClearData; override;

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

{ TfmEmutecaActAddFolder }

procedure TfmEmutecaActAddFolder.SetEmuteca(AValue: cEmuteca);
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

  Self.Enabled := Assigned(Emuteca);
end;

procedure TfmEmutecaActAddFolder.ClearData;
begin

end;

function TfmEmutecaActAddFolder.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := False;

  lSystemInfo.Caption := ' ';
  gbxFolder.Enabled := Assigned(aSystem);

  if not Assigned(aSystem) then
    Exit;

  lSystemInfo.Caption := aSystem.Extensions.CommaText;
  eFolder.RootDir := CreateAbsolutePath(aSystem.BaseFolder,
    ProgramDirectory);
end;

procedure TfmEmutecaActAddFolder.LoadData;
begin
  // Nada
end;

procedure TfmEmutecaActAddFolder.SaveData;
var
  aSystem: cEmutecaSystem;
  SoftSysList: cEmutecaSoftList;
  FolderList, FileList: TStrings;
  aSoft: cEmutecaSoftware;
  i, j: integer;
  Found, IsCompressed: boolean;
begin
  // TODO: Maybe this must be an Emuteca procedure?
  // TODO: Make it faaaster!!!
  if not assigned(Emuteca) then
    Exit;

  if not DirectoryExistsUTF8(eFolder.Text) then
    Exit;

  aSystem := cbxSystem.SelectedSystem;
  if not assigned(aSystem) then
    Exit;

  SoftSysList := cEmutecaSoftList.Create(False);
  FolderList := TStringList.Create;
  FolderList.BeginUpdate;
  FileList := TStringList.Create;
  FileList.BeginUpdate;
  try
    if assigned(Emuteca.ProgressCallBack) then
      Emuteca.ProgressCallBack('Adding files', 'Searching for: ' +
        aSystem.Extensions.CommaText, 'This can take a while', 2, 100);

    // Caching system software
    i := 0;
    while i < Emuteca.SoftManager.FullList.Count do
    begin
      aSoft := Emuteca.SoftManager.FullList[i];
      if aSoft.MatchSystem(aSystem) then
        SoftSysList.Add(aSoft);
      Inc(i);
    end;

    // Searching files
    if chkNoZip.Checked then
    begin
      // Speed up for MAME or Windows (Searching for exe...)
      // 1.- Straight search
      FindAllFiles(FileList, eFolder.Text,
        FileMaskFromStringList(aSystem.Extensions), True);

      // 1.1.- Splitting Folders and Filenames
      i := 0;
      while i < FileList.Count do
      begin
        FolderList.Add(ExtractFilePath(FileList[i]));
        FileList[i] := ExtractFileName(FileList[i]);
        Inc(i);
      end;
    end
    else
      Search7ZFilesByExt(FolderList, FileList, eFolder.Text,
        aSystem.Extensions);

    // For every file found
    i := 0;
    while i < FileList.Count do
    begin

      if assigned(Emuteca.ProgressCallBack) then
        Emuteca.ProgressCallBack('Adding files', FolderList[i],
          FileList[i], i + 1, FileList.Count);

      aSoft := nil;
      Found := False;

      j := 0;
      while (j < SoftSysList.Count) and (not Found) do
      begin
        // Same file
        if (CompareFilenames(SoftSysList[j].Filename, FileList[i]) = 0) and
          (CompareFilenames(SoftSysList[j].Folder, FolderList[i]) = 0) then
        begin
          case rgbFilename.ItemIndex of
            0: // Update
            begin
              aSoft := SoftSysList[j];
            end
            else // Ignore
              aSoft := nil;
          end;
          Found := True;
        end;
        Inc(j);
      end;

      if not Found then
      begin
        aSoft := cEmutecaSoftware.Create(nil);
        aSoft.Folder := FolderList[i];
        aSoft.FileName := FileList[i];
        aSoft.System := aSystem;

        Emuteca.SoftManager.FullList.Add(aSoft);
      end;

      if assigned(aSoft) then
      begin
        IsCompressed := FileExistsUTF8(
          ExcludeTrailingPathDelimiter(FolderList[i]));

        // SHA1 = 0
        // It's updated in background when caching
        aSoft.SHA1 := kEmuTKSHA1Empty;

        // ID
        case aSystem.GameKey of
          TEFKCRC32:
          begin
            // Is a compressed file
            if IsCompressed then
            begin
          { TODO : We can know CRC32 without extracting... with
            w7zListFiles }
              w7zExtractFile(FolderList[i], FileList[i],
                Emuteca.TempFolder + 'Temp', False, '');
              aSoft.ID :=
                CRC32FileStr(Emuteca.TempFolder + 'Temp\' + FileList[i]);
              DeleteDirectory(Emuteca.TempFolder + 'Temp', True);
            end
            else
            begin
              aSoft.ID := CRC32FileStr(aSoft.Folder + aSoft.FileName);
            end;
          end;

          TEFKCustom: aSoft.ID := ExtractFileNameOnly(FileList[i]);

          TEFKFileName: aSoft.ID := ExtractFileNameOnly(FileList[i]);

          else  // TEFKSHA1 by default
            //aSoft.ID := SHA1Print(aSoft.SHA1);
            ;
        end;

        aSoft.Title := RemoveFromBrackets(ExtractFileNameOnly(aSoft.FileName));
        aSoft.Version := CopyFromBrackets(ExtractFileNameOnly(aSoft.FileName));

        case rgbGroup.ItemIndex of
          1: // Group by filename
            aSoft.GroupKey := RemoveFromBrackets(ExtractFileNameOnly(aSoft.FileName))
             else
            aSoft.GroupKey := RemoveFromBrackets(ExtractFileNameOnly(
              ExcludeTrailingPathDelimiter(aSoft.Folder)));
        end;
      end;

      Inc(i);
    end;

    FolderList.EndUpdate;
    FileList.EndUpdate;

  finally

    if assigned(Emuteca.ProgressCallBack) then
      Emuteca.ProgressCallBack('', '', '', 0, 0);

    FreeAndNil(FolderList);
    FreeAndNil(FileList);
    FreeAndNil(SoftSysList);
    Emuteca.CacheData;
  end;
end;

constructor TfmEmutecaActAddFolder.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FcbxSystem := TfmEmutecaSystemCBX.Create(gbxSelectSystem);
    cbxSystem.Align := alTop;
    cbxSystem.OnSelectSystem := @SelectSystem;
    cbxSystem.Parent := gbxSelectSystem;
  end;

begin
  inherited Create(TheOwner);

  Self.Enabled := False;

  CreateFrames;
  Self.Invalidate;
end;

destructor TfmEmutecaActAddFolder.Destroy;
begin
  inherited Destroy;
end;

end.
