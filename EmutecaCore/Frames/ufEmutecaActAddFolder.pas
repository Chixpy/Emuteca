unit ufEmutecaActAddFolder;

{< TfmEmutecaActAddFolder frame unit.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2023 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, EditBtn, LazFileUtils, LazUTF8,
  // CHX units
  uCHX7zWrapper, uCHXFileUtils, uCHXStrUtils, uCHXDlgUtils,
  // CHX forms
  ufrCHXForm,
  // CHX frames
  ufCHXPropEditor,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr, uEmutecaCommon,
  // Enuteca Core abstracts
  uaEmutecaCustomSystem,
  // Emuteca Core clases
  ucEmuteca, ucEmutecaSystem, ucEmutecaSoftList, ucEmutecaSoftware,
  // Emuteca Core frames
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

    procedure SelectSystem(aSystem: cEmutecaSystem);

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure LoadFrameData; override;
    procedure SaveFrameData; override;

    class function SimpleForm(aEmuteca: cEmuteca;
      SelectedSystem: cEmutecaSystem; const aGUIIconsIni: string;
      const aGUIConfigIni: string): integer;
    //< Creates a form with TfmEmutecaActAddFolder frame.

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
  begin
    fmSystemCBX.SystemList := Emuteca.SystemManager.EnabledList;
  end
  else
  begin
    fmSystemCBX.SystemList := nil;
  end;

  fmSystemCBX.SelectedSystem := nil;

  LoadFrameData;
end;

procedure TfmEmutecaActAddFolder.SelectSystem(aSystem: cEmutecaSystem);
begin
  eSystemExtensions.Text := '';
  eSystemExportKey.Text := '';
  eFolder.Text := '';
  gbxFolder.Enabled := Assigned(aSystem);

  if not Assigned(aSystem) then
    Exit;

  eSystemExtensions.Text := aSystem.Extensions.CommaText;
  eSystemExportKey.Text := SoftExportKey2StrK(aSystem.SoftExportKey);

  SetDirEditInitialDir(eFolder, aSystem.BaseFolder);
end;

procedure TfmEmutecaActAddFolder.LoadFrameData;
begin
  inherited LoadFrameData;

  Enabled := Assigned(Emuteca);

  //if not Enabled then
  //begin
  //  ClearFrameData;
  //  Exit;
  //end;
end;

procedure TfmEmutecaActAddFolder.SaveFrameData;

  procedure AddFile(aFolder, aFile: string; aSystem: cEmutecaSystem;
    aCacheSoftList: cEmutecaSoftList);
  var
    aSoft: cEmutecaSoftware;
    aComp: integer;
    Found: boolean;
  begin
    // aCacheSoftList is sorted.
    // aFolder, aFile will enter this procedure sorted too.

    // Search if file is already added to system list,
    //   we will delete items from aCacheSoftList <= aFolder+aFile
    // Similar to import/export dragons

    aSoft := nil;
    Found := False;
    aComp := -1;
    while (aCacheSoftList.Count > 0) and (aComp < 0) do
    begin
      aSoft := aCacheSoftList[0];

      aComp := aSoft.CompareFile(aFolder, aFile);

      if aComp = 0 then
      begin
        // Match file
        Found := True;

        case rgbFilename.ItemIndex of
          0: // Ignore  (set to nil and Found)
            aSoft := nil;
          else // Update SHA
            ;
        end;
      end;

      if aComp <= 0 then
        aCacheSoftList.Delete(0); // Removing from cache
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

      // Add it, if not found
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
  Continue: boolean;
begin
  inherited SaveFrameData;

  if not assigned(Emuteca) then
    Exit;

  if not DirectoryExistsUTF8(eFolder.Text) then
    Exit;

  aSystem := fmSystemCBX.SelectedSystem;
  if not assigned(aSystem) then
    Exit;

  Self.Enabled := False;

  // Loading data if not already loaded
  Emuteca.SystemManager.LoadSystemData(aSystem);

  // Copy actual soft list to CacheSoftList
  CacheSoftList := cEmutecaSoftList.Create(False);
  CacheSoftList.Assign(aSystem.SoftManager.FullList);
  CacheSoftList.Sort(@EmutecaCompareSoftByFileName);

  FileList := TStringList.Create;
  ComprFileList := TStringList.Create;
  try
    if assigned(Emuteca.ProgressCallBack) then
      Continue := Emuteca.ProgressCallBack('Making list of all files',
        Format('This can take a while. Searching for: %0:s',
        [aSystem.Extensions.CommaText]), 1, 100, True);

    // 1.- Straight search of all files
    aFileMask := FileMaskFromStringList(aSystem.Extensions);
    if not chkNoZip.Checked then
      aFileMask := aFileMask + ';' + FileMaskFromStringList(
        Emuteca.Config.CompressedExtensions);

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

      if SupportedExtSL(aFile, aSystem.Extensions) then
      begin // it's a supported file

        if assigned(Emuteca.ProgressCallBack) then
          Continue := Emuteca.ProgressCallBack('Adding files',
            FileList[i], i, FileList.Count, True);

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
          if assigned(Emuteca.ProgressCallBack) then
            Continue := Emuteca.ProgressCallBack('Adding files',
              ComprFileList[j], i, FileList.Count, True);

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
      Emuteca.ProgressCallBack('', '', 0, 0, False);

    ComprFileList.Free;
    FileList.Free;
    CacheSoftList.Free;

    Self.Enabled := True;
  end;
end;

class function TfmEmutecaActAddFolder.SimpleForm(aEmuteca: cEmuteca;
  SelectedSystem: cEmutecaSystem; const aGUIIconsIni: string;
  const aGUIConfigIni: string): integer;
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
    aFrame.fmSystemCBX.SelectedSystem := SelectedSystem;
    // fmSystemCBX.SelectedSystem don't trigger SetSystem() callback.
    aFrame.SelectSystem(SelectedSystem);

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
end;

destructor TfmEmutecaActAddFolder.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmEmutecaActAddFolder);

finalization
  UnRegisterClass(TfmEmutecaActAddFolder);

end.
