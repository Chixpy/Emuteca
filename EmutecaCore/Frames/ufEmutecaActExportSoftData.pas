unit ufEmutecaActExportSoftData;

{< TfmEmutecaActExportSoftData frame unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2023 Chixpy

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
  Buttons, ActnList, StdCtrls, EditBtn, ComCtrls, LazFileUtils, LazUTF8,
  // CHX units
  uCHXDlgUtils,
  // CHX forms
  ufrCHXForm,
  // CHX frames
  ufCHXPropEditor, ufCHXProgressBar,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr, uEmutecaCommon,
  // Emuteca Core abstracts
  uaEmutecaCustomSystem,
  // Emuteca Core classes
  ucEmuteca, ucEmutecaSystem, ucEmutecaSoftware,
  // Emuteca Core frames
  ufEmutecaSystemCBX;

const
  krsfrmEmutecaActExportSoftData = 'frmEmutecaActExportSoftData';

type

  { TfmEmutecaActExportSoftData }

  TfmEmutecaActExportSoftData = class(TfmCHXPropEditor)
    chkCopyInBaseFolder: TCheckBox;
    eExportFile: TFileNameEdit;
    eSoftIDType: TEdit;
    gbxExportFile: TGroupBox;
    gbxSystemInfo: TGroupBox;
    lExportInfo: TLabel;
    lSoftIDType: TLabel;
    lWarning: TLabel;
    pSelectSystem: TPanel;
    procedure eExportFileButtonClick(Sender: TObject);
  private
    FEmuteca: cEmuteca;
    FfmSystemCBX: TfmEmutecaSystemCBX;
    FfmProgressBar: TfmCHXProgressBar;
    FSystem: cEmutecaSystem;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    property fmSystemCBX: TfmEmutecaSystemCBX read FfmSystemCBX;
    property fmProgressBar: TfmCHXProgressBar read FfmProgressBar;


    property System: cEmutecaSystem read FSystem write SetSystem;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    //< Emuteca

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;
    procedure SaveFrameData; override;

    class function SimpleForm(aEmuteca: cEmuteca;
      SelectedSystem: cEmutecaSystem;
      const aGUIConfigIni, aGUIIconsIni: string): integer;
    //< Creates a form with AddFolder frame.

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaActExportSoftData }

procedure TfmEmutecaActExportSoftData.eExportFileButtonClick(Sender: TObject);
begin
  if Assigned(Emuteca) then
    SetFileEditInitialDir(eExportFile, Emuteca.BaseFolder);
end;

procedure TfmEmutecaActExportSoftData.SetEmuteca(AValue: cEmuteca);
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

procedure TfmEmutecaActExportSoftData.SetSystem(AValue: cEmutecaSystem);
var
  aSoft: cEmutecaSoftware;
  iNotCached: integer;
  i: integer;
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  if Assigned(System) then
  begin
    eSoftIDType.Text := SoftExportKey2StrK(System.SoftExportKey);

    // Loading data if not already loaded
    Emuteca.SystemManager.LoadSystemData(System);

    // Testing if all files have SHA1 cached
    iNotCached := System.IsSoftSHA1Cached;

    if iNotCached <= 0 then
    begin
      lWarning.Caption := Format(rsExportingNoSHA1,
        [aSoft.Folder, aSoft.FileName, iNotCached,
        System.SoftManager.FullList.Count]);
      eExportFile.FileName := '';
      bSave.Enabled := False;
    end
    else
    begin
      lWarning.Caption := '';
      eExportFile.FileName :=
        ExtractFilePath(eExportFile.FileName) + System.ListFileName +
        krsFileExtSoft;
      bSave.Enabled := True;
    end;
  end
  else
  begin
    eSoftIDType.Clear;
    lWarning.Caption := 'Warning: There is not assigned system.';
    eExportFile.FileName := '';
    bSave.Enabled := False;
  end;
end;

procedure TfmEmutecaActExportSoftData.ClearFrameData;
begin
  inherited ClearFrameData;

  eSoftIDType.Clear;
  lWarning.Caption := '';
end;

procedure TfmEmutecaActExportSoftData.LoadFrameData;
begin
  inherited LoadFrameData;

  Enabled := Assigned(Emuteca);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmEmutecaActExportSoftData.SaveFrameData;
var
  SysPBCB: TEmutecaProgressCallBack; // System PB Backup
  aFileWOExt: string;
begin
  inherited SaveFrameData;

  if (eExportFile.FileName = '') or (not assigned(System)) then
    Exit;

  Self.Enabled := False;

  SysPBCB := System.ProgressCallBack;
  System.ProgressCallBack := @(fmProgressBar.UpdTextAndBar);

  aFileWOExt := ChangeFileExt(eExportFile.FileName, '');

  System.ExportSoftGroupLists(aFileWOExt, False);

  System.ProgressCallBack := SysPBCB;

  if chkCopyInBaseFolder.Checked then
  begin
    if (System.BaseFolder <> '') and DirectoryExistsUTF8(
      System.BaseFolder) then
    begin
      CopyFile(UTF8ToSys(aFileWOExt + krsFileExtGroup),
        UTF8ToSys(System.BaseFolder + System.ListFileName + krsFileExtGroup));
      CopyFile(UTF8ToSys(aFileWOExt + krsFileExtSoft),
        UTF8ToSys(System.BaseFolder + System.ListFileName + krsFileExtSoft));
    end;
  end;

  Self.Enabled := True;
end;

class function TfmEmutecaActExportSoftData.SimpleForm(aEmuteca: cEmuteca;
  SelectedSystem: cEmutecaSystem;
  const aGUIConfigIni, aGUIIconsIni: string): integer;
var
  aFrame: TfmEmutecaActExportSoftData;
begin
  aFrame := TfmEmutecaActExportSoftData.Create(nil);

  aFrame.SaveButtons := True;
  aFrame.ButtonClose := True;
  aFrame.Align := alClient;

  aFrame.Emuteca := aEmuteca;
  aFrame.fmSystemCBX.SelectedSystem := SelectedSystem;
  // fmSystemCBX.SelectedSystem don't trigger SetSystem() callback.
  aFrame.System := SelectedSystem;

  Result := GenSimpleModalForm(aFrame, krsfrmEmutecaActExportSoftData,
    Format(krsFmtWindowCaption, [Application.Title, 'Export soft data']),
    aGUIConfigIni, aGUIIconsIni);
end;

constructor TfmEmutecaActExportSoftData.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmSystemCBX := TfmEmutecaSystemCBX.Create(pSelectSystem);
    fmSystemCBX.Align := alTop;
    fmSystemCBX.FirstItem := ETKSysCBXFISelect;
    fmSystemCBX.OnSelectSystem := @SetSystem;
    fmSystemCBX.Parent := pSelectSystem;

    FfmProgressBar := TfmCHXProgressBar.SimpleForm('');
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;

  // Database file mask
  eExportFile.Filter := rsFileMaskDescSoft + '|' + krsFileMaskSoft;
end;

destructor TfmEmutecaActExportSoftData.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmEmutecaActExportSoftData);

finalization
  UnRegisterClass(TfmEmutecaActExportSoftData);
end.
