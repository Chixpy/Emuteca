unit ufEmutecaActExportSoftData;

{< TfmEmutecaActExportSoftData frame unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2018 Chixpy

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

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    //< Emuteca

    class function SimpleForm(aEmuteca: cEmuteca;
      SelectedSystem: cEmutecaSystem;
      const aGUIIconsIni, aGUIConfigIni: string): integer;
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
  IsCached: boolean;
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
    IsCached := True;
    if System.SoftExportKey = TEFKSHA1 then
    begin
      i := 0;
      while IsCached and (i < System.SoftManager.FullList.Count) do
      begin
        aSoft := System.SoftManager.FullList[i];
        IsCached := not aSoft.SHA1IsEmpty;
        Inc(i);
      end;
    end;

    eExportFile.Enabled := IsCached;

    if not IsCached then
    begin
      lWarning.Caption := Format(rsExportingNoSHA1,
        [aSoft.Folder, aSoft.FileName, i, System.SoftManager.FullList.Count]);
      eExportFile.FileName := '';
    end
    else
    begin
      lWarning.Caption := '';
      eExportFile.FileName :=
        ExtractFilePath(eExportFile.FileName) + System.ListFileName +
        krsFileExtSoft;
    end;
  end
  else
  begin
    eSoftIDType.Clear;
    lWarning.Caption := '';
    eExportFile.Enabled := False;
  end;
end;

procedure TfmEmutecaActExportSoftData.DoClearFrameData;
begin
  eSoftIDType.Clear;
  lWarning.Caption := '';
end;

procedure TfmEmutecaActExportSoftData.DoLoadFrameData;
begin
  Enabled := Assigned(Emuteca);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmEmutecaActExportSoftData.DoSaveFrameData;
var
  SysPBCB: TEmutecaProgressCallBack; // System PB Backup
  aFileWOExt: string;
begin
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
  const aGUIIconsIni, aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmEmutecaActExportSoftData;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmEmutecaActExportSoftData';
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, 'Export soft data']);

    aFrame := TfmEmutecaActExportSoftData.Create(aForm);
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.Emuteca := aEmuteca;
    aFrame.fmSystemCBX.SelectedSystem := SelectedSystem;
    // fmSystemCBX.SelectedSystem don't trigger SetSystem() callback.
    aFrame.System := SelectedSystem;

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
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

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;

  // Database file mask
  eExportFile.Filter := rsFileMaskDescSoft + '|' + krsFileMaskSoft;
end;

destructor TfmEmutecaActExportSoftData.Destroy;
begin
  inherited Destroy;
end;

end.
