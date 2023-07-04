unit ufEmutecaActImportSoftData;

{< TfmEmutecaActImportSoftData frame unit.

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
  StdCtrls, Buttons, ActnList, EditBtn,
  // CHX units
  uCHXDlgUtils,
  // CHX forms
  ufrCHXForm,
  // CHX frames
  ufCHXPropEditor, ufCHXProgressBar,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr, uEmutecaCommon,
  // Emuteca Core classes
  ucEmuteca, ucEmutecaSystem, ucEmutecaSoftware,
  // Emuteca Core frames
  ufEmutecaSystemCBX;

type

  { TfmEmutecaActImportSoftData }

  TfmEmutecaActImportSoftData = class(TfmCHXPropEditor)
    eImportFile: TFileNameEdit;
    eSoftIDType: TEdit;
    gbxImportFile: TGroupBox;
    gbxSystemInfo: TGroupBox;
    lSoftIDType: TLabel;
    lWarning: TLabel;
    pSelectSystem: TPanel;
    procedure eImportFileButtonClick(Sender: TObject);

  private
    FEmuteca: cEmuteca;
    FfmProgressBar: TfmCHXProgressBar;
    FfmSystemCBX: TfmEmutecaSystemCBX;
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

{ TfmEmutecaActImportSoftData }

procedure TfmEmutecaActImportSoftData.SetSystem(AValue: cEmutecaSystem);
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

    if not IsCached then
    begin
      lWarning.Caption := Format(rsImportingNoSHA1,
        [aSoft.Folder, aSoft.FileName, i, System.SoftManager.FullList.Count]);
    end
    else
      lWarning.Caption := '';

    eImportFile.Enabled := True;
  end
  else
  begin
    eSoftIDType.Clear;
    lWarning.Caption := '';
    eImportFile.Enabled := False;
  end;

end;

procedure TfmEmutecaActImportSoftData.eImportFileButtonClick(Sender: TObject);
begin
  if Assigned(Emuteca) then
    SetFileEditInitialDir(eImportFile, Emuteca.BaseFolder);
end;

procedure TfmEmutecaActImportSoftData.SetEmuteca(AValue: cEmuteca);
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

procedure TfmEmutecaActImportSoftData.LoadFrameData;
begin
  inherited LoadFrameData;

  Enabled := Assigned(Emuteca);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmEmutecaActImportSoftData.SaveFrameData;
var
  SysPBCB: TEmutecaProgressCallBack; // System PB Backup
begin
  inherited SaveFrameData;

  if (eImportFile.FileName = '') or (not assigned(System)) then
    Exit;

  Self.Enabled := False;

  SysPBCB := System.ProgressCallBack;
  System.ProgressCallBack := @(fmProgressBar.UpdTextAndBar);

  System.ImportSoftGroupLists(ChangeFileExt(eImportFile.FileName, ''));

  System.ProgressCallBack := SysPBCB;

  Self.Enabled := True;
end;

class function TfmEmutecaActImportSoftData.SimpleForm(aEmuteca: cEmuteca;
  SelectedSystem: cEmutecaSystem;
  const aGUIConfigIni, aGUIIconsIni: string): integer;
var
  aFrame: TfmEmutecaActImportSoftData;
begin
  aFrame := TfmEmutecaActImportSoftData.Create(nil);

    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.Emuteca := aEmuteca;
    aFrame.fmSystemCBX.SelectedSystem := SelectedSystem;
    // fmSystemCBX.SelectedSystem don't trigger SetSystem() callback.
    aFrame.System := SelectedSystem;
    Result := GenSimpleModalForm(aFrame, 'frmEmutecaActImportSoftData',
      Format(krsFmtWindowCaption, [Application.Title, 'Import soft data...']),
      aGUIConfigIni, aGUIIconsIni);
end;

constructor TfmEmutecaActImportSoftData.Create(TheOwner: TComponent);

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

  // Extension filter
  eImportFile.Filter := rsFileMaskDescSoft + '|' + krsFileMaskSoft;
end;

destructor TfmEmutecaActImportSoftData.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmEmutecaActImportSoftData);

finalization
  UnRegisterClass(TfmEmutecaActImportSoftData);

end.
