unit ufETKGUISysPreview;
{< TfmETKGUISysPreview frame unit.

  ----

  This file is part of Emuteca GUI.

  Copyright (C) 2011-2017 Chixpy

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, LCLIntf, ComCtrls, LazFileUtils, IniFiles,
  // CHX units
  uCHXImageUtils, uCHXStrUtils,
  // CHX frames
  ufCHXFrame, ufCHXImgViewer,
  // Emuteca Core units
  uEmutecaConst,uEmutecaRscStr,
  // Emuteca Core classes
  ucEmutecaSystem, ucEmutecaEmulator,
  // Emuteca Core frames
  ufEmutecaEmulatorCBX,
  // Emuteca GUI units
  uETKGUIConst;

type

  { TfmETKGUISysPreview }

  TfmETKGUISysPreview = class(TfmCHXFrame)
    actOpenSystemFolder: TAction;
    ActionList: TActionList;
    eEmuLastTime: TEdit;
    eSysNSoft: TEdit;
    eSysNGroups: TEdit;
    eEmuPlayedTime: TEdit;
    eEmuNTimes: TEdit;
    gbxEmulator: TGroupBox;
    ilActions: TImageList;
    eSysLastTime: TEdit;
    eSysNTimes: TEdit;
    eSysPlayedTime: TEdit;
    gbxStats: TGroupBox;
    lSystemTitle: TLabel;
    pEmulator: TPanel;
    pSysImage: TPanel;
    Splitter1: TSplitter;
    SysImage: TImage;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure actOpenSystemFolderExecute(Sender: TObject);
    procedure SysImageDblClick(Sender: TObject);

  private
    FCurrentEmu: cEmutecaEmulator;
    FfEmulatorCBX: TfmEmutecaEmulatorCBX;
    FGUIConfigIni: string;
    FGUIIconsIni: string;
    FOnChangeEmulator: TEmutecaReturnEmulatorCB;
    FSHA1Folder: string;
    FSystem: cEmutecaSystem;
    procedure SetCurrentEmu(const aCurrentEmu: cEmutecaEmulator);
    procedure SetGUIConfigIni(AValue: string);
    procedure SetGUIIconsIni(AValue: string);
    procedure SetOnChangeEmulator(
      const aOnSelectEmulator: TEmutecaReturnEmulatorCB);
    procedure SetSHA1Folder(AValue: string);
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    property fEmulatorCBX: TfmEmutecaEmulatorCBX read FfEmulatorCBX;

    property GUIIconsIni: string read FGUIIconsIni write SetGUIIconsIni;
    property GUIConfigIni: string read FGUIConfigIni write SetGUIConfigIni;

    function DoSelectEmulator(aEmulator: cEmutecaEmulator): boolean;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;

    procedure DoLoadGUIConfig(aIniFile: TIniFile);
    procedure DoLoadGUIIcons(aIconsIni: TIniFile; const aBaseFolder: string);

  public
    property System: cEmutecaSystem read FSystem write SetSystem;

    property CurrentEmu: cEmutecaEmulator read FCurrentEmu write SetCurrentEmu;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    property OnChangeEmulator: TEmutecaReturnEmulatorCB
      read FOnChangeEmulator write SetOnChangeEmulator;
    {< Callback when selecting an emulator. }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmETKGUISysPreview }

procedure TfmETKGUISysPreview.actOpenSystemFolderExecute(Sender: TObject);
begin
  if not Assigned(System) then
    Exit;

  OpenDocument(System.BaseFolder);
end;

procedure TfmETKGUISysPreview.SysImageDblClick(Sender: TObject);
begin
  if not Assigned(System) then
    Exit;

  if FileExistsUTF8(System.ImageFile) then
    TfmCHXImgViewer.SimpleFormI(System.ImageFile, SHA1Folder,
      GUIIconsIni, GUIConfigIni);
end;

procedure TfmETKGUISysPreview.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  LoadFrameData;
end;

function TfmETKGUISysPreview.DoSelectEmulator(aEmulator:
  cEmutecaEmulator): boolean;
begin
  Result := True;

  Self.CurrentEmu := aEmulator;
end;

procedure TfmETKGUISysPreview.DoLoadGUIIcons(aIconsIni: TIniFile;
  const aBaseFolder: string);
begin
  GUIIconsIni := aIconsIni.FileName;
  ReadActionsIconsIni(aIconsIni, aBaseFolder, Self.Name,
    ilActions, ActionList);
  FixComponentImagesFromActions(Self);
end;

constructor TfmETKGUISysPreview.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfEmulatorCBX := TfmEmutecaEmulatorCBX.Create(pEmulator);

    fEmulatorCBX.OnSelectEmulator := @DoSelectEmulator;
    fEmulatorCBX.Align := alClient;
    fEmulatorCBX.Parent := pEmulator;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;

  OnLoadGUIIcons := @DoLoadGUIIcons;
  OnLoadGUIConfig := @DoLoadGUIConfig;
end;

destructor TfmETKGUISysPreview.Destroy;
begin
  inherited Destroy;
end;

procedure TfmETKGUISysPreview.SetSHA1Folder(AValue: string);
begin
  if FSHA1Folder = AValue then
    Exit;
  FSHA1Folder := AValue;
end;

procedure TfmETKGUISysPreview.SetGUIConfigIni(AValue: string);
begin
  FGUIConfigIni := SetAsFile(AValue);
end;

procedure TfmETKGUISysPreview.SetCurrentEmu(
  const aCurrentEmu: cEmutecaEmulator);
begin
  if FCurrentEmu = aCurrentEmu then
    Exit;
  FCurrentEmu := aCurrentEmu;

  fEmulatorCBX.SelectedEmulator := aCurrentEmu;

  if assigned(aCurrentEmu) then
  begin
    eEmuPlayedTime.Text := aCurrentEmu.Stats.PlayingTimeStr;
    eEmuNTimes.Text := aCurrentEmu.Stats.TimesPlayedStr;
    eEmuLastTime.Text := aCurrentEmu.Stats.LastTimeStr;
  end
  else
  begin
    eEmuPlayedTime.Clear;
    eEmuNTimes.Clear;
    eEmuLastTime.Clear;
  end;

  if Assigned(OnChangeEmulator) then
    {Var := } OnChangeEmulator(aCurrentEmu);

end;

procedure TfmETKGUISysPreview.SetGUIIconsIni(AValue: string);
begin
  FGUIIconsIni := SetAsFile(AValue);
end;

procedure TfmETKGUISysPreview.SetOnChangeEmulator(
  const aOnSelectEmulator: TEmutecaReturnEmulatorCB);
begin
  if FOnChangeEmulator = aOnSelectEmulator then
    Exit;
  FOnChangeEmulator := aOnSelectEmulator;
end;

procedure TfmETKGUISysPreview.DoClearFrameData;
begin
  SysImage.Picture.Clear;

  lSystemTitle.Caption := 'System';
  eSysNSoft.Clear;
  eSysNGroups.Clear;
  eSysPlayedTime.Clear;
  eSysNTimes.Clear;
  eSysLastTime.Clear;

  fEmulatorCBX.EmulatorList := nil;
  eEmuPlayedTime.Clear;
  eEmuNTimes.Clear;
  eEmuLastTime.Clear;
end;

procedure TfmETKGUISysPreview.DoLoadFrameData;
begin
  Enabled := Assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  if FileExistsUTF8(System.ImageFile) then
    SysImage.Picture.LoadFromFile(System.ImageFile)
  else
    SysImage.Picture.Clear;

  fEmulatorCBX.EmulatorList := System.EmulatorList;
  fEmulatorCBX.SelectedEmulator := System.CurrentEmulator;

  lSystemTitle.Caption := System.Title;
  eSysNSoft.Text := Format(rsFmtNVersions,
    [System.SoftManager.FullList.Count]);
  eSysNGroups.Text := Format(rsFmtNGroups,
    [System.GroupManager.VisibleList.Count]);
  eSysPlayedTime.Text := System.Stats.PlayingTimeStr;
  eSysNTimes.Text := System.Stats.TimesPlayedStr;
  eSysLastTime.Text := System.Stats.LastTimeStr;

end;

procedure TfmETKGUISysPreview.DoLoadGUIConfig(aIniFile: TIniFile);
begin
  GUIConfigIni := aIniFile.FileName;
end;

end.
