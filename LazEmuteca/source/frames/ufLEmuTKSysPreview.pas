{ System preview frame of LazEmuteca

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
unit ufLEmuTKSysPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, LCLIntf, ComCtrls, LazFileUtils, IniFiles,
  uCHXImageUtils, uCHXStrUtils,
  ufCHXFrame, ufCHXImgViewer,
  ucEmutecaSystem, ucEmutecaEmulator,
  ufEmutecaEmulatorCBX,
  uLEmuTKCommon;

type

  { TfmLEmuTKSysPreview }

  TfmLEmuTKSysPreview = class(TfmCHXFrame)
    actOpenSystemFolder: TAction;
    ActionList: TActionList;
    eNSoft: TEdit;
    eNGroups: TEdit;
    gbxEmulator: TGroupBox;
    ilActions: TImageList;
    eLastTime: TEdit;
    eNTimes: TEdit;
    ePlayedTime: TEdit;
    gbxStats: TGroupBox;
    lSystemTitle: TLabel;
    Splitter1: TSplitter;
    SysImage: TImage;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure actOpenSystemFolderExecute(Sender: TObject);
    procedure SysImageDblClick(Sender: TObject);
  private
    FfEmulatorCBX: TfmEmutecaEmulatorCBX;
    FGUIConfigIni: string;
    FGUIIconsIni: string;
    FSHA1Folder: string;
    FSystem: cEmutecaSystem;
    procedure SetGUIConfigIni(AValue: string);
    procedure SetGUIIconsIni(AValue: string);
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
    procedure DoLoadGUIIcons(aIconsIni: TIniFile; aBaseFolder: string);

  public
    property System: cEmutecaSystem read FSystem write SetSystem;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSysPreview }

procedure TfmLEmuTKSysPreview.actOpenSystemFolderExecute(Sender: TObject);
begin
  if not Assigned(System) then
    Exit;

  OpenDocument(System.BaseFolder);
end;

procedure TfmLEmuTKSysPreview.SysImageDblClick(Sender: TObject);
begin
  if FileExistsUTF8(System.ImageFile) then
    TfmCHXImgViewer.SimpleFormI(System.ImageFile, SHA1Folder,
      GUIIconsIni, GUIConfigIni);
end;

procedure TfmLEmuTKSysPreview.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  LoadFrameData;
end;

function TfmLEmuTKSysPreview.DoSelectEmulator(aEmulator: cEmutecaEmulator
  ): boolean;
begin
  System.CurrentEmulator := aEmulator;
end;

procedure TfmLEmuTKSysPreview.DoLoadGUIIcons(aIconsIni: TIniFile;
  aBaseFolder: string);
begin
  GUIIconsIni := aIconsIni.FileName;
  ReadActionsIconsIni(aIconsIni, aBaseFolder, Self.Name,
    ilActions, ActionList);
  FixComponentImagesFromActions(Self);
end;

constructor TfmLEmuTKSysPreview.Create(TheOwner: TComponent);
  procedure CreateFrames;
  begin
    FfEmulatorCBX := TfmEmutecaEmulatorCBX.Create(gbxEmulator);

    fEmulatorCBX.OnSelectEmulator := @DoSelectEmulator;
    fEmulatorCBX.Align := alClient;
    fEmulatorCBX.Parent := gbxEmulator;
  end;
begin
  inherited Create(TheOwner);

  CreateFrames;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;

  OnLoadGUIIcons := @DoLoadGUIIcons;
  OnLoadGUIConfig := @DoLoadGUIConfig;
end;

destructor TfmLEmuTKSysPreview.Destroy;
begin
  inherited Destroy;
end;

procedure TfmLEmuTKSysPreview.SetSHA1Folder(AValue: string);
begin
  if FSHA1Folder = AValue then
    Exit;
  FSHA1Folder := AValue;
end;

procedure TfmLEmuTKSysPreview.SetGUIConfigIni(AValue: string);
begin
  FGUIConfigIni := SetAsFile(AValue);
end;

procedure TfmLEmuTKSysPreview.SetGUIIconsIni(AValue: string);
begin
  FGUIIconsIni := SetAsFile(AValue);
end;

procedure TfmLEmuTKSysPreview.DoClearFrameData;
begin
  SysImage.Picture.Clear;

  lSystemTitle.Caption := 'System';
  eNSoft.Clear;
  eNGroups.Clear;
  ePlayedTime.Clear;
  eNTimes.Clear;
  eLastTime.Clear;

  fEmulatorCBX.EmulatorList := nil;
end;

procedure TfmLEmuTKSysPreview.DoLoadFrameData;
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
  eNSoft.Text := Format(rsFmtNVersions, [System.SoftManager.FullList.Count]);
  eNGroups.Text := Format(rsFmtNGroups,
    [System.GroupManager.VisibleList.Count]);
  ePlayedTime.Text := System.Stats.PlayingTimeStr;
  eNTimes.Text := System.Stats.TimesPlayedStr;
  eLastTime.Text := System.Stats.LastTimeStr;

end;

procedure TfmLEmuTKSysPreview.DoLoadGUIConfig(aIniFile: TIniFile);
begin
  GUIConfigIni := aIniFile.FileName;
end;

end.
