{ System preview frame of Emuteca GUI

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
unit ufETKGUISysPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, LCLIntf, ComCtrls, LazFileUtils, IniFiles,
  // CHX units
  uCHXImageUtils, uCHXStrUtils,
  // CHX frames
  ufCHXFrame, ufCHXImgViewer,
  // Emuteca units
  uEmutecaCommon,
  // Emuteca classes
  ucEmutecaSystem, ucEmutecaEmulator,
  // Emuteca frames
  ufEmutecaEmulatorCBX,
  // Emuteca GUI units
  uETKGUICommon;

type

  { TfmETKGUISysPreview }

  TfmETKGUISysPreview = class(TfmCHXFrame)
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
    pSysImage: TPanel;
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

{ TfmETKGUISysPreview }

procedure TfmETKGUISysPreview.actOpenSystemFolderExecute(Sender: TObject);
begin
  if not Assigned(System) then
    Exit;

  OpenDocument(System.BaseFolder);
end;

procedure TfmETKGUISysPreview.SysImageDblClick(Sender: TObject);
begin
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

function TfmETKGUISysPreview.DoSelectEmulator(aEmulator: cEmutecaEmulator
  ): boolean;
begin
  Result := True;
  System.CurrentEmulator := aEmulator;
end;

procedure TfmETKGUISysPreview.DoLoadGUIIcons(aIconsIni: TIniFile;
  aBaseFolder: string);
begin
  GUIIconsIni := aIconsIni.FileName;
  ReadActionsIconsIni(aIconsIni, aBaseFolder, Self.Name,
    ilActions, ActionList);
  FixComponentImagesFromActions(Self);
end;

constructor TfmETKGUISysPreview.Create(TheOwner: TComponent);
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

procedure TfmETKGUISysPreview.SetGUIIconsIni(AValue: string);
begin
  FGUIIconsIni := SetAsFile(AValue);
end;

procedure TfmETKGUISysPreview.DoClearFrameData;
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
  eNSoft.Text := Format(rsFmtNVersions, [System.SoftManager.FullList.Count]);
  eNGroups.Text := Format(rsFmtNGroups,
    [System.GroupManager.VisibleList.Count]);
  ePlayedTime.Text := System.Stats.PlayingTimeStr;
  eNTimes.Text := System.Stats.TimesPlayedStr;
  eLastTime.Text := System.Stats.LastTimeStr;

end;

procedure TfmETKGUISysPreview.DoLoadGUIConfig(aIniFile: TIniFile);
begin
  GUIConfigIni := aIniFile.FileName;
end;

end.
