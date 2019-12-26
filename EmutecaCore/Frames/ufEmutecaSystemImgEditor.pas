unit ufEmutecaSystemImgEditor;
{< TfmEmutecaSystemImgEditor frame unit.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2018 Chixpy

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
  Classes, SysUtils, LazFileUtils, Forms, Controls, StdCtrls,
  EditBtn, ExtCtrls, Buttons, ActnList, ComCtrls, PairSplitter, inifiles,
  // CHX units
  uCHXStrUtils, uCHXDlgUtils,
  // CHX frames
  ufCHXPropEditor, ufCHXImgViewer,
  // Emuteca Core classes
  ucEmutecaSystem;

type

  { TfmEmutecaSystemImgEditor }

  TfmEmutecaSystemImgEditor = class(TfmCHXPropEditor)
    eSoftIconFolder: TDirectoryEdit;
    eSystemIcon: TFileNameEdit;
    eSystemBG: TFileNameEdit;
    eDefSoftIcon: TFileNameEdit;
    eSystemImage: TFileNameEdit;
    gbxIconFolder: TGroupBox;
    gbxImages: TGroupBox;
    gbxSystemIcon: TGroupBox;
    gbxSystemBG: TGroupBox;
    gbxSystemIcons: TGroupBox;
    gbxSystemImage: TGroupBox;
    iSystemIcon: TImage;
    iSystemBG: TImage;
    iDefSoftIcon: TImage;
    iSystemImage: TImage;
    lDefSoftIcon: TLabel;
    lSoftIconFolder: TLabel;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Splitter1: TSplitter;
    procedure eDefSoftIconAcceptFileName(Sender: TObject; Var Value: String);
    procedure eDefSoftIconButtonClick(Sender: TObject);
    procedure eSoftIconFolderButtonClick(Sender: TObject);
    procedure eSystemBGAcceptFileName(Sender: TObject; var Value: string);
    procedure eSystemBGButtonClick(Sender: TObject);
    procedure eSystemIconAcceptFileName(Sender: TObject; var Value: string);
    procedure eSystemIconButtonClick(Sender: TObject);
    procedure eSystemImageAcceptFileName(Sender: TObject; var Value: string);
    procedure eSystemImageButtonClick(Sender: TObject);
    procedure iDefSoftIconDblClick(Sender: TObject);
    procedure iSystemBGDblClick(Sender: TObject);
    procedure iSystemIconDblClick(Sender: TObject);
    procedure iSystemImageDblClick(Sender: TObject);

  private
    FGUIConfigIni: string;
    FGUIIconsIni: string;
    FSHA1Folder: string;
    FSystem: cEmutecaSystem;

    procedure SetGUIConfigIni(AValue: string);
    procedure SetGUIIconsIni(AValue: string);
    procedure SetSHA1Folder(AValue: string);
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    property GUIIconsIni: string read FGUIIconsIni write SetGUIIconsIni;
    property GUIConfigIni: string read FGUIConfigIni write SetGUIConfigIni;

    procedure UpdateImage(aTImage: TImage; aFile: string);

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

    procedure DoLoadGUIIcons(aIconsIni: TIniFile; const aBaseFolder: string);
      override;
    procedure DoLoadGUIConfig(aIniFile: TIniFile);

  public
    { public declarations }
    property System: cEmutecaSystem read FSystem write SetSystem;
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemImgEditor }

procedure TfmEmutecaSystemImgEditor.eSystemImageAcceptFileName(Sender: TObject;
  var Value: string);
begin
  UpdateImage(iSystemImage, Value);
end;

procedure TfmEmutecaSystemImgEditor.eSystemImageButtonClick(Sender: TObject);
begin
  if Assigned(System) then
    SetFileEditInitialDir(eSystemImage, System.BaseFolder)
  else
    SetFileEditInitialDir(eSystemImage, '');
end;

procedure TfmEmutecaSystemImgEditor.iDefSoftIconDblClick(Sender: TObject);
begin
    TfmCHXImgViewer.SimpleFormI(eDefSoftIcon.Text, SHA1Folder, GUIIconsIni,
    GUIConfigIni);
end;

procedure TfmEmutecaSystemImgEditor.iSystemBGDblClick(Sender: TObject);
begin
  TfmCHXImgViewer.SimpleFormI(eSystemBG.Text, SHA1Folder, GUIIconsIni,
    GUIConfigIni);
end;

procedure TfmEmutecaSystemImgEditor.iSystemIconDblClick(Sender: TObject);
begin
  TfmCHXImgViewer.SimpleFormI(eSystemIcon.Text, SHA1Folder, GUIIconsIni,
    GUIConfigIni);
end;

procedure TfmEmutecaSystemImgEditor.iSystemImageDblClick(Sender: TObject);
begin
  TfmCHXImgViewer.SimpleFormI(eSystemImage.Text, SHA1Folder,
    GUIIconsIni, GUIConfigIni);
end;

procedure TfmEmutecaSystemImgEditor.eSystemIconAcceptFileName(Sender: TObject;
  var Value: string);
begin
  UpdateImage(iSystemIcon, Value);
end;

procedure TfmEmutecaSystemImgEditor.eSystemIconButtonClick(Sender: TObject);
begin
  if Assigned(System) then
    SetFileEditInitialDir(eSystemIcon, System.BaseFolder)
  else
    SetFileEditInitialDir(eSystemIcon, '');
end;

procedure TfmEmutecaSystemImgEditor.eSystemBGAcceptFileName(Sender: TObject;
  var Value: string);
begin
  UpdateImage(iSystemBG, Value);
end;

procedure TfmEmutecaSystemImgEditor.eSoftIconFolderButtonClick(Sender: TObject);
begin
  if Assigned(System) then
    SetDirEditInitialDir(eSoftIconFolder, System.BaseFolder)
  else
    SetDirEditInitialDir(eSoftIconFolder, '');
end;

procedure TfmEmutecaSystemImgEditor.eDefSoftIconAcceptFileName(Sender: TObject;
  var Value: String);
begin
  UpdateImage(iDefSoftIcon, Value);
end;

procedure TfmEmutecaSystemImgEditor.eDefSoftIconButtonClick(Sender: TObject);
begin
    if Assigned(System) then
    SetFileEditInitialDir(eDefSoftIcon, System.BaseFolder)
  else
    SetFileEditInitialDir(eDefSoftIcon, '');
end;

procedure TfmEmutecaSystemImgEditor.eSystemBGButtonClick(Sender: TObject);
begin
  if Assigned(System) then
    SetFileEditInitialDir(eSystemBG, System.BaseFolder)
  else
    SetFileEditInitialDir(eSystemBG, '');
end;

procedure TfmEmutecaSystemImgEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  LoadFrameData;
end;

procedure TfmEmutecaSystemImgEditor.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
end;

procedure TfmEmutecaSystemImgEditor.SetGUIConfigIni(AValue: string);
begin
  FGUIConfigIni := SetAsFile(AValue);
end;

procedure TfmEmutecaSystemImgEditor.SetGUIIconsIni(AValue: string);
begin
  FGUIIconsIni := SetAsFile(AValue);
end;

procedure TfmEmutecaSystemImgEditor.DoSaveFrameData;
begin
  if not Enabled then
    Exit;

  System.IconFile := eSystemIcon.FileName;
  System.ImageFile := eSystemImage.FileName;
  System.BackgroundFile := eSystemBG.FileName;
  System.IconFolder := eSoftIconFolder.Directory;
  System.SoftIconFile := eDefSoftIcon.FileName;
end;

procedure TfmEmutecaSystemImgEditor.DoLoadGUIIcons(aIconsIni: TIniFile;
  const aBaseFolder: string);
begin
  inherited DoLoadGUIIcons(aIconsIni, aBaseFolder);
  GUIIconsIni := aIconsIni.FileName;
end;

procedure TfmEmutecaSystemImgEditor.DoLoadGUIConfig(aIniFile: TIniFile);
begin
  GUIConfigIni := aIniFile.FileName;
end;

procedure TfmEmutecaSystemImgEditor.DoLoadFrameData;
begin
  Enabled := Assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  eSystemImage.FileName := System.ImageFile;
  UpdateImage(iSystemImage, eSystemImage.FileName);
  eSystemBG.FileName := System.BackgroundFile;
  UpdateImage(iSystemBG, eSystemBG.FileName);
  eSystemIcon.FileName := System.IconFile;
  UpdateImage(iSystemIcon, eSystemIcon.FileName);
  eSoftIconFolder.Directory := System.IconFolder;
  eDefSoftIcon.FileName := System.SoftIconFile;
  UpdateImage(iDefSoftIcon, eDefSoftIcon.FileName);

end;

constructor TfmEmutecaSystemImgEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
  OnLoadGUIConfig := @DoLoadGUIConfig;
  OnLoadGUIIcons := @DoLoadGUIIcons;
end;

destructor TfmEmutecaSystemImgEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TfmEmutecaSystemImgEditor.DoClearFrameData;
begin
  eSystemImage.Clear;
  iSystemImage.Picture.Clear;
  eSystemBG.Clear;
  iSystemBG.Picture.Clear;
  eSystemIcon.Clear;
  iSystemIcon.Picture.Clear;
  eSoftIconFolder.Clear;
  eDefSoftIcon.Clear;
  iDefSoftIcon.Picture.Clear;
end;

procedure TfmEmutecaSystemImgEditor.UpdateImage(aTImage: TImage; aFile: string);
begin
  if not Assigned(aTImage) then
    Exit;

  if FileExistsUTF8(aFile) then
    aTImage.Picture.LoadFromFile(aFile)
  else
    aTImage.Picture.Clear;
end;

end.
