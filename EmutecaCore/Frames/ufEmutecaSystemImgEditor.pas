unit ufEmutecaSystemImgEditor;

{< TfmEmutecaSystemImgEditor frame unit.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2023 Chixpy
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
    eSoftLogoFolder: TDirectoryEdit;
    eSystemIcon: TFileNameEdit;
    eDefSoftIcon: TFileNameEdit;
    eSystemImage: TFileNameEdit;
    gbxIconFolder: TGroupBox;
    gbxImages: TGroupBox;
    gbxSystemIcon: TGroupBox;
    gbxSystemIcons: TGroupBox;
    iSystemIcon: TImage;
    iDefSoftIcon: TImage;
    iSystemImage: TImage;
    lDefSoftIcon: TLabel;
    lSoftIconFolder: TLabel;
    lSoftLogoFolder: TLabel;
    Splitter1: TSplitter;
    procedure eDefSoftIconAcceptFileName(Sender: TObject; var Value: string);
    procedure eSystemIconAcceptFileName(Sender: TObject; var Value: string);
    procedure eSystemImageAcceptFileName(Sender: TObject; var Value: string);
    procedure iDefSoftIconDblClick(Sender: TObject);
    procedure iSystemIconDblClick(Sender: TObject);
    procedure iSystemImageDblClick(Sender: TObject);

    procedure DirEditButtonClick(Sender: TObject);
    procedure FileEditButtonClick(Sender: TObject);

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

    procedure DoLoadGUIIcons(aIconsIni: TIniFile; const aBaseFolder: string);
      override;
    procedure DoLoadGUIConfig(aIniFile: TIniFile); override;

  public
    { public declarations }
    property System: cEmutecaSystem read FSystem write SetSystem;
    //< System to edit.
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;
    //< SHA1 Folder (for image viewer)

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;
    procedure SaveFrameData; override;

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

procedure TfmEmutecaSystemImgEditor.iDefSoftIconDblClick(Sender: TObject);
begin
  TfmCHXImgViewer.SimpleFormI(eDefSoftIcon.Text, SHA1Folder, GUIIconsIni,
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

procedure TfmEmutecaSystemImgEditor.eDefSoftIconAcceptFileName(Sender: TObject;
  var Value: string);
begin
  UpdateImage(iDefSoftIcon, Value);
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

procedure TfmEmutecaSystemImgEditor.SaveFrameData;
begin
  if not Enabled then
    Exit;

  inherited SaveFrameData;

  System.IconFile := eSystemIcon.FileName;
  System.ImageFile := eSystemImage.FileName;
  System.IconFolder := eSoftIconFolder.Directory;
  System.LogoFolder := eSoftLogoFolder.Directory;
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
  inherited DoLoadGUIConfig(aIniFile);

  GUIConfigIni := aIniFile.FileName;
end;

procedure TfmEmutecaSystemImgEditor.LoadFrameData;
begin
  inherited LoadFrameData;

  Enabled := Assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  eSystemImage.FileName := System.ImageFile;
  UpdateImage(iSystemImage, eSystemImage.FileName);
  eSystemIcon.FileName := System.IconFile;
  UpdateImage(iSystemIcon, eSystemIcon.FileName);
  eSoftIconFolder.Directory := System.IconFolder;
  eSoftLogoFolder.Directory := System.LogoFolder;
  eDefSoftIcon.FileName := System.SoftIconFile;
  UpdateImage(iDefSoftIcon, eDefSoftIcon.FileName);

end;

constructor TfmEmutecaSystemImgEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmutecaSystemImgEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TfmEmutecaSystemImgEditor.ClearFrameData;
begin
  inherited ClearFrameData;

  eSystemImage.Clear;
  iSystemImage.Picture.Clear;
  eSystemIcon.Clear;
  iSystemIcon.Picture.Clear;
  eSoftIconFolder.Clear;
  eSoftLogoFolder.Clear;
  eDefSoftIcon.Clear;
  iDefSoftIcon.Picture.Clear;
end;

procedure TfmEmutecaSystemImgEditor.UpdateImage(aTImage: TImage;
  aFile: string);
begin
  if not Assigned(aTImage) then
    Exit;

  if FileExistsUTF8(aFile) then
    aTImage.Picture.LoadFromFile(aFile)
  else
    aTImage.Picture.Clear;
end;

procedure TfmEmutecaSystemImgEditor.DirEditButtonClick(Sender: TObject);
begin
  if not (Sender is TDirectoryEdit) then
    Exit;

  if Assigned(System) then // In case...
    SetDirEditInitialDir(TDirectoryEdit(Sender), System.BaseFolder)
  else
    SetDirEditInitialDir(TDirectoryEdit(Sender), '');
end;

procedure TfmEmutecaSystemImgEditor.FileEditButtonClick(Sender: TObject);
begin
  if not (Sender is TFileNameEdit) then
    Exit;

  if Assigned(System) then // In case...
    SetFileEditInitialDir(TFileNameEdit(Sender), System.BaseFolder)
  else
    SetFileEditInitialDir(TFileNameEdit(Sender), '');
end;

initialization
  RegisterClass(cEmutecaSystem);

finalization
  UnRegisterClass(cEmutecaSystem);
  
end.
{
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
