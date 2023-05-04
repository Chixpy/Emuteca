unit ufETKGUISoftMedia;

{< TfmETKGUISoftMedia frame unit.

  This file is part of Emuteca GUI.

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
  ComCtrls, ActnList, StdCtrls, Menus, IniFiles,
  // CHX units
  uCHXStrUtils, uCHXImageUtils,
  // CHX frames
  ufCHXFrame,
  // Emuteca common
  uEmutecaCommon,
  // Emuteca classes
  ucEmutecaGroup, ucEmutecaSoftware,
  // Emuteca GUI abstract frames
  uafETKGUISoftFoldersPreview,
  // Emuteca GUI frames
  ufETKGUISoftImgPreview, ufETKGUISoftTxtPreview, ufETKGUISoftMusicPreview,
  ufETKGUISoftVideoPreview;

const
  krsIniSoftMediaFrameSection = 'SoftMedia';
  krsIniSoftIconLogoPanelHeight = 'IconLogo_Height';
  krsIniSoftMediaNPanels = 'NPanels';
  krsIniSoftMediaPanelType = 'Panel%0:d_Type';
  krsIniSoftMediaPanelHeight = 'Panel%0:d_Height';
  krsIniSoftMediaPanelCaption = 'Panel%0:d_Caption';

  krsIniImagePanelKey = 'Image';
  krsIniTextPanelKey = 'Text';
  krsIniVideoPanelKey = 'Video';
  krsIniMusicPanelKey = 'Music';
  krsIniUnknownPanelKey = 'Unknown';

  krsTempIconDir = 'Icon';
  krsTempLogoDir = 'Logo';

type

  { TfmETKGUISoftMedia }

  TfmETKGUISoftMedia = class(TfmCHXFrame)
    actAddImagePanel: TAction;
    actAddTextPanel: TAction;
    actAddMusicPanel: TAction;
    actAddVideoPanel: TAction;
    actClearPanels: TAction;
    actOpenIconInViewer: TAction;
    alMediaPanel: TActionList;
    iIcon: TImage;
    ilMediaPanel: TImageList;
    iLogo: TImage;
    miILFilterImages: TMenuItem;
    pIconLogo: TGroupBox;
    pmIconLogo: TPopupMenu;
    sbxMediaPanels: TScrollBox;
    Splitter1: TSplitter;
    tbSoftMediaPanel: TToolBar;
    tbAddImagePanel: TToolButton;
    tbAddTextPanel: TToolButton;
    tbAddVideoPanel: TToolButton;
    tbAddMusicPanel: TToolButton;
    ToolButton1: TToolButton;
    tbClearPanels: TToolButton;
    procedure actAddImagePanelExecute(Sender: TObject);
    procedure actAddMusicPanelExecute(Sender: TObject);
    procedure actAddTextPanelExecute(Sender: TObject);
    procedure actAddVideoPanelExecute(Sender: TObject);
    procedure actClearPanelsExecute(Sender: TObject);
    procedure pIconLogoResize(Sender: TObject);
  private
    FGroup: cEmutecaGroup;
    FImageExt: TStrings;
    FMPlayerPath: string;
    FMusicExt: TStrings;
    FSHA1Folder: string;
    FSoftware: cEmutecaSoftware;
    FTempFolder: string;
    FTextExt: TStrings;
    FVideoExt: TStrings;
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetImageExt(AValue: TStrings);
    procedure SetMPlayerPath(const aMPlayerPath: string);
    procedure SetMusicExt(const aMusicExt: TStrings);
    procedure SetSHA1Folder(const AValue: string);
    procedure SetSoftware(AValue: cEmutecaSoftware);
    procedure SetTempFolder(const aTempFolder: string);
    procedure SetTextExt(AValue: TStrings);
    procedure SetVideoExt(const aVideoExt: TStrings);

  protected
    function AddMediaPanel(aPanelClass: TfmaETKGUISoftFoldersPreviewClass;
      aHeight: integer): TfmaETKGUISoftFoldersPreview;

    procedure UpdateChildrenConfig(aComponent: TComponent);
    //< Updates config of all media panels: Extensions and paths
    procedure UpdateChildrenGroup(aComponent: TComponent);
    //< Updates group of all media panels.
    procedure UpdateChildrenSoft(aComponent: TComponent);
    //< Updates soft of all media panels.

    procedure DoLoadGUIConfig(aIniFile: TIniFile);
    procedure DoSaveGUIConfig(aIniFile: TIniFile);
    procedure DoLoadGUIIcons(aIconsIni: TIniFile; const aBaseFolder: string);

  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Group: cEmutecaGroup read FGroup write SetGroup;
    property TempFolder: string read FTempFolder write SetTempFolder;

    property ImageExt: TStrings read FImageExt write SetImageExt;
    property TextExt: TStrings read FTextExt write SetTextExt;
    property MusicExt: TStrings read FMusicExt write SetMusicExt;
    property VideoExt: TStrings read FVideoExt write SetVideoExt;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    property MPlayerPath: string read FMPlayerPath write SetMPlayerPath;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmETKGUISoftMedia }

procedure TfmETKGUISoftMedia.actAddImagePanelExecute(Sender: TObject);
begin
  AddMediaPanel(TfmETKGUISoftImgPreview, ClientWidth);
end;

procedure TfmETKGUISoftMedia.actAddMusicPanelExecute(Sender: TObject);
begin
  AddMediaPanel(TfmETKGUISoftMusicPreview, ClientWidth);
end;

procedure TfmETKGUISoftMedia.actAddTextPanelExecute(Sender: TObject);
begin
  AddMediaPanel(TfmETKGUISoftTxtPreview, ClientWidth);
end;

procedure TfmETKGUISoftMedia.actAddVideoPanelExecute(Sender: TObject);
begin
  AddMediaPanel(TfmETKGUISoftVideoPreview, ClientWidth);
end;

procedure TfmETKGUISoftMedia.actClearPanelsExecute(Sender: TObject);
begin
  // Maybe there is a better way to delete panels...
  while sbxMediaPanels.ComponentCount > 0 do
    sbxMediaPanels.Components[0].Free;
end;

procedure TfmETKGUISoftMedia.pIconLogoResize(Sender: TObject);
begin
  iIcon.Width := iIcon.Height;
end;

procedure TfmETKGUISoftMedia.SetGroup(AValue: cEmutecaGroup);
var
  aImageFile: String;
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;


  if Assigned(Group) then
  begin
    iIcon.Picture := Group.Stats.Icon;

    aImageFile := EmuTKSearchFirstRelatedFile(Group.CachedSystem.LogoFolder,
      Group.MediaFileName, ImageExt, True, True,
      SetAsFolder(TempFolder) + krsTempLogoDir);

    if aImageFile = '' then
      iLogo.Picture.Clear
    else
      iLogo.Picture.LoadFromFile(aImageFile);
  end
  else
  begin
    iIcon.Picture.Clear;
    iLogo.Picture.Clear;
  end;

  UpdateChildrenGroup(sbxMediaPanels);

  LoadFrameData;
end;

procedure TfmETKGUISoftMedia.SetImageExt(AValue: TStrings);
begin
  if FImageExt = AValue then
    Exit;
  FImageExt := AValue;

  UpdateChildrenConfig(sbxMediaPanels);
end;

procedure TfmETKGUISoftMedia.SetMPlayerPath(const aMPlayerPath: string);
begin
  if FMPlayerPath = aMPlayerPath then
    Exit;
  FMPlayerPath := aMPlayerPath;

  UpdateChildrenConfig(sbxMediaPanels);
end;

procedure TfmETKGUISoftMedia.SetMusicExt(const aMusicExt: TStrings);
begin
  if FMusicExt = aMusicExt then
    Exit;
  FMusicExt := aMusicExt;

  UpdateChildrenConfig(sbxMediaPanels);
end;

procedure TfmETKGUISoftMedia.SetSHA1Folder(const AValue: string);
begin
  if FSHA1Folder = AValue then
    Exit;
  FSHA1Folder := AValue;

  UpdateChildrenConfig(sbxMediaPanels);
end;

procedure TfmETKGUISoftMedia.SetSoftware(AValue: cEmutecaSoftware);
var
  aImageFile: string;
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;

  if Assigned(FSoftware) then
  begin
    iIcon.Picture := FSoftware.Stats.Icon;

    aImageFile := EmuTKSearchFirstRelatedFile(Software.CachedSystem.LogoFolder,
      Software.MediaFileName, ImageExt, True, True,
      SetAsFolder(TempFolder) + krsTempIconDir);

    if (aImageFile = '') and (not Software.MatchGroupFile) then
      aImageFile := EmuTKSearchFirstRelatedFile(
        Software.CachedSystem.LogoFolder, Software.CachedGroup.MediaFileName,
        ImageExt, True, True, SetAsFolder(TempFolder) + krsTempLogoDir);

    if aImageFile = '' then
      iLogo.Picture.Clear
    else
      iLogo.Picture.LoadFromFile(aImageFile);
  end
  else
  begin
    iIcon.Picture.Clear;
    iLogo.Picture.Clear;
    Group := nil;;
  end;

  UpdateChildrenSoft(sbxMediaPanels);

  LoadFrameData;
end;

procedure TfmETKGUISoftMedia.SetTempFolder(const aTempFolder: string);
begin
  FTempFolder := SetAsFolder(aTempFolder);

  UpdateChildrenConfig(sbxMediaPanels);
end;

procedure TfmETKGUISoftMedia.SetTextExt(AValue: TStrings);
begin
  if FTextExt = AValue then
    Exit;
  FTextExt := AValue;

  UpdateChildrenConfig(sbxMediaPanels);
end;

procedure TfmETKGUISoftMedia.SetVideoExt(const aVideoExt: TStrings);
begin
  if FVideoExt = aVideoExt then
    Exit;
  FVideoExt := aVideoExt;

  UpdateChildrenConfig(sbxMediaPanels);
end;

function TfmETKGUISoftMedia.AddMediaPanel(
  aPanelClass: TfmaETKGUISoftFoldersPreviewClass;
  aHeight: integer): TfmaETKGUISoftFoldersPreview;
var
  aSplitter: TSplitter;
  aMediaPanel: TfmaETKGUISoftFoldersPreview;
begin

  aSplitter := TSplitter.Create(sbxMediaPanels);
  aSplitter.Align := alTop;
  aSplitter.Parent := sbxMediaPanels;

  aMediaPanel := aPanelClass.Create(sbxMediaPanels);
  // Unique component name
  aMediaPanel.Name := aMediaPanel.Name +
    IntToStr(sbxMediaPanels.ComponentCount div 2);

  // Music preview have fixed size
  if not (aMediaPanel is TfmETKGUISoftMusicPreview) then
    aMediaPanel.Height := aHeight;

  //aMediaPanel.LoadGUIConfig();

  UpdateChildrenConfig(sbxMediaPanels);
  aMediaPanel.Align := alTop;
  aMediaPanel.Parent := sbxMediaPanels;

  Result := aMediaPanel;
end;

procedure TfmETKGUISoftMedia.UpdateChildrenConfig(aComponent: TComponent);
var
  aChild: TComponent;
  i: integer;
begin
  i := 0;
  while i < aComponent.ComponentCount do
  begin
    aChild := aComponent.Components[i];

    if (aChild is TfmaETKGUISoftFoldersPreview) then
    begin
      TfmaETKGUISoftFoldersPreview(aChild).TempFolder := TempFolder;

      if (aChild is TfmETKGUISoftImgPreview) then
      begin
        TfmETKGUISoftImgPreview(aChild).FileExt := ImageExt;
        TfmETKGUISoftImgPreview(aChild).SHA1Folder := SHA1Folder;
      end
      else if (aChild is TfmETKGUISoftTxtPreview) then
      begin
        TfmETKGUISoftTxtPreview(aChild).FileExt := TextExt;
      end
      else if (aChild is TfmETKGUISoftVideoPreview) then
      begin
        TfmETKGUISoftVideoPreview(aChild).FileExt := VideoExt;
        TfmETKGUISoftVideoPreview(aChild).MPlayerPath := MPlayerPath;
      end
      else if (aChild is TfmETKGUISoftMusicPreview) then
      begin
        TfmETKGUISoftMusicPreview(aChild).FileExt := MusicExt;
        TfmETKGUISoftMusicPreview(aChild).MPlayerPath := MPlayerPath;
      end;
    end
    else
      UpdateChildrenConfig(aChild);

    Inc(i);
  end;
end;

procedure TfmETKGUISoftMedia.UpdateChildrenGroup(aComponent: TComponent);
var
  aChild: TComponent;
  i: integer;
begin
  i := 0;
  while i < aComponent.ComponentCount do
  begin
    aChild := aComponent.Components[i];

    if (aChild is TfmaETKGUISoftFoldersPreview) then
    begin
      TfmaETKGUISoftFoldersPreview(aChild).Group := Group;
    end
    else
      UpdateChildrenGroup(aChild);

    Inc(i);
  end;
end;

procedure TfmETKGUISoftMedia.UpdateChildrenSoft(aComponent: TComponent);
var
  aChild: TComponent;
  i: integer;
begin
  i := 0;
  while i < aComponent.ComponentCount do
  begin
    aChild := aComponent.Components[i];

    if (aChild is TfmaETKGUISoftFoldersPreview) then
    begin
      TfmaETKGUISoftFoldersPreview(aChild).Software := Software;
    end
    else
      UpdateChildrenSoft(aChild);

    Inc(i);
  end;
end;

procedure TfmETKGUISoftMedia.DoLoadGUIConfig(aIniFile: TIniFile);
var
  NPanels, i, PanelHeight: integer;
  PanelType, PanelCaption: string;
  aPanel: TfmaETKGUISoftFoldersPreview;
begin
  pIconLogo.Height := aIniFile.ReadInteger(krsIniSoftMediaFrameSection,
    krsIniSoftIconLogoPanelHeight, pIconLogo.Height);

  NPanels := aIniFile.ReadInteger(krsIniSoftMediaFrameSection,
    krsIniSoftMediaNPanels, -1);

  if NPanels = -1 then
  begin
    // Default config
    AddMediaPanel(TfmETKGUISoftMusicPreview, sbxMediaPanels.ClientWidth);
    AddMediaPanel(TfmETKGUISoftVideoPreview, sbxMediaPanels.ClientWidth);
    AddMediaPanel(TfmETKGUISoftImgPreview, sbxMediaPanels.ClientWidth);
    AddMediaPanel(TfmETKGUISoftTxtPreview, sbxMediaPanels.ClientWidth);
  end
  else
  begin
    // Loading Config
    i := 0;
    while i < NPanels do
    begin
      aPanel := nil;
      PanelType := aIniFile.ReadString(krsIniSoftMediaFrameSection,
        Format(krsIniSoftMediaPanelType, [i]), krsIniUnknownPanelKey);

      PanelHeight := aIniFile.ReadInteger(krsIniSoftMediaFrameSection,
        Format(krsIniSoftMediaPanelHeight, [i]), sbxMediaPanels.ClientWidth);
      PanelCaption := aIniFile.ReadString(krsIniSoftMediaFrameSection,
        Format(krsIniSoftMediaPanelCaption, [i]), '');

      if CompareText(PanelType, krsIniMusicPanelKey) = 0 then
        aPanel := AddMediaPanel(TfmETKGUISoftMusicPreview, PanelHeight)
      else if CompareText(PanelType, krsIniVideoPanelKey) = 0 then
        aPanel := AddMediaPanel(TfmETKGUISoftVideoPreview, PanelHeight)
      else if CompareText(PanelType, krsIniTextPanelKey) = 0 then
        aPanel := AddMediaPanel(TfmETKGUISoftTxtPreview, PanelHeight)
      else if CompareText(PanelType, krsIniImagePanelKey) = 0 then
        aPanel := AddMediaPanel(TfmETKGUISoftImgPreview, PanelHeight);

      if Assigned(aPanel) then
        aPanel.LastCaption := PanelCaption;

      Inc(i);
    end;
  end;
end;

procedure TfmETKGUISoftMedia.DoSaveGUIConfig(aIniFile: TIniFile);
var
  i, j: integer;
  aPreview: TComponent;
begin
  aIniFile.WriteInteger(krsIniSoftMediaFrameSection,
    krsIniSoftIconLogoPanelHeight, pIconLogo.Height);

  // sbxMediaPanels.ComponentCount div 2 => Splitter + MediaPanel
  aIniFile.WriteInteger(krsIniSoftMediaFrameSection,
    krsIniSoftMediaNPanels, sbxMediaPanels.ComponentCount div 2);

  i := 0;
  while i < sbxMediaPanels.ComponentCount do
  begin
    aPreview := sbxMediaPanels.Components[i];
    j := i div 2;

    if (aPreview is TfmaETKGUISoftFoldersPreview) then
    begin
      if aPreview is TfmETKGUISoftMusicPreview then
        aIniFile.WriteString(krsIniSoftMediaFrameSection,
          Format(krsIniSoftMediaPanelType, [j]), krsIniMusicPanelKey)
      else if aPreview is TfmETKGUISoftVideoPreview then
        aIniFile.WriteString(krsIniSoftMediaFrameSection,
          Format(krsIniSoftMediaPanelType, [j]), krsIniVideoPanelKey)
      else if aPreview is TfmETKGUISoftImgPreview then
        aIniFile.WriteString(krsIniSoftMediaFrameSection,
          Format(krsIniSoftMediaPanelType, [j]), krsIniImagePanelKey)
      else if aPreview is TfmETKGUISoftTxtPreview then
        aIniFile.WriteString(krsIniSoftMediaFrameSection,
          Format(krsIniSoftMediaPanelType, [j]), krsIniTextPanelKey);

      aIniFile.WriteInteger(krsIniSoftMediaFrameSection,
        Format(krsIniSoftMediaPanelHeight, [j]),
        TfmaETKGUISoftFoldersPreview(aPreview).Height);
      aIniFile.WriteString(krsIniSoftMediaFrameSection,
        Format(krsIniSoftMediaPanelCaption, [j]),
        TfmaETKGUISoftFoldersPreview(aPreview).LastCaption);
    end;
    // else it's a Splitter

    Inc(i);
  end;
end;

procedure TfmETKGUISoftMedia.DoLoadGUIIcons(aIconsIni: TIniFile;
  const aBaseFolder: string);
begin
  ReadActionsIconsIni(aIconsIni, aBaseFolder, Self.Name,
    ilMediaPanel, alMediaPanel);
end;

constructor TfmETKGUISoftMedia.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // This frame can be enabled while empty to add frames and add/delete frames
  Enabled := True;

  OnLoadGUIConfig := @DoLoadGUIConfig;
  OnSaveGUIConfig := @DoSaveGUIConfig;
  OnLoadGUIIcons := @DoLoadGUIIcons;
end;

destructor TfmETKGUISoftMedia.Destroy;
begin
  inherited Destroy;
end;

end.
