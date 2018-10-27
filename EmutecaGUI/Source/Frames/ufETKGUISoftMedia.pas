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
  ComCtrls, ActnList, IniFiles,
  // CHX units
  uCHXImageUtils,
  // CHX frames
  ufCHXFrame,
  // Emuteca classes
  ucEmutecaGroup, ucEmutecaSoftware,
  // Emuteca GUI frames
  uafETKGUISoftFoldersPreview, ufETKGUISoftImgPreview, ufETKGUISoftTxtPreview,
  ufETKGUISoftMusicPreview, ufETKGUISoftVideoPreview;

const
  krsIniSoftMediaFrameSection = 'SoftMedia';
  krsIniSoftMediaNPanels = 'NPanels';
  krsIniSoftMediaPanelType = 'Panel%0:d_Type';
  krsIniSoftMediaPanelHeight = 'Panel%0:d_Heigth';
  krsIniSoftMediaPanelCaption = 'Panel%0:d_Caption';

  krsIniImagePanelKey = 'Image';
  krsIniTextPanelKey = 'Text';
  krsIniVideoPanelKey = 'Video';
  krsIniMusicPanelKey = 'Music';
  krsIniUnknownPanelKey = 'Unknown';

type

  { TfmETKGUISoftMedia }

  TfmETKGUISoftMedia = class(TfmCHXFrame)
    actAddImagePanel: TAction;
    actAddTextPanel: TAction;
    actAddMusicPanel: TAction;
    actAddVideoPanel: TAction;
    actClearPanels: TAction;
    alMediaPanel: TActionList;
    ilMediaPanel: TImageList;
    ScrollBox1: TScrollBox;
    ToolBar1: TToolBar;
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
    //< Updates config of all childrens: Extensions and paths
    procedure UpdateChildrenGroup(aComponent: TComponent);
    //< Updates group of all childrens.
    procedure UpdateChildrenSoft(aComponent: TComponent);
    //< Updates soft of all childrens.

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
  // Maybe the is a better way to delete panels...
  while ScrollBox1.ComponentCount > 0 do
    ScrollBox1.Components[0].Free;
end;

procedure TfmETKGUISoftMedia.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;

  UpdateChildrenGroup(ScrollBox1);

  LoadFrameData;
end;

procedure TfmETKGUISoftMedia.SetImageExt(AValue: TStrings);
begin
  if FImageExt = AValue then
    Exit;
  FImageExt := AValue;

  UpdateChildrenConfig(ScrollBox1);
end;

procedure TfmETKGUISoftMedia.SetMPlayerPath(const aMPlayerPath: string);
begin
  if FMPlayerPath = aMPlayerPath then
    Exit;
  FMPlayerPath := aMPlayerPath;

  UpdateChildrenConfig(ScrollBox1);
end;

procedure TfmETKGUISoftMedia.SetMusicExt(const aMusicExt: TStrings);
begin
  if FMusicExt = aMusicExt then
    Exit;
  FMusicExt := aMusicExt;

  UpdateChildrenConfig(ScrollBox1);
end;

procedure TfmETKGUISoftMedia.SetSHA1Folder(const AValue: string);
begin
  if FSHA1Folder = AValue then
    Exit;
  FSHA1Folder := AValue;

  UpdateChildrenConfig(ScrollBox1);
end;

procedure TfmETKGUISoftMedia.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;

  UpdateChildrenSoft(ScrollBox1);

  LoadFrameData;
end;

procedure TfmETKGUISoftMedia.SetTempFolder(const aTempFolder: string);
begin
  if FTempFolder = aTempFolder then Exit;
  FTempFolder := aTempFolder;

  UpdateChildrenConfig(ScrollBox1)
end;

procedure TfmETKGUISoftMedia.SetTextExt(AValue: TStrings);
begin
  if FTextExt = AValue then
    Exit;
  FTextExt := AValue;

  UpdateChildrenConfig(ScrollBox1);
end;

procedure TfmETKGUISoftMedia.SetVideoExt(const aVideoExt: TStrings);
begin
  if FVideoExt = aVideoExt then
    Exit;
  FVideoExt := aVideoExt;

  UpdateChildrenConfig(ScrollBox1);
end;

function TfmETKGUISoftMedia.AddMediaPanel(
  aPanelClass: TfmaETKGUISoftFoldersPreviewClass;
  aHeight: integer): TfmaETKGUISoftFoldersPreview;
var
  aPanel: TPanel;
  aMediaPanel: TfmaETKGUISoftFoldersPreview;
begin

  aPanel := TPanel.Create(ScrollBox1);
  aMediaPanel := aPanelClass.Create(aPanel);

  aPanel.Align := alTop;
  if not (aMediaPanel is TfmETKGUISoftMusicPreview) then
    aPanel.Height := aHeight;

  //aMediaPanel.LoadGUIConfig();

  UpdateChildrenConfig(aPanel);

  aMediaPanel.Align := alClient;

  aMediaPanel.Parent := aPanel;
  aPanel.Parent := ScrollBox1;

  Result := aMediaPanel;
end;

procedure TfmETKGUISoftMedia.UpdateChildrenConfig(aComponent: TComponent);
var
  aChild: TComponent;
  i: integer;
begin
  if aComponent.ComponentCount = 0 then
    Exit;

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
  if aComponent.ComponentCount = 0 then
    Exit;

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
  if aComponent.ComponentCount = 0 then
    Exit;

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
  NPanels := aIniFile.ReadInteger(krsIniSoftMediaFrameSection,
    krsIniSoftMediaNPanels, -1);

  if NPanels = -1 then
  begin
    // Default config
    AddMediaPanel(TfmETKGUISoftMusicPreview, ScrollBox1.ClientWidth);
    AddMediaPanel(TfmETKGUISoftVideoPreview, ScrollBox1.ClientWidth);
    AddMediaPanel(TfmETKGUISoftImgPreview, ScrollBox1.ClientWidth);
    AddMediaPanel(TfmETKGUISoftTxtPreview, ScrollBox1.ClientWidth);
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
        Format(krsIniSoftMediaPanelHeight, [i]), ScrollBox1.ClientWidth);
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
  i: integer;
  aPanel: TPanel;
  aPreview: TfmaETKGUISoftFoldersPreview;
begin
  aIniFile.WriteInteger(krsIniSoftMediaFrameSection,
    krsIniSoftMediaNPanels, ScrollBox1.ComponentCount);

  i := 0;
  while i < ScrollBox1.ComponentCount do
  begin
    // Previews are inside TPanels
    if ScrollBox1.Components[i] is TPanel then
    begin
      aPanel := TPanel(ScrollBox1.Components[i]);

    { It's supposed that aPanel have only one component and is a
         TfmaETKGUISoftFoldersPreview... }

      if (aPanel.ComponentCount > 0) and (aPanel.Components[0] is
        TfmaETKGUISoftFoldersPreview) then
      begin
        aPreview := TfmaETKGUISoftFoldersPreview(aPanel.Components[0]);

        if aPreview is TfmETKGUISoftMusicPreview then
          aIniFile.WriteString(krsIniSoftMediaFrameSection,
            Format(krsIniSoftMediaPanelType, [i]), krsIniMusicPanelKey)
        else if aPreview is TfmETKGUISoftVideoPreview then
          aIniFile.WriteString(krsIniSoftMediaFrameSection,
            Format(krsIniSoftMediaPanelType, [i]), krsIniVideoPanelKey)
        else if aPreview is TfmETKGUISoftImgPreview then
          aIniFile.WriteString(krsIniSoftMediaFrameSection,
            Format(krsIniSoftMediaPanelType, [i]), krsIniImagePanelKey)
        else if aPreview is TfmETKGUISoftTxtPreview then
          aIniFile.WriteString(krsIniSoftMediaFrameSection,
            Format(krsIniSoftMediaPanelType, [i]), krsIniTextPanelKey);

        aIniFile.WriteInteger(krsIniSoftMediaFrameSection,
          Format(krsIniSoftMediaPanelHeight, [i]), aPanel.Height);
        aIniFile.WriteString(krsIniSoftMediaFrameSection,
          Format(krsIniSoftMediaPanelCaption, [i]), aPreview.LastCaption);
      end
      else
        aIniFile.WriteString(krsIniSoftMediaFrameSection,
          Format(krsIniSoftMediaPanelType, [i]), krsIniUnknownPanelKey);

    end;

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
