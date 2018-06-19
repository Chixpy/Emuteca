{ Software media frame of Emuteca GUI.

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
unit ufETKGUISoftMedia;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, ActnList, IniFiles,
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
    FTextExt: TStrings;
    FVideoExt: TStrings;
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetImageExt(AValue: TStrings);
    procedure SetMPlayerPath(const aMPlayerPath: string);
    procedure SetMusicExt(const aMusicExt: TStrings);
    procedure SetSHA1Folder(const AValue: string);
    procedure SetSoftware(AValue: cEmutecaSoftware);
    procedure SetTextExt(AValue: TStrings);
    procedure SetVideoExt(const aVideoExt: TStrings);

  protected
    function AddMediaPanel(aPanelClass: TfmaETKGUISoftFoldersPreviewClass;
      aHeight: integer): TfmaETKGUISoftFoldersPreview;

    procedure DoLoadGUIConfig(aIniFile: TIniFile);
    procedure DoSaveGUIConfig(aIniFile: TIniFile);

  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Group: cEmutecaGroup read FGroup write SetGroup;

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
  while ScrollBox1.ComponentCount > 0 do
    ScrollBox1.Components[0].Free;
end;

procedure TfmETKGUISoftMedia.SetGroup(AValue: cEmutecaGroup);
var
  i: integer;
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;

  i := 0;
  while i < ScrollBox1.ComponentCount do
  begin
    if ScrollBox1.Components[i] is TfmaETKGUISoftFoldersPreview then
      TfmaETKGUISoftFoldersPreview(ScrollBox1.Components[i]).Group := Group;
    Inc(i);
  end;

  LoadFrameData;
end;

procedure TfmETKGUISoftMedia.SetImageExt(AValue: TStrings);
var
  i: integer;
begin
  if FImageExt = AValue then
    Exit;
  FImageExt := AValue;

  i := 0;
  while i < ScrollBox1.ComponentCount do
  begin
    if ScrollBox1.Components[i] is TfmETKGUISoftImgPreview then
      TfmETKGUISoftImgPreview(ScrollBox1.Components[i]).FileExt := ImageExt;
    Inc(i);
  end;
end;

procedure TfmETKGUISoftMedia.SetMPlayerPath(const aMPlayerPath: string);
var
  i: integer;
begin
  if FMPlayerPath = aMPlayerPath then
    Exit;
  FMPlayerPath := aMPlayerPath;


  i := 0;
  while i < ScrollBox1.ComponentCount do
  begin
    if ScrollBox1.Components[i] is TfmETKGUISoftMusicPreview then
      TfmETKGUISoftMusicPreview(ScrollBox1.Components[i]).MPlayerPath :=
        MPlayerPath
    else if ScrollBox1.Components[i] is TfmETKGUISoftVideoPreview then
      TfmETKGUISoftVideoPreview(ScrollBox1.Components[i]).MPlayerPath :=
        MPlayerPath;
    Inc(i);
  end;
end;

procedure TfmETKGUISoftMedia.SetMusicExt(const aMusicExt: TStrings);
var
  i: integer;
begin
  if FMusicExt = aMusicExt then
    Exit;
  FMusicExt := aMusicExt;


  i := 0;
  while i < ScrollBox1.ComponentCount do
  begin
    if ScrollBox1.Components[i] is TfmETKGUISoftMusicPreview then
      TfmETKGUISoftMusicPreview(ScrollBox1.Components[i]).FileExt := MusicExt;
    Inc(i);
  end;
end;

procedure TfmETKGUISoftMedia.SetSHA1Folder(const AValue: string);
var
  i: integer;
begin
  if FSHA1Folder = AValue then
    Exit;
  FSHA1Folder := AValue;

  i := 0;
  while i < ScrollBox1.ComponentCount do
  begin
    if ScrollBox1.Components[i] is TfmETKGUISoftImgPreview then
      TfmETKGUISoftImgPreview(ScrollBox1.Components[i]).SHA1Folder :=
        SHA1Folder;
    Inc(i);
  end;
end;

procedure TfmETKGUISoftMedia.SetSoftware(AValue: cEmutecaSoftware);
var
  i: integer;
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;

  i := 0;
  while i < ScrollBox1.ComponentCount do
  begin
    if ScrollBox1.Components[i] is TfmaETKGUISoftFoldersPreview then
      TfmaETKGUISoftFoldersPreview(ScrollBox1.Components[i]).Software :=
        Software;
    Inc(i);
  end;

  LoadFrameData;
end;

procedure TfmETKGUISoftMedia.SetTextExt(AValue: TStrings);
var
  i: integer;
begin
  if FTextExt = AValue then
    Exit;
  FTextExt := AValue;

  i := 0;
  while i < ScrollBox1.ComponentCount do
  begin
    if ScrollBox1.Components[i] is TfmETKGUISoftTxtPreview then
      TfmETKGUISoftTxtPreview(ScrollBox1.Components[i]).FileExt := TextExt;
    Inc(i);
  end;
end;

procedure TfmETKGUISoftMedia.SetVideoExt(const aVideoExt: TStrings);
var
  i: integer;
begin
  if FVideoExt = aVideoExt then
    Exit;
  FVideoExt := aVideoExt;

  i := 0;
  while i < ScrollBox1.ComponentCount do
  begin
    if ScrollBox1.Components[i] is TfmETKGUISoftVideoPreview then
      TfmETKGUISoftVideoPreview(ScrollBox1.Components[i]).FileExt := VideoExt;
    Inc(i);
  end;
end;

function TfmETKGUISoftMedia.AddMediaPanel(
  aPanelClass: TfmaETKGUISoftFoldersPreviewClass;
  aHeight: integer): TfmaETKGUISoftFoldersPreview;
var
  aPanel: TfmaETKGUISoftFoldersPreview;
begin

  aPanel := aPanelClass.Create(ScrollBox1);

  //aPanel.LoadGUIConfig();

  if aPanel is TfmETKGUISoftImgPreview then
    aPanel.FileExt := ImageExt
  else if aPanel is TfmETKGUISoftTxtPreview then
    aPanel.FileExt := TextExt
  else if aPanel is TfmETKGUISoftMusicPreview then
  begin
    aPanel.FileExt := MusicExt;
    TfmETKGUISoftMusicPreview(aPanel).MPlayerPath := MPlayerPath;
  end
  else if aPanel is TfmETKGUISoftVideoPreview then
  begin
    aPanel.FileExt := VideoExt;
    TfmETKGUISoftVideoPreview(aPanel).MPlayerPath := MPlayerPath;
  end;

  aPanel.Name := aPanel.Name + IntToStr(ScrollBox1.ComponentCount + 1);
  if not (aPanel is TfmETKGUISoftMusicPreview) then
    aPanel.Height := aHeight;
  aPanel.Align := alTop;
  aPanel.Parent := ScrollBox1;

  Result := aPanel;
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

      PanelType := aIniFile.ReadString(krsIniSoftMediaFrameSection,
        Format(krsIniSoftMediaPanelType, [i]), krsIniUnknownPanelKey);

      if CompareText(PanelType, krsIniUnknownPanelKey) <> 0 then
      begin
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
        else //if CompareText(PanelType, krsIniImagePanelKey) = 0 then;
          aPanel := AddMediaPanel(TfmETKGUISoftImgPreview, PanelHeight);

        aPanel.LastCaption := PanelCaption;
      end;

      Inc(i);
    end;
  end;
end;

procedure TfmETKGUISoftMedia.DoSaveGUIConfig(aIniFile: TIniFile);
var
  i: integer;
  aPanel: TfmaETKGUISoftFoldersPreview;
begin
  aIniFile.WriteInteger(krsIniSoftMediaFrameSection,
    krsIniSoftMediaNPanels, ScrollBox1.ComponentCount);

  i := 0;
  while i < ScrollBox1.ComponentCount do
  begin
    if ScrollBox1.Components[i] is TfmaETKGUISoftFoldersPreview then
    begin
      aPanel := TfmaETKGUISoftFoldersPreview(ScrollBox1.Components[i]);

      if aPanel is TfmETKGUISoftMusicPreview then
        aIniFile.WriteString(krsIniSoftMediaFrameSection,
          Format(krsIniSoftMediaPanelType, [i]), krsIniMusicPanelKey)
      else if aPanel is TfmETKGUISoftVideoPreview then
        aIniFile.WriteString(krsIniSoftMediaFrameSection,
          Format(krsIniSoftMediaPanelType, [i]), krsIniVideoPanelKey)
      else if aPanel is TfmETKGUISoftImgPreview then
        aIniFile.WriteString(krsIniSoftMediaFrameSection,
          Format(krsIniSoftMediaPanelType, [i]), krsIniImagePanelKey)
      else if aPanel is TfmETKGUISoftTxtPreview then
        aIniFile.WriteString(krsIniSoftMediaFrameSection,
          Format(krsIniSoftMediaPanelType, [i]), krsIniTextPanelKey);

      aIniFile.WriteInteger(krsIniSoftMediaFrameSection,
        Format(krsIniSoftMediaPanelHeight, [i]), aPanel.Height);
      aIniFile.WriteString(krsIniSoftMediaFrameSection,
        Format(krsIniSoftMediaPanelCaption, [i]), aPanel.LastCaption);
    end
    else
      aIniFile.WriteString(krsIniSoftMediaFrameSection,
        Format(krsIniSoftMediaPanelType, [i]), krsIniUnknownPanelKey);

    Inc(i);
  end;
end;

constructor TfmETKGUISoftMedia.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // This frame can be enabled while empty to add frames and add/delete frames
  Enabled := True;

  OnLoadGUIConfig := @DoLoadGUIConfig;
  OnSaveGUIConfig := @DoSaveGUIConfig;
end;

destructor TfmETKGUISoftMedia.Destroy;
begin
  inherited Destroy;
end;

end.
