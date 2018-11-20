unit ufETKGUISoftImgPreview;

{< TfmETKGUISoftImgPreview frame unit.

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  // CHX units
  uCHXStrUtils,
  // Emuteca Core classes
  ucEmutecaGroup, ucEmutecaSoftware,
  // Emuteca Core frames
  ufEmutecaSoftImgPreview,
  // Emuteca GUI abstracts
  uafETKGUISoftFoldersPreview;

type

  { TfmETKGUISoftImgPreview }

  TfmETKGUISoftImgPreview = class(TfmaETKGUISoftFoldersPreview)
  private
    FSHA1Folder: string;
    procedure SetSHA1Folder(AValue: string);

  protected
    procedure CreateListView; override;
    function GetCaptionList: TStrings; override;
    function GetFolder: string; override;

    procedure SetSoftware(AValue: cEmutecaSoftware); override;
    procedure SetGroup(AValue: cEmutecaGroup); override;

  public
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

  end;

implementation

{$R *.lfm}

{ TfmETKGUISoftImgPreview }

procedure TfmETKGUISoftImgPreview.SetSHA1Folder(AValue: string);
begin
  if FSHA1Folder = AValue then
    Exit;
  FSHA1Folder := AValue;

  TfmEmutecaSoftImgPreview(fmListPreview).SHA1Folder := SHA1Folder;
end;

procedure TfmETKGUISoftImgPreview.CreateListView;
begin
  SetListPreview(TfmEmutecaSoftImgPreview.Create(gbxPanel));
  fmListPreview.Align := alClient;
  fmListPreview.Parent := gbxPanel;
end;

function TfmETKGUISoftImgPreview.GetCaptionList: TStrings;
begin
  Result := nil;

  if Assigned(System) then
    Result := System.ImageCaptions;
end;

function TfmETKGUISoftImgPreview.GetFolder: string;
begin
  Result := '';

  if Assigned(System) then
    Result := SetAsFolder(System.ImageFolders[cbxFolderCaption.ItemIndex]);
end;

procedure TfmETKGUISoftImgPreview.SetSoftware(AValue: cEmutecaSoftware);
begin
  inherited SetSoftware(AValue);

  if Assigned(AValue) then
    TfmEmutecaSoftImgPreview(fmListPreview).SaveImageFolder :=
      SetAsFolder(GetFolder + AValue.GetMediaFileName)
  else
    TfmEmutecaSoftImgPreview(fmListPreview).SaveImageFolder := '';
end;

procedure TfmETKGUISoftImgPreview.SetGroup(AValue: cEmutecaGroup);
begin
  inherited SetGroup(AValue);

  if Assigned(AValue) then
    TfmEmutecaSoftImgPreview(fmListPreview).SaveImageFolder :=
      SetAsFolder(GetFolder + AValue.MediaFileName)
  else
    TfmEmutecaSoftImgPreview(fmListPreview).SaveImageFolder := '';
end;


end.
