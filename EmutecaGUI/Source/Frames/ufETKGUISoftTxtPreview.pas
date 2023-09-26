unit ufETKGUISoftTxtPreview;
{< TfmETKGUISoftTxtPreview frame unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2018-2018 Chixpy
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
  ufEmutecaSoftTxtPreview,
  // Emuteca GUI abstracts
  uafETKGUISoftFoldersPreview;

type

  { TfmETKGUISoftTxtPreview }

  TfmETKGUISoftTxtPreview = class(TfmaETKGUISoftFoldersPreview)
  protected
    procedure CreateListView; override;
    function GetCaptionList: TStrings; override;
    function GetFolder: string; override;

    procedure UpdateFileList; override;
  end;

implementation

{$R *.lfm}

procedure TfmETKGUISoftTxtPreview.CreateListView;
begin
  SetListPreview(TfmEmutecaSoftTxtPreview.Create(gbxPanel));
  fmListPreview.Align := alClient;
  fmListPreview.Parent := gbxPanel;
end;

function TfmETKGUISoftTxtPreview.GetCaptionList: TStrings;
begin
  Result := nil;

  if Assigned(System) then
    Result := System.TextCaptions;
end;

function TfmETKGUISoftTxtPreview.GetFolder: string;
begin
  Result := '';

  if Assigned(System) then
    Result := SetAsFolder(System.TextFolders[cbxFolderCaption.ItemIndex]);
end;

procedure TfmETKGUISoftTxtPreview.UpdateFileList;
begin
      if Assigned(Software) then
  begin
     TfmEmutecaSoftTxtPreview(fmListPreview).SaveTextFolder :=
      SetAsFolder(GetFolder + Software.MediaFileName)
  end
  else if Assigned(Group) then
  begin
     TfmEmutecaSoftTxtPreview(fmListPreview).SaveTextFolder :=
      SetAsFolder(GetFolder + Group.MediaFileName)
  end
  else
  begin
      TfmEmutecaSoftTxtPreview(fmListPreview).SaveTextFolder := '';
  end;

  inherited UpdateFileList;
end;

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
