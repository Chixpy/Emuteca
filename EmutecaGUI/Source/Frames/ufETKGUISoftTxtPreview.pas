{ Text preview frame of Emuteca GUI

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
unit ufETKGUISoftTxtPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  // CHX frames
  ufCHXTxtListPreview,
  // Emuteca GUI abstracts
  uafETKGUISoftFoldersPreview;

type

  { TfmETKGUISoftTxtPreview }

  TfmETKGUISoftTxtPreview = class(TfmaETKGUISoftFoldersPreview)
  private

  protected
    procedure CreateListView; override;
       function GetCaptionList: TStrings; override;
    function GetFolder: string; override;
  public

  end;

implementation

{$R *.lfm}

{ TfmETKGUISoftTxtPreview }

procedure TfmETKGUISoftTxtPreview.CreateListView;
begin
    SetListPreview(TfmCHXTxtListPreview.Create(Self));
    fmListPreview.Align := alClient;
    fmListPreview.Parent := Self;
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
   Result := System.TextFolders[cbxFolderCaption.ItemIndex];
end;

end.

