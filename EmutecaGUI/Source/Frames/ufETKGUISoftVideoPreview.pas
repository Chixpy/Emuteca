{ Video preview frame of Emuteca GUI.

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
unit ufETKGUISoftVideoPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
  // CHX frames
  ufCHXVideoListPreview,
  // Emuteca GUI abstracts
  uafETKGUISoftFoldersPreview;

type

  { TfmETKGUISoftVideoPreview }

  TfmETKGUISoftVideoPreview = class(TfmaETKGUISoftFoldersPreview)
  private
    FMPlayerPath: string;
    procedure SetMPlayerPath(const aMPlayerPath: string);

  protected
    procedure CreateListView; override;
    function GetCaptionList: TStrings; override;
    function GetFolder: string; override;

  public
    property MPlayerPath: string read FMPlayerPath write SetMPlayerPath;

  end;

implementation

{$R *.lfm}

{ TfmETKGUISoftVideoPreview }

procedure TfmETKGUISoftVideoPreview.SetMPlayerPath(const aMPlayerPath: string);
begin
  if FMPlayerPath = aMPlayerPath then Exit;
  FMPlayerPath := aMPlayerPath;

  TfmCHXVideoListPreview(fmListPreview).MPlayerPath := FMPlayerPath;
end;

procedure TfmETKGUISoftVideoPreview.CreateListView;
begin
  SetListPreview(TfmCHXVideoListPreview.Create(Self));
  fmListPreview.Align := alClient;
  fmListPreview.Parent := Self;
end;

function TfmETKGUISoftVideoPreview.GetCaptionList: TStrings;
begin
  Result := nil;

  if Assigned(System) then
    Result := System.VideoCaptions;
end;

function TfmETKGUISoftVideoPreview.GetFolder: string;
begin
  Result := '';

  if Assigned(System) then
    Result := System.VideoFolders[cbxFolderCaption.ItemIndex];
end;

end.

