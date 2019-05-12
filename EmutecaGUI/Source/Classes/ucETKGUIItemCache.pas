unit ucETKGUIItemCache;
{< cETKGUIItemCache class unit.

  ----

  This file is part of Emuteca GUI.

  Copyright (C) 2019 Chixpy

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
  Classes, SysUtils, Graphics, fgl;

type
  { cETKGUIFileListCache }

   cETKGUIFileListCache = specialize TFPGMapObject<string, TStringList>;


  { cETKGUIItemCache }

  cETKGUIItemCache = class(TComponent)
  private
    FIcon: TPicture;
    FMediaFiles: cETKGUIFileListCache;
    procedure SetIcon(const AValue: TPicture);

  public
    property Icon: TPicture read FIcon write SetIcon;
    //< Icon for item.

    property MediaFiles: cETKGUIFileListCache read FMediaFiles;
    //< Media files found

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;
  {< It stores cached data of groups and software for Emuteca GUI.

    @definitionList(
      @itemLabel(Icon)
      @item(Actual icon for the item.)
    )
  }

implementation

{ cETKGUIItemCache }

procedure cETKGUIItemCache.SetIcon(const AValue: TPicture);
begin
  if FIcon = AValue then Exit;
  FIcon := AValue;
end;

constructor cETKGUIItemCache.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FMediaFiles := cETKGUIFileListCache.Create(True);
end;

destructor cETKGUIItemCache.Destroy;
begin
  FMediaFiles.Free;

  inherited Destroy;
end;

initialization
  RegisterClass(cETKGUIItemCache);

finalization
  UnRegisterClass(cETKGUIItemCache);
end.

