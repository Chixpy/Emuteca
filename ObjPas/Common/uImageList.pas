{ This file is part of Emuteca

  Copyright (C) 2006-2012 Chixpy

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

{ cImageList unit. }
unit uImageList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FileUtil, fgl;

type

  { cImageList }
  cImageList = class (specialize TFPGObjectList<TPicture>)
  public
    function AddImageFile(aFile: String): Integer;
    function AddEmptyImage: Integer;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cImageList }

function cImageList.AddImageFile(aFile: String): Integer;
var
  Img: TPicture;
begin
  Result := -1;
  if not FileExistsUTF8(aFile) then Exit;
  Img := TPicture.Create;
  try
    Img.LoadFromFile(aFile);
  except
    // WOOPS, it can't be loaded.
    FreeAndNil(Img);
    Exit;
  end;
  Result := Self.Add(Img);
end;

function cImageList.AddEmptyImage: Integer;
var
  aImage: TPicture;
begin
  aImage := TPicture.Create;
  Result := Self.Add(aImage);
end;

constructor cImageList.Create;
begin
  Inherited Create;
end;

destructor cImageList.Destroy;
begin
  inherited Destroy;
end;

end.

