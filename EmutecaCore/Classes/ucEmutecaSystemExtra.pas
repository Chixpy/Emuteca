{ This file is part of Emuteca

  Copyright (C) 2006-2016 Chixpy

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
unit ucEmutecaSystemExtra;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  uaEmutecaStorable;

type

  TEmutecaSoftKey = (TEFKSHA1, TEFKCRC32, TEFKFileName, TEFKCustom);

  { cEmutecaSystemExtra }

  cEmutecaSystemExtra = class(caEmutecaStorableIni)
  private

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFileIni(IniFile: TCustomIniFile); override;
    procedure SaveToFileIni(IniFile: TCustomIniFile;
      const ExportMode: boolean); override;

  published

  end;


implementation

{ cEmutecaSystemExtra }

constructor cEmutecaSystemExtra.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor cEmutecaSystemExtra.Destroy;
begin

  inherited Destroy;
end;

procedure cEmutecaSystemExtra.LoadFromFileIni(IniFile: TCustomIniFile);
begin
  if IniFile = nil then
    Exit;

end;

procedure cEmutecaSystemExtra.SaveToFileIni(IniFile: TCustomIniFile;
  const ExportMode: boolean);
begin
  if IniFile = nil then
    Exit;

end;

end.
