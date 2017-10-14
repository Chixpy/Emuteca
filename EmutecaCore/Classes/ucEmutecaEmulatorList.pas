{ This file is part of Emuteca

  Copyright (C) 2006-2017 Chixpy

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
unit ucEmutecaEmulatorList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazUTF8,
  ucEmutecaEmulator;

type
  cEmutecaGenEmulatorList = specialize TFPGObjectList<cEmutecaEmulator>;

  { cEmutecaEmulatorList }

  cEmutecaEmulatorList = class(cEmutecaGenEmulatorList)
  public
    procedure AssignToStrLst(aStrList: TStrings);
    function ItemById(aId: string): cEmutecaEmulator;
    {< Returns the emulator with aId key.

       @Result cEmutecaEmulator found or nil.
    }
  end;

implementation

{ cEmutecaEmulatorList }

procedure cEmutecaEmulatorList.AssignToStrLst(aStrList: TStrings);
var
  i: Integer;
  aEmulator: cEmutecaEmulator;
begin
   if not assigned(aStrList) then
    aStrList := TStringList.Create;

  aStrList.BeginUpdate;
  aStrList.Capacity := aStrList.Count + Count; // Speed up?
  i := 0;
  while i < Count do
  begin
    aEmulator := Items[i];
    aStrList.AddObject(aEmulator.EmulatorName, aEmulator);
    Inc(i);
  end;
  aStrList.EndUpdate;
end;

function cEmutecaEmulatorList.ItemById(aId: string): cEmutecaEmulator;
var
  i: integer;
  aEmulator: cEmutecaEmulator;
begin
  Result := nil;

  // Inverse search can be faster
  i := Count;
  while (not assigned(Result)) and (i > 0) do
  begin
    Dec(i);
    aEmulator := Items[i];
    if aEmulator.MatchID(aId) then
      Result := aEmulator;
  end;
end;

end.
