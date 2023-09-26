unit utEmutecaRunEmulator;

{< ctEmutecaRunEmulator thread unit.

  TODO: Lanzar los emuladores en un hilo separado,

  This file is part of Emuteca Core.

  Copyright (C) 2017-2020 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // Emuteca Core classes
  ucEmutecaSoftware;

type

  { ctEmutecaRunEmulator }

  ctEmutecaRunEmulator = class(TThread)
  private
    FSoftware: cEmutecaSoftware;
    procedure SetSoftware(const AValue: cEmutecaSoftware);

  protected
    procedure Execute; override;

  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;

    constructor Create;
  end;

implementation

{ ctEmutecaGetSoftSHA1 }

procedure ctEmutecaRunEmulator.SetSoftware(const AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then Exit;
  FSoftware := AValue;
end;

procedure ctEmutecaRunEmulator.Execute;
begin

end;

constructor ctEmutecaRunEmulator.Create;
begin

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
