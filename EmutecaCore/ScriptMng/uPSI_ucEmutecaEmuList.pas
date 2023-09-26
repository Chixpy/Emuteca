unit uPSI_ucEmutecaEmuList;
{< Exports of ucEmutecaEmulatorList for Pascal Script engine of Emuteca.

  This file is part of Emuteca Core.

  Copyright (C) 2023 Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  // Emuteca Core classes
  ucEmutecaEmulator, ucEmutecaEmulatorList;

type

  { TPSImport_ucEmutecaEmulatorList }

  TPSImport_ucEmutecaEmulatorList = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;

  procedure SIRegister_cEmutecaEmulatorList(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaEmulatorList(CL: TPSPascalCompiler);

procedure RIRegister_cEmutecaEmulatorList(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaEmulatorList(CL: TPSRuntimeClassImporter);

procedure Register;

implementation
 
procedure SIRegister_cEmutecaEmulatorList(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TObject', 'cEmutecaEmulatorList') do
  with CL.AddClassN(CL.FindClass('TObject'), 'cEmutecaEmulatorList') do
  begin
    RegisterProperty('Count', 'integer', iptr);
    RegisterProperty('Items', 'cEmutecaEmulator integer', iptrw);
    SetDefaultPropery('Items');

    RegisterMethod('function Add(Item: cEmutecaEmulator): integer');
    RegisterMethod('procedure Delete(Index: integer)');
    RegisterMethod('function IndexOf(Item: cEmutecaEmulator): integer');
  end;
end;

procedure SIRegister_ucEmutecaEmulatorList(CL: TPSPascalCompiler);
begin
  SIRegister_cEmutecaEmulatorList(CL);
end;


procedure cEmutecaEmulatorListCount_R(Self: cEmutecaEmulatorList; var T: integer);
begin
  T := Self.Count;
end;

procedure cEmutecaEmulatorListItems_R(Self: cEmutecaEmulatorList;
  var T: cEmutecaEmulator; I: longint);
begin
  T := Self.Items[I];
end;

procedure cEmutecaEmulatorListItems_W(Self: cEmutecaEmulatorList;
  const T: cEmutecaEmulator; I: longint);
begin
  Self.Items[I] := T;
end;

procedure RIRegister_cEmutecaEmulatorList(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaEmulatorList) do
  begin
    RegisterPropertyHelper(@cEmutecaEmulatorListCount_R, nil, 'Count');
    RegisterPropertyHelper(@cEmutecaEmulatorListItems_R,
      @cEmutecaEmulatorListItems_W, 'Items');

    RegisterMethod(@cEmutecaEmulatorList.Add, 'Add');
    RegisterMethod(@cEmutecaEmulatorList.Delete, 'Delete');
    RegisterMethod(@cEmutecaEmulatorList.IndexOf, 'IndexOf');
  end;
end;

procedure RIRegister_ucEmutecaEmulatorList(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaEmulatorList(CL);
end;

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaEmulatorList]);
end;

{ TPSImport_ucEmutecaEmulatorList }

procedure TPSImport_ucEmutecaEmulatorList.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaEmulatorList(CompExec.comp);
end;

procedure TPSImport_ucEmutecaEmulatorList.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaEmulatorList(ri);
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

