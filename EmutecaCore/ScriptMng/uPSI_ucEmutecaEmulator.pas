unit uPSI_ucEmutecaEmulator;

{< cEmutecaEmulator import for Pascal Script.

  Copyright (C) 2019-2019 Chixpy

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

// TODO: Clean this unit
interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  ucEmutecaEmulator;

type
  (*----------------------------------------------------------------------------*)
  TPSImport_ucEmutecaEmulator = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;


{ compile-time registration functions }
procedure SIRegister_cEmutecaEmulator(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaEmulator(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_cEmutecaEmulator(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaEmulator(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaEmulator]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_cEmutecaEmulator(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caEmutecaCustomEmu', 'cEmutecaEmulator') do
  with CL.AddClassN(CL.FindClass('caEmutecaCustomEmu'), 'cEmutecaEmulator') do
  begin

  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_ucEmutecaEmulator(CL: TPSPascalCompiler);
begin
  SIRegister_cEmutecaEmulator(CL);
end;



(*----------------------------------------------------------------------------*)
procedure RIRegister_cEmutecaEmulator(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaEmulator) do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_ucEmutecaEmulator(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaEmulator(CL);
end;



{ TPSImport_ucEmutecaEmulator }
(*----------------------------------------------------------------------------*)
procedure TPSImport_ucEmutecaEmulator.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaEmulator(CompExec.comp);
end;

(*----------------------------------------------------------------------------*)
procedure TPSImport_ucEmutecaEmulator.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaEmulator(ri);
end;

(*----------------------------------------------------------------------------*)


end.
