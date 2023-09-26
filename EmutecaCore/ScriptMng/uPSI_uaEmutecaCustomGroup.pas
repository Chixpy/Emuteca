unit uPSI_uaEmutecaCustomGroup;

{< caEmutecaCustomGroup import for Pascal Script.

  This file is part of Emuteca Core.

  Copyright (C) 2018-2023 Chixpy
}
{$mode objfpc}{$H+}
interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  // CHX abstracts
  uaEmutecaCustomGroup;

type

  TPSImport_uaEmutecaCustomGroup = class(TPSPlugin)
  public
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;


{ compile-time registration functions }
procedure SIRegister_caEmutecaCustomGroup(CL: TPSPascalCompiler);
procedure SIRegister_uaEmutecaCustomGroup(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_caEmutecaCustomGroup(CL: TPSRuntimeClassImporter);
procedure RIRegister_uaEmutecaCustomGroup(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_uaEmutecaCustomGroup]);
end;


procedure SIRegister_caEmutecaCustomGroup(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caEmutecaCustomSGItem', 'caEmutecaCustomGroup') do
  with CL.AddClassN(CL.FindClass('caEmutecaCustomSGItem'),
      'caEmutecaCustomGroup') do
  begin
    RegisterProperty('Developer', 'string', iptrw);

    RegisterMethod('function ExportCommaText: string;');
    RegisterMethod('procedure ImportFrom(aGroup: caEmutecaCustomGroup);');
  end;
end;

procedure SIRegister_uaEmutecaCustomGroup(CL: TPSPascalCompiler);
begin
  SIRegister_caEmutecaCustomGroup(CL);
end;

procedure caEmutecaCustomGroupDeveloper_W(Self: caEmutecaCustomGroup;
  const T: string);
begin
  Self.Developer := T;
end;

procedure caEmutecaCustomGroupDeveloper_R(Self: caEmutecaCustomGroup;
  var T: string);
begin
  T := Self.Developer;
end;

procedure RIRegister_caEmutecaCustomGroup(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(caEmutecaCustomGroup) do
  begin
    RegisterPropertyHelper(@caEmutecaCustomGroupDeveloper_R,
      @caEmutecaCustomGroupDeveloper_W, 'Developer');

    RegisterMethod(@caEmutecaCustomGroup.ExportCommaText, 'ExportCommaText');
    RegisterMethod(@caEmutecaCustomGroup.ImportFrom, 'ImportFrom');
  end;
end;

procedure RIRegister_uaEmutecaCustomGroup(CL: TPSRuntimeClassImporter);
begin
  RIRegister_caEmutecaCustomGroup(CL);
end;

procedure TPSImport_uaEmutecaCustomGroup.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_uaEmutecaCustomGroup(CompExec.comp);
end;

procedure TPSImport_uaEmutecaCustomGroup.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_uaEmutecaCustomGroup(ri);
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
