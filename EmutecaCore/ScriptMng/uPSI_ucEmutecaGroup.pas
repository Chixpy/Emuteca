unit uPSI_ucEmutecaGroup;

{< Exports of cEmutecaGroup for Pascal Script engine of Emuteca.

  This file is part of Emuteca Core.

  Copyright (C) 2023 Chixpy

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
interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  // Emuteca Core abstracts
  uaEmutecaCustomSystem,
  // Emuteca Core classes
  ucEmutecaSoftList, ucEmutecaGroup;

type
  (*----------------------------------------------------------------------------*)
  TPSImport_ucEmutecaGroup = class(TPSPlugin)
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;

procedure SIRegister_cEmutecaGroup(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaGroup(CL: TPSPascalCompiler);

procedure RIRegister_cEmutecaGroup(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaGroup(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaGroup]);
end;

procedure SIRegister_cEmutecaGroup(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caEmutecaCustomGroup', 'cEmutecaGroup') do
  with CL.AddClassN(CL.FindClass('caEmutecaCustomGroup'), 'cEmutecaGroup') do
  begin
    RegisterProperty('SoftList', 'cEmutecaSoftList', iptr);
    RegisterProperty('CachedSystem', 'caEmutecaCustomSystem', iptrw);
  end;
end;

procedure SIRegister_ucEmutecaGroup(CL: TPSPascalCompiler);
begin
  SIRegister_cEmutecaGroup(CL);

  CL.AddTypeS('TEmutecaReturnGroupCB', 'function(aGroup: cEmutecaGroup): boolean');
end;

procedure cEmutecaGroupSoftList_R(Self: cEmutecaGroup;
  var T: cEmutecaSoftList);
begin
  T := Self.SoftList;
end;

procedure cEmutecaGroupCachedSystem_R(Self: cEmutecaGroup;
  var T: caEmutecaCustomSystem);
begin
  T := Self.CachedSystem;
end;

procedure cEmutecaGroupCachedSystem_W(Self: cEmutecaGroup;
  const T: caEmutecaCustomSystem);
begin
  Self.CachedSystem := T;
end;

procedure RIRegister_cEmutecaGroup(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaGroup) do
  begin
    RegisterPropertyHelper(@cEmutecaGroupSoftList_R, nil, 'SoftList');
    RegisterPropertyHelper(@cEmutecaGroupCachedSystem_R, @cEmutecaGroupCachedSystem_W, 'CachedSystem');
  end;
end;

procedure RIRegister_ucEmutecaGroup(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaGroup(CL);
end;

procedure TPSImport_ucEmutecaGroup.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaGroup(CompExec.comp);
end;

procedure TPSImport_ucEmutecaGroup.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaGroup(ri);
end;
end.
