unit uPSI_ucEmutecaSystem;
{< Exports of ucEmutecaSystem for Pascal Script engine of Emuteca.

  ----

  This file is part of Emuteca Core.

  Copyright (C) 2011-2018 Chixpy

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

interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  LazFileUtils, LazUTF8,
  // CHX units
  uCHX7zWrapper,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr, uEmutecaCommon,
  // Emuteca Core abstracts
  uaEmutecaCustomSystem,
  // Emuteca Core classes
  ucEmutecaGroupManager, ucEmutecaGroupList, ucEmutecaGroup,
  ucEmutecaSoftManager, ucEmutecaSoftList, ucEmutecaSoftware,
  ucEmutecaSystem;

type
  TPSImport_ucEmutecaSystem = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;


{ compile-time registration functions }
procedure SIRegister_cEmutecaSystem(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaSystem(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_cEmutecaSystem(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaSystem(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaSystem]);
end;

procedure SIRegister_cEmutecaSystem(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caEmutecaCustomSystem', 'cEmutecaSystem') do
  with CL.AddClassN(CL.FindClass('caEmutecaCustomSystem'), 'cEmutecaSystem') do
  begin
    RegisterProperty('ProgressCallBack', 'TEmutecaProgressCallBack', iptrw);
    RegisterMethod('Procedure AddSoft( aSoft : cEmutecaSoftware)');
    RegisterMethod('Procedure LoadSoftGroupLists( aFile : string)');
    RegisterMethod('Procedure ImportSoftGroupLists( aFile : string)');
    RegisterMethod('Procedure SaveSoftGroupLists( aFile : string; ExportMode : boolean)');
    RegisterProperty('GroupManager', 'cEmutecaGroupManager', iptr);
    RegisterProperty('SoftManager', 'cEmutecaSoftManager', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_ucEmutecaSystem(CL: TPSPascalCompiler);
begin
  SIRegister_cEmutecaSystem(CL);
  CL.AddTypeS('TEmutecaReturnSystemCB',
    'Function ( aSystem : cEmutecaSystem) :' + ' boolean');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemSoftManager_R(Self: cEmutecaSystem;
  var T: cEmutecaSoftManager);
begin
  T := Self.SoftManager;
end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemGroupManager_R(Self: cEmutecaSystem;
  var T: cEmutecaGroupManager);
begin
  T := Self.GroupManager;
end;

procedure cEmutecaSystemProgressCallBack_W(Self: cEmutecaSystem;
  const T: TEmutecaProgressCallBack);
begin
  Self.ProgressCallBack := T;
end;

procedure cEmutecaSystemProgressCallBack_R(Self: cEmutecaSystem;
  var T: TEmutecaProgressCallBack);
begin
  T := Self.ProgressCallBack;
end;

procedure RIRegister_cEmutecaSystem(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaSystem) do
  begin
    RegisterPropertyHelper(@cEmutecaSystemProgressCallBack_R,
      @cEmutecaSystemProgressCallBack_W, 'ProgressCallBack');
    RegisterMethod(@cEmutecaSystem.AddSoft, 'AddSoft');
    RegisterMethod(@cEmutecaSystem.LoadSoftGroupLists, 'LoadSoftGroupLists');
    RegisterMethod(@cEmutecaSystem.ImportSoftGroupLists,
      'ImportSoftGroupLists');
    RegisterMethod(@cEmutecaSystem.SaveSoftGroupLists, 'SaveSoftGroupLists');
    RegisterPropertyHelper(@cEmutecaSystemGroupManager_R, nil, 'GroupManager');
    RegisterPropertyHelper(@cEmutecaSystemSoftManager_R, nil, 'SoftManager');
  end;
end;

procedure RIRegister_ucEmutecaSystem(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaSystem(CL);
end;

procedure TPSImport_ucEmutecaSystem.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaSystem(CompExec.comp);
end;

procedure TPSImport_ucEmutecaSystem.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaSystem(ri);
end;

end.
