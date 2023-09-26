unit uPSI_ucEmutecaGroupManager;

{< Exports of ucEmutecaGroupManager for Pascal Script engine of Emuteca.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2023 Chixpy
}
interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr, uEmutecaCommon,
  // Emuteca Core classes
  uaEmutecaCustomSystem, ucEmutecaGroupManager, ucEmutecaGroupList;

type

  TPSImport_ucEmutecaGroupManager = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;


procedure SIRegister_cEmutecaGroupManager(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaGroupManager(CL: TPSPascalCompiler);

procedure RIRegister_cEmutecaGroupManager(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaGroupManager(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaGroupManager]);
end;

procedure SIRegister_cEmutecaGroupManager(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorableTxt', 'cEmutecaGroupManager') do
  with CL.AddClassN(CL.FindClass('caCHXStorableTxt'), 'cEmutecaGroupManager') do
  begin
    RegisterProperty('ProgressCallBack', 'TEmutecaProgressCallBack', iptrw);

    RegisterProperty('FullList', 'cEmutecaGroupList', iptr);
    RegisterProperty('VisibleList', 'cEmutecaGroupList', iptr);

    RegisterMethod('Procedure ClearData');

    RegisterProperty('System', 'caEmutecaCustomSystem', iptrw);
    RegisterMethod('function AddGroup( aID : string) : integer');
  end;
end;

procedure SIRegister_ucEmutecaGroupManager(CL: TPSPascalCompiler);
begin
  SIRegister_cEmutecaGroupManager(CL);
end;

procedure cEmutecaGroupManagerProgressCallBack_R(
  Self: cEmutecaGroupManager; var T: TEmutecaProgressCallBack);
begin
  T := Self.ProgressCallBack;
end;

procedure cEmutecaGroupManagerProgressCallBack_W(
  Self: cEmutecaGroupManager; const T: TEmutecaProgressCallBack);
begin
  Self.ProgressCallBack := T;
end;

procedure cEmutecaGroupManagerVisibleList_R(Self: cEmutecaGroupManager;
  var T: cEmutecaGroupList);
begin
  T := Self.VisibleList;
end;

procedure cEmutecaGroupManagerFullList_R(Self: cEmutecaGroupManager;
  var T: cEmutecaGroupList);
begin
  T := Self.FullList;
end;

procedure cEmutecaGroupManagerSystem_W(Self: cEmutecaGroupManager;
  const T: caEmutecaCustomSystem);
begin
  Self.System := T;
end;

procedure cEmutecaGroupManagerSystem_R(Self: cEmutecaGroupManager;
  var T: caEmutecaCustomSystem);
begin
  T := Self.System;
end;

procedure RIRegister_cEmutecaGroupManager(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaGroupManager) do
  begin
    RegisterPropertyHelper(@cEmutecaGroupManagerProgressCallBack_R,
      @cEmutecaGroupManagerProgressCallBack_W, 'ProgressCallBack');

    RegisterPropertyHelper(@cEmutecaGroupManagerFullList_R, nil, 'FullList');
    RegisterPropertyHelper(@cEmutecaGroupManagerVisibleList_R, nil, 'VisibleList');

    RegisterMethod(@cEmutecaGroupManager.ClearData, 'ClearData');

    RegisterPropertyHelper(@cEmutecaGroupManagerSystem_R, @cEmutecaGroupManagerSystem_W, 'System');
    RegisterMethod(@cEmutecaGroupManager.AddGroup, 'AddGroup');
  end;
end;

procedure RIRegister_ucEmutecaGroupManager(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaGroupManager(CL);
end;

procedure TPSImport_ucEmutecaGroupManager.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaGroupManager(CompExec.comp);
end;

procedure TPSImport_ucEmutecaGroupManager.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaGroupManager(ri);
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
