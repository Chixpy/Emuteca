unit uPSI_ucEmutecaSoftManager;

{< Exports of ucEmutecaSoftManager for Pascal Script engine of Emuteca.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2023 Chixpy

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
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr, uEmutecaCommon,
  // Emuteca Core classes
  uaEmutecaCustomSystem, uaEmutecaCustomGroup, ucEmutecaSoftManager, ucEmutecaSoftList;

type

  TPSImport_ucEmutecaSoftManager = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;


procedure SIRegister_cEmutecaSoftManager(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaSoftManager(CL: TPSPascalCompiler);

procedure RIRegister_cEmutecaSoftManager(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaSoftManager(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaSoftManager]);
end;

procedure SIRegister_cEmutecaSoftManager(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorableTXt', 'cEmutecaSoftManager') do
  with CL.AddClassN(CL.FindClass('caCHXStorableTxt'), 'cEmutecaSoftManager') do
  begin
    RegisterProperty('ProgressCallBack', 'TEmutecaProgressCallBack', iptrw);

    RegisterProperty('FullList', 'cEmutecaSoftList', iptr);
    RegisterProperty('VisibleList', 'cEmutecaSoftList', iptr);

    RegisterMethod('Procedure ClearData');
    // RegisterMethod('Procedure LoadData');

    RegisterProperty('System', 'caEmutecaCustomSystem', iptrw);
    RegisterProperty('FilterGroup', 'caEmutecaCustomGroup', iptrw);
  end;
end;

procedure SIRegister_ucEmutecaSoftManager(CL: TPSPascalCompiler);
begin
  SIRegister_cEmutecaSoftManager(CL);
end;

procedure cEmutecaSoftManagerProgressCallBack_R(
  Self: cEmutecaSoftManager; var T: TEmutecaProgressCallBack);
begin
  T := Self.ProgressCallBack;
end;

procedure cEmutecaSoftManagerProgressCallBack_W(
  Self: cEmutecaSoftManager; const T: TEmutecaProgressCallBack);
begin
  Self.ProgressCallBack := T;
end;

procedure cEmutecaSoftManagerFullList_R(Self: cEmutecaSoftManager;
  var T: cEmutecaSoftList);
begin
  T := Self.FullList;
end;

procedure cEmutecaSoftManagerVisibleList_R(Self: cEmutecaSoftManager;
  var T: cEmutecaSoftList);
begin
  T := Self.VisibleList;
end;

procedure cEmutecaSoftManagerFilterGroup_R(Self: cEmutecaSoftManager;
  var T: caEmutecaCustomGroup);
begin
  T := Self.FilterGroup;
end;

procedure cEmutecaSoftManagerFilterGroup_W(Self: cEmutecaSoftManager;
  const T: caEmutecaCustomGroup);
begin
  Self.FilterGroup := T;
end;

procedure cEmutecaSoftManagerSystem_R(Self: cEmutecaSoftManager;
  var T: caEmutecaCustomSystem);
begin
  T := Self.System;
end;

procedure cEmutecaSoftManagerSystem_W(Self: cEmutecaSoftManager;
  const T: caEmutecaCustomSystem);
begin
  Self.System := T;
end;

procedure RIRegister_cEmutecaSoftManager(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaSoftManager) do
  begin
    RegisterPropertyHelper(@cEmutecaSoftManagerProgressCallBack_R,
      @cEmutecaSoftManagerProgressCallBack_W, 'ProgressCallBack');

    RegisterPropertyHelper(@cEmutecaSoftManagerFullList_R, nil, 'FullList');
    RegisterPropertyHelper(@cEmutecaSoftManagerVisibleList_R, nil, 'VisibleList');

    RegisterMethod(@cEmutecaSoftManager.ClearData, 'ClearData');
    //    RegisterMethod(@cEmutecaSoftManager.LoadData, 'LoadData');

    RegisterPropertyHelper(@cEmutecaSoftManagerSystem_R, @cEmutecaSoftManagerSystem_W, 'System');
    RegisterPropertyHelper(@cEmutecaSoftManagerFilterGroup_R, @cEmutecaSoftManagerFilterGroup_W, 'FilterGroup');
  end;
end;

procedure RIRegister_ucEmutecaSoftManager(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaSoftManager(CL);
end;

procedure TPSImport_ucEmutecaSoftManager.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaSoftManager(CompExec.comp);
end;

procedure TPSImport_ucEmutecaSoftManager.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaSoftManager(ri);
end;

end.
