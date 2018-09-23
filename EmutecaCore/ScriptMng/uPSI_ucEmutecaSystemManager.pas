unit uPSI_ucEmutecaSystemManager;

{< Exports of ucEmutecaSystemManager for Pascal Script engine of Emuteca.

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
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr, uEmutecaCommon,
  // Emuteca Core classes
  ucEmutecaSystemList, ucEmutecaSystemManager;

type
  TPSImport_ucEmutecaSystemManager = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;


procedure SIRegister_cEmutecaSystemManager(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaSystemManager(CL: TPSPascalCompiler);

procedure RIRegister_cEmutecaSystemManager(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaSystemManager(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaSystemManager]);
end;

procedure SIRegister_cEmutecaSystemManager(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorableIni', 'cEmutecaSystemManager') do
  with CL.AddClassN(CL.FindClass('caCHXStorableIni'),
      'cEmutecaSystemManager') do
  begin
    RegisterProperty('TempFolder', 'string', iptrw);
    RegisterProperty('SysDataFolder', 'string', iptrw);
    RegisterMethod('Procedure ClearData');
    RegisterMethod('Procedure LoadData');
    RegisterMethod('Procedure SaveData');
    RegisterMethod('Procedure UpdateEnabledList');
    RegisterProperty('ProgressCallBack', 'TEmutecaProgressCallBack', iptrw);
    RegisterMethod('Procedure AssingAllTo( aList : TStrings)');
    RegisterMethod('Procedure AssingEnabledTo( aList : TStrings)');
    RegisterProperty('FullList', 'cEmutecaSystemList', iptr);
    RegisterProperty('EnabledList', 'cEmutecaSystemList', iptr);
  end;
end;

procedure SIRegister_ucEmutecaSystemManager(CL: TPSPascalCompiler);
begin
  SIRegister_cEmutecaSystemManager(CL);
end;

procedure cEmutecaSystemManagerEnabledList_R(Self: cEmutecaSystemManager;
  var T: cEmutecaSystemList);
begin
  T := Self.EnabledList;
end;

procedure cEmutecaSystemManagerFullList_R(Self: cEmutecaSystemManager;
  var T: cEmutecaSystemList);
begin
  T := Self.FullList;
end;

procedure cEmutecaSystemManagerProgressCallBack_W(Self: cEmutecaSystemManager;
  const T: TEmutecaProgressCallBack);
begin
  Self.ProgressCallBack := T;
end;

procedure cEmutecaSystemManagerProgressCallBack_R(Self: cEmutecaSystemManager;
  var T: TEmutecaProgressCallBack);
begin
  T := Self.ProgressCallBack;
end;

procedure cEmutecaSystemManagerSysDataFolder_W(Self: cEmutecaSystemManager;
  const T: string);
begin
  Self.SysDataFolder := T;
end;

procedure cEmutecaSystemManagerSysDataFolder_R(Self: cEmutecaSystemManager;
  var T: string);
begin
  T := Self.SysDataFolder;
end;

procedure cEmutecaSystemManagerTempFolder_W(Self: cEmutecaSystemManager;
  const T: string);
begin
  Self.TempFolder := T;
end;

procedure cEmutecaSystemManagerTempFolder_R(Self: cEmutecaSystemManager;
  var T: string);
begin
  T := Self.TempFolder;
end;

procedure RIRegister_cEmutecaSystemManager(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaSystemManager) do
  begin
    RegisterPropertyHelper(@cEmutecaSystemManagerTempFolder_R,
      @cEmutecaSystemManagerTempFolder_W, 'TempFolder');
    RegisterPropertyHelper(@cEmutecaSystemManagerSysDataFolder_R,
      @cEmutecaSystemManagerSysDataFolder_W, 'SysDataFolder');
    RegisterMethod(@cEmutecaSystemManager.ClearData, 'ClearData');
    //    RegisterMethod(@cEmutecaSystemManager.LoadData, 'LoadData');
    //    RegisterMethod(@cEmutecaSystemManager.SaveData, 'SaveData');
    RegisterMethod(@cEmutecaSystemManager.UpdateEnabledList,
      'UpdateEnabledList');
    RegisterPropertyHelper(@cEmutecaSystemManagerProgressCallBack_R,
      @cEmutecaSystemManagerProgressCallBack_W, 'ProgressCallBack');
    RegisterPropertyHelper(@cEmutecaSystemManagerFullList_R, nil, 'FullList');
    RegisterPropertyHelper(@cEmutecaSystemManagerEnabledList_R,
      nil, 'EnabledList');
  end;
end;

procedure RIRegister_ucEmutecaSystemManager(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaSystemManager(CL);
end;

procedure TPSImport_ucEmutecaSystemManager.CompileImport1(
  CompExec: TPSScript);
begin
  SIRegister_ucEmutecaSystemManager(CompExec.comp);
end;

procedure TPSImport_ucEmutecaSystemManager.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaSystemManager(ri);
end;


end.
