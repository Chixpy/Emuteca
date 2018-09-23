unit uPSI_ucEmutecaEmulatorManager;
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
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr, uEmutecaCommon,
  // Emuteca Core classes
  ucEmutecaEmulatorManager, ucEmutecaEmulatorList;

procedure SIRegister_ucEmutecaEmulatorManager(CL: TPSPascalCompiler);
procedure SIRegister_cEmutecaEmulatorManager(CL: TPSPascalCompiler);

procedure RIRegister_ucEmutecaEmulatorManager(CL: TPSRuntimeClassImporter);
procedure RIRegister_cEmutecaEmulatorManager(CL: TPSRuntimeClassImporter);


implementation

procedure SIRegister_cEmutecaEmulatorManager(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorableIni', 'cEmutecaEmulatorManager') do
  with CL.AddClassN(CL.FindClass('caCHXStorableIni'),
      'cEmutecaEmulatorManager') do
  begin
    RegisterProperty('ProgressCallBack', 'TEmutecaProgressCallBack', iptrw);
    RegisterMethod('Procedure ClearData');
    RegisterMethod('Procedure LoadData');
    RegisterMethod('Procedure SaveData');
    RegisterMethod('Procedure AssingAllTo( aList : TStrings)');
    RegisterMethod('Procedure AssingEnabledTo( aList : TStrings)');
    RegisterMethod('Function RunEmulator( const EmulatorID, GameFile :' +
      ' string) : longword');
    RegisterProperty('FullList', 'cEmutecaEmulatorList', iptr);
    RegisterProperty('EnabledList', 'cEmutecaEmulatorList', iptr);
  end;
end;

procedure SIRegister_ucEmutecaEmulatorManager(CL: TPSPascalCompiler);
begin
  CL.AddConstantN('rsLoadingEmulatorList', 'String').SetString(
    'Loading emulator list...');
  CL.AddConstantN('rsSavingEmulatorList', 'String').SetString(
    'Saving emulator list...');
  SIRegister_cEmutecaEmulatorManager(CL);
end;

procedure cEmutecaEmulatorManagerEnabledList_R(Self: cEmutecaEmulatorManager;
  var T: cEmutecaEmulatorList);
begin
  T := Self.EnabledList;
end;

procedure cEmutecaEmulatorManagerFullList_R(Self: cEmutecaEmulatorManager;
  var T: cEmutecaEmulatorList);
begin
  T := Self.FullList;
end;

procedure cEmutecaEmulatorManagerProgressCallBack_W(
  Self: cEmutecaEmulatorManager; const T: TEmutecaProgressCallBack);
begin
  Self.ProgressCallBack := T;
end;

procedure cEmutecaEmulatorManagerProgressCallBack_R(
  Self: cEmutecaEmulatorManager; var T: TEmutecaProgressCallBack);
begin
  T := Self.ProgressCallBack;
end;

procedure RIRegister_cEmutecaEmulatorManager(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaEmulatorManager) do
  begin
    RegisterPropertyHelper(@cEmutecaEmulatorManagerProgressCallBack_R,
      @cEmutecaEmulatorManagerProgressCallBack_W, 'ProgressCallBack');
    RegisterMethod(@cEmutecaEmulatorManager.ClearData, 'ClearData');
    //    RegisterMethod(@cEmutecaEmulatorManager.LoadData, 'LoadData');
    //    RegisterMethod(@cEmutecaEmulatorManager.SaveData, 'SaveData');
    RegisterPropertyHelper(@cEmutecaEmulatorManagerFullList_R,
      nil, 'FullList');
    RegisterPropertyHelper(@cEmutecaEmulatorManagerEnabledList_R,
      nil, 'EnabledList');
  end;
end;

procedure RIRegister_ucEmutecaEmulatorManager(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaEmulatorManager(CL);
end;

end.
