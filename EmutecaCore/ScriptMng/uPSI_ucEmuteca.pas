unit uPSI_ucEmuteca;
{< Exports of ucEmuteca for Pascal Script engine of Emuteca.

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
  ucEmuteca, ucEmutecaConfig, ucEmutecaEmulatorManager, ucEmutecaSystemManager;


procedure SIRegister_ucEmuteca(CL: TPSPascalCompiler);
procedure SIRegister_cEmuteca(CL: TPSPascalCompiler);

procedure RIRegister_ucEmuteca(CL: TPSRuntimeClassImporter);
procedure RIRegister_cEmuteca(CL: TPSRuntimeClassImporter);


implementation

procedure SIRegister_cEmuteca(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TComponent'), 'cEmuteca') do
  begin
    RegisterProperty('ProgressCallBack', 'TEmutecaProgressCallBack', iptrw);
    RegisterProperty('TempFolder', 'string', iptr);
    //    RegisterMethod('Procedure LoadConfig(aFile : string)');
    //    RegisterMethod('Procedure ClearAllData');
    //    RegisterMethod('Procedure LoadData');
    RegisterMethod('Procedure SaveData');
    //    RegisterMethod('Procedure CacheData');
    RegisterMethod('Function RunSoftware(const aSoftware: cEmutecaSoftware): integer');
    RegisterProperty('BaseFolder', 'string', iptrw);
    RegisterProperty('Config', 'cEmutecaConfig', iptr);
    RegisterProperty('SystemManager', 'cEmutecaSystemManager', iptr);
    RegisterProperty('EmulatorManager', 'cEmutecaEmulatorManager', iptr);
  end;
end;

procedure SIRegister_ucEmuteca(CL: TPSPascalCompiler);
begin
  SIRegister_cEmuteca(CL);
end;

procedure cEmutecaEmulatorManager_R(Self: cEmuteca;
  var T: cEmutecaEmulatorManager);
begin
  T := Self.EmulatorManager;
end;

procedure cEmutecaSystemManager_R(Self: cEmuteca;
  var T: cEmutecaSystemManager);
begin
  T := Self.SystemManager;
end;

procedure cEmutecaConfig_R(Self: cEmuteca; var T: cEmutecaConfig);
begin
  T := Self.Config;
end;

procedure cEmutecaBaseFolder_W(Self: cEmuteca; const T: string);
begin
  Self.BaseFolder := T;
end;

procedure cEmutecaBaseFolder_R(Self: cEmuteca; var T: string);
begin
  T := Self.BaseFolder;
end;

procedure cEmutecaTempFolder_R(Self: cEmuteca; var T: string);
begin
  T := Self.TempFolder;
end;

procedure cEmutecaProgressCallBack_W(Self: cEmuteca;
  const T: TEmutecaProgressCallBack);
begin
  Self.ProgressCallBack := T;
end;

procedure cEmutecaProgressCallBack_R(Self: cEmuteca;
  var T: TEmutecaProgressCallBack);
begin
  T := Self.ProgressCallBack;
end;

procedure RIRegister_cEmuteca(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmuteca) do
  begin
    RegisterPropertyHelper(@cEmutecaProgressCallBack_R,
      @cEmutecaProgressCallBack_W, 'ProgressCallBack');
    RegisterPropertyHelper(@cEmutecaTempFolder_R, nil, 'TempFolder');
   // RegisterMethod(@cEmuteca.LoadConfig, 'LoadConfig');
   // RegisterMethod(@cEmuteca.ClearAllData, 'ClearAllData');
   // RegisterMethod(@cEmuteca.LoadAllData, 'LoadData');
    RegisterMethod(@cEmuteca.SaveAllData, 'SaveData');
   // RegisterMethod(@cEmuteca.CacheData, 'CacheData');
    RegisterMethod(@cEmuteca.RunSoftware, 'RunSoftware');
    RegisterPropertyHelper(@cEmutecaBaseFolder_R, @cEmutecaBaseFolder_W,
      'BaseFolder');
    RegisterPropertyHelper(@cEmutecaConfig_R, nil, 'Config');
    RegisterPropertyHelper(@cEmutecaSystemManager_R, nil, 'SystemManager');
    RegisterPropertyHelper(@cEmutecaEmulatorManager_R, nil, 'EmulatorManager');
  end;
end;

procedure RIRegister_ucEmuteca(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmuteca(CL);
end;

end.
