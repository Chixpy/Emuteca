unit uPSI_ucEmuteca;

{< Exports of ucEmuteca for Pascal Script engine of Emuteca.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2023 Chixpy
}
{$mode objfpc}{$H+}
interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  // Emuteca Core units
  uEmutecaConst,
  // Emuteca Core classes
  ucEmuteca, ucEmutecaConfig, ucEmutecaGroupList, ucEmutecaEmulatorManager,
  ucEmutecaSystemManager;


type

  { TPSImport_ucEmuteca }

  TPSImport_ucEmuteca = class(TPSPlugin)
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;

procedure SIRegister_ucEmuteca(CL: TPSPascalCompiler);
procedure SIRegister_cEmuteca(CL: TPSPascalCompiler);

procedure RIRegister_ucEmuteca(CL: TPSRuntimeClassImporter);
procedure RIRegister_cEmuteca(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmuteca]);
end;

procedure SIRegister_cEmuteca(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TComponent'), 'cEmuteca') do
  begin
    RegisterProperty('ProgressCallBack', 'TEmutecaProgressCallBack', iptrw);

    RegisterProperty('TempFolder', 'string', iptr);

    RegisterProperty('CurrentGroupList', 'cEmutecaGroupList', iptr);

    RegisterProperty('BaseFolder', 'string', iptrw);
    RegisterProperty('Config', 'cEmutecaConfig', iptr);

    RegisterProperty('SystemManager', 'cEmutecaSystemManager', iptr);
    RegisterProperty('EmulatorManager', 'cEmutecaEmulatorManager', iptr);

    RegisterMethod('procedure LoadConfig(aFile: string);');

    RegisterMethod('procedure ClearAllData;');
    RegisterMethod('procedure LoadAllData;');
    RegisterMethod('procedure SaveAllData;');

    RegisterMethod('procedure CleanSystems;');
    RegisterMethod('procedure CacheData;');

    RegisterMethod('procedure UpdateSysEmulators;');
    RegisterMethod('procedure UpdateCurrentGroupList(aSystem: cEmutecaSystem; const aWordFilter: string; aFileList: TStrings);');

    RegisterMethod('function RunSoftware(const aSoftware: cEmutecaSoftware): integer;');
  end;
end;

procedure SIRegister_ucEmuteca(CL: TPSPascalCompiler);
begin
  SIRegister_cEmuteca(CL);
end;

procedure cEmutecaProgressCallBack_R(Self: cEmuteca;
  var T: TEmutecaProgressCallBack);
begin
  T := Self.ProgressCallBack;
end;

procedure cEmutecaProgressCallBack_W(Self: cEmuteca;
  const T: TEmutecaProgressCallBack);
begin
  Self.ProgressCallBack := T;
end;

procedure cEmutecaTempFolder_R(Self: cEmuteca; var T: string);
begin
  T := Self.TempFolder;
end;

procedure cEmutecaCurrentGroupList_R(Self: cEmuteca; var T: cEmutecaGroupList);
begin
  T := Self.CurrentGroupList;
end;

procedure cEmutecaBaseFolder_R(Self: cEmuteca; var T: string);
begin
  T := Self.BaseFolder;
end;

procedure cEmutecaBaseFolder_W(Self: cEmuteca; const T: string);
begin
  Self.BaseFolder := T;
end;

procedure cEmutecaConfig_R(Self: cEmuteca; var T: cEmutecaConfig);
begin
  T := Self.Config;
end;

procedure cEmutecaSystemManager_R(Self: cEmuteca;
  var T: cEmutecaSystemManager);
begin
  T := Self.SystemManager;
end;

procedure cEmutecaEmulatorManager_R(Self: cEmuteca;
  var T: cEmutecaEmulatorManager);
begin
  T := Self.EmulatorManager;
end;

procedure RIRegister_cEmuteca(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmuteca) do
  begin
    RegisterPropertyHelper(@cEmutecaProgressCallBack_R,
      @cEmutecaProgressCallBack_W, 'ProgressCallBack');

    RegisterPropertyHelper(@cEmutecaTempFolder_R, nil, 'TempFolder');

    RegisterPropertyHelper(@cEmutecaCurrentGroupList_R, nil,
      'CurrentGroupList');

    RegisterPropertyHelper(@cEmutecaBaseFolder_R, @cEmutecaBaseFolder_W,
      'BaseFolder');
    RegisterPropertyHelper(@cEmutecaConfig_R, nil, 'Config');

    RegisterPropertyHelper(@cEmutecaSystemManager_R, nil, 'SystemManager');
    RegisterPropertyHelper(@cEmutecaEmulatorManager_R, nil, 'EmulatorManager');

    RegisterMethod(@cEmuteca.LoadConfig, 'LoadConfig');

    RegisterMethod(@cEmuteca.ClearAllData, 'ClearAllData');
    RegisterMethod(@cEmuteca.LoadAllData, 'LoadData');
    RegisterMethod(@cEmuteca.SaveAllData, 'SaveData');

    RegisterMethod(@cEmuteca.CleanSystems, 'CleanSystems');
    RegisterMethod(@cEmuteca.CacheData, 'CacheData');

    RegisterMethod(@cEmuteca.UpdateSysEmulators, 'UpdateSysEmulators');
    RegisterMethod(@cEmuteca.UpdateCurrentGroupList, 'UpdateCurrentGroupList');

    RegisterMethod(@cEmuteca.RunSoftware, 'RunSoftware');
  end;
end;

procedure RIRegister_ucEmuteca(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmuteca(CL);
end;

{ TPSImport_uaEmutecaCustomSystem }

procedure TPSImport_ucEmuteca.CompileImport1(CompExec: TPSScript);
begin
   SIRegister_ucEmuteca(CompExec.comp);
end;

procedure TPSImport_ucEmuteca.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmuteca(ri);
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
