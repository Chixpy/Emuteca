unit uPSI_ucEmutecaConfig;

{< Exports of ucEmutecaConfig for Pascal Script engine of Emuteca.

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
  // Emuteca Core classes
  ucEmutecaConfig;

type
  { TPSImport_uaEmutecaCustomEmu }

  TPSImport_ucEmutecaConfig = class(TPSPlugin)
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;


procedure SIRegister_ucEmutecaConfig(CL: TPSPascalCompiler);
procedure SIRegister_cEmutecaConfig(CL: TPSPascalCompiler);

procedure RIRegister_ucEmutecaConfig(CL: TPSRuntimeClassImporter);
procedure RIRegister_cEmutecaConfig(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure SIRegister_cEmutecaConfig(CL: TPSPascalCompiler);
begin
  // Actual creation of class
  ////with RegClassS(CL,'caCHXConfig', 'cEmutecaConfig') do
  //with CL.AddClassN(CL.FindClass('caCHXConfig'), 'cEmutecaConfig') do

  // We only want to access its properties, so we will ignore its heritance
  with CL.AddClassN(CL.FindClass('TObject'), 'cEmutecaConfig') do
  begin
    RegisterProperty('z7CMExecutable', 'string', iptrw);
    RegisterProperty('z7GExecutable', 'string', iptrw);
    RegisterProperty('CompressedExtensions', 'TStringList', iptr);

    RegisterProperty('EmulatorsFile', 'string', iptrw);
    RegisterProperty('SystemsFile', 'string', iptrw);

    RegisterProperty('SysDataFolder', 'string', iptrw);
    RegisterProperty('AutoSysFolder', 'string', iptrw);
    RegisterProperty('TagsFolder', 'string', iptrw);
    RegisterProperty('TempSubfolder', 'string', iptrw);
    RegisterProperty('TempFile', 'string', iptrw);
    RegisterProperty('MinPlayTime', 'integer', iptrw);
  end;
end;

procedure SIRegister_ucEmutecaConfig(CL: TPSPascalCompiler);
begin
  // Sections and keys of the ini file are NOT needed in Script Engine

  //// [Config]
  //CL.AddConstantN('krsIniSecConfig', 'string').SetString(krsIniSecConfig);
  //CL.AddConstantN('krsIniKeyDataFolder', 'string').SetString(
  //  krsIniKeyDataFolder);
  //CL.AddConstantN('krsIniKeyEmulatorsFile', 'string').SetString(
  //  krsIniKeyEmulatorsFile);
  //CL.AddConstantN('krsIniKeyAutoSysFoldersFile', 'string').SetString(
  //  krsIniKeyAutoSysFoldersFile);
  //CL.AddConstantN('krsIniKeySystemsFile', 'string').SetString(
  //  krsIniKeySystemsFile);
  //CL.AddConstantN('krsIniKeySysDataFolder', 'string').SetString(
  //  krsIniKeySysDataFolder);
  //CL.AddConstantN('krsIniKeyTagsFolder', 'string').SetString(
  //  krsIniKeyTagsFolder);

  //// [Tools]
  //CL.AddConstantN('krsIniSecTools', 'string').SetString(krsIniSecTools);
  //CL.AddConstantN('krsIniKey7zCMExecutable', 'string').SetString(
  //  krsIniKey7zCMExecutable);
  //CL.AddConstantN('krsIniKey7zGExecutable', 'string').SetString(
  //  krsIniKey7zGExecutable);

  //// [Extensions]
  //CL.AddConstantN('krsIniSecExtensions', 'string').SetString(
  //  krsIniSecExtensions);
  //CL.AddConstantN('krsIniKeyCompressedExtensions',
  //  'string').SetString(krsIniKeyCompressedExtensions);

  //// [Temp]
  //CL.AddConstantN('krsIniSecTemp', 'string').SetString(krsIniSecTemp);
  //CL.AddConstantN('krsIniKeyTempSubfolder', 'string').SetString(
  //  krsIniKeyTempSubfolder);
  //CL.AddConstantN('krsIniKeyTempFile', 'string').SetString(krsIniKeyTempFile);

  //// [Misc]
  //CL.AddConstantN('krsIniSecMisc', 'string').SetString(krsIniSecMisc);
  //CL.AddConstantN('krsIniKeyMinPlayTime', 'string').SetString(
  //  krsIniKeyMinPlayTime);

  SIRegister_cEmutecaConfig(CL);
end;

procedure cEmutecaConfigz7CMExecutable_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.z7CMExecutable;
end;

procedure cEmutecaConfigz7CMExecutable_W(Self: cEmutecaConfig;
  const T: string);
begin
  Self.z7CMExecutable := T;
end;

procedure cEmutecaConfigz7GExecutable_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.z7GExecutable;
end;

procedure cEmutecaConfigz7GExecutable_W(Self: cEmutecaConfig; const T: string);
begin
  Self.z7GExecutable := T;
end;

procedure cEmutecaConfigCompressedExtensions_R(Self: cEmutecaConfig;
  var T: TStringList);
begin
  T := Self.CompressedExtensions;
end;

procedure cEmutecaConfigEmulatorsFile_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.EmulatorsFile;
end;

procedure cEmutecaConfigEmulatorsFile_W(Self: cEmutecaConfig; const T: string);
begin
  Self.EmulatorsFile := T;
end;

procedure cEmutecaConfigSystemsFile_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.SystemsFile;
end;

procedure cEmutecaConfigSystemsFile_W(Self: cEmutecaConfig; const T: string);
begin
  Self.SystemsFile := T;
end;

procedure cEmutecaConfigSysDataFolder_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.SysDataFolder;
end;

procedure cEmutecaConfigSysDataFolder_W(Self: cEmutecaConfig; const T: string);
begin
  Self.SysDataFolder := T;
end;

procedure cEmutecaConfigAutoSysFolder_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.AutoSysFolder;
end;

procedure cEmutecaConfigAutoSysFolder_W(Self: cEmutecaConfig; const T: string);
begin
  Self.AutoSysFolder := T;
end;

procedure cEmutecaConfigTagsFolder_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.TagsFolder;
end;

procedure cEmutecaConfigTagsFolder_W(Self: cEmutecaConfig; const T: string);
begin
  Self.TagsFolder := T;
end;

procedure cEmutecaConfigTempSubfolder_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.TempSubfolder;
end;

procedure cEmutecaConfigTempSubfolder_W(Self: cEmutecaConfig; const T: string);
begin
  Self.TempSubfolder := T;
end;

procedure cEmutecaConfigTempFile_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.TempFile;
end;

procedure cEmutecaConfigTempFile_W(Self: cEmutecaConfig; const T: string);
begin
  Self.TempFile := T;
end;

procedure cEmutecaConfigMinPlayTime_R(Self: cEmutecaConfig; var T: integer);
begin
  T := Self.MinPlayTime;
end;

procedure cEmutecaConfigMinPlayTime_W(Self: cEmutecaConfig; const T: integer);
begin
  Self.MinPlayTime := T;
end;

procedure RIRegister_cEmutecaConfig(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaConfig) do
  begin
    RegisterPropertyHelper(@cEmutecaConfigz7CMExecutable_R,
      @cEmutecaConfigz7CMExecutable_W, 'z7CMExecutable');
    RegisterPropertyHelper(@cEmutecaConfigz7GExecutable_R,
      @cEmutecaConfigz7GExecutable_W, 'z7GExecutable');
    RegisterPropertyHelper(@cEmutecaConfigCompressedExtensions_R,
      nil, 'CompressedExtensions');

    RegisterPropertyHelper(@cEmutecaConfigEmulatorsFile_R,
      @cEmutecaConfigEmulatorsFile_W, 'EmulatorsFile');
    RegisterPropertyHelper(@cEmutecaConfigSystemsFile_R,
      @cEmutecaConfigSystemsFile_W, 'SystemsFile');

    RegisterPropertyHelper(@cEmutecaConfigSysDataFolder_R,
      @cEmutecaConfigSysDataFolder_W, 'SysDataFolder');
    RegisterPropertyHelper(@cEmutecaConfigAutoSysFolder_R,
      @cEmutecaConfigAutoSysFolder_W, 'AutoSysFolder');
    RegisterPropertyHelper(@cEmutecaConfigTagsFolder_R,
      @cEmutecaConfigTagsFolder_W, 'TagsFolder');
    RegisterPropertyHelper(@cEmutecaConfigTempSubfolder_R,
      @cEmutecaConfigTempSubfolder_W, 'TempSubfolder');
    RegisterPropertyHelper(@cEmutecaConfigTempFile_R,
      @cEmutecaConfigTempFile_W, 'TempFile');
    RegisterPropertyHelper(@cEmutecaConfigMinPlayTime_R,
      @cEmutecaConfigMinPlayTime_W, 'MinPlayTime');
  end;
end;

procedure RIRegister_ucEmutecaConfig(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaConfig(CL);
end;

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaConfig]);
end;

{ TPSImport_ucEmutecaSoftList }

procedure TPSImport_ucEmutecaConfig.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaConfig(CompExec.comp);
end;

procedure TPSImport_ucEmutecaConfig.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaConfig(ri);
end;

end.
