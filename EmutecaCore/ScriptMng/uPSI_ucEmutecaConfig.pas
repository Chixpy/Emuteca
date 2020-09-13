unit uPSI_ucEmutecaConfig;
{< Exports of ucEmutecaConfig for Pascal Script engine of Emuteca.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2020 Chixpy

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
  SysUtils, Classes, LazFileUtils, LazUTF8, Graphics, uPSComponent, uPSRuntime,
  uPSCompiler,
  // CHX units
  uCHXStrUtils, uCHXRscStr, uCHX7zWrapper,
  // Emuteca Core classes
  ucEmutecaConfig;

procedure SIRegister_ucEmutecaConfig(CL: TPSPascalCompiler);
procedure SIRegister_cEmutecaConfig(CL: TPSPascalCompiler);

procedure RIRegister_ucEmutecaConfig(CL: TPSRuntimeClassImporter);
procedure RIRegister_cEmutecaConfig(CL: TPSRuntimeClassImporter);

implementation

procedure SIRegister_cEmutecaConfig(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TComponent', 'cEmutecaConfig') do
  with CL.AddClassN(CL.FindClass('TComponent'), 'caCHXConfig') do
  begin
    RegisterProperty('z7CMExecutable', 'string', iptrw);
    RegisterProperty('z7GExecutable', 'string', iptrw);
    RegisterProperty('EmulatorsFile', 'string', iptrw);;
    RegisterProperty('SystemsFile', 'string', iptrw);
    RegisterProperty('SysDataFolder', 'string', iptrw);
    RegisterProperty('AutoSysFolder', 'string', iptrw);
    RegisterProperty('TagsFolder', 'string', iptrw);
    RegisterProperty('CompressedExtensions', 'TStringList', iptr);
    RegisterProperty('TempSubfolder', 'string', iptrw);
    RegisterProperty('TempFile', 'string', iptrw);
    RegisterProperty('MinPlayTime', 'integer', iptrw);

    //RegisterMethod('Procedure LoadFromIni(aFileName : string)');
    //RegisterMethod('Procedure SaveToIni(aFilename : string)');
    //RegisterMethod('Procedure ResetDefaultConfig');
  end;
end;

procedure SIRegister_ucEmutecaConfig(CL: TPSPascalCompiler);
begin
  //CL.AddConstantN('krsIniSecConfig', 'String').SetString('Config');
  //CL.AddConstantN('krsIniKeyDataFolder', 'String').SetString('DataFolder');
  //CL.AddConstantN('krsIniKeySoftFile', 'String').SetString('SoftFile');
  //CL.AddConstantN('krsIniKeyEmulatorsFile', 'String').SetString(
  //  'EmulatorsFile');
  //CL.AddConstantN('krsIniKeyAutoSysFolders', 'String').SetString(
  //  'AutoSysFolders');
  //CL.AddConstantN('krsIniKeySystemsFile', 'String').SetString('SystemsFile');
  //CL.AddConstantN('krsIniKeySysDataFolder', 'String').SetString(
  //  'SysDataFolder');
  //CL.AddConstantN('krsIniKeyTagsFolder', 'String').SetString('TagsFolder');
  //CL.AddConstantN('krsIniSecTools', 'String').SetString('Tools');
  //CL.AddConstantN('krsIniKey7zCMExecutable', 'String').SetString(
  //  '7zCMExecutable');
  //CL.AddConstantN('krsIniKey7zGExecutable', 'String').SetString(
  //  '7zGExecutable');
  //CL.AddConstantN('krsIniSecExtensions', 'String').SetString('Extensions');
  //CL.AddConstantN('krsIniKeyCompressedExtensions', 'String').SetString(
  //  'CompressedExtensions');
  //CL.AddConstantN('krsIniSecTemp', 'String').SetString('Temp');
  //CL.AddConstantN('krsIniKeyTempSubfolder', 'String').SetString(
  //  'TempSubfolder');
  //CL.AddConstantN('krsIniKeyTempFile', 'String').SetString('TempFile');
  //CL.AddConstantN('krsIniSecMisc', 'String').SetString('Misc');
  //CL.AddConstantN('krsIniKeyMinPlayTime', 'String').SetString('MinPlayTime');
  SIRegister_cEmutecaConfig(CL);
end;

procedure cEmutecaConfigMinPlayTime_W(Self: cEmutecaConfig; const T: integer);
begin
  Self.MinPlayTime := T;
end;

procedure cEmutecaConfigMinPlayTime_R(Self: cEmutecaConfig; var T: integer);
begin
  T := Self.MinPlayTime;
end;

procedure cEmutecaConfigTempFile_W(Self: cEmutecaConfig; const T: string);
begin
  Self.TempFile := T;
end;

procedure cEmutecaConfigTempFile_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.TempFile;
end;

procedure cEmutecaConfigTempSubfolder_W(Self: cEmutecaConfig; const T: string);
begin
  Self.TempSubfolder := T;
end;

procedure cEmutecaConfigTempSubfolder_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.TempSubfolder;
end;

procedure cEmutecaConfigCompressedExtensions_R(Self: cEmutecaConfig;
  var T: TStringList);
begin
  T := Self.CompressedExtensions;
end;

procedure cEmutecaConfigTagsFolder_W(Self: cEmutecaConfig; const T: string);
begin
  Self.TagsFolder := T;
end;

procedure cEmutecaConfigTagsFolder_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.TagsFolder;
end;

procedure cEmutecaConfigAutoSysFolder_W(Self: cEmutecaConfig; const T: string);
begin
  Self.AutoSysFolder := T;
end;

procedure cEmutecaConfigAutoSysFolder_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.AutoSysFolder;
end;

procedure cEmutecaConfigSysDataFolder_W(Self: cEmutecaConfig; const T: string);
begin
  Self.SysDataFolder := T;
end;

procedure cEmutecaConfigSysDataFolder_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.SysDataFolder;
end;

procedure cEmutecaConfigSystemsFile_W(Self: cEmutecaConfig; const T: string);
begin
  Self.SystemsFile := T;
end;

procedure cEmutecaConfigSystemsFile_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.SystemsFile;
end;

procedure cEmutecaConfigEmulatorsFile_W(Self: cEmutecaConfig; const T: string);
begin
  Self.EmulatorsFile := T;
end;

procedure cEmutecaConfigEmulatorsFile_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.EmulatorsFile;
end;

procedure cEmutecaConfigz7GExecutable_W(Self: cEmutecaConfig; const T: string);
begin
  Self.z7GExecutable := T;
end;

procedure cEmutecaConfigz7GExecutable_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.z7GExecutable;
end;

procedure cEmutecaConfigz7CMExecutable_W(Self: cEmutecaConfig;
  const T: string);
begin
  Self.z7CMExecutable := T;
end;

procedure cEmutecaConfigz7CMExecutable_R(Self: cEmutecaConfig; var T: string);
begin
  T := Self.z7CMExecutable;
end;

procedure RIRegister_cEmutecaConfig(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaConfig) do
  begin
    //RegisterPropertyHelper(@cEmutecaConfigz7CMExecutable_R,
    //  @cEmutecaConfigz7CMExecutable_W, 'z7CMExecutable');
    //RegisterPropertyHelper(@cEmutecaConfigz7GExecutable_R,
    //  @cEmutecaConfigz7GExecutable_W, 'z7GExecutable');
    //RegisterPropertyHelper(@cEmutecaConfigEmulatorsFile_R,
    //  @cEmutecaConfigEmulatorsFile_W, 'EmulatorsFile');
    //RegisterPropertyHelper(@cEmutecaConfigSystemsFile_R,
    //  @cEmutecaConfigSystemsFile_W, 'SystemsFile');
    //RegisterPropertyHelper(@cEmutecaConfigSysDataFolder_R,
    //  @cEmutecaConfigSysDataFolder_W, 'SysDataFolder');
    //RegisterPropertyHelper(@cEmutecaConfigAutoSysFolder_R,
    //  @cEmutecaConfigAutoSysFolder_W, 'AutoSysFolder');
    //RegisterPropertyHelper(@cEmutecaConfigTagsFolder_R,
    //  @cEmutecaConfigTagsFolder_W, 'TagsFolder');
    //RegisterPropertyHelper(@cEmutecaConfigCompressedExtensions_R,
    //  nil, 'CompressedExtensions');
    //RegisterPropertyHelper(@cEmutecaConfigTempSubfolder_R,
    //  @cEmutecaConfigTempSubfolder_W, 'TempSubfolder');
    //RegisterPropertyHelper(@cEmutecaConfigTempFile_R,
    //  @cEmutecaConfigTempFile_W, 'TempFile');
    //RegisterPropertyHelper(@cEmutecaConfigMinPlayTime_R,
    //  @cEmutecaConfigMinPlayTime_W, 'MinPlayTime');
    //RegisterMethod(@cEmutecaConfig.LoadFromIni, 'LoadFromIni');
    //RegisterMethod(@cEmutecaConfig.SaveToIni, 'SaveToIni');
    //RegisterMethod(@cEmutecaConfig.ResetDefaultConfig, 'ResetDefaultConfig');
  end;
end;

procedure RIRegister_ucEmutecaConfig(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaConfig(CL);
end;

end.
