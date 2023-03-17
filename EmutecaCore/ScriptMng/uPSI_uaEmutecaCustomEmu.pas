unit uPSI_uaEmutecaCustomEmu;

{< caEmutecaCustomEmu import for Pascal Script.

  Copyright (C) 2019-2023 Chixpy

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
  Classes, SysUtils, uPSComponent, uPSRuntime, uPSCompiler,
  // Emuteca Core abstracts
  uaEmutecaCustomEmu,
  // Emuteca Core classes
  ucEmutecaPlayingStats;

type

  { TPSImport_uaEmutecaCustomEmu }

  TPSImport_uaEmutecaCustomEmu = class(TPSPlugin)
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;


{ Compile-time registration functions }
procedure SIRegister_caEmutecaCustomEmu(CL: TPSPascalCompiler);
procedure SIRegister_uaEmutecaCustomEmu(CL: TPSPascalCompiler);

{ Run-time registration functions }
procedure RIRegister_caEmutecaCustomEmu(CL: TPSRuntimeClassImporter);
procedure RIRegister_uaEmutecaCustomEmu(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_uaEmutecaCustomEmu]);
end;

procedure SIRegister_caEmutecaCustomEmu(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorableIni', 'caEmutecaCustomGroup') do
  with CL.AddClassN(CL.FindClass('caCHXStorableIni'), 'caEmutecaCustomEmu') do
  begin
    RegisterProperty('ID', 'string', iptrw);
    RegisterProperty('Enabled', 'boolean', iptrw);
    RegisterProperty('Title', 'string', iptrw);
    RegisterProperty('ExeFile', 'string', iptrw);
    RegisterProperty('WorkingFolder', 'string', iptrw);
    RegisterProperty('Parameters', 'string', iptrw);

    RegisterProperty('CoreIDKey', 'string', iptrw);
    RegisterProperty('CoreIDParamFormat', 'string', iptrw);
    RegisterProperty('ExtensionParamFormat', 'TStringList', iptr);
    RegisterProperty('ExtraParamFormat', 'TStringList', iptr);

    RegisterProperty('FileExt', 'TStringList', iptr);
    RegisterProperty('ExitCode', 'integer', iptrw);

    RegisterProperty('Developer', 'string', iptrw);
    RegisterProperty('WebPage', 'string', iptrw);
    RegisterProperty('IconFile', 'string', iptrw);
    RegisterProperty('InfoFile', 'string', iptrw);

    RegisterProperty('Stats', 'cEmutecaPlayingStats', iptr);

    RegisterMethod('function CompareID(aID: string): integer;');
    RegisterMethod('function MatchID(aID: string): boolean;');

    RegisterMethod('function Execute(GameFile : string) : integer;');
    RegisterMethod('function ExecuteAlone : integer;');
  end;
end;

procedure SIRegister_uaEmutecaCustomEmu(CL: TPSPascalCompiler);
begin
  CL.AddConstantN('krsEmutecaEmuDirKey', 'string').SetString('%EMUDIR%');
  CL.AddConstantN('krsEmutecaROMDirKey', 'string').SetString('%ROMDIR%');
  CL.AddConstantN('krsEmutecaCurrentDirKey', 'string').SetString(
    '%CURRENTDIR%');

  CL.AddConstantN('krsEmutecaROMPathKey', 'string').SetString('%ROM%');
  CL.AddConstantN('krsEmutecaROMFileNameKey', 'string').SetString('%ROMNAME%');
  CL.AddConstantN('krsEmutecaROMFileNameNoExtKey',
    'string').SetString('%ROMNAMENOEXT%');
  CL.AddConstantN('krsEmutecaROMFileExtKey', 'string').SetString('%ROMEXT%');


  CL.AddConstantN('krsEmutecaROMSysIDKey', 'string').SetString('%SYSID%');
  CL.AddConstantN('krsEmutecaROMExtensionParamKey',
    'string').SetString('%EXTPARAM%');
  CL.AddConstantN('krsEmutecaROMExtraParamKey', 'string').SetString('%EXTRA%');

  SIRegister_caEmutecaCustomEmu(CL);
end;

procedure caEmutecaCustomEmuID_R(Self: caEmutecaCustomEmu; var T: string);
begin
  T := Self.ID;
end;

procedure caEmutecaCustomEmuID_W(Self: caEmutecaCustomEmu; const T: string);
begin
  Self.ID := T;
end;

procedure caEmutecaCustomEmuEnabled_R(Self: caEmutecaCustomEmu;
  var T: boolean);
begin
  T := Self.Enabled;
end;

procedure caEmutecaCustomEmuEnabled_W(Self: caEmutecaCustomEmu;
  const T: boolean);
begin
  Self.Enabled := T;
end;

procedure caEmutecaCustomEmuTitle_R(Self: caEmutecaCustomEmu; var T: string);
begin
  T := Self.Title;
end;

procedure caEmutecaCustomEmuTitle_W(Self: caEmutecaCustomEmu;
  const T: string);
begin
  Self.Title := T;
end;

procedure caEmutecaCustomEmuExeFile_R(Self: caEmutecaCustomEmu; var T: string);
begin
  T := Self.ExeFile;
end;

procedure caEmutecaCustomEmuExeFile_W(Self: caEmutecaCustomEmu;
  const T: string);
begin
  Self.ExeFile := T;
end;

procedure caEmutecaCustomEmuWorkingFolder_R(Self: caEmutecaCustomEmu;
  var T: string);
begin
  T := Self.WorkingFolder;
end;

procedure caEmutecaCustomEmuWorkingFolder_W(Self: caEmutecaCustomEmu;
  const T: string);
begin
  Self.WorkingFolder := T;
end;

procedure caEmutecaCustomEmuParameters_R(Self: caEmutecaCustomEmu;
  var T: string);
begin
  T := Self.Parameters;
end;

procedure caEmutecaCustomEmuParameters_W(Self: caEmutecaCustomEmu;
  const T: string);
begin
  Self.Parameters := T;
end;

procedure caEmutecaCustomEmuCoreIDKey_R(Self: caEmutecaCustomEmu;
  var T: string);
begin
  T := Self.CoreIDKey;
end;

procedure caEmutecaCustomEmuCoreIDKey_W(Self: caEmutecaCustomEmu;
  const T: string);
begin
  Self.CoreIDKey := T;
end;

procedure caEmutecaCustomEmuCoreIDParamFormat_R(Self: caEmutecaCustomEmu;
  var T: string);
begin
  T := Self.CoreIDParamFormat;
end;

procedure caEmutecaCustomEmuCoreIDParamFormat_W(Self: caEmutecaCustomEmu;
  const T: string);
begin
  Self.CoreIDParamFormat := T;
end;

procedure caEmutecaCustomEmuExtensionParamFormat_R(Self: caEmutecaCustomEmu;
  var T: TStringList);
begin
  T := Self.ExtensionParamFormat;
end;

procedure caEmutecaCustomEmuExtraParamFormat_R(Self: caEmutecaCustomEmu;
  var T: TStringList);
begin
  T := Self.ExtraParamFormat;
end;

procedure caEmutecaCustomEmuFileExt_R(Self: caEmutecaCustomEmu;
  var T: TStringList);
begin
  T := Self.FileExt;
end;

procedure caEmutecaCustomEmuExitCode_R(Self: caEmutecaCustomEmu;
  var T: integer);
begin
  T := Self.ExitCode;
end;

procedure caEmutecaCustomEmuExitCode_W(Self: caEmutecaCustomEmu;
  const T: integer);
begin
  Self.ExitCode := T;
end;

procedure caEmutecaCustomEmuDeveloper_R(Self: caEmutecaCustomEmu;
  var T: string);
begin
  T := Self.Developer;
end;

procedure caEmutecaCustomEmuDeveloper_W(Self: caEmutecaCustomEmu;
  const T: string);
begin
  Self.Developer := T;
end;

procedure caEmutecaCustomEmuWebPage_R(Self: caEmutecaCustomEmu; var T: string);
begin
  T := Self.WebPage;
end;

procedure caEmutecaCustomEmuWebPage_W(Self: caEmutecaCustomEmu;
  const T: string);
begin
  Self.WebPage := T;
end;

procedure caEmutecaCustomEmuIconFile_R(Self: caEmutecaCustomEmu;
  var T: string);
begin
  T := Self.IconFile;
end;

procedure caEmutecaCustomEmuIconFile_W(Self: caEmutecaCustomEmu;
  const T: string);
begin
  Self.IconFile := T;
end;

procedure caEmutecaCustomEmuInfoFile_R(Self: caEmutecaCustomEmu;
  var T: string);
begin
  T := Self.InfoFile;
end;

procedure caEmutecaCustomEmuInfoFile_W(Self: caEmutecaCustomEmu;
  const T: string);
begin
  Self.InfoFile := T;
end;

procedure caEmutecaCustomEmuStats_R(Self: caEmutecaCustomEmu;
  var T: cEmutecaPlayingStats);
begin
  T := Self.Stats;
end;

procedure RIRegister_caEmutecaCustomEmu(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(caEmutecaCustomEmu) do
  begin
    RegisterPropertyHelper(@caEmutecaCustomEmuID_R,
      @caEmutecaCustomEmuID_W, 'ID');
    RegisterPropertyHelper(@caEmutecaCustomEmuEnabled_R,
      @caEmutecaCustomEmuEnabled_W, 'Enabled');
    RegisterPropertyHelper(@caEmutecaCustomEmuTitle_R,
      @caEmutecaCustomEmuTitle_W, 'Title');
    RegisterPropertyHelper(@caEmutecaCustomEmuExeFile_R,
      @caEmutecaCustomEmuExeFile_W, 'ExeFile');
    RegisterPropertyHelper(@caEmutecaCustomEmuWorkingFolder_R,
      @caEmutecaCustomEmuWorkingFolder_W, 'WorkingFolder');
    RegisterPropertyHelper(@caEmutecaCustomEmuParameters_R,
      @caEmutecaCustomEmuParameters_W, 'Parameters');

    RegisterPropertyHelper(@caEmutecaCustomEmuCoreIDKey_R,
      @caEmutecaCustomEmuCoreIDKey_W, 'CoreIDKey');
    RegisterPropertyHelper(@caEmutecaCustomEmuCoreIDParamFormat_R,
      @caEmutecaCustomEmuCoreIDParamFormat_W, 'CoreIDParamFormat');
    RegisterPropertyHelper(@caEmutecaCustomEmuExtensionParamFormat_R,
      nil, 'ExtensionParamFormat');
    RegisterPropertyHelper(@caEmutecaCustomEmuExtraParamFormat_R,
      nil, 'ExtraParamFormat');

    RegisterPropertyHelper(@caEmutecaCustomEmuFileExt_R, nil, 'FileExt');
    RegisterPropertyHelper(@caEmutecaCustomEmuExitCode_R,
      @caEmutecaCustomEmuExitCode_W, 'ExitCode');

    RegisterPropertyHelper(@caEmutecaCustomEmuDeveloper_R,
      @caEmutecaCustomEmuDeveloper_W, 'Developer');
    RegisterPropertyHelper(@caEmutecaCustomEmuWebPage_R,
      @caEmutecaCustomEmuWebPage_W, 'WebPage');
    RegisterPropertyHelper(@caEmutecaCustomEmuIconFile_R,
      @caEmutecaCustomEmuIconFile_W, 'IconFile');
    RegisterPropertyHelper(@caEmutecaCustomEmuInfoFile_R,
      @caEmutecaCustomEmuInfoFile_W, 'InfoFile');

    RegisterPropertyHelper(@caEmutecaCustomEmuStats_R, nil, 'Stats');

    RegisterMethod(@caEmutecaCustomEmu.CompareID, 'CompareID');
    RegisterMethod(@caEmutecaCustomEmu.MatchID, 'MatchID');

    RegisterMethod(@caEmutecaCustomEmu.Execute, 'Execute');
    RegisterMethod(@caEmutecaCustomEmu.ExecuteAlone, 'ExecuteAlone');
  end;
end;

procedure RIRegister_uaEmutecaCustomEmu(CL: TPSRuntimeClassImporter);
begin
  RIRegister_caEmutecaCustomEmu(CL);
end;

{ TPSImport_uaEmutecaCustomEmu }

procedure TPSImport_uaEmutecaCustomEmu.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_uaEmutecaCustomEmu(CompExec.comp);
end;

procedure TPSImport_uaEmutecaCustomEmu.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_uaEmutecaCustomEmu(ri);
end;

end.
