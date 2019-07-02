unit uPSI_uaEmutecaCustomEmu;
{< caEmutecaCustomEmu import for Pascal Script.

  Copyright (C) 2019-2019 Chixpy

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

// TODO: Clean this unit

interface

uses
  Classes, SysUtils, uPSComponent, uPSRuntime, uPSCompiler,
  uaEmutecaCustomEmu,
  ucEmutecaPlayingStats;

 type

  { TPSImport_uaEmutecaCustomEmu }

  TPSImport_uaEmutecaCustomEmu = class(TPSPlugin)
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;


{ compile-time registration functions }
procedure SIRegister_caEmutecaCustomEmu(CL: TPSPascalCompiler);
procedure SIRegister_uaEmutecaCustomEmu(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_caEmutecaCustomEmu(CL: TPSRuntimeClassImporter);
procedure RIRegister_uaEmutecaCustomEmu(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure SIRegister_caEmutecaCustomEmu(CL: TPSPascalCompiler);
begin
    //with RegClassS(CL,'caCHXStorableIni', 'caEmutecaCustomGroup') do
  with CL.AddClassN(CL.FindClass('caCHXStorableIni'),
      'caEmutecaCustomEmu') do
  begin
     RegisterMethod('Function Execute( GameFile : string) : integer');
    RegisterMethod('Function ExecuteAlone : integer');
    RegisterMethod('Procedure LoadFromIni( aIniFile : TCustomIniFile)');
    RegisterMethod('Procedure SaveToIni( aIniFile : TCustomIniFile; const ExportMode : boolean)');
    RegisterProperty('ID', 'string', iptrw);
    RegisterProperty('Enabled', 'boolean', iptrw);
    RegisterProperty('EmulatorName', 'string', iptrw);
    RegisterProperty('ExeFile', 'string', iptrw);
    RegisterProperty('WorkingFolder', 'string', iptrw);
    RegisterProperty('Parameters', 'string', iptrw);
    RegisterProperty('FileExt', 'TStringList', iptrw);
    RegisterProperty('ExitCode', 'integer', iptrw);
    RegisterProperty('Developer', 'string', iptrw);
    RegisterProperty('WebPage', 'string', iptrw);
    RegisterProperty('Icon', 'string', iptrw);
    RegisterProperty('InfoFile', 'string', iptrw);
    RegisterProperty('Stats', 'cEmutecaPlayingStats', iptr);
  end;
end;

procedure SIRegister_uaEmutecaCustomEmu(CL: TPSPascalCompiler);
begin
   CL.AddConstantN('krsEmulatorEnabledKey','String').SetString( 'Enabled');
 CL.AddConstantN('krsEmulatorNameKey','String').SetString( 'Name');
 CL.AddConstantN('krsEmulatorWorkingFoldeKey','String').SetString( 'WorkingFolder');
 CL.AddConstantN('krsEmulatorParametersKey','String').SetString( 'Parameters');
 CL.AddConstantN('krsEmulatorExitCodeKey','String').SetString( 'ExitCode');
 CL.AddConstantN('krsEmulatorExeFileKey','String').SetString( 'ExeFile');
 CL.AddConstantN('krsEmulatorFileExtKey','String').SetString( 'Extensions');
 CL.AddConstantN('krsEmulatorDeveloperKey','String').SetString( 'Developer');
 CL.AddConstantN('krsEmulatorWebPageKey','String').SetString( 'WebPage');
 CL.AddConstantN('krsEmulatorIconKey','String').SetString( 'Icon');
 CL.AddConstantN('krsEmulatorImageKey','String').SetString( 'Image');
 CL.AddConstantN('krsEmulatorInfoFileKey','String').SetString( 'InfoFile');
 CL.AddConstantN('kEmutecaEmuDirKey','String').SetString( '%EMUDIR%');
 CL.AddConstantN('kEmutecaRomDirKey','String').SetString( '%ROMDIR%');
 CL.AddConstantN('kEmutecaCurrentDirKey','String').SetString( '%CURRENTDIR%');
 CL.AddConstantN('kEmutecaROMPathKey','String').SetString( '%ROM%');
 CL.AddConstantN('kEmutecaROMFileNameKey','String').SetString( '%ROMNAME%');
 CL.AddConstantN('kEmutecaROMFileNameNoExtKey','String').SetString( '%ROMNAMENOEXT%');
 CL.AddConstantN('kEmutecaROMFileExtKey','String').SetString( '%ROMEXT%');
   SIRegister_caEmutecaCustomEmu(CL);
  CL.AddTypeS('TEmutecaReturnEmulatorCB', 'Function ( aEmulator : cEmutecaEmula'
   +'tor) : boolean');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuStats_R(Self: caEmutecaCustomEmu; var T: cEmutecaPlayingStats);
begin T := Self.Stats; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuInfoFile_W(Self: caEmutecaCustomEmu; const T: string);
begin Self.InfoFile := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuInfoFile_R(Self: caEmutecaCustomEmu; var T: string);
begin T := Self.InfoFile; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuIcon_W(Self: caEmutecaCustomEmu; const T: string);
begin Self.IconFile := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuIcon_R(Self: caEmutecaCustomEmu; var T: string);
begin T := Self.IconFile; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuWebPage_W(Self: caEmutecaCustomEmu; const T: string);
begin Self.WebPage := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuWebPage_R(Self: caEmutecaCustomEmu; var T: string);
begin T := Self.WebPage; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuDeveloper_W(Self: caEmutecaCustomEmu; const T: string);
begin Self.Developer := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuDeveloper_R(Self: caEmutecaCustomEmu; var T: string);
begin T := Self.Developer; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuExitCode_W(Self: caEmutecaCustomEmu; const T: integer);
begin Self.ExitCode := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuExitCode_R(Self: caEmutecaCustomEmu; var T: integer);
begin T := Self.ExitCode; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuFileExt_W(Self: caEmutecaCustomEmu; const T: TStringList);
begin Self.FileExt := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuFileExt_R(Self: caEmutecaCustomEmu; var T: TStringList);
begin T := Self.FileExt; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuParameters_W(Self: caEmutecaCustomEmu; const T: string);
begin Self.Parameters := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuParameters_R(Self: caEmutecaCustomEmu; var T: string);
begin T := Self.Parameters; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuWorkingFolder_W(Self: caEmutecaCustomEmu; const T: string);
begin Self.WorkingFolder := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuWorkingFolder_R(Self: caEmutecaCustomEmu; var T: string);
begin T := Self.WorkingFolder; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuExeFile_W(Self: caEmutecaCustomEmu; const T: string);
begin Self.ExeFile := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuExeFile_R(Self: caEmutecaCustomEmu; var T: string);
begin T := Self.ExeFile; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuEmulatorName_W(Self: caEmutecaCustomEmu; const T: string);
begin Self.Title := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuEmulatorName_R(Self: caEmutecaCustomEmu; var T: string);
begin T := Self.Title; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuEnabled_W(Self: caEmutecaCustomEmu; const T: boolean);
begin Self.Enabled := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuEnabled_R(Self: caEmutecaCustomEmu; var T: boolean);
begin T := Self.Enabled; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuID_W(Self: caEmutecaCustomEmu; const T: string);
begin Self.ID := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomEmuID_R(Self: caEmutecaCustomEmu; var T: string);
begin T := Self.ID; end;

procedure RIRegister_caEmutecaCustomEmu(CL: TPSRuntimeClassImporter);
begin
    with CL.Add(caEmutecaCustomEmu) do
  begin
    RegisterMethod(@caEmutecaCustomEmu.Execute, 'Execute');
    RegisterMethod(@caEmutecaCustomEmu.ExecuteAlone, 'ExecuteAlone');
    RegisterMethod(@caEmutecaCustomEmu.LoadFromIni, 'LoadFromIni');
    RegisterMethod(@caEmutecaCustomEmu.SaveToIni, 'SaveToIni');
    RegisterPropertyHelper(@caEmutecaCustomEmuID_R,@caEmutecaCustomEmuID_W,'ID');
    RegisterPropertyHelper(@caEmutecaCustomEmuEnabled_R,@caEmutecaCustomEmuEnabled_W,'Enabled');
    RegisterPropertyHelper(@caEmutecaCustomEmuEmulatorName_R,@caEmutecaCustomEmuEmulatorName_W,'EmulatorName');
    RegisterPropertyHelper(@caEmutecaCustomEmuExeFile_R,@caEmutecaCustomEmuExeFile_W,'ExeFile');
    RegisterPropertyHelper(@caEmutecaCustomEmuWorkingFolder_R,@caEmutecaCustomEmuWorkingFolder_W,'WorkingFolder');
    RegisterPropertyHelper(@caEmutecaCustomEmuParameters_R,@caEmutecaCustomEmuParameters_W,'Parameters');
    RegisterPropertyHelper(@caEmutecaCustomEmuFileExt_R,@caEmutecaCustomEmuFileExt_W,'FileExt');
    RegisterPropertyHelper(@caEmutecaCustomEmuExitCode_R,@caEmutecaCustomEmuExitCode_W,'ExitCode');
    RegisterPropertyHelper(@caEmutecaCustomEmuDeveloper_R,@caEmutecaCustomEmuDeveloper_W,'Developer');
    RegisterPropertyHelper(@caEmutecaCustomEmuWebPage_R,@caEmutecaCustomEmuWebPage_W,'WebPage');
    RegisterPropertyHelper(@caEmutecaCustomEmuIcon_R,@caEmutecaCustomEmuIcon_W,'Icon');
    RegisterPropertyHelper(@caEmutecaCustomEmuInfoFile_R,@caEmutecaCustomEmuInfoFile_W,'InfoFile');
    RegisterPropertyHelper(@caEmutecaCustomEmuStats_R,nil,'Stats');
  end;
end;

procedure RIRegister_uaEmutecaCustomEmu(CL: TPSRuntimeClassImporter);
begin
  RIRegister_caEmutecaCustomEmu(CL);
end;

procedure Register;
begin
   RegisterComponents('Pascal Script', [TPSImport_uaEmutecaCustomEmu]);
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

