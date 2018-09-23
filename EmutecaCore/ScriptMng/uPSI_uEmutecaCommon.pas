unit uPSI_uEmutecaCommon;

{< Exports of uEmutecaCommon for Pascal Script engine of Emuteca.

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
  uEmutecaCommon;

procedure SIRegister_uEmutecaCommon(CL: TPSPascalCompiler);
procedure RIRegister_uEmutecaCommon_Routines(S: TPSExec);

implementation

procedure SIRegister_uEmutecaCommon(CL: TPSPascalCompiler);
begin
  CL.AddDelphiFunction(
    'function Str2SoftExportKey(aString: string): TEmutecaSoftExportKey');
  CL.AddDelphiFunction(
    'function SoftExportKey2StrK(aSOK: TEmutecaSoftExportKey): string');

  CL.AddDelphiFunction(
    'function Key2DumpSt(aString: string): TEmutecaDumpStatus');
  CL.AddDelphiFunction(
    'function DumpSt2Key(aEDS: TEmutecaDumpStatus): string');
  CL.AddDelphiFunction(
    'function DumpSt2Str(aEDS: TEmutecaDumpStatus): string');
  CL.AddDelphiFunction(
    'function DumpSt2StrK(aEDS: TEmutecaDumpStatus): string');

  CL.AddDelphiFunction(
    'procedure EmuTKSearchAllRelatedFiles(OutFileList: TStrings; ' +
    'aFolder: string; aFileName: string; Extensions: TStrings; ' +
    'SearchInComp: boolean; DecompressFolder: string)');
  CL.AddDelphiFunction(
    'function EmuTKSearchFirstRelatedFile(aFolder: string; ' +
    'aFileName: string; Extensions: TStrings; SearchInComp: boolean; ' +
    'AutoDecompress: boolean; DecompressFolder: string): string');
  CL.AddDelphiFunction(
    'procedure EmuTKSearchAllFilesByNameExtCT(aFileList: TStrings; ' +
    'aBaseFileName: string; aExtList: string)');
  CL.AddDelphiFunction(
    'procedure EmuTKSearchAllFilesByNameExtSL(aFileList: TStrings; ' +
    'aBaseFileName: string; aExtList: TStrings)');
  CL.AddDelphiFunction(
    'function EmuTKSearchFirstFileByNameExtCT(aBaseFileName: string; ' +
    'aExtList: string): string');
  CL.AddDelphiFunction(
    'function EmuTKSearchFirstFileByNameExtSL(aBaseFileName: string; ' +
    'aExtList: TStrings): string');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure RIRegister_uEmutecaCommon_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@Str2SoftExportKey, 'Str2SoftExportKey',
    cdRegister);
  S.RegisterDelphiFunction(@SoftExportKey2StrK, 'SoftExportKey2StrK',
    cdRegister);

  S.RegisterDelphiFunction(@Key2DumpSt, 'Key2DumpSt', cdRegister);
  S.RegisterDelphiFunction(@DumpSt2Key, 'DumpSt2Key', cdRegister);
  S.RegisterDelphiFunction(@DumpSt2Str, 'DumpSt2Str', cdRegister);
  S.RegisterDelphiFunction(@DumpSt2StrK, 'DumpSt2StrK', cdRegister);

  S.RegisterDelphiFunction(@EmuTKSearchAllRelatedFiles,
    'EmuTKSearchAllRelatedFiles', cdRegister);
  S.RegisterDelphiFunction(@EmuTKSearchFirstRelatedFile,
    'EmuTKSearchFirstRelatedFile', cdRegister);
  S.RegisterDelphiFunction(@EmuTKSearchAllFilesByNameExtCT,
    'EmuTKSearchAllFilesByNameExtCT', cdRegister);
  S.RegisterDelphiFunction(@EmuTKSearchAllFilesByNameExtSL,
    'EmuTKSearchAllFilesByNameExtSL', cdRegister);
  S.RegisterDelphiFunction(@EmuTKSearchFirstFileByNameExtCT,
    'EmuTKSearchFirstFileByNameExtCT', cdRegister);
  S.RegisterDelphiFunction(@EmuTKSearchFirstFileByNameExtSL,
    'EmuTKSearchFirstFileByNameExtSL', cdRegister);
end;

end.
