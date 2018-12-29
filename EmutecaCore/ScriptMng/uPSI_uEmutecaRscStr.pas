unit uPSI_uEmutecaRscStr;
 {< Exports of uEmutecaRscStr for Pascal Script engine of Emuteca.

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
    uEmutecaRscStr;

  procedure SIRegister_uEmutecaRscStr(CL: TPSPascalCompiler);
  procedure RIRegister_uEmutecaRscStr_Routines(S: TPSExec);

implementation

procedure SIRegister_uEmutecaRscStr(CL: TPSPascalCompiler);
begin
  // Misc
  // ----
  CL.AddConstantN('rsNever', 'String').SetString(rsNever);
  CL.AddConstantN('rsUnknown', 'String').SetString(rsUnknown);

  // File actions
  // ------------
  CL.AddConstantN('rsFileAlreadyAdded', 'String').SetString(
    rsFileAlreadyAdded);
  CL.AddConstantN('rsChooseImageFileFormat', 'String').SetString(
    rsChooseImageFileFormat);
  CL.AddConstantN('rsConfirmOverwriteFile', 'String').SetString(
    rsConfirmOverwriteFile);
  CL.AddConstantN('rsCorfirmDeleteFile', 'String').SetString(
    rsCorfirmDeleteFile);
  CL.AddConstantN('rsErrorDeletingFile', 'String').SetString(
    rsErrorDeletingFile);

  // List action
  // -----------
  CL.AddConstantN('rsLoadingSystemList', 'String').SetString(
    rsLoadingSystemList);
  CL.AddConstantN('rsSavingSystemList', 'String').SetString(
    rsSavingSystemList);
  CL.AddConstantN('rsImportingSystemList', 'String').SetString(
    rsImportingSystemList);
  CL.AddConstantN('rsExportingSystemList', 'String').SetString(
    rsExportingSystemList);
  CL.AddConstantN('rsLoadingGroupList', 'String').SetString(
    rsLoadingGroupList);
  CL.AddConstantN('rsSavingGroupList', 'String').SetString(rsSavingGroupList);
  CL.AddConstantN('rsImportingGroupList', 'String').SetString(
    rsImportingGroupList);
  CL.AddConstantN('rsExportingGroupList', 'String').SetString(
    rsExportingGroupList);
  CL.AddConstantN('rsLoadingSoftList', 'String').SetString(rsLoadingSoftList);
  CL.AddConstantN('rsSavingSoftList', 'String').SetString(rsSavingSoftList);
  CL.AddConstantN('rsImportingSoftList', 'String').SetString(
    rsImportingSoftList);
  CL.AddConstantN('rsExportingSoftList', 'String').SetString(
    rsExportingSoftList);
  CL.AddConstantN('rsLoadingEmulatorList', 'String').SetString(rsLoadingEmulatorList);
  CL.AddConstantN('rsSavingEmulatorList', 'String').SetString(rsSavingEmulatorList);
  CL.AddConstantN('rsImportingEmulatorList', 'String').SetString(rsImportingEmulatorList);
  CL.AddConstantN('rsExportingEmulatorList', 'String').SetString(
    rsExportingEmulatorList);
  CL.AddConstantN('rsCleaningSystemData', 'String').SetString(
    rsCleaningSystemData);

  // Importing/Exporting Warnings
  CL.AddConstantN('rsImportingNoSHA1', 'String').SetString(rsImportingNoSHA1);
  CL.AddConstantN('rsExportingNoSHA1', 'String').SetString(rsExportingNoSHA1);

  // File mask descriptions
  // ----------------------
  CL.AddConstantN('rsFileMaskDescGroup', 'String').SetString(
    rsFileMaskDescGroup);
  CL.AddConstantN('rsFileMaskDescSoft', 'String').SetString(
    rsFileMaskDescSoft);
  CL.AddConstantN('rsFileMaskDescINI', 'String').SetString(rsFileMaskDescINI);
  CL.AddConstantN('rsFileMaskDescScript', 'String').SetString(
    rsFileMaskDescScript);
  CL.AddConstantN('rsFileMaskDescTXT', 'String').SetString(rsFileMaskDescTXT);

  // Strings for DumpStatus, translatable
  // ------------------------------------
  CL.AddConstantN('rsEDSVerified', 'String').SetString(rsEDSVerified);
  CL.AddConstantN('rsEDSGood', 'String').SetString(rsEDSGood);
  CL.AddConstantN('rsEDSAlternate', 'String').SetString(rsEDSAlternate);
  CL.AddConstantN('rsEDSOverDump', 'String').SetString(rsEDSOverDump);
  CL.AddConstantN('rsEDSBadDump', 'String').SetString(rsEDSBadDump);
  CL.AddConstantN('rsEDSUnderDump', 'String').SetString(rsEDSUnderDump);
  CL.AddConstantN('rsEDSUnknown', 'String').SetString(rsEDSUnknown);
  CL.AddConstantN('rsEDSKeepValue', 'String').SetString(rsEDSKeepValue);


  // Formated statistics
  // -------------------
  CL.AddConstantN('rsFmtNGroups', 'String').SetString(rsFmtNGroups);
  CL.AddConstantN('rsFmtNVersions', 'String').SetString(rsFmtNVersions);
  CL.AddConstantN('rsFmtNItems', 'String').SetString(rsFmtNItems);
  CL.AddConstantN('rsFmtNTimes', 'String').SetString(rsFmtNTimes);
end;

procedure RIRegister_uEmutecaRscStr_Routines(S: TPSExec);
begin

end;


end.
