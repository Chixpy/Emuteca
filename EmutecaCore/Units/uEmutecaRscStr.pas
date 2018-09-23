unit uEmutecaRscStr;
{< Localizable strings unit of Emuteca Core.

  Mainly used for example GUI frames.

  ----

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
  // Emuteca Core units
  uEmutecaConst;

resourcestring

  // Misc
  rsNever = 'Never';
  rsUnknown = 'Unknown';
  rsFileAlreadyAdded = 'This file is already added.';
  rsCleaningSystemData = 'Cleaning system data...';

  // List action
  rsLoadingSystemList = 'Loading system list...';
  rsSavingSystemList = 'Saving system list...';
  rsImportingSystemList = 'Importing system list...';
  rsExportingSystemList = 'Exporting system list...';
  rsLoadingGroupList = 'Loading group list...';
  rsSavingGroupList = 'Saving group list...';
  rsImportingGroupList = 'Importing group list...';
  rsExportingGroupList = 'Exporting group list...';
  rsLoadingSoftList = 'Loading soft list...';
  rsSavingSoftList = 'Saving soft list...';
  rsImportingSoftList = 'Importing soft list...';
  rsExportingSoftList = 'Exporting soft list...';
  rsLoadingEmulatorList = 'Loading emulator list...';
  rsSavingEmulatorList = 'Saving emulator list...';
  rsImportingEmulatorList = 'Importing emulator list...';
  rsExportingEmulatorList = 'Exporting emulator list...';

  // Importing/Exporting Warnings
  rsImportingNoSHA1 =
    'Warning: Some info could not be imported because some files haven''t got SHA1 cached.'
    + LineEnding + '(%2:d/%3:d) %0:s%1:s';
  rsExportingNoSHA1 =
    'Warning: We can''t export because not all files have SHA1 cached.' +
    LineEnding + '(%2:d/%3:d) %0:s%1:s';

  // File mask descriptions
  // ----------------------
  rsFileMaskDescGroup =
    'Group file list (' + krsFileMaskGroup + ')';
  {< Description of file mask for group lists. }
  rsFileMaskDescSoft =
    'Soft file list (' + krsFileMaskSoft + ')';
  {< Description of file mask for soft lists. }
  rsFileMaskDescINI = 'Soft file list (' + krsFileMaskINI + ')';
  {< Description of file mask for ini databases (Systems, Emulators, Export/Import, ...). }
  rsFileMaskDescScript =
    'Soft file list (' + krsFileMaskScript + ')';
  {< Description of file mask for script files. }
  rsFileMaskDescTXT = 'Soft file list (' + krsFileMaskTXT + ')';
  {< Description of file mask for generic text files. }

  // Strings for DumpStatus, translatable
  // ------------------------------------
  rsEDSVerified = 'Verified';
  rsEDSGood = 'GoodDump';
  rsEDSAlternate = 'Alternate';
  rsEDSOverDump = 'OverDump';
  rsEDSBadDump = 'BadDump';
  rsEDSUnderDump = 'UnderDump';
  rsEDSUnknown = 'Unknown';
  rsEDSKeepValue = 'Keep value'; // Only for imports


  // Formated statistics
  rsFmtNGroups = '%0:d groups';
  rsFmtNVersions = '%0:d versions.';
  rsFmtNItems = '%1:d visible of %0:d items.';
  {<
     %0:d = Number of items.
     %1:d = Number of visible items.
  }

  rsFmtNTimes = '%0:d times.';
{< %0:d = Number of times. }

const
    EmutecaDumpStatusStr: array [TEmutecaDumpStatus] of string =
    (rsEDSVerified, rsEDSGood, rsEDSAlternate, rsEDSOverDump,
    rsEDSBadDump, rsEDSUnderDump, rsEDSUnknown, rsEDSKeepValue);
//< Strings for DumpStatus (localizable)


// Nothing to implement... XD
implementation

end.

