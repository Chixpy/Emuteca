unit uEmutecaRscStr;
{< Localizable strings unit of Emuteca Core.

  Mainly used for example frames.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2022 Chixpy

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

  // File actions
  rsFileAlreadyAdded = 'This file is already added.';
  rsChooseImageFileFormat =
    'Do you want to save it in a lossless format?' + LineEnding +
    'YES -> .png (lossless, for screenshots)' + LineEnding +
    'NO -> .jpg (better for photographs or scans)';
  rsConfirmOverwriteFile = '%0:s' + LineEnding +
    'The file already exists.' + LineEnding +
    'Do you want overwrite it?';


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
  rsCleaningSystemData = 'Cleaning system data...';

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
    'Group list files  (' + krsFileMaskGroup + ')';
  {< Description of file mask for group lists. }
  rsFileMaskDescSoft =
    'Soft list files  (' + krsFileMaskSoft + ')';
  {< Description of file mask for soft lists. }
  rsFileMaskDescINI = 'Database files (' + krsFileMaskINI + ')';
  {< Description of file mask for ini databases (Systems, Emulators, ...). }
  rsFileMaskDescScript =
    'Script files (' + krsFileMaskScript + ')';
  {< Description of file mask for script files. }
  rsFileMaskDescTXT = 'Text files (' + krsFileMaskTXT + ')';
  {< Description of file mask for generic text files. }

  // Strings for DumpStatus, translatable
  // ------------------------------------
  rsEDSFavorite = 'Favorite';
  rsEDSGood = 'GoodDump';
//  rsEDSAlternate = 'Alternate';
  rsEDSOverDump = 'OverDump';
  rsEDSBadDump = 'BadDump';
  rsEDSUnderDump = 'UnderDump';
  rsEDSUnknown = 'Unknown';
  rsEDSKeepValue = 'Keep value'; //< Only for imports


  // Formated statistics
  rsFmtNGroups = '%0:d groups';
  {< %0:d = Number of groups. }
  rsFmtNVersions = '%0:d versions.';
  {< %0:d = Number of versions. }
  rsFmtNItems = '%0:d items.';
  {< %0:d = Number of items. }
  rsFmtNTimes = '%0:d times.';
  {< %0:d = Number of times. }

const
    EmutecaDumpStatusStr: array [TEmutecaDumpStatus] of string =
    (rsEDSFavorite, rsEDSGood, {rsEDSAlternate,} rsEDSOverDump,
    rsEDSBadDump, rsEDSUnderDump, rsEDSUnknown, rsEDSKeepValue);
//< Strings for DumpStatus (localizable)


// Nothing to implement... XD
implementation

end.

