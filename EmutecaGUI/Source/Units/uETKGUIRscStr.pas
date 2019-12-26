unit uETKGUIRscStr;

{< Resource strings unit.

  This file is part of Emuteca GUI.

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

resourcestring
  rsFmtApplicationTitle = '%0:s %1:s';
  {< Aplication title used in forms, formated by
       uEmutecaCommon.krsFmtWindowCaption.

     @param(%0:s 'Emuteca' (Application name)).
     @param(%1:s Version).
  }

  rsETKGUISystemEditorTitle = 'System Editor';
  {< Title for (full) System Editor form. }
  rsETKGUIEmuEditorTitle = 'Emulator Editor';
  {< Title for (full) Emulator Editor form. }
  rsETKGUIConfigTitle = 'Config Editor';
  {< Title for (full) Config Editor form. }

  rsGameKey = 'Key';
  {< Game Key property. }
  rsZones = 'Zones';
  {< Game Zones property. }
  rsDeveloper = 'Developer';
  {< Game Developer property. }
  rsPublisher = 'Publisher';
  {< Game Publisher property. }
  rsVersion = 'Version';
  {< Game Version property. }
  rsFilename = 'Filename';
  {< Game Filename property. }

  rsFmtNotFound = 'Not Found.' + LineEnding + LineEnding +
    'Current folder: %0:s' + LineEnding + 'Searched file: %1:s';
  {< File not found.

     %0:d = Current folder.
     %1:d = File/folder searched.
  }
  rsEmuWebEmpty = 'Emulator''s webpage not configured';
  {< Emulator's web page is empty. }
  rsWarning = 'Warning';
  rsAutoFolderWarning = 'This action will:' + LineEnding +
    '* Save current system data.' + LineEnding +
    '* Create many folders in "%0:s".' + LineEnding +
    '* Change all defined image, music, video folders with created ones.' +
    LineEnding + LineEnding + '¿Are you sure?';
  rsSoftEditorCaption = 'Soft Editor';
  rsSoftMediaCaption = 'Soft Media';
  rsTagsCaption = 'Tags';
  rsSystemCaption = 'System';
  rsBasicCaption  = 'Basic';
  rsAdvancedCaption = 'Advanced';

  rsRunSoftwareUnkError = 'RunSoftware: Unknown Error.' +
    LineEnding + '%0:s' + LineEnding + '%1:s';
  rsRunSoftwareNoSoft = 'RunSoftware: Software = nil.';
  rsRunSoftwareNoEmu = 'RunSoftware: Emulator = nil.' + LineEnding +
    '%0:s' + LineEnding + '%1:s';
  rsRunSoftwareNoSoftFile =
    'RunSoftware: Soft file not found.' + LineEnding + '%0:s' +
    LineEnding + '%1:s';
  rsRunSoftwareNoEmuExe = 'RunSoftware: Emulator executable not found';
  rsRunSoftware7zError =
    'TfmLEmuTKMain.RunSoftware: Unknown decompress error.';
  rsRunSoftwareExitCode = 'TfmLEmuTKMain.RunSoftware: Emulator returned: %0:d';

  rsAskDeleteItem = 'Do you want to delete this item from Emuteca?' + LineEnding + '%0:s';

implementation

end.
