{ Commons unit of LazEmuteca.

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
unit uLEmuTKCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uEmutecaCommon;

const
  LazEmuTKDumpInfoIconFiles: array [0..6] of string =
    ('Fixed', 'Trainer', 'Translation', 'Pirate', 'Cracked', 'Modified', 'Hack');

  krsEmuteca = 'Emuteca';
  {< Main 'Emuteca' String!!! }

  krsVirtualFolderExt = '.(folder)';
  krsVirtualExt = '.(ext)';

resourcestring
  rsFmtApplicationTitle = '%0:s %1:s';
  {<
    %0:s = 'Emuteca' (Application name).
    %1:s = Version.
  }


  rsUnknown = 'Unknown';

  // Game / Group property names
  // ---------------------------
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

  // Formated statistics
  // -------------------
  rsFmtNGroups = '%0:d groups';
  rsFmtNVersions = '%0:d versions.';
  rsFmtNItems = '%1:d visible of %0:d items.';
  {<
     %0:d = Number of items.
     %1:d = Number of visible items.
  }

  rsFmtNTimes = '%0:d times.';
  {< %0:d = Number of times. }

  // ERRORS & WARNINGS
  // -----------------
  rsFmtNotFound = 'Not Found.' + LineEnding + LineEnding +
    'Current folder: %0:s' + LineEnding + 'Searched file: %1:s';
  {<
     %0:d = Current folder.
     %1:d = File/folder searched.
  }
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



implementation

end.
