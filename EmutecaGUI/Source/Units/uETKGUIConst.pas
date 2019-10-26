unit uETKGUIConst;
{< Constants unit.

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

const
  LazEmuTKDumpInfoIconFiles: array [0..6] of string =
    ('Fixed', 'Trainer', 'Translation', 'Pirate', 'Cracked',
    'Modified', 'Hack');
  {< Filenames for dump status icons. }

  krsEmuteca = 'Emuteca';
  {< Main 'Emuteca' string. }

  // TODO: Is it used only by Media Manager?
  krsVirtualFolderExt = '.(folder)';
  {< String to identify folders in Media Manager. }
  krsVirtualExt = '.(ext)';
  {< }

  krsETKGUISystemEditorID = 'frmETKGUIFullSysEditor';
  {< ID for (full) System Editor form. }
  krsETKGUIEmuEditorID = 'frmETKGUIFullEmuEditor';
  {< ID for (full) Emulator Editor form. }
  krsETKGUIConfigID = 'frmETKGUIConfig';
  {< ID for Emuteca GUI config form. }

implementation

end.
