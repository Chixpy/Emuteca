{ This file is part of Emuteca

  Copyright (C) 2006-2011 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{ Main program file. }
program LazEmuteca;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, pl_virtualtrees, pl_pascalscript,
  // Shared
  uRscStr, uConst,
  // Program forms
  fGameManager, fMediaManager, fImageViewer, fScriptManager, fProgressBar,
  fAbout, fSMAskFile, fSMAskFolder, fConfigManager, fSystemManager,
  fEmulatorManager,
  // Emuteca core
  uEmutecaConst, uEmutecaRscStr, uEmutecaMainManager, uEmutecaSystem,
  uEmutecaSystemManager, uEmutecaEmulatorManager, uEmutecaEmulator,
  uEmutecaGameGroup, uEmutecaScriptEngine, uEmutecaPlayingStats,
  // Pascal Script
  uPSI_uGame, uPSI_uGameGroup, uPSI_uGameManager, uPSI_u7zWrapper, uPSI_uSystem,
  uPSI_uEmulator, uPSI_uPlayingStats,
  // Custom
  uConfig, uCHXStrUtils, uCHXFileUtils, uCHXImageList, uCHXImageUtils,
  uGenericGroupManager, uEmutecaGroupManager;

{$R LazEmuteca.res}

begin
  Application.Title := 'Emuteca';
  Application.Initialize;
  Application.CreateForm(TfrmGameManager, frmGameManager);
  Application.Run;
end.
