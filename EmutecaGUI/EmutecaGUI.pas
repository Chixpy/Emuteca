program EmutecaGUI;
{< Main program of Emuteca GUI.

  This file is part of Emuteca GUI.

  Copyright (C) 2006-2019 Chixpy

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
{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mplayercontrollaz, lazcontrols, pascalscript,
  // CHX units
  uCHXDlgUtils, uCHXImageUtils, uCHXExecute, uCHXConst, uCHXRscStr,
  // CHX abstract classes
  uaCHXConfig,
  uaCHXStorable,
  // CHX frames
  ufCHXChkLstPropEditor,
  ufCHXFrame,
  ufCHXImgListPreview,
  ufCHXListPreview,
  ufCHXMultiFolderEditor,
  ufCHXPropEditor,
  ufCHXScriptManager,
  ufCHXFileListPreview,
  ufCHXTagTree,
  ufCHXTxtListPreview,
  // CHX Script Engine stuff
  uPSI_CHXBasic,
  ufSMAskMultiFile,
  // Emuteca units
  // Emuteca abstract classes
  uaEmutecaCustomGroup,
  uaEmutecaCustomManager,
  uaEmutecaCustomSoft,
  uaEmutecaCustomSystem,
  // Emuteca classes
  ucEmuteca,
  ucEmutecaConfig,
  ucEmutecaEmulator,
  ucEmutecaEmulatorList,
  ucEmutecaEmulatorManager,
  ucEmutecaGroup,
  ucEmutecaGroupList,
  ucEmutecaGroupManager,
  ucEmutecaScriptEngine,
  ucEmutecaSoftList,
  ucEmutecaSoftManager,
  ucEmutecaSoftware,
  ucEmutecaSystem,
  ucEmutecaSystemList,
  ucEmutecaSystemManager,
  // Emuteca frames
  ufEmutecaActAddFolder,
  ufEmutecaActAddSoft,
  ufEmutecaActExportSoftData,
  ufEmutecaActImportSoftData,
  ufEmutecaEmulatorCBX,
  ufEmutecaEmulatorEditor,
  ufEmutecaGroupCBX,
  ufEmutecaGroupEditor,
  ufEmutecaSoftEditor,
  ufEmutecaSoftTree,
  ufEmutecaSystemCBX,
  ufEmutecaSystemEditor,
  ufEmutecaSystemImgEditor,
  ufEmutecaSystemITFEditor,
  ufEmutecaSystemMVFEditor,
  // Emuteca threads
  utEmutecaGetSoftSHA1,
  // Emuteca GUI units
  uETKGUICommon,
  // Emuteca GUI abstracts
  uafETKGUISoftFoldersPreview,
  // Emuteca GUI classes
  ucETKGUIConfig,
  // Emuteca GUI frames
  ufCHXProgressBar,
  ufETKGUIEmuManager,
  ufETKGUIFullEmuEditor,
  ufETKGUIFullSoftEditor,
  ufETKGUIFullSysEditor,
  ufETKGUIIcnSoftTree,
  ufETKGUIIcnSysCBX,
  ufETKGUIMain,
  ufETKGUIMediaManager,
  ufETKGUIScriptManager,
  ufETKGUISoftImgPreview,
  ufETKGUISoftMedia,
  ufETKGUISoftTxtPreview,
  ufETKGUISysManager,
  ufETKGUISysPreview,
  ufETKGUIactMergeGroup,
  // Emuteca GUI forms
  ufrETKGUIAbout,
  ufrETKGUIMain,
  // Emuteca GUI threads
  utETKGUICacheGrpIcons, utETKGUICacheSysIcons, utETKGUICacheSoftIcons,
  ufETKGUISoftVideoPreview, ufCHXVideoListPreview, ufETKGUISoftMusicPreview,
  uEmutecaRscStr, uEmutecaConst, uaEmutecaCustomEmu, uPSI_uEmutecaConst,
  uPSI_uEmutecaRscStr, uETKGUIConst, uETKGUIRscStr, ufETKGUIFullConfigEditor,
  ufEmutecaSoftImgPreview, ufEmutecaSoftTxtPreview, ufEmutecaSoftVideoPreview,
  ufCHXImgViewer;

{$R *.res}

begin
  Application.Title:='Emuteca GUI';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmETKGUIMain, frmETKGUIMain);
  Application.Run;
end.
