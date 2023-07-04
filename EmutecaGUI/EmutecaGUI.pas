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
  Forms,
  mplayercontrollaz,
  lazcontrols,
  pascalscript,
  // CHX units
  uCHXConst,
  uCHXDlgUtils,
  uCHXExecute,
  uCHXImageUtils,
  uCHXMenuUtils,
  uCHXRscStr,
  // CHX abstract classes
  uaCHXConfig,
  uaCHXStorable,
  // CHX frames
  ufCHXAbout,
  ufCHXChkLstPropEditor,
  ufCHXFileListPreview,
  ufCHXFrame,
  ufCHXImgListPreview,
  ufCHXImgViewer,
  ufCHXListPreview,
  ufCHXMultiFolderEditor,
  ufCHXProgressBar,
  ufCHXPropEditor,
  ufCHXScriptManager,
  ufCHXTagTree,
  ufCHXTxtListPreview,
  ufCHXVideoListPreview,
  // CHX Pascal Script imported units
  uPSI_CHXBasic,
  uPSI_FPCDateUtils,
  uPSI_FPCFileUtil,
  uPSI_FPCLazUTF8,
  uPSI_FPCSysUtils,
  uPSI_uaCHXStorable,
  // Emuteca units
  uEmutecaConst,
  uEmutecaRscStr,
  // Emuteca abstract classes
  uaEmutecaCustomEmu,
  uaEmutecaCustomGroup,
  uaEmutecaCustomManager,
  uaEmutecaCustomSGItem,
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
  ucEmutecaTagsFile,
  // Emuteca frames
  ufEmutecaActAddFolder,
  ufEmutecaActAddSoft,
  ufEmutecaActExportSoftData,
  ufEmutecaActImportSoftData,
  ufEmutecaEmulatorAdvParamsEditor,
  ufEmutecaEmulatorCBX,
  ufEmutecaEmulatorEditor,
  ufEmutecaGroupCBX,
  ufEmutecaGroupEditor,
  ufEmutecaSoftEditor,
  ufEmutecaSoftImgPreview,
  ufEmutecaSoftTree,
  ufEmutecaSoftTxtPreview,
  ufEmutecaSoftVideoPreview,
  ufEmutecaSystemCBX,
  ufEmutecaSystemEditor,
  ufEmutecaSystemITFEditor,
  ufEmutecaSystemImgEditor,
  ufEmutecaSystemMVFEditor,
  ufEmutecaTagTree,
  // Emuteca Pascal Script imported units
  uPSI_uEmutecaConst,
  uPSI_uEmutecaGUIDialogs,
  uPSI_uEmutecaRscStr,
  uPSI_uaEmutecaCustomEmu,
  uPSI_uaEmutecaCustomSGItem,
  uPSI_ucEmutecaEmuList,
  uPSI_ucEmutecaGroupList,
  uPSI_ucEmutecaSoftList,
  uPSI_ucEmutecaSystemList,
  // Emuteca Pascal Script frames
  ufSMAskMultiFile,
  ufSMAskOption,
  // Emuteca threads
  utEmutecaGetSoftSHA1,
  utEmutecaRunEmulator,
  // ETKGUI units
  uETKGUICommon,
  uETKGUIConst,
  uETKGUIRscStr,
  // ETKGUI abstract classes
  uafETKGUISoftFoldersPreview,
  // ETKGUI classes
  ucETKGUIConfig,
  ucETKGUIItemCache,
  // ETKGUI frames
  ufETKGUIAbout,
  ufETKGUICompareSG,
  ufETKGUIEmuManager,
  ufETKGUIFullConfigEditor,
  ufETKGUIFullEmuEditor,
  ufETKGUIFullSoftEditor,
  ufETKGUIFullSysEditor,
  ufETKGUIIcnEmuCBX,
  ufETKGUIIcnSoftTree,
  ufETKGUIIcnSysCBX,
  ufETKGUIMain,
  ufETKGUIMediaManager,
  ufETKGUIScriptManager,
  ufETKGUISoftImgPreview,
  ufETKGUISoftMedia,
  ufETKGUISoftMusicPreview,
  ufETKGUISoftTxtPreview,
  ufETKGUISoftVideoPreview,
  ufETKGUISysManager,
  ufETKGUISysPreview,
  ufETKGUIactMergeGroup,
  // ETKGUI Forms
  ufrETKGUIMain,
  // ETKGUI threads
  utETKGUICacheEmuIcons,
  utETKGUICacheGrpIcons,
  utETKGUICacheSoftIcons,
  utETKGUICacheSysIcons;


{$R *.res}

begin
  Application.Title := 'Emuteca GUI';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmETKGUIMain, frmETKGUIMain);
  Application.Run;
end.
