program LazEmuteca;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bs_controls, lz_rtticontrols, pl_pascalscript, pl_virtualtrees,
  ucEmuteca, ucEmutecaGroup, ucEmutecaConfig, ufrLEmuTKMain,
  ucEmutecaEmulatorManager, ucEmutecaGroupManager, ucEmutecaSystemManager,
  ucEmutecaEmulator, ucEmutecaSoftware, ucEmutecaSystem, 
  ucEmutecaSoftManager, uGUIConfig, uaCHXStorable, uaCHXConfig,
  ufEmutecaSystemImgEditor, ucEmutecaScriptEngine, ufESMSoftList,
  ufESMGroupList, ufEmutecaSystemEditor,
  ufLEmuTKFullSystemEditor, ufCHXChkLstPropEditor, ufLEmuTKSysManager,
  ufLEmuTKEmuManager, ufEmutecaSoftEditor, ufLEmuTKFullEmuEditor,
  ufEmutecaEmulatorEditor, ufEmutecaActAddSoft, ufCHXForm,
  ufEmutecaActAddFolder, ucEmutecaSoftList, ucEmutecaSystemList,
  ucEmutecaGroupList, ucEmutecaEmulatorList, uaEmutecaCustomGroup,
  uaEmutecaCustomSoft, uaEmutecaCustomSystem, ufrLEmuTKAbout, ufLEmuTKIcnSysCBX,
  ufCHXFrame, ufCHXPropEditor, ufEmutecaSoftTree, ufLEmuTKIcnSoftTree,
  ufCHXImgViewer, ufCHXTagTree, ufLEmuTKSysPreview, ufCHXListPreview,
  ufCHXMultiFolderEditor, ufEmutecaSystemMVFEditor, ufLEmuTKSoftMedia,
  ufLEmuTKMain, uLEmuTKCommon, uafLEmuTKSoftFoldersPreview,
  ufLEmuTKSoftTxtPreview, ufCHXStrLstPreview, ufCHXImgListPreview,
  ufCHXTxtListPreview, uCHXDlgUtils, ufLEmuTKSoftImgPreview, ufEmutecaSystemCBX,
  ufEmutecaSystemITFEditor, ufLEmuTKMediaManager, ufEmutecaActExportSoftData,
  uaEmutecaCustomManager, ufEmutecaActImportSoftData, ufCHXScriptManager, 
ufLEmuTKScriptManager, ufEmutecaGroupEditor, ufLEmuTKFullSoftEditor, 
ufEmutecaGroupCBX, ufSMAskMultiFile;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmLEmuTKMain, frmLEmuTKMain);
  Application.Run;
end.

